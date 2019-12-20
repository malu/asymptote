/* Asymptote, a UCI chess engine
   Copyright (C) 2018-2019  Maximilian Lupke

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

use std::cmp;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;

use crate::bitboard::{Square, ALL_SQUARES};
use crate::eval::*;
use crate::history::History;
use crate::movegen::Piece;
use crate::movepick::MovePicker;
use crate::position::Position;
use crate::types::SquareMap;

// # Texel Tuning
//
// We aim to minimize the following sum:
//     1/N * \sum_i^N (r_i - \sigma(q_i))^2.
// Here,
//     \sigma(q) := (1 + 10^{-K q / 400})^{-1}
// is the sigmoid function where K is some scaling constant, and q_i is the
// quiescence score of the i-th position.
//
// For q = \sum_j p_j * x_j, we have
//     dq/dp_j = x_j,
//     d\sigma(q)/dp_j
//         = dq/dp_j * d\sigma(q)/dq
//         = x_j     * \sigma(q) * (1 - \sigma(q))
//     d\sum_i (r_i - \sigma(q_i))^2 / dp_j
//         = \sum_i 2 * (r_i - \sigma(q_i)) * - (x_j * \sigma(q) * (1-\sigma(q)))
//         = -2 \sum_i x_j * (r_i - \sigma(q_i)) * \sigma(q) * (1-\sigma(q))

const TUNE_MATERIAL_PAWN: bool = false;
const TUNE_MATERIAL_KNIGHT: bool = false;
const TUNE_MATERIAL_BISHOP: bool = false;
const TUNE_MATERIAL_ROOK: bool = false;
const TUNE_MATERIAL_QUEEN: bool = false;

const TUNE_MOBILITY_PAWN: bool = false;
const TUNE_MOBILITY_KNIGHT: bool = false;
const TUNE_MOBILITY_BISHOP: bool = false;
const TUNE_MOBILITY_ROOK: bool = false;
const TUNE_MOBILITY_QUEEN: bool = false;

const TUNE_PAWNS_DOUBLED: bool = false;
const TUNE_PAWNS_OPEN_ISOLATED: bool = false;
const TUNE_PAWNS_ISOLATED: bool = false;
const TUNE_PAWNS_PASSED: bool = false;

const TUNE_BISHOPS_XRAY: bool = false;
const TUNE_BISHOPS_PAIR: bool = false;

const TUNE_ROOKS_OPEN_FILE: bool = false;
const TUNE_ROOKS_HALFOPEN_FILE: bool = false;
const TUNE_ROOKS_PAIR: bool = false;

const TUNE_KING_SAFETY: bool = false;
const TUNE_KING_CHECK_KNIGHT: bool = false;
const TUNE_KING_CHECK_BISHOP: bool = false;
const TUNE_KING_CHECK_ROOK: bool = false;
const TUNE_KING_CHECK_QUEEN: bool = false;
const TUNE_KING_DANGER: bool = false;

const TUNE_PST_PAWN: bool = false;
const TUNE_PST_KNIGHT: bool = false;
const TUNE_PST_BISHOP: bool = false;
const TUNE_PST_ROOK: bool = false;
const TUNE_PST_QUEEN: bool = false;
const TUNE_PST_KING: bool = false;

#[derive(Clone)]
pub struct Trace {
    result: f32,
    pub material: [[i8; 2]; 6],

    pub mobility_pawn: [i8; 2],
    pub mobility_knight: [[i8; 2]; 9],
    pub mobility_bishop: [[i8; 2]; 14],
    pub mobility_rook: [[i8; 2]; 15],
    pub mobility_queen: [[i8; 2]; 29],

    pub pawns_doubled: [i8; 2],
    pub pawns_passed: [[i8; 2]; 8],
    pub pawns_passed_file: [[i8; 2]; 8],
    pub pawns_open_isolated: [i8; 2],
    pub pawns_isolated: [i8; 2],

    pub bishops_xray: [i8; 2],
    pub bishops_pair: [i8; 2],

    pub rooks_open_file: [i8; 2],
    pub rooks_halfopen_file: [i8; 2],
    pub rooks_pair: [i8; 2],

    pub king_safety: [[i8; 2]; 30],
    pub king_check_knight: [i8; 2],
    pub king_check_bishop: [i8; 2],
    pub king_check_rook: [i8; 2],
    pub king_check_queen: [i8; 2],
    pub king_danger: [[i8; 2]; 6],
    pub king_danger_attacks: [i8; 2],

    pub pst_pawn: SquareMap<[i8; 2]>,
    pub pst_knight: SquareMap<[i8; 2]>,
    pub pst_bishop: SquareMap<[i8; 2]>,
    pub pst_rook: SquareMap<[i8; 2]>,
    pub pst_queen: SquareMap<[i8; 2]>,
    pub pst_king: SquareMap<[i8; 2]>,

    pub phase: i8,
    pub sf: i8,
}

#[derive(Clone)]
pub struct CompactTrace {
    result: f32,
    material: [i8; 6],

    mobility_pawn: i8,
    mobility_knight: [i8; 9],
    mobility_bishop: [i8; 14],
    mobility_rook: [i8; 15],
    mobility_queen: [i8; 29],

    pawns_doubled: i8,
    pawns_passed: [i8; 8],
    pawns_passed_file: [i8; 8],
    pawns_open_isolated: i8,
    pawns_isolated: i8,

    bishops_xray: i8,
    bishops_pair: i8,

    rooks_open_file: i8,
    rooks_halfopen_file: i8,
    rooks_pair: i8,

    king_safety: [i8; 30],
    king_check_knight: i8,
    king_check_bishop: i8,
    king_check_rook: i8,
    king_check_queen: i8,
    // Since the king danger computation is non linear, we have to keep the individual counts
    king_danger: [[i8; 2]; 6],
    king_danger_attacks: [i8; 2],

    pst_pawn: SquareMap<i8>,
    pst_knight: SquareMap<i8>,
    pst_bishop: SquareMap<i8>,
    pst_rook: SquareMap<i8>,
    pst_queen: SquareMap<i8>,
    pst_king: SquareMap<i8>,

    phase: i8,
    sf: i8,
}

impl From<Trace> for CompactTrace {
    fn from(t: Trace) -> Self {
        let mut material = [0; 6];
        for i in 0..6 {
            material[i] = t.material[i][1] - t.material[i][0];
        }

        let mut mobility_knight = [0; 9];
        for i in 0..9 {
            mobility_knight[i] = t.mobility_knight[i][1] - t.mobility_knight[i][0];
        }

        let mut mobility_bishop = [0; 14];
        for i in 0..14 {
            mobility_bishop[i] = t.mobility_bishop[i][1] - t.mobility_bishop[i][0];
        }

        let mut mobility_rook = [0; 15];
        for i in 0..15 {
            mobility_rook[i] = t.mobility_rook[i][1] - t.mobility_rook[i][0];
        }

        let mut mobility_queen = [0; 29];
        for i in 0..29 {
            mobility_queen[i] = t.mobility_queen[i][1] - t.mobility_queen[i][0];
        }

        let mut pawns_passed = [0; 8];
        for i in 0..8 {
            pawns_passed[i] = t.pawns_passed[i][1] - t.pawns_passed[i][0];
        }

        let mut pawns_passed_file = [0; 8];
        for i in 0..8 {
            pawns_passed_file[i] = t.pawns_passed_file[i][1] - t.pawns_passed_file[i][0];
        }

        let mut king_safety = [0; 30];
        for i in 0..30 {
            king_safety[i] = t.king_safety[i][1] - t.king_safety[i][0];
        }

        let mut pst_pawn = SquareMap::default();
        let mut pst_knight = SquareMap::default();
        let mut pst_bishop = SquareMap::default();
        let mut pst_rook = SquareMap::default();
        let mut pst_queen = SquareMap::default();
        let mut pst_king = SquareMap::default();
        for i in ALL_SQUARES.squares() {
            pst_pawn[i] = t.pst_pawn[i][1] - t.pst_pawn[i][0];
            pst_knight[i] = t.pst_knight[i][1] - t.pst_knight[i][0];
            pst_bishop[i] = t.pst_bishop[i][1] - t.pst_bishop[i][0];
            pst_rook[i] = t.pst_rook[i][1] - t.pst_rook[i][0];
            pst_queen[i] = t.pst_queen[i][1] - t.pst_queen[i][0];
            pst_king[i] = t.pst_king[i][1] - t.pst_king[i][0];
        }

        CompactTrace {
            result: t.result,
            material,

            mobility_pawn: t.mobility_pawn[1] - t.mobility_pawn[0],
            mobility_knight,
            mobility_bishop,
            mobility_rook,
            mobility_queen,

            pawns_doubled: t.pawns_doubled[1] - t.pawns_doubled[0],
            pawns_passed,
            pawns_passed_file,
            pawns_open_isolated: t.pawns_open_isolated[1] - t.pawns_open_isolated[0],
            pawns_isolated: t.pawns_isolated[1] - t.pawns_isolated[0],

            bishops_xray: t.bishops_xray[1] - t.bishops_xray[0],
            bishops_pair: t.bishops_pair[1] - t.bishops_pair[0],

            rooks_open_file: t.rooks_open_file[1] - t.rooks_open_file[0],
            rooks_halfopen_file: t.rooks_halfopen_file[1] - t.rooks_halfopen_file[0],
            rooks_pair: t.rooks_pair[1] - t.rooks_pair[0],

            king_safety,
            king_check_knight: t.king_check_knight[1] - t.king_check_knight[0],
            king_check_bishop: t.king_check_bishop[1] - t.king_check_bishop[0],
            king_check_rook: t.king_check_rook[1] - t.king_check_rook[0],
            king_check_queen: t.king_check_queen[1] - t.king_check_queen[0],
            king_danger: t.king_danger,
            king_danger_attacks: t.king_danger_attacks,

            pst_pawn,
            pst_knight,
            pst_bishop,
            pst_rook,
            pst_queen,
            pst_king,

            phase: t.phase,
            sf: t.sf,
        }
    }
}

#[derive(Clone)]
pub struct Parameters {
    pub k: f32,
    material: [(f32, f32); 6],

    mobility_pawn: (f32, f32),
    mobility_knight: [(f32, f32); 9],
    mobility_bishop: [(f32, f32); 14],
    mobility_rook: [(f32, f32); 15],
    mobility_queen: [(f32, f32); 29],

    pawns_doubled: (f32, f32),
    pawns_passed: [(f32, f32); 8],
    pawns_passed_file: [(f32, f32); 8],
    pawns_open_isolated: (f32, f32),
    pawns_isolated: (f32, f32),

    bishops_xray: (f32, f32),
    bishops_pair: (f32, f32),

    rooks_open_file: (f32, f32),
    rooks_halfopen_file: (f32, f32),
    rooks_pair: (f32, f32),

    king_safety: [f32; 30],
    king_check_knight: f32,
    king_check_bishop: f32,
    king_check_rook: f32,
    king_check_queen: f32,
    king_danger: [f32; 6],
    king_danger_attacks: [f32; 7],

    pst_pawn: SquareMap<(f32, f32)>,
    pst_knight: SquareMap<(f32, f32)>,
    pst_bishop: SquareMap<(f32, f32)>,
    pst_rook: SquareMap<(f32, f32)>,
    pst_queen: SquareMap<(f32, f32)>,
    pst_king: SquareMap<(f32, f32)>,
}

pub fn epd_to_positions<P: AsRef<Path>>(path: P) -> impl Iterator<Item = (f32, Position)> {
    let file = File::open(path).expect("Could not open file");
    let buf_reader = BufReader::new(file);
    let lines = buf_reader.lines();
    lines.flatten().map(|line| {
        let result = match line.split_whitespace().last().unwrap() {
            "\"1-0\";" => 1.,
            "\"1/2-1/2\";" => 0.5,
            "\"0-1\";" => 0.,
            s => panic!("Unexpected result {}", s),
        };

        let pos = Position::from(line.as_ref());
        (result, pos)
    })
}

pub fn fens_to_positions<P: AsRef<Path>>(path: P) -> impl Iterator<Item = (f32, Position)> {
    let file = File::open(path).expect("Could not open file");
    let buf_reader = BufReader::new(file);
    let lines = buf_reader.lines();
    lines.flatten().map(|line| {
        let result = match line.split_whitespace().last().unwrap() {
            "1-0" => 1.,
            "1/2-1/2" => 0.5,
            "0-1" => 0.,
            s => panic!("Unexpected result {}", s),
        };

        let pos = Position::from(line.as_ref());
        (result, pos)
    })
}

pub fn pgn_to_positions<P: AsRef<Path>>(path: P) -> impl Iterator<Item = (f32, Position)> {
    let file = File::open(path).expect("Could not open file");
    let buf_reader = BufReader::new(file);
    let lines = buf_reader.lines();
    lines
        .flatten()
        .filter(|line| line.starts_with("{") || line.starts_with("[Result"))
        .scan(false, |ignore, line| {
            // skip positions form the opening book and positions where a mate was found
            if *ignore {
                *ignore = false;
                Some(None)
            } else {
                *ignore = line.starts_with("{ book")
                    || line.starts_with("{ -M")
                    || line.starts_with("{ +M");
                let skip_this = *ignore
                    || line.starts_with("{ -")
                    || line.starts_with("{ +")
                    || line.starts_with("{ 0");
                if *ignore || skip_this {
                    Some(None)
                } else {
                    Some(Some(line))
                }
            }
        })
        .flatten()
        .scan(None, |result, line| {
            if line == "[Result \"1-0\"]" {
                *result = Some(1.);
                return Some(None);
            } else if line == "[Result \"1/2-1/2\"]" {
                *result = Some(0.5);
                return Some(None);
            } else if line == "[Result \"0-1\"]" {
                *result = Some(0.);
                return Some(None);
            } else {
                if let Some(r) = *result {
                    assert!(line.starts_with("{ "));
                    let fen = &line[2..];
                    let pos = Position::from(fen);
                    return Some(Some((r, pos)));
                }

                return Some(None);
            }
        })
        .flatten()
}

impl Default for Trace {
    fn default() -> Trace {
        Trace {
            result: -1.,
            phase: 0,
            sf: SF_NORMAL as i8,
            material: [[0; 2]; 6],

            mobility_pawn: [0; 2],
            mobility_knight: [[0; 2]; 9],
            mobility_bishop: [[0; 2]; 14],
            mobility_rook: [[0; 2]; 15],
            mobility_queen: [[0; 2]; 29],

            pawns_doubled: [0; 2],
            pawns_passed: [[0; 2]; 8],
            pawns_passed_file: [[0; 2]; 8],
            pawns_open_isolated: [0; 2],
            pawns_isolated: [0; 2],

            bishops_xray: [0; 2],
            bishops_pair: [0; 2],

            rooks_open_file: [0; 2],
            rooks_halfopen_file: [0; 2],
            rooks_pair: [0; 2],

            king_safety: [[0; 2]; 30],
            king_check_knight: [0; 2],
            king_check_bishop: [0; 2],
            king_check_rook: [0; 2],
            king_check_queen: [0; 2],
            king_danger: [[0; 2]; 6],
            king_danger_attacks: [0; 2],

            pst_pawn: SquareMap::default(),
            pst_knight: SquareMap::default(),
            pst_bishop: SquareMap::default(),
            pst_rook: SquareMap::default(),
            pst_queen: SquareMap::default(),
            pst_king: SquareMap::default(),
        }
    }
}

impl Trace {
    pub fn from_position(result: f32, mut position: Position) -> Trace {
        let (_, pos) = qsearch(&mut position, -MATE_SCORE, MATE_SCORE);

        let mut e = Eval::from(&pos);
        let _ = e.score(&pos, 0);
        e.trace.result = result;

        e.trace
    }
}

impl CompactTrace {
    fn evaluate(&self, params: &Parameters) -> f32 {
        let phase = self.phase as f32;
        let sf = self.sf as f32 / SF_NORMAL as f32;
        let mut score = (0., 0.);

        // Material
        evaluate_array(&mut score, &params.material, &self.material);

        // Mobility
        evaluate_single(&mut score, params.mobility_pawn, self.mobility_pawn);
        evaluate_array(&mut score, &params.mobility_knight, &self.mobility_knight);
        evaluate_array(&mut score, &params.mobility_bishop, &self.mobility_bishop);
        evaluate_array(&mut score, &params.mobility_rook, &self.mobility_rook);
        evaluate_array(&mut score, &params.mobility_queen, &self.mobility_queen);

        // Pawns
        evaluate_single(&mut score, params.pawns_doubled, self.pawns_doubled);
        evaluate_single(
            &mut score,
            params.pawns_open_isolated,
            self.pawns_open_isolated,
        );
        evaluate_single(&mut score, params.pawns_isolated, self.pawns_isolated);
        evaluate_array(&mut score, &params.pawns_passed, &self.pawns_passed);

        for (i, &coeff) in self.pawns_passed_file.iter().enumerate() {
            evaluate_single(&mut score, params.pawns_passed_file[i], coeff);
        }

        // Bishops
        evaluate_single(&mut score, params.bishops_xray, self.bishops_xray);
        evaluate_single(&mut score, params.bishops_pair, self.bishops_pair);

        // Rooks
        evaluate_single(&mut score, params.rooks_open_file, self.rooks_open_file);
        evaluate_single(
            &mut score,
            params.rooks_halfopen_file,
            self.rooks_halfopen_file,
        );
        evaluate_single(&mut score, params.rooks_pair, self.rooks_pair);

        // King safety
        score.0 += params.king_check_knight * self.king_check_knight as f32;
        score.0 += params.king_check_bishop * self.king_check_bishop as f32;
        score.0 += params.king_check_rook * self.king_check_rook as f32;
        score.0 += params.king_check_queen * self.king_check_queen as f32;
        for i in 0..30 {
            score.0 += params.king_safety[i] * self.king_safety[i] as f32;
        }

        let mut danger_white = 0.;
        let mut danger_black = 0.;
        for i in 0..6 {
            danger_white += params.king_danger[i] * self.king_danger[i][1] as f32;
            danger_black += params.king_danger[i] * self.king_danger[i][0] as f32;
        }

        score.0 +=
            danger_white * params.king_danger_attacks[self.king_danger_attacks[1] as usize] / 128.;
        score.0 -=
            danger_black * params.king_danger_attacks[self.king_danger_attacks[0] as usize] / 128.;

        // PST
        for i in ALL_SQUARES.squares() {
            evaluate_single(&mut score, params.pst_pawn[i], self.pst_pawn[i]);
            evaluate_single(&mut score, params.pst_knight[i], self.pst_knight[i]);
            evaluate_single(&mut score, params.pst_bishop[i], self.pst_bishop[i]);
            evaluate_single(&mut score, params.pst_rook[i], self.pst_rook[i]);
            evaluate_single(&mut score, params.pst_queen[i], self.pst_queen[i]);
            evaluate_single(&mut score, params.pst_king[i], self.pst_king[i]);
        }

        sf * (score.0 as f32 * phase + score.1 as f32 * (62. - phase)) / 62.
    }
}

fn sigmoid(k: f32, q: f32) -> f32 {
    1. / (1. + 10_f32.powf(-k * q / 400.))
}

impl Parameters {
    pub fn print_weights(&self) {
        if TUNE_MATERIAL_PAWN {
            print_single(self.material[0], "PAWN_SCORE");
        }

        if TUNE_MATERIAL_KNIGHT {
            print_single(self.material[1], "KNIGHT_SCORE");
        }

        if TUNE_MATERIAL_BISHOP {
            print_single(self.material[2], "BISHOP_SCORE");
        }

        if TUNE_MATERIAL_ROOK {
            print_single(self.material[3], "ROOK_SCORE");
        }

        if TUNE_MATERIAL_QUEEN {
            print_single(self.material[4], "QUEEN_SCORE");
        }

        if TUNE_MOBILITY_PAWN {
            print_single(self.mobility_pawn, "PAWN_MOBILITY");
        }

        if TUNE_MOBILITY_KNIGHT {
            print_array(&self.mobility_knight, "KNIGHT_MOBILITY");
        }

        if TUNE_MOBILITY_BISHOP {
            print_array(&self.mobility_bishop, "BISHOP_MOBILITY");
        }

        if TUNE_MOBILITY_ROOK {
            print_array(&self.mobility_rook, "ROOK_MOBILITY");
        }

        if TUNE_MOBILITY_QUEEN {
            print_array(&self.mobility_queen, "QUEEN_MOBILITY");
        }

        if TUNE_PAWNS_DOUBLED {
            print_single(self.pawns_doubled, "DOUBLED_PAWN");
        }

        if TUNE_PAWNS_PASSED {
            print_array(&self.pawns_passed, "PASSED_PAWN_ON_RANK");
            print_array(&self.pawns_passed_file, "PASSED_PAWN_ON_FILE");
        }

        if TUNE_PAWNS_OPEN_ISOLATED {
            print_single(self.pawns_open_isolated, "OPEN_ISOLATED_PAWN");
        }

        if TUNE_PAWNS_ISOLATED {
            print_single(self.pawns_isolated, "ISOLATED_PAWN");
        }

        if TUNE_BISHOPS_XRAY {
            print_single(self.bishops_xray, "XRAYED_SQUARE");
        }

        if TUNE_BISHOPS_PAIR {
            print_single(self.bishops_pair, "BISHOP_PAIR");
        }

        if TUNE_ROOKS_OPEN_FILE {
            print_single(self.rooks_open_file, "ROOK_OPEN_FILE");
        }

        if TUNE_ROOKS_HALFOPEN_FILE {
            print_single(self.rooks_halfopen_file, "ROOK_HALFOPEN_FILE");
        }

        if TUNE_ROOKS_PAIR {
            print_single(self.rooks_pair, "ROOK_PAIR");
        }

        if TUNE_KING_SAFETY {
            print_array_mg(&self.king_safety, "KING_SAFETY");
        }

        if TUNE_KING_CHECK_KNIGHT {
            print_single((self.king_check_knight, 0.), "KING_CHECK_KNIGHT");
        }

        if TUNE_KING_CHECK_BISHOP {
            print_single((self.king_check_bishop, 0.), "KING_CHECK_BISHOP");
        }

        if TUNE_KING_CHECK_ROOK {
            print_single((self.king_check_rook, 0.), "KING_CHECK_ROOK");
        }

        if TUNE_KING_CHECK_QUEEN {
            print_single((self.king_check_queen, 0.), "KING_CHECK_QUEEN");
        }

        if TUNE_KING_DANGER {
            print_array_mg(&self.king_danger, "KING_DANGER");
            print_array_mg(&self.king_danger_attacks, "KING_DANGER_WEIGHT");
        }

        if TUNE_PST_PAWN {
            print_pst(&self.pst_pawn, "PAWN_PST");
        }

        if TUNE_PST_KNIGHT {
            print_pst(&self.pst_knight, "KNIGHT_PST");
        }

        if TUNE_PST_BISHOP {
            print_pst(&self.pst_bishop, "BISHOP_PST");
        }

        if TUNE_PST_ROOK {
            print_pst(&self.pst_rook, "ROOK_PST");
        }

        if TUNE_PST_QUEEN {
            print_pst(&self.pst_queen, "QUEEN_PST");
        }

        if TUNE_PST_KING {
            print_pst(&self.pst_king, "KING_PST");
        }
    }

    pub fn total_error(&self, traces: &[CompactTrace]) -> f32 {
        let mut total = 0.;
        let n = traces.len() as f32;

        for trace in traces {
            total += (trace.result - sigmoid(self.k, trace.evaluate(&self))).powf(2.);
        }

        total / n
    }

    pub fn compute_optimal_k(&mut self, traces: &[CompactTrace]) {
        let mut delta = 1.;
        let mut best = self.k;
        let mut best_error = self.total_error(traces);
        let mut low = best - delta;

        for _ in 0..5 {
            let step = delta / 5.;

            for i in 0..11 {
                let candidate = low + step * i as f32;
                self.k = candidate;
                let error = self.total_error(traces);

                if error < best_error {
                    best = candidate;
                    best_error = error;
                }
            }

            delta /= 10.;
            low = best - delta;
        }

        self.k = best;
    }

    pub fn step(&mut self, traces: &[CompactTrace], f: f32) {
        const BATCH_SIZE: usize = 2048;
        let n = traces.len();
        for batch in 0..(n - 1) / BATCH_SIZE + 1 {
            let from = batch * BATCH_SIZE;
            let to = cmp::min(n, (batch + 1) * BATCH_SIZE);
            let traces = &traces[from..to];
            self.gradient_descent_step(traces, f);
        }
    }

    pub fn gradient_descent_step(&mut self, traces: &[CompactTrace], f: f32) {
        let mut g_material = [(0., 0.); 6];

        let mut g_mobility_pawn = (0., 0.);
        let mut g_mobility_knight = [(0., 0.); 9];
        let mut g_mobility_bishop = [(0., 0.); 14];
        let mut g_mobility_rook = [(0., 0.); 15];
        let mut g_mobility_queen = [(0., 0.); 29];

        let mut g_pawns_doubled = (0., 0.);
        let mut g_pawns_passed = [(0., 0.); 8];
        let mut g_pawns_passed_file = [(0., 0.); 8];
        let mut g_pawns_open_isolated = (0., 0.);
        let mut g_pawns_isolated = (0., 0.);

        let mut g_bishops_xray = (0., 0.);
        let mut g_bishops_pair = (0., 0.);

        let mut g_rooks_open_file = (0., 0.);
        let mut g_rooks_halfopen_file = (0., 0.);
        let mut g_rooks_pair = (0., 0.);

        let mut g_king_safety = [0.; 30];
        let mut g_king_check_knight = 0.;
        let mut g_king_check_bishop = 0.;
        let mut g_king_check_rook = 0.;
        let mut g_king_check_queen = 0.;
        let mut g_king_danger = [0.; 6];
        let mut g_king_danger_attacks = [0.; 7];

        let mut g_pst_pawn = SquareMap::<(f32, f32)>::default();
        let mut g_pst_knight = SquareMap::<(f32, f32)>::default();
        let mut g_pst_bishop = SquareMap::<(f32, f32)>::default();
        let mut g_pst_rook = SquareMap::<(f32, f32)>::default();
        let mut g_pst_queen = SquareMap::<(f32, f32)>::default();
        let mut g_pst_king = SquareMap::<(f32, f32)>::default();

        let n = traces.len() as f32;
        let _g = self.k * 10_f32.ln() / 400.;

        for trace in traces {
            let phase = trace.phase as f32;

            let r = trace.result;
            let s = sigmoid(self.k, trace.evaluate(&self));
            let sf = trace.sf as f32 / SF_NORMAL as f32;
            let grad = -(r - s) * s * (1. - s) * sf;

            if TUNE_MATERIAL_PAWN {
                // For pawns we only tune endgame scores and leave the midgame scores fixed at 100.
                let i = Piece::Pawn.index();
                update_gradient(&mut g_material[i], trace.material[i], grad, phase);
                g_material[i].0 = 0.;
            }

            if TUNE_MATERIAL_KNIGHT {
                let i = Piece::Knight.index();
                update_gradient(&mut g_material[i], trace.material[i], grad, phase);
            }

            if TUNE_MATERIAL_BISHOP {
                let i = Piece::Bishop.index();
                update_gradient(&mut g_material[i], trace.material[i], grad, phase);
            }

            if TUNE_MATERIAL_ROOK {
                let i = Piece::Rook.index();
                update_gradient(&mut g_material[i], trace.material[i], grad, phase);
            }

            if TUNE_MATERIAL_QUEEN {
                let i = Piece::Queen.index();
                update_gradient(&mut g_material[i], trace.material[i], grad, phase);
            }

            if TUNE_MOBILITY_PAWN {
                update_gradient(&mut g_mobility_pawn, trace.mobility_pawn, grad, phase);
            }

            if TUNE_MOBILITY_KNIGHT {
                update_gradient_array(&mut g_mobility_knight, &trace.mobility_knight, grad, phase);
            }

            if TUNE_MOBILITY_BISHOP {
                update_gradient_array(&mut g_mobility_bishop, &trace.mobility_bishop, grad, phase);
            }

            if TUNE_MOBILITY_ROOK {
                update_gradient_array(&mut g_mobility_rook, &trace.mobility_rook, grad, phase);
            }

            if TUNE_MOBILITY_QUEEN {
                update_gradient_array(&mut g_mobility_queen, &trace.mobility_queen, grad, phase);
            }

            if TUNE_PAWNS_DOUBLED {
                update_gradient(&mut g_pawns_doubled, trace.pawns_doubled, grad, phase);
            }

            if TUNE_PAWNS_OPEN_ISOLATED {
                update_gradient(
                    &mut g_pawns_open_isolated,
                    trace.pawns_open_isolated,
                    grad,
                    phase,
                );
            }

            if TUNE_PAWNS_ISOLATED {
                update_gradient(&mut g_pawns_isolated, trace.pawns_isolated, grad, phase);
            }

            if TUNE_PAWNS_PASSED {
                update_gradient_array(&mut g_pawns_passed, &trace.pawns_passed, grad, phase);
                update_gradient_array(
                    &mut g_pawns_passed_file,
                    &trace.pawns_passed_file,
                    grad,
                    phase,
                );
            }

            if TUNE_BISHOPS_XRAY {
                update_gradient(&mut g_bishops_xray, trace.bishops_xray, grad, phase);
            }

            if TUNE_BISHOPS_PAIR {
                update_gradient(&mut g_bishops_pair, trace.bishops_pair, grad, phase);
            }

            if TUNE_ROOKS_OPEN_FILE {
                update_gradient(&mut g_rooks_open_file, trace.rooks_open_file, grad, phase);
            }

            if TUNE_ROOKS_HALFOPEN_FILE {
                update_gradient(
                    &mut g_rooks_halfopen_file,
                    trace.rooks_halfopen_file,
                    grad,
                    phase,
                );
            }

            if TUNE_ROOKS_PAIR {
                update_gradient(&mut g_rooks_pair, trace.rooks_pair, grad, phase);
            }

            if TUNE_KING_SAFETY {
                for i in 0..30 {
                    let x = trace.king_safety[i] as f32;
                    g_king_safety[i] += x * grad * phase / 62.;
                }
            }

            if TUNE_KING_CHECK_KNIGHT {
                let x = trace.king_check_knight as f32;
                g_king_check_knight += x * grad * phase / 62.;
            }

            if TUNE_KING_CHECK_BISHOP {
                let x = trace.king_check_bishop as f32;
                g_king_check_bishop += x * grad * phase / 62.;
            }

            if TUNE_KING_CHECK_ROOK {
                let x = trace.king_check_rook as f32;
                g_king_check_rook += x * grad * phase / 62.;
            }

            if TUNE_KING_CHECK_QUEEN {
                let x = trace.king_check_queen as f32;
                g_king_check_queen += x * grad * phase / 62.;
            }

            if TUNE_KING_DANGER {
                let mut danger_white = 0.;
                let mut danger_black = 0.;

                for i in 0..6 {
                    danger_white += self.king_danger[i] * trace.king_danger[i][1] as f32;
                    danger_black += self.king_danger[i] * trace.king_danger[i][0] as f32;

                    let x = (trace.king_danger[i][1] as f32
                        * self.king_danger_attacks[trace.king_danger_attacks[1] as usize]
                        - trace.king_danger[i][0] as f32
                            * self.king_danger_attacks[trace.king_danger_attacks[0] as usize])
                        / 128.;
                    g_king_danger[i] += x * grad * phase / 62.;
                }

                let x = danger_white / 128.;
                g_king_danger_attacks[trace.king_danger_attacks[1] as usize] +=
                    x * grad * phase / 62.;

                let x = -danger_black / 128.;
                g_king_danger_attacks[trace.king_danger_attacks[0] as usize] +=
                    x * grad * phase / 62.;
            }

            if TUNE_PST_PAWN {
                for i in ALL_SQUARES.squares() {
                    update_gradient(&mut g_pst_pawn[i], trace.pst_pawn[i], grad, phase);
                }
            }

            if TUNE_PST_KNIGHT {
                for i in ALL_SQUARES.squares() {
                    update_gradient(&mut g_pst_knight[i], trace.pst_knight[i], grad, phase);
                }
            }

            if TUNE_PST_BISHOP {
                for i in ALL_SQUARES.squares() {
                    update_gradient(&mut g_pst_bishop[i], trace.pst_bishop[i], grad, phase);
                }
            }

            if TUNE_PST_ROOK {
                for i in ALL_SQUARES.squares() {
                    update_gradient(&mut g_pst_rook[i], trace.pst_rook[i], grad, phase);
                }
            }

            if TUNE_PST_QUEEN {
                for i in ALL_SQUARES.squares() {
                    update_gradient(&mut g_pst_queen[i], trace.pst_queen[i], grad, phase);
                }
            }

            if TUNE_PST_KING {
                for i in ALL_SQUARES.squares() {
                    update_gradient(&mut g_pst_king[i], trace.pst_king[i], grad, phase);
                }
            }
        }

        let mut norm = 0.;

        norm += norm_array(&g_material);

        norm += norm_single(g_mobility_pawn);
        norm += norm_array(&g_mobility_knight);
        norm += norm_array(&g_mobility_bishop);
        norm += norm_array(&g_mobility_rook);
        norm += norm_array(&g_mobility_queen);

        norm += norm_single(g_pawns_doubled);
        norm += norm_single(g_pawns_open_isolated);
        norm += norm_single(g_pawns_isolated);
        norm += norm_array(&g_pawns_passed);
        norm += norm_array(&g_pawns_passed_file);

        norm += norm_single(g_bishops_xray);
        norm += norm_single(g_bishops_pair);

        norm += norm_single(g_rooks_open_file);
        norm += norm_single(g_rooks_halfopen_file);
        norm += norm_single(g_rooks_pair);

        for i in 0..30 {
            norm += g_king_safety[i].powf(2.);
        }

        norm += g_king_check_knight.powf(2.);
        norm += g_king_check_bishop.powf(2.);
        norm += g_king_check_rook.powf(2.);
        norm += g_king_check_queen.powf(2.);

        for i in 0..6 {
            norm += g_king_danger[i].powf(2.);
        }

        for i in 0..7 {
            norm += g_king_danger_attacks[i].powf(2.);
        }

        for i in ALL_SQUARES.squares() {
            norm += norm_single(g_pst_pawn[i]);
            norm += norm_single(g_pst_knight[i]);
            norm += norm_single(g_pst_bishop[i]);
            norm += norm_single(g_pst_rook[i]);
            norm += norm_single(g_pst_queen[i]);
            norm += norm_single(g_pst_king[i]);
        }

        if norm == 0.0 {
            return;
        }

        norm = norm.sqrt();
        let f = f / norm;

        update_parameter_array(&mut self.material, &g_material, f / n);

        update_parameter(&mut self.mobility_pawn, g_mobility_pawn, f / n);
        update_parameter_array(&mut self.mobility_knight, &g_mobility_knight, f / n);
        update_parameter_array(&mut self.mobility_bishop, &g_mobility_bishop, f / n);
        update_parameter_array(&mut self.mobility_rook, &g_mobility_rook, f / n);
        update_parameter_array(&mut self.mobility_queen, &g_mobility_queen, f / n);

        update_parameter(&mut self.pawns_doubled, g_pawns_doubled, f / n);
        update_parameter(&mut self.pawns_isolated, g_pawns_isolated, f / n);
        update_parameter(&mut self.pawns_open_isolated, g_pawns_open_isolated, f / n);
        update_parameter_array(&mut self.pawns_passed, &g_pawns_passed, f / n);
        update_parameter_array(&mut self.pawns_passed_file, &g_pawns_passed_file, f / n);

        update_parameter(&mut self.bishops_pair, g_bishops_pair, f / n);
        update_parameter(&mut self.bishops_xray, g_bishops_xray, f / n);

        update_parameter(&mut self.rooks_open_file, g_rooks_open_file, f / n);
        update_parameter(&mut self.rooks_halfopen_file, g_rooks_halfopen_file, f / n);
        update_parameter(&mut self.rooks_pair, g_rooks_pair, f / n);

        for i in 0..30 {
            self.king_safety[i] -= 2. / n * f * g_king_safety[i];
        }

        self.king_check_knight -= 2. / n * f * g_king_check_knight;
        self.king_check_bishop -= 2. / n * f * g_king_check_bishop;
        self.king_check_rook -= 2. / n * f * g_king_check_rook;
        self.king_check_queen -= 2. / n * f * g_king_check_queen;

        for i in 0..6 {
            self.king_danger[i] -= 2. / n * f * g_king_danger[i];
        }

        for i in 0..7 {
            self.king_danger_attacks[i] -= 2. / n * f * g_king_danger_attacks[i];
        }

        for i in ALL_SQUARES.squares() {
            update_parameter(&mut self.pst_pawn[i], g_pst_pawn[i], f / n);
            update_parameter(&mut self.pst_knight[i], g_pst_knight[i], f / n);
            update_parameter(&mut self.pst_bishop[i], g_pst_bishop[i], f / n);
            update_parameter(&mut self.pst_rook[i], g_pst_rook[i], f / n);
            update_parameter(&mut self.pst_queen[i], g_pst_queen[i], f / n);
            update_parameter(&mut self.pst_king[i], g_pst_king[i], f / n);
        }
    }
}

impl Default for Parameters {
    fn default() -> Parameters {
        let mut mobility_knight = [(0., 0.); 9];
        for i in 0..9 {
            mobility_knight[i] = (mg(KNIGHT_MOBILITY[i]) as f32, eg(KNIGHT_MOBILITY[i]) as f32);
        }

        let mut mobility_bishop = [(0., 0.); 14];
        for i in 0..14 {
            mobility_bishop[i] = (mg(BISHOP_MOBILITY[i]) as f32, eg(BISHOP_MOBILITY[i]) as f32);
        }

        let mut mobility_rook = [(0., 0.); 15];
        for i in 0..15 {
            mobility_rook[i] = (mg(ROOK_MOBILITY[i]) as f32, eg(ROOK_MOBILITY[i]) as f32);
        }

        let mut mobility_queen = [(0., 0.); 29];
        for i in 0..29 {
            mobility_queen[i] = (mg(QUEEN_MOBILITY[i]) as f32, eg(QUEEN_MOBILITY[i]) as f32);
        }

        let pawns_doubled = (mg(DOUBLED_PAWN) as f32, eg(DOUBLED_PAWN) as f32);
        let pawns_open_isolated = (mg(OPEN_ISOLATED_PAWN) as f32, eg(OPEN_ISOLATED_PAWN) as f32);
        let pawns_isolated = (mg(ISOLATED_PAWN) as f32, eg(ISOLATED_PAWN) as f32);
        let mut pawns_passed = [(0., 0.); 8];
        for i in 0..8 {
            pawns_passed[i] = (
                mg(PASSED_PAWN_ON_RANK[i]) as f32,
                eg(PASSED_PAWN_ON_RANK[i]) as f32,
            );
        }

        let mut pawns_passed_file = [(0., 0.); 8];
        for (i, &weight) in PASSED_PAWN_ON_FILE.iter().enumerate() {
            pawns_passed_file[i] = (mg(weight) as f32, eg(weight) as f32);
        }

        let mut king_safety = [0.; 30];
        for i in 0..30 {
            king_safety[i] = KING_SAFETY[i] as f32;
        }

        let mut king_danger = [0.; 6];
        for i in 0..6 {
            king_danger[i] = KING_DANGER[i] as f32;
        }
        let mut king_danger_attacks = [0.; 7];
        for i in 0..7 {
            king_danger_attacks[i] = KING_DANGER_WEIGHT[i] as f32;
        }

        let mut pst_pawn = SquareMap::default();
        let mut pst_knight = SquareMap::default();
        let mut pst_bishop = SquareMap::default();
        let mut pst_rook = SquareMap::default();
        let mut pst_queen = SquareMap::default();
        let mut pst_king = SquareMap::default();

        for i in ALL_SQUARES.squares() {
            pst_pawn[i] = (mg(PAWN_PST[i]) as f32, eg(PAWN_PST[i]) as f32);
            pst_knight[i] = (mg(KNIGHT_PST[i]) as f32, eg(KNIGHT_PST[i]) as f32);
            pst_bishop[i] = (mg(BISHOP_PST[i]) as f32, eg(BISHOP_PST[i]) as f32);
            pst_rook[i] = (mg(ROOK_PST[i]) as f32, eg(ROOK_PST[i]) as f32);
            pst_queen[i] = (mg(QUEEN_PST[i]) as f32, eg(QUEEN_PST[i]) as f32);
            pst_king[i] = (mg(KING_PST[i]) as f32, eg(KING_PST[i]) as f32);
        }

        Parameters {
            k: 1.,
            material: [
                (mg(PAWN_SCORE) as f32, eg(PAWN_SCORE) as f32),
                (mg(KNIGHT_SCORE) as f32, eg(KNIGHT_SCORE) as f32),
                (mg(BISHOP_SCORE) as f32, eg(BISHOP_SCORE) as f32),
                (mg(ROOK_SCORE) as f32, eg(ROOK_SCORE) as f32),
                (mg(QUEEN_SCORE) as f32, eg(QUEEN_SCORE) as f32),
                (10000., 10000.),
            ],
            mobility_pawn: (6., 6.),
            mobility_knight,
            mobility_bishop,
            mobility_rook,
            mobility_queen,

            pawns_doubled,
            pawns_passed,
            pawns_passed_file,
            pawns_open_isolated,
            pawns_isolated,

            bishops_xray: (mg(XRAYED_SQUARE) as f32, eg(XRAYED_SQUARE) as f32),
            bishops_pair: (mg(BISHOP_PAIR) as f32, eg(BISHOP_PAIR) as f32),

            rooks_open_file: (mg(ROOK_OPEN_FILE) as f32, eg(ROOK_OPEN_FILE) as f32),
            rooks_halfopen_file: (mg(ROOK_HALFOPEN_FILE) as f32, eg(ROOK_HALFOPEN_FILE) as f32),
            rooks_pair: (mg(ROOK_PAIR) as f32, eg(ROOK_PAIR) as f32),

            king_safety,
            king_check_knight: mg(KING_CHECK_KNIGHT) as f32,
            king_check_bishop: mg(KING_CHECK_BISHOP) as f32,
            king_check_rook: mg(KING_CHECK_ROOK) as f32,
            king_check_queen: mg(KING_CHECK_QUEEN) as f32,
            king_danger,
            king_danger_attacks,

            pst_pawn,
            pst_knight,
            pst_bishop,
            pst_rook,
            pst_queen,
            pst_king,
        }
    }
}

fn qsearch(position: &mut Position, alpha: Score, beta: Score) -> (Score, Position) {
    let in_check = position.in_check();
    let mut alpha = alpha;

    if !in_check {
        let mut e = Eval::from(position as &_);
        let eval = e.score(position, 0);
        if eval >= beta {
            return (eval, position.clone());
        }

        if alpha < eval {
            alpha = eval;
        }
    }

    let mut moves = MovePicker::qsearch(&position);

    let mut best_score = -MATE_SCORE;
    let mut best_pos = position.clone();

    let mut num_moves = 0;
    let history = History::default();
    while let Some((_mtype, mov)) = moves.next(&position, &history) {
        if !position.move_is_legal(mov) {
            continue;
        }

        let details = position.details;
        position.make_move(mov);
        num_moves += 1;

        let (value, pos) = qsearch(position, -beta, -alpha);
        let score = -value;

        position.unmake_move(mov, details);

        if score > best_score {
            best_score = score;
            best_pos = pos;
            if score > alpha {
                alpha = score;
                if score >= beta {
                    return (score, best_pos);
                }
            }
        }
    }

    if num_moves == 0 {
        if in_check {
            return (-MATE_SCORE, position.clone());
        } else {
            return (0, position.clone());
        }
    }

    (alpha, best_pos)
}

fn print_single((x, y): (f32, f32), name: &str) {
    println!(
        "pub const {}: EScore = S({}, {});",
        name,
        x.round() as isize,
        y.round() as isize
    );
}

fn print_array(array: &[(f32, f32)], name: &str) {
    println!("#[rustfmt::skip]");
    print!("pub const {}: [EScore; {}] = [", name, array.len());
    for (i, &x) in array.iter().enumerate() {
        if i % 4 == 0 {
            println!();
            print!("    ");
        }
        print!(
            "S({:>4}, {:>4}), ",
            x.0.round() as isize,
            x.1.round() as isize
        );
    }
    println!();
    println!("];");
}

fn print_array_mg(array: &[f32], name: &str) {
    println!("#[rustfmt::skip]");
    print!("pub const {}: [Score; {}] = [", name, array.len());
    for (i, &x) in array.iter().enumerate() {
        if i % 4 == 0 {
            println!();
            print!("    ");
        }
        print!("{:>4}, ", x.round() as isize);
    }
    println!();
    println!("];");
}

fn print_pst(pst: &SquareMap<(f32, f32)>, name: &str) {
    println!("#[rustfmt::skip]");
    println!(
        "pub static {}: SquareMap<EScore> = SquareMap::from_array([",
        name
    );
    for rank in 0..8 {
        print!("    ");

        for file in 0..8 {
            let sq = Square::file_rank(file, rank);
            print!(
                "S({:>4}, {:>4}), ",
                pst[sq].0.round() as isize,
                pst[sq].1.round() as isize
            );
        }

        println!();
    }
    println!("]);");
}

fn evaluate_single(score: &mut (f32, f32), param: (f32, f32), coeff: i8) {
    score.0 += param.0 * coeff as f32;
    score.1 += param.1 * coeff as f32;
}

fn evaluate_array(score: &mut (f32, f32), params: &[(f32, f32)], coeffs: &[i8]) {
    assert!(params.len() == coeffs.len());

    for (&param, &coeff) in params.iter().zip(coeffs) {
        evaluate_single(score, param, coeff);
    }
}

fn norm_single(param: (f32, f32)) -> f32 {
    param.0.powf(2.) + param.1.powf(2.)
}

fn norm_array(params: &[(f32, f32)]) -> f32 {
    let mut result = 0.;
    for &param in params {
        result += norm_single(param);
    }

    result
}

fn update_gradient(gradient: &mut (f32, f32), coeff: i8, grad: f32, phase: f32) {
    let x = coeff as f32;
    gradient.0 += x * grad * phase / 62.;
    gradient.1 += x * grad * (62. - phase) / 62.;
}

fn update_gradient_array(gradients: &mut [(f32, f32)], coeffs: &[i8], grad: f32, phase: f32) {
    assert!(gradients.len() == coeffs.len());

    for (gradient, &coeff) in gradients.iter_mut().zip(coeffs) {
        update_gradient(gradient, coeff, grad, phase);
    }
}

fn update_parameter(param: &mut (f32, f32), gradient: (f32, f32), step: f32) {
    param.0 -= 2. * step * gradient.0;
    param.1 -= 2. * step * gradient.1;
}

fn update_parameter_array(params: &mut [(f32, f32)], gradients: &[(f32, f32)], step: f32) {
    assert!(gradients.len() == params.len());

    for (param, &gradient) in params.iter_mut().zip(gradients) {
        update_parameter(param, gradient, step);
    }
}
