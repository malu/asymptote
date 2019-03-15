/* Asymptote, a UCI chess engine
   Copyright (C) 2018  Maximilian Lupke

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

use std::cell::RefCell;
use std::cmp;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;
use std::rc::Rc;

use crate::eval::*;
use crate::history::History;
use crate::movegen::Piece;
use crate::movepick::{MovePicker, MovePickerAllocations};
use crate::position::Position;

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
const TUNE_PAWNS_ISOLATED: bool = false;
const TUNE_PAWNS_PASSED: bool = false;

const TUNE_BISHOPS_XRAY: bool = false;
const TUNE_BISHOPS_PAIR: bool = false;

const TUNE_ROOKS_OPEN_FILE: bool = false;
const TUNE_ROOKS_HALFOPEN_FILE: bool = false;

const TUNE_KING_SAFETY: bool = false;
const TUNE_KING_CHECK_KNIGHT: bool = false;
const TUNE_KING_CHECK_BISHOP: bool = false;
const TUNE_KING_CHECK_ROOK: bool = false;
const TUNE_KING_CHECK_QUEEN: bool = false;

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
    pub pawns_isolated: [i8; 2],

    pub bishops_xray: [i8; 2],
    pub bishops_pair: [i8; 2],

    pub rooks_open_file: [i8; 2],
    pub rooks_halfopen_file: [i8; 2],

    pub king_safety: [[i8; 2]; 30],
    pub king_check_knight: [i8; 2],
    pub king_check_bishop: [i8; 2],
    pub king_check_rook: [i8; 2],
    pub king_check_queen: [i8; 2],

    pub pst_pawn: [[i8; 2]; 64],
    pub pst_knight: [[i8; 2]; 64],
    pub pst_bishop: [[i8; 2]; 64],
    pub pst_rook: [[i8; 2]; 64],
    pub pst_queen: [[i8; 2]; 64],
    pub pst_king: [[i8; 2]; 64],

    pub phase: i8,
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
    pawns_isolated: i8,

    bishops_xray: i8,
    bishops_pair: i8,

    rooks_open_file: i8,
    rooks_halfopen_file: i8,

    king_safety: [i8; 30],
    king_check_knight: i8,
    king_check_bishop: i8,
    king_check_rook: i8,
    king_check_queen: i8,

    pst_pawn: [i8; 64],
    pst_knight: [i8; 64],
    pst_bishop: [i8; 64],
    pst_rook: [i8; 64],
    pst_queen: [i8; 64],
    pst_king: [i8; 64],

    phase: i8,
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

        let mut king_safety = [0; 30];
        for i in 0..30 {
            king_safety[i] = t.king_safety[i][1] - t.king_safety[i][0];
        }

        let mut pst_pawn = [0; 64];
        let mut pst_knight = [0; 64];
        let mut pst_bishop = [0; 64];
        let mut pst_rook = [0; 64];
        let mut pst_queen = [0; 64];
        let mut pst_king = [0; 64];
        for i in 0..64 {
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
            pawns_isolated: t.pawns_isolated[1] - t.pawns_isolated[0],

            bishops_xray: t.bishops_xray[1] - t.bishops_xray[0],
            bishops_pair: t.bishops_pair[1] - t.bishops_pair[0],

            rooks_open_file: t.rooks_open_file[1] - t.rooks_open_file[0],
            rooks_halfopen_file: t.rooks_halfopen_file[1] - t.rooks_halfopen_file[0],

            king_safety,
            king_check_knight: t.king_check_knight[1] - t.king_check_knight[0],
            king_check_bishop: t.king_check_bishop[1] - t.king_check_bishop[0],
            king_check_rook: t.king_check_rook[1] - t.king_check_rook[0],
            king_check_queen: t.king_check_queen[1] - t.king_check_queen[0],

            pst_pawn,
            pst_knight,
            pst_bishop,
            pst_rook,
            pst_queen,
            pst_king,

            phase: t.phase,
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
    pawns_isolated: (f32, f32),

    bishops_xray: (f32, f32),
    bishops_pair: (f32, f32),

    rooks_open_file: (f32, f32),
    rooks_halfopen_file: (f32, f32),

    king_safety: [f32; 30],
    king_check_knight: f32,
    king_check_bishop: f32,
    king_check_rook: f32,
    king_check_queen: f32,

    pst_pawn: [(f32, f32); 64],
    pst_knight: [(f32, f32); 64],
    pst_bishop: [(f32, f32); 64],
    pst_rook: [(f32, f32); 64],
    pst_queen: [(f32, f32); 64],
    pst_king: [(f32, f32); 64],
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
                    || line.starts_with("{ 0");;
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
            material: [[0; 2]; 6],

            mobility_pawn: [0; 2],
            mobility_knight: [[0; 2]; 9],
            mobility_bishop: [[0; 2]; 14],
            mobility_rook: [[0; 2]; 15],
            mobility_queen: [[0; 2]; 29],

            pawns_doubled: [0; 2],
            pawns_passed: [[0; 2]; 8],
            pawns_isolated: [0; 2],

            bishops_xray: [0; 2],
            bishops_pair: [0; 2],

            rooks_open_file: [0; 2],
            rooks_halfopen_file: [0; 2],

            king_safety: [[0; 2]; 30],
            king_check_knight: [0; 2],
            king_check_bishop: [0; 2],
            king_check_rook: [0; 2],
            king_check_queen: [0; 2],

            pst_pawn: [[0; 2]; 64],
            pst_knight: [[0; 2]; 64],
            pst_bishop: [[0; 2]; 64],
            pst_rook: [[0; 2]; 64],
            pst_queen: [[0; 2]; 64],
            pst_king: [[0; 2]; 64],
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

        let mut score = (0., 0.);
        for i in 0..6 {
            score.0 += params.material[i].0 * self.material[i] as f32;
            score.1 += params.material[i].1 * self.material[i] as f32;
        }

        score.0 += params.mobility_pawn.0 * self.mobility_pawn as f32;
        score.1 += params.mobility_pawn.1 * self.mobility_pawn as f32;

        for i in 0..9 {
            score.0 += params.mobility_knight[i].0 * self.mobility_knight[i] as f32;
            score.1 += params.mobility_knight[i].1 * self.mobility_knight[i] as f32;
        }

        for i in 0..14 {
            score.0 += params.mobility_bishop[i].0 * self.mobility_bishop[i] as f32;
            score.1 += params.mobility_bishop[i].1 * self.mobility_bishop[i] as f32;
        }

        for i in 0..15 {
            score.0 += params.mobility_rook[i].0 * self.mobility_rook[i] as f32;
            score.1 += params.mobility_rook[i].1 * self.mobility_rook[i] as f32;
        }

        for i in 0..29 {
            score.0 += params.mobility_queen[i].0 * self.mobility_queen[i] as f32;
            score.1 += params.mobility_queen[i].1 * self.mobility_queen[i] as f32;
        }

        score.0 += params.pawns_doubled.0 * self.pawns_doubled as f32;
        score.1 += params.pawns_doubled.1 * self.pawns_doubled as f32;

        for i in 0..8 {
            score.0 += params.pawns_passed[i].0 * self.pawns_passed[i] as f32;
            score.1 += params.pawns_passed[i].1 * self.pawns_passed[i] as f32;
        }

        score.0 += params.pawns_isolated.0 * self.pawns_isolated as f32;
        score.1 += params.pawns_isolated.1 * self.pawns_isolated as f32;

        score.0 += params.bishops_xray.0 * self.bishops_xray as f32;
        score.1 += params.bishops_xray.1 * self.bishops_xray as f32;

        score.0 += params.bishops_pair.0 * self.bishops_pair as f32;
        score.1 += params.bishops_pair.1 * self.bishops_pair as f32;

        score.0 += params.rooks_open_file.0 * self.rooks_open_file as f32;
        score.1 += params.rooks_open_file.1 * self.rooks_open_file as f32;

        score.0 += params.rooks_halfopen_file.0 * self.rooks_halfopen_file as f32;
        score.1 += params.rooks_halfopen_file.1 * self.rooks_halfopen_file as f32;

        for i in 0..30 {
            score.0 += params.king_safety[i] * self.king_safety[i] as f32;
        }

        score.0 += params.king_check_knight * self.king_check_knight as f32;

        score.0 += params.king_check_bishop * self.king_check_bishop as f32;

        score.0 += params.king_check_rook * self.king_check_rook as f32;

        score.0 += params.king_check_queen * self.king_check_queen as f32;

        for i in 0..64 {
            score.0 += params.pst_pawn[i].0 * self.pst_pawn[i] as f32;
            score.1 += params.pst_pawn[i].1 * self.pst_pawn[i] as f32;

            score.0 += params.pst_knight[i].0 * self.pst_knight[i] as f32;
            score.1 += params.pst_knight[i].1 * self.pst_knight[i] as f32;

            score.0 += params.pst_bishop[i].0 * self.pst_bishop[i] as f32;
            score.1 += params.pst_bishop[i].1 * self.pst_bishop[i] as f32;

            score.0 += params.pst_rook[i].0 * self.pst_rook[i] as f32;
            score.1 += params.pst_rook[i].1 * self.pst_rook[i] as f32;

            score.0 += params.pst_queen[i].0 * self.pst_queen[i] as f32;
            score.1 += params.pst_queen[i].1 * self.pst_queen[i] as f32;

            score.0 += params.pst_king[i].0 * self.pst_king[i] as f32;
            score.1 += params.pst_king[i].1 * self.pst_king[i] as f32;
        }

        (score.0 as f32 * phase + score.1 as f32 * (62. - phase)) / 62.
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
            print_array(&self.pawns_passed, "PASSED_PAWN");
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
        let mut g_pawns_isolated = (0., 0.);

        let mut g_bishops_xray = (0., 0.);
        let mut g_bishops_pair = (0., 0.);

        let mut g_rooks_open_file = (0., 0.);
        let mut g_rooks_halfopen_file = (0., 0.);

        let mut g_king_safety = [0.; 30];
        let mut g_king_check_knight = 0.;
        let mut g_king_check_bishop = 0.;
        let mut g_king_check_rook = 0.;
        let mut g_king_check_queen = 0.;

        let mut g_pst_pawn = [(0., 0.); 64];
        let mut g_pst_knight = [(0., 0.); 64];
        let mut g_pst_bishop = [(0., 0.); 64];
        let mut g_pst_rook = [(0., 0.); 64];
        let mut g_pst_queen = [(0., 0.); 64];
        let mut g_pst_king = [(0., 0.); 64];

        let n = traces.len() as f32;
        let _g = self.k * 10_f32.ln() / 400.;

        for trace in traces {
            let phase = trace.phase as f32;

            let r = trace.result;
            let s = sigmoid(self.k, trace.evaluate(&self));
            let grad = -(r - s) * s * (1. - s);

            if TUNE_MATERIAL_PAWN {
                // For pawns we only tune endgame scores and leave the midgame scores fixed at 100.
                let x = trace.material[0] as f32;
                g_material[0].1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_MATERIAL_KNIGHT {
                let i = Piece::Knight.index();
                let x = trace.material[i] as f32;
                g_material[i].0 += x * grad * phase / 62.;
                g_material[i].1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_MATERIAL_BISHOP {
                let i = Piece::Bishop.index();
                let x = trace.material[i] as f32;
                g_material[i].0 += x * grad * phase / 62.;
                g_material[i].1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_MATERIAL_ROOK {
                let i = Piece::Rook.index();
                let x = trace.material[i] as f32;
                g_material[i].0 += x * grad * phase / 62.;
                g_material[i].1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_MATERIAL_QUEEN {
                let i = Piece::Queen.index();
                let x = trace.material[i] as f32;
                g_material[i].0 += x * grad * phase / 62.;
                g_material[i].1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_MOBILITY_PAWN {
                let x = trace.mobility_pawn as f32;
                g_mobility_pawn.0 += x * grad * phase / 62.;
                g_mobility_pawn.1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_MOBILITY_KNIGHT {
                for i in 0..9 {
                    let x = trace.mobility_knight[i] as f32;
                    g_mobility_knight[i].0 += x * grad * phase / 62.;
                    g_mobility_knight[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_MOBILITY_BISHOP {
                for i in 0..14 {
                    let x = trace.mobility_bishop[i] as f32;
                    g_mobility_bishop[i].0 += x * grad * phase / 62.;
                    g_mobility_bishop[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_MOBILITY_ROOK {
                for i in 0..15 {
                    let x = trace.mobility_rook[i] as f32;
                    g_mobility_rook[i].0 += x * grad * phase / 62.;
                    g_mobility_rook[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_MOBILITY_QUEEN {
                for i in 0..29 {
                    let x = trace.mobility_queen[i] as f32;
                    g_mobility_queen[i].0 += x * grad * phase / 62.;
                    g_mobility_queen[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_PAWNS_DOUBLED {
                let x = trace.pawns_doubled as f32;
                g_pawns_doubled.0 += x * grad * phase / 62.;
                g_pawns_doubled.1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_PAWNS_ISOLATED {
                let x = trace.pawns_isolated as f32;
                g_pawns_isolated.0 += x * grad * phase / 62.;
                g_pawns_isolated.1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_PAWNS_PASSED {
                for i in 0..8 {
                    let x = trace.pawns_passed[i] as f32;
                    g_pawns_passed[i].0 += x * grad * phase / 62.;
                    g_pawns_passed[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_BISHOPS_XRAY {
                let x = trace.bishops_xray as f32;
                g_bishops_xray.0 += x * grad * phase / 62.;
                g_bishops_xray.1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_BISHOPS_PAIR {
                let x = trace.bishops_pair as f32;
                g_bishops_pair.0 += x * grad * phase / 62.;
                g_bishops_pair.1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_ROOKS_OPEN_FILE {
                let x = trace.rooks_open_file as f32;
                g_rooks_open_file.0 += x * grad * phase / 62.;
                g_rooks_open_file.1 += x * grad * (62. - phase) / 62.;
            }

            if TUNE_ROOKS_HALFOPEN_FILE {
                let x = trace.rooks_halfopen_file as f32;
                g_rooks_halfopen_file.0 += x * grad * phase / 62.;
                g_rooks_halfopen_file.1 += x * grad * (62. - phase) / 62.;
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

            if TUNE_PST_PAWN {
                for i in 0..64 {
                    let x = trace.pst_pawn[i] as f32;
                    g_pst_pawn[i].0 += x * grad * phase / 62.;
                    g_pst_pawn[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_PST_KNIGHT {
                for i in 0..64 {
                    let x = trace.pst_knight[i] as f32;
                    g_pst_knight[i].0 += x * grad * phase / 62.;
                    g_pst_knight[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_PST_BISHOP {
                for i in 0..64 {
                    let x = trace.pst_bishop[i] as f32;
                    g_pst_bishop[i].0 += x * grad * phase / 62.;
                    g_pst_bishop[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_PST_ROOK {
                for i in 0..64 {
                    let x = trace.pst_rook[i] as f32;
                    g_pst_rook[i].0 += x * grad * phase / 62.;
                    g_pst_rook[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_PST_QUEEN {
                for i in 0..64 {
                    let x = trace.pst_queen[i] as f32;
                    g_pst_queen[i].0 += x * grad * phase / 62.;
                    g_pst_queen[i].1 += x * grad * (62. - phase) / 62.;
                }
            }

            if TUNE_PST_KING {
                for i in 0..64 {
                    let x = trace.pst_king[i] as f32;
                    g_pst_king[i].0 += x * grad * phase / 62.;
                    g_pst_king[i].1 += x * grad * (62. - phase) / 62.;
                }
            }
        }

        let mut norm = 0.;

        for i in 0..5 {
            norm += g_material[i].0.powf(2.);
            norm += g_material[i].1.powf(2.);
        }

        norm += g_mobility_pawn.0.powf(2.);
        norm += g_mobility_pawn.1.powf(2.);

        for i in 0..9 {
            norm += g_mobility_knight[i].0.powf(2.);
            norm += g_mobility_knight[i].1.powf(2.);
        }

        for i in 0..14 {
            norm += g_mobility_bishop[i].0.powf(2.);
            norm += g_mobility_bishop[i].1.powf(2.);
        }

        for i in 0..15 {
            norm += g_mobility_rook[i].0.powf(2.);
            norm += g_mobility_rook[i].1.powf(2.);
        }

        for i in 0..29 {
            norm += g_mobility_queen[i].0.powf(2.);
            norm += g_mobility_queen[i].1.powf(2.);
        }

        norm += g_pawns_doubled.0.powf(2.);
        norm += g_pawns_doubled.1.powf(2.);

        norm += g_pawns_isolated.0.powf(2.);
        norm += g_pawns_isolated.1.powf(2.);

        for i in 0..8 {
            norm += g_pawns_passed[i].0.powf(2.);
            norm += g_pawns_passed[i].1.powf(2.);
        }

        norm += g_bishops_xray.0.powf(2.);
        norm += g_bishops_xray.1.powf(2.);

        norm += g_bishops_pair.0.powf(2.);
        norm += g_bishops_pair.1.powf(2.);

        norm += g_rooks_open_file.0.powf(2.);
        norm += g_rooks_open_file.1.powf(2.);

        norm += g_rooks_halfopen_file.0.powf(2.);
        norm += g_rooks_halfopen_file.1.powf(2.);

        for i in 0..30 {
            norm += g_king_safety[i].powf(2.);
        }

        norm += g_king_check_knight.powf(2.);
        norm += g_king_check_bishop.powf(2.);
        norm += g_king_check_rook.powf(2.);
        norm += g_king_check_queen.powf(2.);

        for i in 0..64 {
            norm += g_pst_pawn[i].0.powf(2.);
            norm += g_pst_pawn[i].1.powf(2.);
            norm += g_pst_knight[i].0.powf(2.);
            norm += g_pst_knight[i].1.powf(2.);
            norm += g_pst_bishop[i].0.powf(2.);
            norm += g_pst_bishop[i].1.powf(2.);
            norm += g_pst_rook[i].0.powf(2.);
            norm += g_pst_rook[i].1.powf(2.);
            norm += g_pst_queen[i].0.powf(2.);
            norm += g_pst_queen[i].1.powf(2.);
            norm += g_pst_king[i].0.powf(2.);
            norm += g_pst_king[i].1.powf(2.);
        }

        norm = norm.sqrt();
        //norm = 1.;
        let f = f / norm;

        for i in 0..6 {
            self.material[i].0 -= 2. / n * f * g_material[i].0;
            self.material[i].1 -= 2. / n * f * g_material[i].1;
        }

        self.mobility_pawn.0 -= 2. / n * f * g_mobility_pawn.0;
        self.mobility_pawn.1 -= 2. / n * f * g_mobility_pawn.1;

        for i in 0..9 {
            self.mobility_knight[i].0 -= 2. / n * f * g_mobility_knight[i].0;
            self.mobility_knight[i].1 -= 2. / n * f * g_mobility_knight[i].1;
        }

        for i in 0..14 {
            self.mobility_bishop[i].0 -= 2. / n * f * g_mobility_bishop[i].0;
            self.mobility_bishop[i].1 -= 2. / n * f * g_mobility_bishop[i].1;
        }

        for i in 0..15 {
            self.mobility_rook[i].0 -= 2. / n * f * g_mobility_rook[i].0;
            self.mobility_rook[i].1 -= 2. / n * f * g_mobility_rook[i].1;
        }

        for i in 0..29 {
            self.mobility_queen[i].0 -= 2. / n * f * g_mobility_queen[i].0;
            self.mobility_queen[i].1 -= 2. / n * f * g_mobility_queen[i].1;
        }

        self.pawns_doubled.0 -= 2. / n * f * g_pawns_doubled.0;
        self.pawns_doubled.1 -= 2. / n * f * g_pawns_doubled.1;

        for i in 0..8 {
            self.pawns_passed[i].0 -= 2. / n * f * g_pawns_passed[i].0;
            self.pawns_passed[i].1 -= 2. / n * f * g_pawns_passed[i].1;
        }

        self.pawns_isolated.0 -= 2. / n * f * g_pawns_isolated.0;
        self.pawns_isolated.1 -= 2. / n * f * g_pawns_isolated.1;

        self.bishops_xray.0 -= 2. / n * f * g_bishops_xray.0;
        self.bishops_xray.1 -= 2. / n * f * g_bishops_xray.1;

        self.bishops_pair.0 -= 2. / n * f * g_bishops_pair.0;
        self.bishops_pair.1 -= 2. / n * f * g_bishops_pair.1;

        self.rooks_open_file.0 -= 2. / n * f * g_rooks_open_file.0;
        self.rooks_open_file.1 -= 2. / n * f * g_rooks_open_file.1;

        self.rooks_halfopen_file.0 -= 2. / n * f * g_rooks_halfopen_file.0;
        self.rooks_halfopen_file.1 -= 2. / n * f * g_rooks_halfopen_file.1;

        for i in 0..30 {
            self.king_safety[i] -= 2. / n * f * g_king_safety[i];
        }

        self.king_check_knight -= 2. / n * f * g_king_check_knight;

        self.king_check_bishop -= 2. / n * f * g_king_check_bishop;

        self.king_check_rook -= 2. / n * f * g_king_check_rook;

        self.king_check_queen -= 2. / n * f * g_king_check_queen;

        for i in 0..64 {
            self.pst_pawn[i].0 -= 2. / n * f * g_pst_pawn[i].0;
            self.pst_pawn[i].1 -= 2. / n * f * g_pst_pawn[i].1;

            self.pst_knight[i].0 -= 2. / n * f * g_pst_knight[i].0;
            self.pst_knight[i].1 -= 2. / n * f * g_pst_knight[i].1;

            self.pst_bishop[i].0 -= 2. / n * f * g_pst_bishop[i].0;
            self.pst_bishop[i].1 -= 2. / n * f * g_pst_bishop[i].1;

            self.pst_rook[i].0 -= 2. / n * f * g_pst_rook[i].0;
            self.pst_rook[i].1 -= 2. / n * f * g_pst_rook[i].1;

            self.pst_queen[i].0 -= 2. / n * f * g_pst_queen[i].0;
            self.pst_queen[i].1 -= 2. / n * f * g_pst_queen[i].1;

            self.pst_king[i].0 -= 2. / n * f * g_pst_king[i].0;
            self.pst_king[i].1 -= 2. / n * f * g_pst_king[i].1;
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
        let pawns_isolated = (mg(ISOLATED_PAWN) as f32, eg(ISOLATED_PAWN) as f32);
        let mut pawns_passed = [(0., 0.); 8];
        for i in 0..8 {
            pawns_passed[i] = (mg(PASSED_PAWN[i]) as f32, eg(PASSED_PAWN[i]) as f32);
        }

        let mut king_safety = [0.; 30];
        for i in 0..30 {
            king_safety[i] = KING_SAFETY[i] as f32;
        }

        let mut pst_pawn = [(0., 0.); 64];
        let mut pst_knight = [(0., 0.); 64];
        let mut pst_bishop = [(0., 0.); 64];
        let mut pst_rook = [(0., 0.); 64];
        let mut pst_queen = [(0., 0.); 64];
        let mut pst_king = [(0., 0.); 64];

        for i in 0..64 {
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
            pawns_isolated,

            bishops_xray: (mg(XRAYED_SQUARE) as f32, eg(XRAYED_SQUARE) as f32),
            bishops_pair: (mg(BISHOP_PAIR) as f32, eg(BISHOP_PAIR) as f32),

            rooks_open_file: (mg(ROOK_OPEN_FILE) as f32, eg(ROOK_OPEN_FILE) as f32),
            rooks_halfopen_file: (mg(ROOK_HALFOPEN_FILE) as f32, eg(ROOK_HALFOPEN_FILE) as f32),

            king_safety,
            king_check_knight: mg(KING_CHECK_KNIGHT) as f32,
            king_check_bishop: mg(KING_CHECK_BISHOP) as f32,
            king_check_rook: mg(KING_CHECK_ROOK) as f32,
            king_check_queen: mg(KING_CHECK_QUEEN) as f32,

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

    let mut mp_allocations = MovePickerAllocations::default();

    let moves = if in_check {
        MovePicker::qsearch_in_check(
            position.clone(),
            Rc::new(RefCell::new(History::default())),
            &mut mp_allocations,
        )
    } else {
        MovePicker::qsearch(
            position.clone(),
            Rc::new(RefCell::new(History::default())),
            &mut mp_allocations,
        )
    };

    let mut best_score = -MATE_SCORE;
    let mut best_pos = position.clone();

    let mut num_moves = 0;
    for (_mtype, mov) in moves {
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
        name, x as isize, y as isize
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
        print!("S({:>4}, {:>4}), ", x.0 as isize, x.1 as isize);
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
        print!("{:>4}, ", x as isize);
    }
    println!();
    println!("];");
}

fn print_pst(pst: &[(f32, f32)], name: &str) {
    println!("#[rustfmt::skip]");
    println!("pub const {}: [EScore; 64] = [", name);
    for rank in 0..8 {
        print!("    ");

        for file in 0..8 {
            print!(
                "S({:>4}, {:>4}), ",
                pst[rank * 8 + file].0 as isize,
                pst[rank * 8 + file].1 as isize
            );
        }

        println!();
    }
    println!("];");
}
