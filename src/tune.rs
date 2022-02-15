/* Asymptote, a UCI chess engine
   Copyright (C) 2018-2022  Maximilian Lupke

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

use crate::bitboard::ALL_SQUARES;
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

const TUNE_TEMPO: bool = false;

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

const TUNE_CENTER_CONTROL: bool = false;

const TUNE_PAWNS_DOUBLED: bool = false;
const TUNE_PAWNS_ISOLATED: bool = false;
const TUNE_PAWNS_OPEN_ISOLATED: bool = false;
const TUNE_PAWNS_PASSED: bool = false;
const TUNE_PAWNS_PASSED_BLOCKED: bool = false;
const TUNE_PAWNS_WEAK: bool = false;
const TUNE_PAWNS_RUNNER: bool = false;

const TUNE_KNIGHT_OUTPOST: bool = true;
const TUNE_KNIGHT_OUTPOST_DEFENDED: bool = true;

const TUNE_BISHOPS_PAIR: bool = false;
const TUNE_BISHOPS_XRAY: bool = false;
const TUNE_BISHOPS_PAWNS_COLOR: bool = false;

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
    pub phase: i8,
    pub sf: i8,

    pub base_eval: EScore,

    pub tempo: [i8; 2],
    pub material: [[i8; 2]; 6],

    pub mobility_pawn: [i8; 2],
    pub mobility_knight: [[i8; 2]; 9],
    pub mobility_bishop: [[i8; 2]; 14],
    pub mobility_rook: [[i8; 2]; 15],
    pub mobility_queen: [[i8; 2]; 29],

    pub center_control: [i8; 2],

    pub pawns_doubled: [i8; 2],
    pub pawns_passed: [[i8; 2]; 8],
    pub pawns_passed_blocked: [i8; 2],
    pub pawns_passed_file: [[i8; 2]; 8],
    pub pawns_open_isolated: [i8; 2],
    pub pawns_isolated: [i8; 2],
    pub pawns_weak: [i8; 2],
    pub pawns_runner: [i8; 2],

    pub knight_outposts: [i8; 2],
    pub knight_outposts_defended: [i8; 2],

    pub bishops_xray: [i8; 2],
    pub bishops_pair: [i8; 2],
    pub bishops_pawns_color: [[i8; 2]; 4],

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
}

#[derive(Clone)]
pub struct CompactTrace {
    result: f32,
    phase: i8,
    sf: i8,

    base_eval: (f32, f32),
    linear: Vec<i8>,

    king_safety: [i8; 30],
    king_check_knight: i8,
    king_check_bishop: i8,
    king_check_rook: i8,
    king_check_queen: i8,
    // Since the king danger computation is non linear, we have to keep the individual counts
    king_danger: [[i8; 2]; 6],
    king_danger_attacks: [i8; 2],
}

impl From<Trace> for CompactTrace {
    fn from(t: Trace) -> Self {
        let mut linear = Vec::new();

        if TUNE_MATERIAL_PAWN {
            linear.push(t.material[Piece::Pawn.index()][1] - t.material[Piece::Pawn.index()][0]);
        }

        if TUNE_MATERIAL_KNIGHT {
            linear
                .push(t.material[Piece::Knight.index()][1] - t.material[Piece::Knight.index()][0]);
        }

        if TUNE_MATERIAL_BISHOP {
            linear
                .push(t.material[Piece::Bishop.index()][1] - t.material[Piece::Bishop.index()][0]);
        }

        if TUNE_MATERIAL_ROOK {
            linear.push(t.material[Piece::Rook.index()][1] - t.material[Piece::Rook.index()][0]);
        }

        if TUNE_MATERIAL_QUEEN {
            linear.push(t.material[Piece::Queen.index()][1] - t.material[Piece::Queen.index()][0]);
        }

        if TUNE_TEMPO {
            linear.push(t.tempo[1] - t.tempo[0]);
        }

        if TUNE_CENTER_CONTROL {
            linear.push(t.center_control[1] - t.center_control[0]);
        }

        if TUNE_MOBILITY_PAWN {
            linear.push(t.mobility_pawn[1] - t.mobility_pawn[0]);
        }

        if TUNE_MOBILITY_KNIGHT {
            for i in 0..9 {
                linear.push(t.mobility_knight[i][1] - t.mobility_knight[i][0]);
            }
        }

        if TUNE_MOBILITY_BISHOP {
            for i in 0..14 {
                linear.push(t.mobility_bishop[i][1] - t.mobility_bishop[i][0]);
            }
        }

        if TUNE_MOBILITY_ROOK {
            for i in 0..15 {
                linear.push(t.mobility_rook[i][1] - t.mobility_rook[i][0]);
            }
        }

        if TUNE_MOBILITY_QUEEN {
            for i in 0..29 {
                linear.push(t.mobility_queen[i][1] - t.mobility_queen[i][0]);
            }
        }

        if TUNE_PAWNS_DOUBLED {
            linear.push(t.pawns_doubled[1] - t.pawns_doubled[0]);
        }

        if TUNE_PAWNS_ISOLATED {
            linear.push(t.pawns_isolated[1] - t.pawns_isolated[0]);
        }

        if TUNE_PAWNS_OPEN_ISOLATED {
            linear.push(t.pawns_open_isolated[1] - t.pawns_open_isolated[0]);
        }

        if TUNE_PAWNS_PASSED {
            for i in 0..8 {
                linear.push(t.pawns_passed[i][1] - t.pawns_passed[i][0]);
            }

            for i in 0..8 {
                linear.push(t.pawns_passed_file[i][1] - t.pawns_passed_file[i][0]);
            }
        }

        if TUNE_PAWNS_PASSED_BLOCKED {
            linear.push(t.pawns_passed_blocked[1] - t.pawns_passed_blocked[0]);
        }

        if TUNE_PAWNS_WEAK {
            linear.push(t.pawns_weak[1] - t.pawns_weak[0]);
        }

        if TUNE_PAWNS_RUNNER {
            linear.push(t.pawns_runner[1] - t.pawns_runner[0]);
        }

        if TUNE_KNIGHT_OUTPOST {
            linear.push(t.knight_outposts[1] - t.knight_outposts[0]);
        }

        if TUNE_KNIGHT_OUTPOST_DEFENDED {
            linear.push(t.knight_outposts_defended[1] - t.knight_outposts_defended[0]);
        }

        if TUNE_BISHOPS_PAIR {
            linear.push(t.bishops_pair[1] - t.bishops_pair[0]);
        }

        if TUNE_BISHOPS_XRAY {
            linear.push(t.bishops_xray[1] - t.bishops_xray[0]);
        }

        if TUNE_BISHOPS_PAWNS_COLOR {
            for i in 0..4 {
                linear.push(t.bishops_pawns_color[i][1] - t.bishops_pawns_color[i][0]);
            }
        }

        if TUNE_ROOKS_HALFOPEN_FILE {
            linear.push(t.rooks_halfopen_file[1] - t.rooks_halfopen_file[0]);
        }

        if TUNE_ROOKS_OPEN_FILE {
            linear.push(t.rooks_open_file[1] - t.rooks_open_file[0]);
        }

        if TUNE_ROOKS_PAIR {
            linear.push(t.rooks_pair[1] - t.rooks_pair[0]);
        }

        if TUNE_PST_PAWN {
            for i in ALL_SQUARES.squares() {
                linear.push(t.pst_pawn[i][1] - t.pst_pawn[i][0]);
            }
        }

        if TUNE_PST_KNIGHT {
            for i in ALL_SQUARES.squares() {
                linear.push(t.pst_knight[i][1] - t.pst_knight[i][0]);
            }
        }

        if TUNE_PST_BISHOP {
            for i in ALL_SQUARES.squares() {
                linear.push(t.pst_bishop[i][1] - t.pst_bishop[i][0]);
            }
        }

        if TUNE_PST_ROOK {
            for i in ALL_SQUARES.squares() {
                linear.push(t.pst_rook[i][1] - t.pst_rook[i][0]);
            }
        }

        if TUNE_PST_QUEEN {
            for i in ALL_SQUARES.squares() {
                linear.push(t.pst_queen[i][1] - t.pst_queen[i][0]);
            }
        }

        if TUNE_PST_KING {
            for i in ALL_SQUARES.squares() {
                linear.push(t.pst_king[i][1] - t.pst_king[i][0]);
            }
        }

        let mut base_eval = (mg(t.base_eval) as f32, eg(t.base_eval) as f32);

        let params = Parameters::default();
        let mut linear_eval = (0., 0.);
        evaluate_linear(&mut linear_eval, &params.linear, &linear);
        base_eval.0 -= linear_eval.0;
        base_eval.1 -= linear_eval.1;

        let mut king_safety = [0; 30];
        for i in 0..30 {
            king_safety[i] = t.king_safety[i][1] - t.king_safety[i][0];
        }

        CompactTrace {
            result: t.result,

            base_eval,
            linear,

            king_safety,
            king_check_knight: t.king_check_knight[1] - t.king_check_knight[0],
            king_check_bishop: t.king_check_bishop[1] - t.king_check_bishop[0],
            king_check_rook: t.king_check_rook[1] - t.king_check_rook[0],
            king_check_queen: t.king_check_queen[1] - t.king_check_queen[0],
            king_danger: t.king_danger,
            king_danger_attacks: t.king_danger_attacks,

            phase: t.phase,
            sf: t.sf,
        }
    }
}

#[derive(Clone)]
pub struct Parameters {
    pub k: f32,
    linear: Vec<(f32, f32)>,

    king_safety: [f32; 30],
    king_check_knight: f32,
    king_check_bishop: f32,
    king_check_rook: f32,
    king_check_queen: f32,
    king_danger: [f32; 6],
    king_danger_attacks: [f32; 7],
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
            tempo: [0; 2],
            material: [[0; 2]; 6],

            base_eval: S(0, 0),

            mobility_pawn: [0; 2],
            mobility_knight: [[0; 2]; 9],
            mobility_bishop: [[0; 2]; 14],
            mobility_rook: [[0; 2]; 15],
            mobility_queen: [[0; 2]; 29],

            center_control: [0; 2],

            pawns_doubled: [0; 2],
            pawns_passed: [[0; 2]; 8],
            pawns_passed_blocked: [0; 2],
            pawns_passed_file: [[0; 2]; 8],
            pawns_open_isolated: [0; 2],
            pawns_isolated: [0; 2],
            pawns_weak: [0; 2],
            pawns_runner: [0; 2],

            knight_outposts: [0; 2],
            knight_outposts_defended: [0; 2],

            bishops_xray: [0; 2],
            bishops_pair: [0; 2],
            bishops_pawns_color: [[0; 2]; 4],

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
        let mut score = self.base_eval;

        evaluate_linear(&mut score, &params.linear, &self.linear);

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

        sf * (score.0 as f32 * phase + score.1 as f32 * (62. - phase)) / 62.
    }
}

fn sigmoid(k: f32, q: f32) -> f32 {
    1. / (1. + 10_f32.powf(-k * q / 400.))
}

impl Parameters {
    pub fn print_weights(&self) {
        let mut i = 0;

        if TUNE_MATERIAL_PAWN {
            print_single(self.linear[i], "PAWN_SCORE");
            i += 1;
        }

        if TUNE_MATERIAL_KNIGHT {
            print_single(self.linear[i], "KNIGHT_SCORE");
            i += 1;
        }

        if TUNE_MATERIAL_BISHOP {
            print_single(self.linear[i], "BISHOP_SCORE");
            i += 1;
        }

        if TUNE_MATERIAL_ROOK {
            print_single(self.linear[i], "ROOK_SCORE");
            i += 1;
        }

        if TUNE_MATERIAL_QUEEN {
            print_single(self.linear[i], "QUEEN_SCORE");
            i += 1;
        }

        if TUNE_TEMPO {
            print_single(self.linear[i], "TEMPO_SCORE");
            i += 1;
        }

        if TUNE_CENTER_CONTROL {
            print_single(self.linear[i], "CENTER_CONTROL");
            i += 1;
        }

        if TUNE_MOBILITY_PAWN {
            print_single(self.linear[i], "PAWN_MOBILITY");
            i += 1;
        }

        if TUNE_MOBILITY_KNIGHT {
            print_array(&self.linear[i..i + 9], "KNIGHT_MOBILITY");
            i += 9;
        }

        if TUNE_MOBILITY_BISHOP {
            print_array(&self.linear[i..i + 14], "BISHOP_MOBILITY");
            i += 14;
        }

        if TUNE_MOBILITY_ROOK {
            print_array(&self.linear[i..i + 15], "ROOK_MOBILITY");
            i += 15;
        }

        if TUNE_MOBILITY_QUEEN {
            print_array(&self.linear[i..i + 29], "QUEEN_MOBILITY");
            i += 29;
        }

        if TUNE_PAWNS_DOUBLED {
            print_single(self.linear[i], "DOUBLED_PAWN");
            i += 1;
        }

        if TUNE_PAWNS_ISOLATED {
            print_single(self.linear[i], "ISOLATED_PAWN");
            i += 1;
        }

        if TUNE_PAWNS_OPEN_ISOLATED {
            print_single(self.linear[i], "OPEN_ISOLATED_PAWN");
            i += 1;
        }

        if TUNE_PAWNS_PASSED {
            print_array(&self.linear[i..i + 8], "PASSED_PAWN_ON_RANK");
            i += 8;
            print_array(&self.linear[i..i + 8], "PASSED_PAWN_ON_FILE");
            i += 8;
        }

        if TUNE_PAWNS_PASSED_BLOCKED {
            print_single(self.linear[i], "BLOCKED_PASSED_PAWN");
            i += 1;
        }

        if TUNE_PAWNS_WEAK {
            print_single(self.linear[i], "WEAK_PAWN");
            i += 1;
        }

        if TUNE_PAWNS_RUNNER {
            print_single(self.linear[i], "PAWN_RUNNER");
            i += 1;
        }

        if TUNE_KNIGHT_OUTPOST {
            print_single(self.linear[i], "KNIGHT_OUTPOST");
            i += 1;
        }

        if TUNE_KNIGHT_OUTPOST_DEFENDED {
            print_single(self.linear[i], "KNIGHT_DEFENDED_OUTPOST");
            i += 1;
        }

        if TUNE_BISHOPS_PAIR {
            print_single(self.linear[i], "BISHOP_PAIR");
            i += 1;
        }

        if TUNE_BISHOPS_XRAY {
            print_single(self.linear[i], "XRAYED_SQUARE");
            i += 1;
        }

        if TUNE_BISHOPS_PAWNS_COLOR {
            print_array(&self.linear[i..i + 4], "BISHOP_PAWNS_ON_COLOR");
            i += 4;
        }

        if TUNE_ROOKS_HALFOPEN_FILE {
            print_single(self.linear[i], "ROOK_HALFOPEN_FILE");
            i += 1;
        }

        if TUNE_ROOKS_OPEN_FILE {
            print_single(self.linear[i], "ROOK_OPEN_FILE");
            i += 1;
        }

        if TUNE_ROOKS_PAIR {
            print_single(self.linear[i], "ROOK_PAIR");
            i += 1;
        }

        if TUNE_PST_PAWN {
            print_pst(&self.linear[i..i + 64], "PAWN_PST");
            i += 64;
        }

        if TUNE_PST_KNIGHT {
            print_pst(&self.linear[i..i + 64], "KNIGHT_PST");
            i += 64;
        }

        if TUNE_PST_BISHOP {
            print_pst(&self.linear[i..i + 64], "BISHOP_PST");
            i += 64;
        }

        if TUNE_PST_ROOK {
            print_pst(&self.linear[i..i + 64], "ROOK_PST");
            i += 64;
        }

        if TUNE_PST_QUEEN {
            print_pst(&self.linear[i..i + 64], "QUEEN_PST");
            i += 64;
        }

        if TUNE_PST_KING {
            print_pst(&self.linear[i..i + 64], "KING_PST");
            i += 64;
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
        let mut g_linear = vec![(0., 0.); self.linear.len()];

        let mut g_king_safety = [0.; 30];
        let mut g_king_check_knight = 0.;
        let mut g_king_check_bishop = 0.;
        let mut g_king_check_rook = 0.;
        let mut g_king_check_queen = 0.;
        let mut g_king_danger = [0.; 6];
        let mut g_king_danger_attacks = [0.; 7];

        let n = traces.len() as f32;
        let _g = self.k * 10_f32.ln() / 400.;

        for trace in traces {
            let phase = trace.phase as f32;

            let r = trace.result;
            let s = sigmoid(self.k, trace.evaluate(&self));
            let sf = trace.sf as f32 / SF_NORMAL as f32;
            let grad = -(r - s) * s * (1. - s) * sf;

            update_gradient_array(&mut g_linear, &trace.linear, grad, phase);
            if TUNE_MATERIAL_PAWN {
                // For pawns we only tune endgame scores and leave the midgame scores fixed at 100.
                g_linear[0].0 = 0.;
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
        }

        let mut norm = 0.;
        norm += norm_array(&g_linear);

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

        if norm == 0.0 {
            return;
        }

        norm = norm.sqrt();
        let f = f / norm;

        update_parameter_array(&mut self.linear, &g_linear, f / n);

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
    }
}

impl Default for Parameters {
    fn default() -> Parameters {
        let mut linear = Vec::new();

        if TUNE_MATERIAL_PAWN {
            linear.push((mg(PAWN_SCORE) as f32, eg(PAWN_SCORE) as f32));
        }

        if TUNE_MATERIAL_KNIGHT {
            linear.push((mg(KNIGHT_SCORE) as f32, eg(KNIGHT_SCORE) as f32));
        }

        if TUNE_MATERIAL_BISHOP {
            linear.push((mg(BISHOP_SCORE) as f32, eg(BISHOP_SCORE) as f32));
        }

        if TUNE_MATERIAL_ROOK {
            linear.push((mg(ROOK_SCORE) as f32, eg(ROOK_SCORE) as f32));
        }

        if TUNE_MATERIAL_QUEEN {
            linear.push((mg(QUEEN_SCORE) as f32, eg(QUEEN_SCORE) as f32));
        }

        if TUNE_TEMPO {
            linear.push((mg(TEMPO_SCORE) as f32, eg(TEMPO_SCORE) as f32));
        }

        if TUNE_CENTER_CONTROL {
            linear.push((mg(CENTER_CONTROL) as f32, eg(CENTER_CONTROL) as f32));
        }

        if TUNE_MOBILITY_PAWN {
            linear.push((mg(PAWN_MOBILITY) as f32, eg(PAWN_MOBILITY) as f32));
        }

        if TUNE_MOBILITY_KNIGHT {
            for i in 0..9 {
                linear.push((mg(KNIGHT_MOBILITY[i]) as f32, eg(KNIGHT_MOBILITY[i]) as f32));
            }
        }

        if TUNE_MOBILITY_BISHOP {
            for i in 0..14 {
                linear.push((mg(BISHOP_MOBILITY[i]) as f32, eg(BISHOP_MOBILITY[i]) as f32));
            }
        }

        if TUNE_MOBILITY_ROOK {
            for i in 0..15 {
                linear.push((mg(ROOK_MOBILITY[i]) as f32, eg(ROOK_MOBILITY[i]) as f32));
            }
        }

        if TUNE_MOBILITY_QUEEN {
            for i in 0..29 {
                linear.push((mg(QUEEN_MOBILITY[i]) as f32, eg(QUEEN_MOBILITY[i]) as f32));
            }
        }

        if TUNE_PAWNS_DOUBLED {
            linear.push((mg(DOUBLED_PAWN) as f32, eg(DOUBLED_PAWN) as f32));
        }

        if TUNE_PAWNS_ISOLATED {
            linear.push((mg(ISOLATED_PAWN) as f32, eg(ISOLATED_PAWN) as f32));
        }

        if TUNE_PAWNS_OPEN_ISOLATED {
            linear.push((mg(OPEN_ISOLATED_PAWN) as f32, eg(OPEN_ISOLATED_PAWN) as f32));
        }

        if TUNE_PAWNS_PASSED {
            for &weight in PASSED_PAWN_ON_RANK.iter() {
                linear.push((mg(weight) as f32, eg(weight) as f32));
            }

            for &weight in PASSED_PAWN_ON_FILE.iter() {
                linear.push((mg(weight) as f32, eg(weight) as f32));
            }
        }

        if TUNE_PAWNS_PASSED_BLOCKED {
            linear.push((
                mg(BLOCKED_PASSED_PAWN) as f32,
                eg(BLOCKED_PASSED_PAWN) as f32,
            ));
        }

        if TUNE_PAWNS_WEAK {
            linear.push((mg(WEAK_PAWN) as f32, eg(WEAK_PAWN) as f32));
        }

        if TUNE_PAWNS_RUNNER {
            linear.push((mg(PAWN_RUNNER) as f32, eg(PAWN_RUNNER) as f32));
        }

        if TUNE_KNIGHT_OUTPOST {
            linear.push((mg(KNIGHT_OUTPOST) as f32, eg(KNIGHT_OUTPOST) as f32));
        }

        if TUNE_KNIGHT_OUTPOST_DEFENDED {
            linear.push((
                mg(KNIGHT_DEFENDED_OUTPOST) as f32,
                eg(KNIGHT_DEFENDED_OUTPOST) as f32,
            ));
        }

        if TUNE_BISHOPS_PAIR {
            linear.push((mg(BISHOP_PAIR) as f32, eg(BISHOP_PAIR) as f32));
        }

        if TUNE_BISHOPS_XRAY {
            linear.push((mg(XRAYED_SQUARE) as f32, eg(XRAYED_SQUARE) as f32));
        }

        if TUNE_BISHOPS_PAWNS_COLOR {
            for &weight in BISHOP_PAWNS_ON_COLOR.iter() {
                linear.push((mg(weight) as f32, eg(weight) as f32));
            }
        }

        if TUNE_ROOKS_HALFOPEN_FILE {
            linear.push((mg(ROOK_HALFOPEN_FILE) as f32, eg(ROOK_HALFOPEN_FILE) as f32));
        }

        if TUNE_ROOKS_OPEN_FILE {
            linear.push((mg(ROOK_OPEN_FILE) as f32, eg(ROOK_OPEN_FILE) as f32));
        }

        if TUNE_ROOKS_PAIR {
            linear.push((mg(ROOK_PAIR) as f32, eg(ROOK_PAIR) as f32));
        }

        if TUNE_PST_PAWN {
            for i in ALL_SQUARES.squares() {
                linear.push((mg(PAWN_PST[i]) as f32, eg(PAWN_PST[i]) as f32));
            }
        }

        if TUNE_PST_KNIGHT {
            for i in ALL_SQUARES.squares() {
                linear.push((mg(KNIGHT_PST[i]) as f32, eg(KNIGHT_PST[i]) as f32));
            }
        }

        if TUNE_PST_BISHOP {
            for i in ALL_SQUARES.squares() {
                linear.push((mg(BISHOP_PST[i]) as f32, eg(BISHOP_PST[i]) as f32));
            }
        }

        if TUNE_PST_ROOK {
            for i in ALL_SQUARES.squares() {
                linear.push((mg(ROOK_PST[i]) as f32, eg(ROOK_PST[i]) as f32));
            }
        }

        if TUNE_PST_QUEEN {
            for i in ALL_SQUARES.squares() {
                linear.push((mg(QUEEN_PST[i]) as f32, eg(QUEEN_PST[i]) as f32));
            }
        }

        if TUNE_PST_KING {
            for i in ALL_SQUARES.squares() {
                linear.push((mg(KING_PST[i]) as f32, eg(KING_PST[i]) as f32));
            }
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

        Parameters {
            k: 1.,
            linear,

            king_safety,
            king_check_knight: mg(KING_CHECK_KNIGHT) as f32,
            king_check_bishop: mg(KING_CHECK_BISHOP) as f32,
            king_check_rook: mg(KING_CHECK_ROOK) as f32,
            king_check_queen: mg(KING_CHECK_QUEEN) as f32,
            king_danger,
            king_danger_attacks,
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

fn print_pst(pst: &[(f32, f32)], name: &str) {
    println!("#[rustfmt::skip]");
    println!(
        "pub static {}: SquareMap<EScore> = SquareMap::from_array([",
        name
    );
    for rank in 0..8 {
        print!("    ");

        for file in 0..8 {
            let sq = 8 * rank + file;
            print!(
                "S({:>4}, {:>4}), ",
                pst[sq as usize].0.round() as isize,
                pst[sq as usize].1.round() as isize
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

fn evaluate_linear(score: &mut (f32, f32), params: &[(f32, f32)], coeffs: &[i8]) {
    assert_eq!(params.len(), coeffs.len());

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
