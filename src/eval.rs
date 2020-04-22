/* Asymptote, a UCI chess engine
   Copyright (C) 2018-2020  Maximilian Lupke

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

use crate::bitboard::*;
use crate::hash::*;
use crate::movegen::*;
use crate::position::*;
use crate::types::SquareMap;

#[cfg(feature = "tune")]
use crate::tune::*;

#[derive(Clone)]
pub struct Eval {
    material: [[u8; 5]; 2],
    pst: [EScore; 2],
    pawn_table: [PawnHashEntry; PAWN_TABLE_NUM_ENTRIES],
    attacked_by: [[Bitboard; 6]; 2],
    attacked_by_1: [Bitboard; 2],
    attacked_by_2: [Bitboard; 2],

    #[cfg(feature = "tune")]
    pub trace: Trace,
}

const PAWN_TABLE_NUM_ENTRIES: usize = 2 * 1024;

#[derive(Copy, Clone, Debug, Default)]
struct PawnHashEntry {
    hash: Hash,
    score: EScore,
}

pub type Score = i16;
pub type EScore = i32;

pub const fn S(mg: i16, eg: i16) -> EScore {
    ((eg as u32) << 16) as EScore + mg as EScore
}

pub const fn mg(s: EScore) -> i16 {
    (s & 0xFFFF) as i16
}

pub const fn eg(s: EScore) -> i16 {
    ((s + 0x8000) as u32 >> 16) as i16
}

fn interpolate(score: EScore, phase: i16) -> i32 {
    let mg = i32::from(mg(score));
    let eg = i32::from(eg(score));
    let phase = i32::from(phase);

    (mg * phase + eg * (62 - phase)) / 62
}

pub const MATE_SCORE: Score = 20000;
pub const SF_NORMAL: i32 = 64;
const SF_PAWNLESS: i32 = 32;

pub const TEMPO_SCORE: EScore = S(26, 23);

pub const PAWN_SCORE: EScore = S(100, 121);
pub const KNIGHT_SCORE: EScore = S(330, 290);
pub const BISHOP_SCORE: EScore = S(324, 318);
pub const ROOK_SCORE: EScore = S(459, 568);
pub const QUEEN_SCORE: EScore = S(990, 1000);

pub const PAWN_MOBILITY: EScore = S(9, 6);
#[rustfmt::skip]
pub const KNIGHT_MOBILITY: [EScore; 9] = [
    S(-137, -138), S( -58,  -67), S( -18,  -23), S(   8,   14),
    S(  28,   24), S(  32,   34), S(  43,   39), S(  53,   49),
    S(  67,   42),
];
#[rustfmt::skip]
pub const BISHOP_MOBILITY: [EScore; 14] = [
    S(-110, -110), S( -68,  -68), S( -23,  -29), S(  -7,  -12),
    S(   7,   -1), S(  12,    4), S(  18,   12), S(  19,   27),
    S(  22,   22), S(  25,   25), S(  30,   30), S(  35,   35),
    S(  40,   40), S(  45,   45),
];
#[rustfmt::skip]
pub const ROOK_MOBILITY: [EScore; 15] = [
    S(-105, -105), S( -65,  -65), S( -22,  -26), S( -17,  -18),
    S(  -8,   -7), S(  -7,   -1), S(  -2,    4), S(   4,   11),
    S(  10,   19), S(  20,   20), S(  25,   25), S(  30,   30),
    S(  35,   35), S(  40,   40), S(  45,   45),
];

#[rustfmt::skip]
pub const QUEEN_MOBILITY: [EScore; 29] = [
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(   0,    0),
];

pub const CENTER_CONTROL: EScore = S(5, 1);

pub const DOUBLED_PAWN: EScore = S(-5, -23);
pub const OPEN_ISOLATED_PAWN: EScore = S(-26, -11);
pub const ISOLATED_PAWN: EScore = S(-27, 5);

#[rustfmt::skip]
pub const PASSED_PAWN_ON_RANK: [EScore; 8] = [
    S(   0,    0), S(   7,  -11), S(   7,   -2), S(   8,   21), 
    S(  30,   47), S(  49,  111), S(  66,  176), S(   0,    0), 
];

#[rustfmt::skip]
pub const PASSED_PAWN_ON_FILE: [EScore; 8] = [
    S(   1,   19), S(   3,   10), S(  -2,    0), S(  -6,  -13), 
    S(  -8,  -15), S(  -2,  -10), S(  -3,    7), S(  -6,    9), 
];

pub const KNIGHT_OUTPOST: EScore = S(29, -8);

pub const XRAYED_SQUARE: EScore = S(5, 0);
pub const BISHOP_PAIR: EScore = S(42, 48);

pub const ROOK_OPEN_FILE: EScore = S(30, 8);
pub const ROOK_HALFOPEN_FILE: EScore = S(10, 18);
pub const ROOK_PAIR: EScore = S(17, -58);

#[rustfmt::skip]
pub const KING_SAFETY: [Score; 30] = [
       0,   -1,   -3,  -15,
     -23,  -31,  -38,  -49,
     -55,  -63,  -80, -105,
    -134, -174, -200, -233,
    -258, -291, -327, -361,
    -400, -441, -484, -529,
    -576, -625, -676, -729,
    -784, -841,
];

pub const KING_CHECK_KNIGHT: EScore = S(-85, 0);
pub const KING_CHECK_BISHOP: EScore = S(-15, 0);
pub const KING_CHECK_ROOK: EScore = S(-52, 0);
pub const KING_CHECK_QUEEN: EScore = S(-49, 0);

#[rustfmt::skip]
pub const KING_DANGER: [Score; 6] = [
      -1,  -28,  -13,  -32,
     -65,    1,
];

#[rustfmt::skip]
pub const KING_DANGER_WEIGHT: [i32; 7] = [
       0,    5,   58,  106,
     113,  121,  124,
];

#[rustfmt::skip]
pub const PAWN_PST: SquareMap<EScore> = SquareMap::from_array([
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(  20,   28), S(  28,   29), S(  26,   22), S(  32,   24), S(  34,   26), S(  25,   22), S(  22,   21), S(  15,   20),
    S(   2,   28), S(   9,   21), S(  10,    8), S(   8,   -9), S(  17,   -7), S(  16,    1), S(   5,    9), S(  -4,   14),
    S( -11,    9), S( -18,    1), S(   1,   -9), S(  15,  -18), S(  15,  -17), S(   8,  -16), S( -22,   -1), S( -19,   -5),
    S( -23,   -7), S( -24,   -8), S(  -6,  -15), S(  13,  -18), S(   9,  -15), S(   7,  -20), S( -15,  -13), S( -22,  -16),
    S( -19,  -17), S( -27,  -11), S( -13,  -19), S( -11,  -11), S(   3,   -9), S(  -2,  -11), S(  12,  -19), S(  -8,  -24),
    S( -26,  -15), S( -18,  -20), S( -25,   -9), S( -20,  -11), S( -14,   -3), S(  12,  -13), S(  17,  -23), S( -20,  -37),
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
]);
#[rustfmt::skip]
pub const KNIGHT_PST: SquareMap<EScore> = SquareMap::from_array([
    S( -10,   -6), S( -11,  -10), S(  -7,   -7), S(  -9,  -11), S(  -4,   -5), S(  -8,  -10), S(  -9,   -9), S( -10,   -7),
    S( -11,   -7), S(  -1,    0), S(  11,    0), S(  16,   12), S(  12,    9), S(  11,    3), S(   0,   -1), S(  -9,  -11),
    S( -11,  -10), S(   6,    0), S(  12,   12), S(  20,   15), S(  21,   13), S(  17,   11), S(   8,    1), S(  -6,   -7),
    S(  -5,    0), S(  16,    7), S(  14,   14), S(  33,   22), S(  26,   21), S(  22,   15), S(  14,   13), S(   1,   -3),
    S(   9,    0), S(   2,    0), S(   6,   11), S(  14,   25), S(  21,   14), S(  26,   16), S(   7,    3), S(  18,    3),
    S(  13,   -7), S(  -6,    0), S(   7,   -2), S(   4,    9), S(  15,    8), S(  15,    1), S(  27,   -1), S(   9,   -5),
    S(  -8,   -8), S(  -3,   -3), S( -11,   -5), S(  14,   -5), S(  13,   -2), S(   9,   -3), S(   0,    1), S(   3,   -6),
    S( -10,   -7), S(  12,   -7), S( -15,  -11), S( -11,   -8), S(  -4,   -5), S( -12,  -10), S(  13,   -6), S(  -7,   -8),
]);
#[rustfmt::skip]
pub const BISHOP_PST: SquareMap<EScore> = SquareMap::from_array([
    S( -11,   -9), S(  -8,   -7), S( -11,  -12), S( -12,  -10), S(  -9,   -8), S(  -8,   -9), S(  -7,   -6), S( -11,   -9),
    S(  -7,   -2), S(  -1,    0), S(   2,    3), S(   5,   -1), S(   8,    7), S(   4,    3), S(  -1,    5), S( -17,   -9),
    S(  -7,    0), S(   6,    4), S(   4,    3), S(  16,    7), S(  16,   12), S(  13,   10), S(   6,    7), S(  -1,    2),
    S(  -7,   -1), S( -18,    7), S(   2,    5), S(  13,    6), S(   8,    4), S(   8,    7), S( -10,    1), S(   1,    1),
    S(  -7,   -4), S(   0,    5), S( -17,   11), S(   2,    6), S(   7,    1), S( -10,    6), S(  -1,    1), S(  -3,   -2),
    S(  -7,   -3), S(  10,    9), S(  -5,    5), S( -11,    8), S(  -2,    8), S(  13,    3), S(  13,    6), S(  -5,   -2),
    S(  -5,   -7), S(   9,   -3), S(   6,   -2), S(  -8,    1), S(  -1,    5), S(   6,    3), S(  33,   -3), S(  -6,  -12),
    S(  -9,   -9), S(  -4,   -4), S(   6,   -2), S(  -8,   -4), S(  -5,   -2), S(  11,  -10), S(  -9,   -7), S(  -8,   -7),
]);
#[rustfmt::skip]
pub const ROOK_PST: SquareMap<EScore> = SquareMap::from_array([
    S(  15,   17), S(  18,   18), S(  15,   17), S(  18,   16), S(  20,   15), S(  19,   21), S(  18,   19), S(  17,   18),
    S(  20,   24), S(  18,   24), S(  21,   23), S(  23,   17), S(  21,   13), S(  21,   18), S(  17,   22), S(  22,   23),
    S(   3,   14), S(   5,   15), S(   5,   11), S(   7,    9), S(   6,    6), S(   1,    4), S(   2,    8), S(   3,    9),
    S(  -1,   11), S(   1,    8), S(   2,   11), S(   7,    8), S(   3,    5), S(   3,    9), S(   0,    2), S(  -1,    8),
    S( -12,    5), S(  -3,    6), S(  -1,    6), S(  -2,    0), S(   1,    1), S(  -1,    2), S(   0,    2), S(  -6,   -1),
    S( -13,    0), S(  -4,   -1), S(  -4,   -6), S(   1,   -2), S(   0,   -6), S(   0,   -1), S(   1,    2), S( -15,   -8),
    S( -18,    1), S(  -5,   -3), S(  -4,   -1), S(   5,    1), S(   6,   -2), S(   2,   -3), S(  -6,   -7), S( -31,   -4),
    S(   6,    0), S(  -3,    6), S(  10,    6), S(  25,    1), S(  21,   -4), S(  34,    0), S( -21,    6), S(   5,  -15),
]);
#[rustfmt::skip]
pub const QUEEN_PST: SquareMap<EScore> = SquareMap::from_array([
    S( -19,  -13), S(  -3,   -3), S(  -2,    0), S(  -2,   -2), S(   0,    0), S(  -1,    0), S(  -1,   -1), S(  -6,   -3),
    S(  -8,   -4), S( -28,   -5), S(  -1,    1), S(   0,    3), S(   0,    2), S(   1,    2), S(   2,    3), S(   6,    3),
    S(  -6,   -4), S(  -2,   -1), S(  -6,    0), S(   2,    5), S(   2,    6), S(   4,    4), S(   1,    0), S(   8,    6),
    S(  -4,    2), S(  -9,    1), S(  -2,    2), S(  -8,    5), S(  -3,    5), S(   2,    6), S(   4,    6), S(  -3,    6),
    S(  -4,   -2), S(  -8,    1), S(  -4,    3), S( -13,    7), S(  -2,    7), S(   7,    7), S(   3,    5), S(   3,    3),
    S(  -5,   -3), S(  12,   -2), S(   0,    3), S(   6,    3), S(   4,    5), S(  13,    8), S(  11,    4), S(   4,    2),
    S( -13,   -6), S(  -4,   -2), S(  14,   -5), S(  13,   -5), S(  26,   -1), S(   8,    2), S(  -1,   -2), S(   1,   -1),
    S(  -5,   -5), S(  -6,   -4), S(   1,   -1), S(  26,   -6), S(   2,    4), S(  -9,   -5), S(  -1,   -1), S(  -4,   -4),
]);
#[rustfmt::skip]
pub const KING_PST: SquareMap<EScore> = SquareMap::from_array([
    S(   0,  -17), S(   0,  -16), S(   0,  -17), S(   0,  -16), S(   0,  -16), S(   0,  -14), S(   0,  -14), S(   0,  -16),
    S(   0,  -18), S(   0,   -6), S(   1,   -7), S(   1,   -6), S(   1,   -5), S(   1,   -1), S(   1,   -4), S(   0,  -12),
    S(   0,  -13), S(   1,   -5), S(   2,    0), S(   1,   -2), S(   2,    5), S(   4,   15), S(   4,   10), S(   1,  -11),
    S(   0,  -19), S(   1,   -1), S(   2,    8), S(   2,   11), S(   2,   14), S(   3,   16), S(   3,    8), S(  -1,  -14),
    S(  -1,  -22), S(   1,  -12), S(   1,    5), S(   0,   12), S(   0,   16), S(   1,   13), S(   2,   -2), S(  -3,  -24),
    S(  -1,  -23), S(  -1,  -16), S(   2,    0), S(  -1,    6), S(   1,   12), S(   0,    7), S(   7,    0), S(  -2,  -22),
    S(  -4,  -31), S(   0,  -20), S(   0,   -8), S( -16,   -6), S( -21,    2), S( -11,   -5), S(   6,  -14), S(  18,  -33),
    S( -10,  -37), S(   4,  -36), S(   4,  -31), S( -26,  -30), S(  14,  -38), S( -32,  -26), S(  19,  -40), S(   8,  -62),
]);

impl Eval {
    pub fn score(&mut self, pos: &Position, pawn_hash: Hash) -> Score {
        let mut score = S(0, 0);

        score += self.pst(pos, true) - self.pst(pos, false);
        score += self.mobility_for_side(pos, true) - self.mobility_for_side(pos, false);
        score += self.center_control(true) - self.center_control(false);
        score += self.knights_for_side(pos, true) - self.knights_for_side(pos, false);
        score += self.bishops_for_side(pos, true) - self.bishops_for_side(pos, false);
        score += self.rooks_for_side(pos, true) - self.rooks_for_side(pos, false);
        score += self.material(true) - self.material(false);
        score += self.king_safety_for_side(pos, true) - self.king_safety_for_side(pos, false);
        score += self.pawns(pos, pawn_hash);

        if pos.white_to_move {
            score += TEMPO_SCORE;
        } else {
            score -= TEMPO_SCORE;
        }

        #[cfg(feature = "tune")]
        {
            self.trace.tempo[pos.white_to_move as usize] = 1;
            self.trace.base_eval = score;
        }

        let phase = self.phase();
        let mut score = interpolate(score, phase);

        let sf = self.endgame_scale_factor(score);
        score *= sf;
        score /= SF_NORMAL;

        let score = score as Score;

        if pos.white_to_move {
            score
        } else {
            -score
        }
    }

    fn pst(&mut self, pos: &Position, white: bool) -> EScore {
        #[cfg(feature = "tune")]
        {
            self.trace_pst(pos, white);
        }

        self.pst[white as usize]
    }

    fn mobility_for_side(&mut self, pos: &Position, white: bool) -> EScore {
        let s = white as usize;
        let us = pos.us(white);
        let them = pos.them(white);
        let rank3 = if white { RANK_3 } else { RANK_6 };

        self.attacked_by[s] = [Bitboard::from(0); 6];
        self.attacked_by_1[s] = Bitboard::from(0);
        self.attacked_by_2[s] = Bitboard::from(0);

        let pawn_stop_squares = (pos.pawns() & us).forward(white, 1);
        let mut pawn_mobility = pawn_stop_squares & !pos.all_pieces;
        pawn_mobility |= (pawn_mobility & rank3).forward(white, 1) & !pos.all_pieces;
        pawn_mobility |= them & (pawn_stop_squares.left(1) | pawn_stop_squares.right(1));

        let b = pawn_stop_squares.left(1);
        self.attacked_by[s][Piece::Pawn.index()] |= b;
        self.attacked_by_2[s] |= self.attacked_by_1[s] & b;
        self.attacked_by_1[s] |= b;

        let b = pawn_stop_squares.right(1);
        self.attacked_by[s][Piece::Pawn.index()] |= b;
        self.attacked_by_2[s] |= self.attacked_by_1[s] & b;
        self.attacked_by_1[s] |= b;

        let mut score = S(0, 0);
        let their_pawns = pos.pawns() & !us;
        let their_pawn_attacks = (their_pawns.left(1) | their_pawns.right(1)).forward(!white, 1);
        for knight in (pos.knights() & us).squares() {
            let b = KNIGHT_ATTACKS[knight];
            let mobility = b & !their_pawn_attacks;
            score += KNIGHT_MOBILITY[mobility.popcount()];
            self.attacked_by[s][Piece::Knight.index()] |= b;
            self.attacked_by_2[s] |= self.attacked_by_1[s] & b;
            self.attacked_by_1[s] |= b;
            #[cfg(feature = "tune")]
            {
                self.trace.mobility_knight[mobility.popcount()][s] += 1;
            }
        }

        for bishop in (pos.bishops() & us).squares() {
            let b = get_bishop_attacks_from(bishop, pos.all_pieces);
            score += BISHOP_MOBILITY[b.popcount()];
            self.attacked_by[s][Piece::Bishop.index()] |= b;
            self.attacked_by_2[s] |= self.attacked_by_1[s] & b;
            self.attacked_by_1[s] |= b;

            #[cfg(feature = "tune")]
            {
                self.trace.mobility_bishop[b.popcount()][s] += 1;
            }
        }

        for rook in (pos.rooks() & us).squares() {
            let b = get_rook_attacks_from(rook, pos.all_pieces);
            score += ROOK_MOBILITY[b.popcount()];
            self.attacked_by[s][Piece::Rook.index()] |= b;
            self.attacked_by_2[s] |= self.attacked_by_1[s] & b;
            self.attacked_by_1[s] |= b;

            #[cfg(feature = "tune")]
            {
                self.trace.mobility_rook[b.popcount()][s] += 1;
            }
        }

        for queen in (pos.queens() & us).squares() {
            let b = get_bishop_attacks_from(queen, pos.all_pieces)
                | get_rook_attacks_from(queen, pos.all_pieces);
            score += QUEEN_MOBILITY[b.popcount()];
            self.attacked_by[s][Piece::Queen.index()] |= b;
            self.attacked_by_2[s] |= self.attacked_by_1[s] & b;
            self.attacked_by_1[s] |= b;

            #[cfg(feature = "tune")]
            {
                self.trace.mobility_queen[b.popcount()][s] += 1;
            }
        }

        let b = KING_ATTACKS[pos.king_sq(white)];
        self.attacked_by[s][Piece::King.index()] |= b;
        self.attacked_by_2[s] |= self.attacked_by_1[s] & b;
        self.attacked_by_1[s] |= b;

        let pawn_mobility = pawn_mobility.popcount() as i32;
        #[cfg(feature = "tune")]
        {
            self.trace.mobility_pawn[s] = pawn_mobility as i8;
        }
        score += PAWN_MOBILITY * pawn_mobility;
        score
    }

    fn material(&mut self, white: bool) -> EScore {
        let p = Piece::Pawn.index();
        let n = Piece::Knight.index();
        let b = Piece::Bishop.index();
        let r = Piece::Rook.index();
        let q = Piece::Queen.index();
        let side = white as usize;

        let mut score = 0;
        score += self.material[side][p] as EScore * PAWN_SCORE;
        score += self.material[side][n] as EScore * KNIGHT_SCORE;
        score += self.material[side][b] as EScore * BISHOP_SCORE;
        score += self.material[side][r] as EScore * ROOK_SCORE;
        score += self.material[side][q] as EScore * QUEEN_SCORE;

        if self.material[side][b] > 1 {
            score += BISHOP_PAIR;
        }

        if self.material[side][r] > 1 {
            score += ROOK_PAIR;
        }

        #[cfg(feature = "tune")]
        {
            let k = Piece::King.index();
            self.trace.material[p][side] = self.material[side][p] as i8;
            self.trace.material[n][side] = self.material[side][n] as i8;
            self.trace.material[b][side] = self.material[side][b] as i8;
            self.trace.material[r][side] = self.material[side][r] as i8;
            self.trace.material[q][side] = self.material[side][q] as i8;
            self.trace.material[k][side] = 1;

            self.trace.bishops_pair[side] = cmp::min(1, self.material[side][b] as i8);
            self.trace.rooks_pair[side] = cmp::min(1, self.material[side][r] as i8);
        }

        score
    }

    fn center_control(&mut self, white: bool) -> EScore {
        let side = white as usize;
        let controlled = self.attacked_by_1[side] & !self.attacked_by_1[1 - side]
            | self.attacked_by_2[side] & !self.attacked_by_2[1 - side];
        let controlled_center = CENTER & controlled;
        let controlled_count = controlled_center.popcount();

        #[cfg(feature = "tune")]
        {
            self.trace.center_control[side] = controlled_count as i8;
        }

        controlled_count as i32 * CENTER_CONTROL
    }

    fn pawns(&mut self, pos: &Position, pawn_hash: Hash) -> EScore {
        // Don't do pawn hash lookups if we are tuning
        #[cfg(not(feature = "tune"))]
        {
            let pawn_hash_entry = &self.pawn_table[pawn_hash as usize % PAWN_TABLE_NUM_ENTRIES];
            if pawn_hash_entry.hash == pawn_hash {
                return pawn_hash_entry.score;
            }
        }

        let score = self.pawns_for_side(pos, true) - self.pawns_for_side(pos, false);

        let pawn_hash_entry = &mut self.pawn_table[pawn_hash as usize % PAWN_TABLE_NUM_ENTRIES];
        pawn_hash_entry.hash = pawn_hash;
        pawn_hash_entry.score = score;
        score
    }

    fn pawns_for_side(&mut self, pos: &Position, white: bool) -> EScore {
        let us = pos.us(white);
        let them = pos.them(white);
        let side = white as usize;

        let mut score = S(0, 0);

        for pawn in (pos.pawns() & us).squares() {
            let stop_sq = pawn.forward(white, 1);
            let corridor_bb = PAWN_CORRIDOR[side][pawn];
            let file = pawn.file() as usize;
            let file_bb = FILES[file];
            let file_forward_bb = corridor_bb & file_bb;
            let doubled = (file_forward_bb & us & pos.pawns()).at_least_one();
            let isolated = ((file_bb.left(1) | file_bb.right(1)) & pos.pawns() & us).is_empty();
            let halfopen_file = (file_forward_bb & pos.pawns() & them).is_empty();
            let passed_after_push = !(pos.pawns() & stop_sq)
                && (PAWN_CORRIDOR[side][stop_sq] & them & pos.pawns()).is_empty();

            if doubled {
                score += DOUBLED_PAWN;

                #[cfg(feature = "tune")]
                {
                    self.trace.pawns_doubled[side] += 1;
                }
            }

            if passed_after_push && !doubled {
                let relative_rank = pawn.relative_rank(white) as usize;

                score += PASSED_PAWN_ON_RANK[relative_rank];
                score += PASSED_PAWN_ON_FILE[file];

                #[cfg(feature = "tune")]
                {
                    self.trace.pawns_passed[relative_rank][side] += 1;
                    self.trace.pawns_passed_file[file][side] += 1;
                }
            }

            if isolated {
                if halfopen_file {
                    score += OPEN_ISOLATED_PAWN;

                    #[cfg(feature = "tune")]
                    {
                        self.trace.pawns_open_isolated[side] += 1;
                    }
                } else {
                    score += ISOLATED_PAWN;

                    #[cfg(feature = "tune")]
                    {
                        self.trace.pawns_isolated[side] += 1;
                    }
                }
            }
        }

        score
    }

    pub fn knights_for_side(&mut self, pos: &Position, white: bool) -> EScore {
        let us = pos.us(white);
        let them = pos.them(white);
        let s = white as usize;
        let mut score = 0;

        let attackable_by_pawn = |sq: Square| {
            let file = FILES[sq.file() as usize];
            let possible_attackers = PAWN_CORRIDOR[s][sq] & !file;
            (possible_attackers & pos.pawns() & them).at_least_one()
        };

        for knight in (pos.knights() & us).squares() {
            if KNIGHT_OUTPOSTS[s] & knight && !attackable_by_pawn(knight) {
                score += KNIGHT_OUTPOST;

                #[cfg(feature = "tune")]
                {
                    self.trace.knight_outposts[s] += 1;
                }
            }
        }

        score
    }

    pub fn bishops_for_side(&mut self, pos: &Position, white: bool) -> EScore {
        let us = pos.us(white);
        let mut score = 0;

        for bishop in (pos.bishops() & us).squares() {
            // Give bonus for attacked squares, ignoring everything but pawns
            let xray = get_bishop_attacks_from(bishop, pos.pawns());
            score += XRAYED_SQUARE * xray.popcount() as EScore;
            #[cfg(feature = "tune")]
            {
                self.trace.bishops_xray[white as usize] += xray.popcount() as i8;
            }
        }

        score
    }

    pub fn rooks_for_side(&mut self, pos: &Position, white: bool) -> EScore {
        let us = pos.us(white);

        let mut score = 0;

        for rook in (pos.rooks() & us).squares() {
            let file_bb = FILES[rook.file() as usize];
            if (pos.pawns() & file_bb).is_empty() {
                score += ROOK_OPEN_FILE;
                #[cfg(feature = "tune")]
                {
                    self.trace.rooks_open_file[white as usize] += 1;
                }
            } else if (pos.pawns() & us & file_bb).is_empty() {
                score += ROOK_HALFOPEN_FILE;
                #[cfg(feature = "tune")]
                {
                    self.trace.rooks_halfopen_file[white as usize] += 1;
                }
            }
        }

        score
    }

    fn king_safety_for_side(&mut self, pos: &Position, white: bool) -> EScore {
        let us = pos.us(white);
        let them = pos.them(white);
        let side = white as usize;

        let mut index = 0;

        let king = pos.kings() & us;
        let king_sq = pos.king_sq(white);
        let file = king_sq.file() as usize;
        let king_file = FILES[file];
        let adjacent_files = king.left(1) | king | king.right(1);
        let front = adjacent_files.forward(white, 1);
        let distant_front = adjacent_files.forward(white, 2);

        let skip_king_safety = self.material[1 - side][Piece::Queen.index()] == 0
            && self.material[1 - side][Piece::Rook.index()] <= 1;
        if skip_king_safety {
            return S(0, 0);
        }

        index += (3 - (front & pos.pawns() & us).popcount()) * 2;
        index += 3 - (distant_front & pos.pawns() & us).popcount();
        index += (front & pos.pawns() & them).popcount();
        index += (distant_front & pos.pawns() & them).popcount();

        // is king on open file
        if (king_file & pos.pawns()).is_empty() {
            index += 2;
        }

        // is king on half-open file
        if (king_file & pos.pawns()).popcount() == 1 {
            index += 1;
        }

        // on same file as opposing rook
        if (king_file & pos.rooks() & them).at_least_one() {
            index += 1;
        }

        let safe_squares = KING_ATTACKS[king_sq] & !us & !self.attacked_by_1[1 - side];
        const SAFE_SQUARES_PENALTY: [usize; 9] = [3, 2, 1, 0, 0, 0, 0, 0, 0];
        index += SAFE_SQUARES_PENALTY[safe_squares.popcount()];

        let queen_contact_checks = KING_ATTACKS[king_sq]
            & self.attacked_by[1 - side][Piece::Queen.index()]
            & self.attacked_by_2[1 - side]
            & !self.attacked_by_2[side];
        if queen_contact_checks.at_least_one() {
            index += 5;
        }

        let mut score = S(KING_SAFETY[index], 0);

        let mut attack_value = 0;
        let mut attack_count = 0;
        let king_area = KING_ATTACKS[king_sq];

        for piece in &Piece::all() {
            if (king_area & self.attacked_by[1 - side][piece.index()]).at_least_one() {
                attack_value += S(KING_DANGER[piece.index()], 0);
                attack_count += 1;

                #[cfg(feature = "tune")]
                {
                    self.trace.king_danger[piece.index()][side] += 1;
                    self.trace.king_danger_attacks[side] += 1;
                }
            }
        }

        score += attack_value * KING_DANGER_WEIGHT[attack_count] / 128;

        #[cfg(feature = "tune")]
        {
            self.trace.king_safety[index][side] += 1;
        }

        let safe_knight_checks = !self.attacked_by_1[side] & KNIGHT_ATTACKS[king_sq];
        let safe_bishop_checks =
            !self.attacked_by_1[side] & get_bishop_attacks_from(king_sq, pos.all_pieces);
        let safe_rook_checks =
            !self.attacked_by_1[side] & get_rook_attacks_from(king_sq, pos.all_pieces);

        if (safe_knight_checks & self.attacked_by[1 - side][Piece::Knight.index()]).at_least_one() {
            score += KING_CHECK_KNIGHT;
            #[cfg(feature = "tune")]
            {
                self.trace.king_check_knight[side] += 1;
            }
        }

        if (safe_bishop_checks & self.attacked_by[1 - side][Piece::Bishop.index()]).at_least_one() {
            score += KING_CHECK_BISHOP;
            #[cfg(feature = "tune")]
            {
                self.trace.king_check_bishop[side] += 1;
            }
        }

        if (safe_rook_checks & self.attacked_by[1 - side][Piece::Rook.index()]).at_least_one() {
            score += KING_CHECK_ROOK;
            #[cfg(feature = "tune")]
            {
                self.trace.king_check_rook[side] += 1;
            }
        }

        if ((safe_bishop_checks | safe_rook_checks)
            & self.attacked_by[1 - side][Piece::Queen.index()])
        .at_least_one()
        {
            score += KING_CHECK_QUEEN;
            #[cfg(feature = "tune")]
            {
                self.trace.king_check_queen[side] += 1;
            }
        }

        score
    }

    pub fn phase(&mut self) -> i16 {
        let phase = cmp::min(
            62,
            self.non_pawn_material(false) + self.non_pawn_material(true),
        );

        #[cfg(feature = "tune")]
        {
            self.trace.phase = phase as i8;
        }

        phase
    }

    fn endgame_scale_factor(&mut self, score: i32) -> i32 {
        let sf = if self.material[(score > 0) as usize][Piece::Pawn.index()] == 0 {
            SF_PAWNLESS
        } else {
            SF_NORMAL
        };

        #[cfg(feature = "tune")]
        {
            self.trace.sf = sf as i8;
        }

        sf
    }

    pub fn make_move(&mut self, mov: Move, white: bool) {
        let side = white as usize;
        self.pst[side] -= pst(&PST[mov.piece.index()], white, mov.from);

        if let Some(promoted) = mov.promoted {
            self.material[side][Piece::Pawn.index()] -= 1;
            self.material[side][promoted.index()] += 1;
            self.pst[side] += pst(&PST[promoted.index()], white, mov.to);
        } else {
            self.pst[side] += pst(&PST[mov.piece.index()], white, mov.to);
        }

        if let Some(captured) = mov.captured {
            self.material[1 - side][captured.index()] -= 1;
            if mov.en_passant {
                self.pst[1 - side] -=
                    pst(&PST[Piece::Pawn.index()], !white, mov.to.backward(white, 1));
            } else {
                self.pst[1 - side] -= pst(&PST[captured.index()], !white, mov.to);
            }
        }

        if mov.piece == Piece::King {
            if mov.is_kingside_castle() {
                self.pst[side] -= pst(&PST[Piece::Rook.index()], white, mov.to.right(1));
                self.pst[side] += pst(&PST[Piece::Rook.index()], white, mov.to.left(1));
            } else if mov.is_queenside_castle() {
                self.pst[side] -= pst(&PST[Piece::Rook.index()], white, mov.to.left(2));
                self.pst[side] += pst(&PST[Piece::Rook.index()], white, mov.to.right(1));
            }
        }
    }

    pub fn unmake_move(&mut self, mov: Move, unmaking_white_move: bool) {
        let side = unmaking_white_move as usize;

        self.pst[side] += pst(&PST[mov.piece.index()], unmaking_white_move, mov.from);

        if let Some(captured) = mov.captured {
            self.material[1 - side][captured.index()] += 1;
            if mov.en_passant {
                self.pst[1 - side] += pst(
                    &PST[Piece::Pawn.index()],
                    !unmaking_white_move,
                    mov.to.backward(unmaking_white_move, 1),
                );
            } else {
                self.pst[1 - side] += pst(&PST[captured.index()], !unmaking_white_move, mov.to);
            }
        }

        if let Some(promoted) = mov.promoted {
            self.material[side][Piece::Pawn.index()] += 1;
            self.material[side][promoted.index()] -= 1;
            self.pst[side] -= pst(&PST[promoted.index()], unmaking_white_move, mov.to);
        } else {
            self.pst[side] -= pst(&PST[mov.piece.index()], unmaking_white_move, mov.to);
        }

        if mov.piece == Piece::King {
            if mov.is_kingside_castle() {
                self.pst[side] += pst(
                    &PST[Piece::Rook.index()],
                    unmaking_white_move,
                    mov.to.right(1),
                );
                self.pst[side] -= pst(
                    &PST[Piece::Rook.index()],
                    unmaking_white_move,
                    mov.to.left(1),
                );
            } else if mov.is_queenside_castle() {
                self.pst[side] += pst(
                    &PST[Piece::Rook.index()],
                    unmaking_white_move,
                    mov.to.left(2),
                );
                self.pst[side] -= pst(
                    &PST[Piece::Rook.index()],
                    unmaking_white_move,
                    mov.to.right(1),
                );
            }
        }
    }

    pub fn is_material_draw(&self) -> bool {
        let material = &self.material;
        let pawn = Piece::Pawn.index();
        let knight = Piece::Knight.index();
        let bishop = Piece::Bishop.index();
        let rook = Piece::Rook.index();
        let queen = Piece::Queen.index();

        for side_mat in material.iter() {
            if side_mat[pawn] > 0 || side_mat[rook] > 0 || side_mat[queen] > 0 {
                return false;
            }
        }

        for side in 0..2 {
            if material[side][bishop] == 0 && material[side][knight] == 0 {
                if material[1 - side][bishop] == 0 && material[1 - side][knight] < 3 {
                    return true;
                }

                return material[1 - side][bishop] == 0
                    || material[1 - side][bishop] + material[1 - side][knight] <= 1;
            }
        }

        false
    }

    pub fn non_pawn_material(&self, white: bool) -> Score {
        let mut material = 0;
        let side = white as usize;
        material += 3 * self.material[side][Piece::Knight.index()] as Score;
        material += 3 * self.material[side][Piece::Bishop.index()] as Score;
        material += 5 * self.material[side][Piece::Rook.index()] as Score;
        material += 9 * self.material[side][Piece::Queen.index()] as Score;
        material
    }

    #[cfg(feature = "tune")]
    fn trace_pst(&mut self, pos: &Position, white: bool) {
        let us = pos.us(white);
        let side = white as usize;
        for pawn in (pos.pawns() & us).squares() {
            let relative = if white { pawn.flip_rank() } else { pawn };
            self.trace.pst_pawn[relative][side] += 1;
        }

        for knight in (pos.knights() & us).squares() {
            let relative = if white { knight.flip_rank() } else { knight };
            self.trace.pst_knight[relative][side] += 1;
        }

        for bishop in (pos.bishops() & us).squares() {
            let relative = if white { bishop.flip_rank() } else { bishop };
            self.trace.pst_bishop[relative][side] += 1;
        }

        for rook in (pos.rooks() & us).squares() {
            let relative = if white { rook.flip_rank() } else { rook };
            self.trace.pst_rook[relative][side] += 1;
        }

        for queen in (pos.queens() & us).squares() {
            let relative = if white { queen.flip_rank() } else { queen };
            self.trace.pst_queen[relative][side] += 1;
        }

        for king in (pos.kings() & us).squares() {
            let relative = if white { king.flip_rank() } else { king };
            self.trace.pst_king[relative][side] += 1;
        }
    }
}

impl<'p> From<&'p Position> for Eval {
    fn from(pos: &Position) -> Eval {
        Eval {
            material: [
                [
                    (pos.black_pieces() & pos.pawns()).popcount() as u8,
                    (pos.black_pieces() & pos.knights()).popcount() as u8,
                    (pos.black_pieces() & pos.bishops()).popcount() as u8,
                    (pos.black_pieces() & pos.rooks()).popcount() as u8,
                    (pos.black_pieces() & pos.queens()).popcount() as u8,
                ],
                [
                    (pos.white_pieces() & pos.pawns()).popcount() as u8,
                    (pos.white_pieces() & pos.knights()).popcount() as u8,
                    (pos.white_pieces() & pos.bishops()).popcount() as u8,
                    (pos.white_pieces() & pos.rooks()).popcount() as u8,
                    (pos.white_pieces() & pos.queens()).popcount() as u8,
                ],
            ],
            pst: init_pst_score(pos),
            pawn_table: [PawnHashEntry::default(); PAWN_TABLE_NUM_ENTRIES],
            attacked_by: [[Bitboard::from(0); 6]; 2],
            attacked_by_1: [Bitboard::from(0); 2],
            attacked_by_2: [Bitboard::from(0); 2],

            #[cfg(feature = "tune")]
            trace: Trace::default(),
        }
    }
}

fn init_pst_score(pos: &Position) -> [EScore; 2] {
    let mut white = S(0, 0);
    white += (pos.white_pieces() & pos.pawns())
        .squares()
        .map(|sq| pst(&PST[Piece::Pawn.index()], true, sq))
        .sum::<EScore>();
    white += (pos.white_pieces() & pos.knights())
        .squares()
        .map(|sq| pst(&PST[Piece::Knight.index()], true, sq))
        .sum::<EScore>();
    white += (pos.white_pieces() & pos.bishops())
        .squares()
        .map(|sq| pst(&PST[Piece::Bishop.index()], true, sq))
        .sum::<EScore>();
    white += (pos.white_pieces() & pos.rooks())
        .squares()
        .map(|sq| pst(&PST[Piece::Rook.index()], true, sq))
        .sum::<EScore>();
    white += (pos.white_pieces() & pos.queens())
        .squares()
        .map(|sq| pst(&PST[Piece::Queen.index()], true, sq))
        .sum::<EScore>();
    white += (pos.white_pieces() & pos.kings())
        .squares()
        .map(|sq| pst(&PST[Piece::King.index()], true, sq))
        .sum::<EScore>();

    let mut black = S(0, 0);
    black += (pos.black_pieces() & pos.pawns())
        .squares()
        .map(|sq| pst(&PST[Piece::Pawn.index()], false, sq))
        .sum::<EScore>();
    black += (pos.black_pieces() & pos.knights())
        .squares()
        .map(|sq| pst(&PST[Piece::Knight.index()], false, sq))
        .sum::<EScore>();
    black += (pos.black_pieces() & pos.bishops())
        .squares()
        .map(|sq| pst(&PST[Piece::Bishop.index()], false, sq))
        .sum::<EScore>();
    black += (pos.black_pieces() & pos.rooks())
        .squares()
        .map(|sq| pst(&PST[Piece::Rook.index()], false, sq))
        .sum::<EScore>();
    black += (pos.black_pieces() & pos.queens())
        .squares()
        .map(|sq| pst(&PST[Piece::Queen.index()], false, sq))
        .sum::<EScore>();
    black += (pos.black_pieces() & pos.kings())
        .squares()
        .map(|sq| pst(&PST[Piece::King.index()], false, sq))
        .sum::<EScore>();

    [black, white]
}

pub const PST: &[SquareMap<EScore>] = &[
    PAWN_PST, KNIGHT_PST, BISHOP_PST, ROOK_PST, QUEEN_PST, KING_PST,
];

pub fn pst(pst: &SquareMap<EScore>, from_white_perspective: bool, sq: Square) -> EScore {
    if from_white_perspective {
        pst[sq.flip_rank()]
    } else {
        pst[sq]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escore() {
        assert_eq!(mg(S(1, 1)), 1);
        assert_eq!(eg(S(1, 1)), 1);
        assert_eq!(mg(S(1, -1)), 1);
        assert_eq!(eg(S(1, -1)), -1);
        assert_eq!(mg(S(-1, 1)), -1);
        assert_eq!(eg(S(-1, 1)), 1);
        assert_eq!(mg(S(-1, -1)), -1);
        assert_eq!(eg(S(-1, -1)), -1);
    }

    #[test]
    fn test_escore_calculus() {
        assert_eq!(S(1, 2) + S(3, 4), S(4, 6));
        assert_eq!(S(-1, -2) + S(3, 4), S(2, 2));
        assert_eq!(S(3, 4) - S(1, 2), S(2, 2));
        assert_eq!(S(3, 0) - S(1, 2), S(2, -2));
    }
}
