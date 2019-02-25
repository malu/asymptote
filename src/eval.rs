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
use std::cmp;

use crate::bitboard::*;
use crate::hash::*;
use crate::movegen::*;
use crate::position::*;

#[derive(Debug)]
pub struct Eval {
    material: [[usize; 5]; 2],
    pst: [Score; 2],
    pawn_table: Vec<PawnHashEntry>,
    attacked_by: [[Bitboard; 6]; 2],
    attacked_by_1: [Bitboard; 2],
    attacked_by_2: [Bitboard; 2],
}

const PAWN_TABLE_NUM_ENTRIES: usize = 2 * 1024;

#[derive(Debug, Default)]
struct PawnHashEntry {
    hash: Hash,
    mg: Score,
    eg: Score,
}

pub type Score = i16;

pub const MATE_SCORE: Score = 20000;

pub const PAWN_SCORE: Score = 100;
pub const KNIGHT_SCORE: Score = 300;
pub const BISHOP_SCORE: Score = 320;
pub const ROOK_SCORE: Score = 500;
pub const QUEEN_SCORE: Score = 1000;

const KNIGHT_MOBILITY: [Score; 9] = [-138, -68, -28, 12, 22, 32, 42, 52, 62];

const BISHOP_MOBILITY: [Score; 14] = [
    -110, -70, -30, -10, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45,
];

const ROOK_MOBILITY: [Score; 15] = [
    -105, -65, -25, -15, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45,
];

impl Eval {
    fn mobility_for_side(&mut self, white: bool, pos: &Position) -> Score {
        let us = pos.us(white);
        let them = pos.them(white);
        let rank3 = if white { RANK_3 } else { RANK_6 };

        let s = white as usize;
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

        let mut knight_mobility = 0;
        let their_pawns = pos.pawns() & !us;
        let their_pawn_attacks =
            their_pawns.forward(!white, 1).left(1) | their_pawns.forward(!white, 1).right(1);
        for knight in (pos.knights() & us).squares() {
            let b = KNIGHT_ATTACKS[knight.0 as usize];
            let mobility = b & !their_pawn_attacks;
            knight_mobility += KNIGHT_MOBILITY[mobility.popcount() as usize];
            self.attacked_by[s][Piece::Knight.index()] |= b;
            self.attacked_by_2[s] |= self.attacked_by_1[s] & b;
            self.attacked_by_1[s] |= b;
        }

        let mut bishop_mobility = 0;
        for bishop in (pos.bishops() & us).squares() {
            let b = get_bishop_attacks_from(bishop, pos.all_pieces);
            let x = get_bishop_attacks_from(bishop, pos.all_pieces ^ (pos.queens() & us));
            bishop_mobility += BISHOP_MOBILITY[b.popcount() as usize];
            self.attacked_by[s][Piece::Bishop.index()] |= b;
            self.attacked_by_2[s] |= self.attacked_by_1[s] & x;
            self.attacked_by_1[s] |= b;
        }

        let mut rook_mobility = 0;
        for rook in (pos.rooks() & us).squares() {
            let b = get_rook_attacks_from(rook, pos.all_pieces);
            let x = get_rook_attacks_from(rook, pos.all_pieces ^ (pos.queens() & us));
            rook_mobility += ROOK_MOBILITY[b.popcount() as usize];
            self.attacked_by[s][Piece::Rook.index()] |= b;
            self.attacked_by_2[s] |= self.attacked_by_1[s] & x;
            self.attacked_by_1[s] |= b;
        }

        for queen in (pos.queens() & us).squares() {
            let b = get_bishop_attacks_from(queen, pos.all_pieces)
                | get_rook_attacks_from(queen, pos.all_pieces);
            let x = get_bishop_attacks_from(queen, pos.all_pieces ^ (pos.queens() & us))
                | get_rook_attacks_from(queen, pos.all_pieces ^ (pos.queens() & us));
            self.attacked_by[s][Piece::Queen.index()] |= b;
            self.attacked_by_2[s] |= self.attacked_by_1[s] & x;
            self.attacked_by_1[s] |= b;
        }

        for king in (pos.kings() & us).squares() {
            let b = KING_ATTACKS[king.0 as usize];
            self.attacked_by[s][Piece::King.index()] |= b;
            self.attacked_by_2[s] |= self.attacked_by_1[s] & b;
            self.attacked_by_1[s] |= b;
        }

        6 * pawn_mobility.popcount() as Score + knight_mobility + bishop_mobility + rook_mobility
    }

    fn material_score(&self) -> Score {
        let pawn = Piece::Pawn.index();
        let knight = Piece::Knight.index();
        let bishop = Piece::Bishop.index();
        let rook = Piece::Rook.index();
        let queen = Piece::Queen.index();
        let black = 0;
        let white = 1;

        let mut score = 0;
        score += PAWN_SCORE * self.material[white][pawn] as Score;
        score += KNIGHT_SCORE * self.material[white][knight] as Score;
        score += BISHOP_SCORE * self.material[white][bishop] as Score;
        score += ROOK_SCORE * self.material[white][rook] as Score;
        score += QUEEN_SCORE * self.material[white][queen] as Score;

        score -= PAWN_SCORE * self.material[black][pawn] as Score;
        score -= KNIGHT_SCORE * self.material[black][knight] as Score;
        score -= BISHOP_SCORE * self.material[black][bishop] as Score;
        score -= ROOK_SCORE * self.material[black][rook] as Score;
        score -= QUEEN_SCORE * self.material[black][queen] as Score;

        // encourage trading pieces if ahead in material or pawns if behind in material
        if score > 50 {
            score += 4 * self.material[white][pawn] as Score;
            score += 4 * self.material[black][pawn] as Score;
        } else if score < -50 {
            score -= 4 * self.material[black][pawn] as Score;
            score -= 4 * self.material[white][pawn] as Score;
        }

        if self.material[white][bishop] > 1 {
            score += 40;
        }

        if self.material[black][bishop] > 1 {
            score -= 40;
        }

        score
    }

    pub fn score(&mut self, pos: &Position, pawn_hash: Hash) -> Score {
        let mut score = 0;
        score += self.material_score();
        score += self.pst[1] - self.pst[0];
        score += self.mobility_for_side(true, pos) - self.mobility_for_side(false, pos);
        score += self.bishops_for_side(pos, true) - self.bishops_for_side(pos, false);
        score += self.rooks_for_side(pos, true) - self.rooks_for_side(pos, false);

        let mut mg = score;
        let mut eg = score;

        let (king_mg, king_eg) = self.king_safety(pos);
        mg += king_mg;
        eg += king_eg;

        let (pawns_mg, pawns_eg) = self.pawns(pos, pawn_hash);
        mg += pawns_mg;
        eg += pawns_eg;

        let mg = i32::from(mg);
        let eg = i32::from(eg);
        let phase = i32::from(self.phase());

        let score = (mg * phase + eg * (62 - phase)) / 62;
        let score = score as Score;

        if pos.white_to_move {
            score
        } else {
            -score
        }
    }

    fn pawns(&mut self, pos: &Position, pawn_hash: Hash) -> (Score, Score) {
        {
            let pawn_hash_entry = &self.pawn_table[pawn_hash as usize % PAWN_TABLE_NUM_ENTRIES];
            if pawn_hash_entry.hash == pawn_hash {
                return (pawn_hash_entry.mg, pawn_hash_entry.eg);
            }
        }

        let (wmg, weg) = self.pawns_for_side(pos, true);
        let (bmg, beg) = self.pawns_for_side(pos, false);

        let pawn_hash_entry = &mut self.pawn_table[pawn_hash as usize % PAWN_TABLE_NUM_ENTRIES];
        pawn_hash_entry.hash = pawn_hash;
        pawn_hash_entry.mg = wmg - bmg;
        pawn_hash_entry.eg = weg - beg;
        (wmg - bmg, weg - beg)
    }

    fn pawns_for_side(&mut self, pos: &Position, white: bool) -> (Score, Score) {
        let us = pos.us(white);
        let them = pos.them(white);
        let side = white as usize;

        const PASSER_ON_RANK_BONUS_EG: [Score; 8] = [0, 160, 80, 40, 20, 10, 10, 0];
        const PASSER_ON_RANK_BONUS_MG: [Score; 8] = [0, 60, 50, 40, 30, 20, 10, 0];
        const ISOLATED_PAWN_PENALTY_EG: Score = 10;
        const ISOLATED_PAWN_PENALTY_MG: Score = 10;
        const DOUBLED_PAWN_PENALTY_EG: Score = 30;
        const DOUBLED_PAWN_PENALTY_MG: Score = 30;

        let mut mg = 0;
        let mut eg = 0;
        for pawn in (pos.pawns() & us).squares() {
            let sq = pawn.0 as usize;
            let corridor_bb = PAWN_CORRIDOR[side][sq];
            let file_bb = FILES[pawn.file() as usize];
            let file_forward_bb = corridor_bb & file_bb;
            let passed = (corridor_bb & them & pos.pawns()).is_empty();
            let doubled = (file_forward_bb & us & pos.pawns()).at_least_one();
            let isolated = ((file_bb.left(1) | file_bb.right(1)) & pos.pawns() & us).is_empty();

            if doubled {
                mg -= DOUBLED_PAWN_PENALTY_MG;
                eg -= DOUBLED_PAWN_PENALTY_EG;
            }

            if passed && !doubled {
                let relative_rank = if white {
                    pawn.rank() as usize ^ 7
                } else {
                    pawn.rank() as usize
                };

                mg += PASSER_ON_RANK_BONUS_MG[relative_rank];
                eg += PASSER_ON_RANK_BONUS_EG[relative_rank];
            }

            if isolated {
                mg -= ISOLATED_PAWN_PENALTY_MG;

                if !passed {
                    eg -= ISOLATED_PAWN_PENALTY_EG;
                }
            }
        }

        (mg, eg)
    }

    pub fn bishops_for_side(&self, pos: &Position, white: bool) -> Score {
        let us = pos.us(white);
        const XRAYED_SQUARE: Score = 2;
        let mut score = 0;

        for bishop in (pos.bishops() & us).squares() {
            let xray = get_bishop_attacks_from(bishop, pos.pawns());
            score += XRAYED_SQUARE * xray.popcount() as Score;
        }

        score
    }

    pub fn rooks_for_side(&self, pos: &Position, white: bool) -> Score {
        let us = pos.us(white);
        const OPEN_FILE_BONUS: Score = 15;
        const HALF_OPEN_FILE_BONUS: Score = 5;
        let mut score = 0;

        for rook in (pos.rooks() & us).squares() {
            let file_bb = FILES[rook.file() as usize];
            if (pos.pawns() & file_bb).is_empty() {
                score += OPEN_FILE_BONUS;
            } else if (pos.pawns() & us & file_bb).is_empty() {
                score += HALF_OPEN_FILE_BONUS;
            }
        }

        score
    }

    pub fn king_safety(&self, pos: &Position) -> (Score, Score) {
        let (wmg, weg) = self.king_safety_for_side(pos, true);
        let (bmg, beg) = self.king_safety_for_side(pos, false);
        (wmg - bmg, weg - beg)
    }

    fn king_safety_for_side(&self, pos: &Position, white: bool) -> (Score, Score) {
        let us = pos.us(white);
        let them = pos.them(white);
        let side = white as usize;

        #[rustfmt::skip]
        const CENTER_DISTANCE: [Score; 64] = [
            3, 3, 3, 3, 3, 3, 3, 3,
            3, 2, 2, 2, 2, 2, 2, 3,
            3, 2, 1, 1, 1, 1, 2, 3,
            3, 2, 1, 0, 0, 1, 2, 3,
            3, 2, 1, 0, 0, 1, 2, 3,
            3, 2, 1, 1, 1, 1, 2, 3,
            3, 2, 2, 2, 2, 2, 2, 3,
            3, 3, 3, 3, 3, 3, 3, 3,
        ];

        let mut index = 0;

        let king = pos.kings() & us;
        let king_sq = king.squares().nth(0).unwrap();
        let file = king_sq.file();
        let king_file = FILES[file as usize];
        let adjacent_files = king.left(1) | king | king.right(1);
        let front = adjacent_files.forward(white, 1);
        let distant_front = adjacent_files.forward(white, 2);

        let eg_penalty = 5 * CENTER_DISTANCE[king_sq.0 as usize];

        let skip_king_safety = self.material[1 - side][Piece::Queen.index()] == 0
            && self.material[1 - side][Piece::Rook.index()] <= 1;
        if skip_king_safety {
            return (0, -eg_penalty);
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

        let safe_squares = KING_ATTACKS[king_sq.0 as usize] & !us & !self.attacked_by_1[1 - side];
        const SAFE_SQUARES_PENALTY: [usize; 9] = [3, 2, 1, 0, 0, 0, 0, 0, 0];
        index += SAFE_SQUARES_PENALTY[safe_squares.popcount()];

        let safe_knight_checks = !self.attacked_by_1[side] & KNIGHT_ATTACKS[king_sq.0 as usize];
        let safe_bishop_checks =
            !self.attacked_by_1[side] & get_bishop_attacks_from(king_sq, pos.all_pieces);
        let safe_rook_checks =
            !self.attacked_by_1[side] & get_rook_attacks_from(king_sq, pos.all_pieces);

        let queen_contact_checks = KING_ATTACKS[king_sq.0 as usize]
            & self.attacked_by[1 - side][Piece::Queen.index()]
            & self.attacked_by_2[1 - side]
            & !self.attacked_by_2[side];
        if queen_contact_checks.at_least_one() {
            index += 5;
        }

        let mut mg_penalty = (index * index) as Score;

        if (safe_knight_checks & self.attacked_by[1 - side][Piece::Knight.index()]).at_least_one() {
            mg_penalty += 50;
        }

        if (safe_bishop_checks & self.attacked_by[1 - side][Piece::Bishop.index()]).at_least_one() {
            mg_penalty += 25;
        }

        if (safe_rook_checks & self.attacked_by[1 - side][Piece::Rook.index()]).at_least_one() {
            mg_penalty += 25;
        }

        if ((safe_bishop_checks | safe_rook_checks)
            & self.attacked_by[1 - side][Piece::Queen.index()])
        .at_least_one()
        {
            mg_penalty += 50;
        }

        (-mg_penalty, -eg_penalty)
    }

    pub fn phase(&self) -> i16 {
        cmp::min(
            62,
            self.non_pawn_material(false) + self.non_pawn_material(true),
        )
    }

    pub fn make_move(&mut self, mov: Move, pos: &Position) {
        let side = pos.white_to_move as usize;
        self.pst[side] -= pst(&PST[mov.piece.index()], pos.white_to_move, mov.from);

        if let Some(promoted) = mov.promoted {
            self.material[side][Piece::Pawn.index()] -= 1;
            self.material[side][promoted.index()] += 1;
            self.pst[side] += pst(&PST[promoted.index()], pos.white_to_move, mov.to);
        } else {
            self.pst[side] += pst(&PST[mov.piece.index()], pos.white_to_move, mov.to);
        }

        if let Some(captured) = mov.captured {
            self.material[1 - side][captured.index()] -= 1;
            if mov.en_passant {
                self.pst[1 - side] -= pst(
                    &PST[Piece::Pawn.index()],
                    !pos.white_to_move,
                    mov.to.backward(pos.white_to_move, 1),
                );
            } else {
                self.pst[1 - side] -= pst(&PST[captured.index()], !pos.white_to_move, mov.to);
            }
        }

        if mov.piece == Piece::King {
            if mov.to.0 == mov.from.0 + 2 {
                // castle kingside
                self.pst[side] -= pst(
                    &PST[Piece::Rook.index()],
                    pos.white_to_move,
                    mov.to.right(1),
                );
                self.pst[side] += pst(&PST[Piece::Rook.index()], pos.white_to_move, mov.to.left(1));
            } else if mov.from.0 == mov.to.0 + 2 {
                // castle queenside
                self.pst[side] -= pst(&PST[Piece::Rook.index()], pos.white_to_move, mov.to.left(2));
                self.pst[side] += pst(
                    &PST[Piece::Rook.index()],
                    pos.white_to_move,
                    mov.to.right(1),
                );
            }
        }
    }

    pub fn unmake_move(&mut self, mov: Move, pos: &Position) {
        let unmaking_white_move = !pos.white_to_move;
        let side = unmaking_white_move as usize;

        self.pst[side] += pst(&PST[mov.piece.index()], unmaking_white_move, mov.from);

        if let Some(captured) = mov.captured {
            self.material[1 - side][captured.index()] += 1;
            if mov.en_passant {
                self.pst[1 - side] += pst(
                    &PST[Piece::Pawn.index()],
                    pos.white_to_move,
                    mov.to.backward(unmaking_white_move, 1),
                );
            } else {
                self.pst[1 - side] += pst(&PST[captured.index()], pos.white_to_move, mov.to);
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
            if mov.to.0 == mov.from.0 + 2 {
                // castle kingside
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
            } else if mov.from.0 == mov.to.0 + 2 {
                // castle queenside
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
}

impl<'p> From<&'p Position> for Eval {
    fn from(pos: &Position) -> Eval {
        let mut pawn_table = Vec::with_capacity(PAWN_TABLE_NUM_ENTRIES);
        for _ in 0..PAWN_TABLE_NUM_ENTRIES {
            pawn_table.push(PawnHashEntry::default());
        }

        Eval {
            material: [
                [
                    (pos.black_pieces() & pos.pawns()).popcount() as usize,
                    (pos.black_pieces() & pos.knights()).popcount() as usize,
                    (pos.black_pieces() & pos.bishops()).popcount() as usize,
                    (pos.black_pieces() & pos.rooks()).popcount() as usize,
                    (pos.black_pieces() & pos.queens()).popcount() as usize,
                ],
                [
                    (pos.white_pieces() & pos.pawns()).popcount() as usize,
                    (pos.white_pieces() & pos.knights()).popcount() as usize,
                    (pos.white_pieces() & pos.bishops()).popcount() as usize,
                    (pos.white_pieces() & pos.rooks()).popcount() as usize,
                    (pos.white_pieces() & pos.queens()).popcount() as usize,
                ],
            ],
            pst: init_pst_score(pos),
            pawn_table,
            attacked_by: [[Bitboard::from(0); 6]; 2],
            attacked_by_1: [Bitboard::from(0); 2],
            attacked_by_2: [Bitboard::from(0); 2],
        }
    }
}

fn init_pst_score(pos: &Position) -> [Score; 2] {
    let mut white = 0;
    white += (pos.white_pieces() & pos.pawns())
        .squares()
        .map(|sq| pst(&PST[Piece::Pawn.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces() & pos.knights())
        .squares()
        .map(|sq| pst(&PST[Piece::Knight.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces() & pos.bishops())
        .squares()
        .map(|sq| pst(&PST[Piece::Bishop.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces() & pos.rooks())
        .squares()
        .map(|sq| pst(&PST[Piece::Rook.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces() & pos.queens())
        .squares()
        .map(|sq| pst(&PST[Piece::Queen.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces() & pos.kings())
        .squares()
        .map(|sq| pst(&PST[Piece::King.index()], true, sq))
        .sum::<Score>();

    let mut black = 0;
    black += (pos.black_pieces() & pos.pawns())
        .squares()
        .map(|sq| pst(&PST[Piece::Pawn.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces() & pos.knights())
        .squares()
        .map(|sq| pst(&PST[Piece::Knight.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces() & pos.bishops())
        .squares()
        .map(|sq| pst(&PST[Piece::Bishop.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces() & pos.rooks())
        .squares()
        .map(|sq| pst(&PST[Piece::Rook.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces() & pos.queens())
        .squares()
        .map(|sq| pst(&PST[Piece::Queen.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces() & pos.kings())
        .squares()
        .map(|sq| pst(&PST[Piece::King.index()], false, sq))
        .sum::<Score>();

    [black, white]
}

#[rustfmt::skip]
pub const PAWN_PST: [Score; 64] = [
     24,  28,  35,  50,  50,  35,  28,  24,
     16,  23,  27,  34,  34,  27,  23,  16,
      5,   7,  11,  20,  20,  11,   7,   5,
    -12,  -9,  -2,  11,  11,  -2,  -9, -12,
    -21, -20, -12,   2,   2, -12, -20, -21,
    -17, -14, -14,  -6,  -6, -14, -14, -17,
    -21, -20, -18, -15, -15, -18, -20, -21,
      0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
pub const KNIGHT_PST: [Score; 64] = [
    -10, -10, -10, -10, -10, -10, -10, -10,
    -10,   0,  10,  15,  15,  10,   0, -10,
    -10,   5,  15,  20,  20,  15,   5, -10,
    -10,   5,  15,  20,  20,  15,   5, -10,
    -10,   0,  15,  20,  20,  15,   0, -10,
    -10,   0,  10,  10,  10,  10,   0, -10,
    -10,   0,   0,   5,   5,   0,   0, -10,
    -10, -10, -10, -10, -10, -10, -10, -10,
];

#[rustfmt::skip]
pub const BISHOP_PST: [Score; 64] = [
    -10, -10, -10, -10, -10, -10, -10, -10,
    -10,   0,   5,  10,  10,   5,   0, -10,
    -10,   5,  10,  20,  20,  10,   5, -10,
    -10,   5,  10,  20,  20,  10,   5, -10,
    -10,   0,  10,  15,  15,  10,   0, -10,
    -10,   5,  10,  10,  10,  10,   5, -10,
    -10,  10,   0,   5,   5,   0,  10, -10,
    -10, -10, -10, -10, -10, -10, -10, -10,
];

#[rustfmt::skip]
pub const ROOK_PST: [Score; 64] = [
     20, 20, 20, 25, 25, 20, 20,  20,
     20, 20, 20, 25, 25, 20, 20,  20,
      0,  0,  0,  5,  5,  0,  0,   0,
      0,  0,  0,  5,  5,  0,  0,   0,
      0,  0,  0,  5,  5,  0,  0,   0,
     -5,  0,  0, 10, 10,  0,  0,  -5,
     -5, -5,  0, 15, 15,  0, -5,  -5,
    -10, -5, 10, 25, 25, 10, -5, -10,
];

#[rustfmt::skip]
pub const QUEEN_PST: [Score; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
];

#[rustfmt::skip]
pub const KING_PST: [Score; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
];

pub const PST: &[[Score; 64]] = &[
    PAWN_PST, KNIGHT_PST, BISHOP_PST, ROOK_PST, QUEEN_PST, KING_PST,
];

pub fn pst(pst: &[Score; 64], from_white_perspective: bool, sq: Square) -> Score {
    if from_white_perspective {
        pst[sq.0 as usize ^ 0b11_1000]
    } else {
        pst[sq.0 as usize]
    }
}
