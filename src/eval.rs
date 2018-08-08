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
use bitboard::*;
use hash::*;
use movegen::*;
use position::*;

#[derive(Debug)]
pub struct Eval {
    pub material: Material,
    pst: [Score; 2],
    positional: Positional,
    pawn_table: Vec<PawnHashEntry>,
}

#[derive(Debug)]
pub struct Material {
    white_pawns: usize,
    white_knights: usize,
    white_bishops: usize,
    white_rooks: usize,
    white_queens: usize,
    black_pawns: usize,
    black_knights: usize,
    black_bishops: usize,
    black_rooks: usize,
    black_queens: usize,
}

#[derive(Debug)]
struct Positional {
    white_pawns_per_file: [usize; 8],
    black_pawns_per_file: [usize; 8],
}

const PAWN_TABLE_NUM_ENTRIES: usize = 2 * 1024;

#[derive(Debug, Default)]
struct PawnHashEntry {
    pawns: Bitboard,
    color: Bitboard,
    mg: Score,
    eg: Score,
}

pub type Score = i16;

pub const MATE_SCORE: Score = 20000;

pub const PAWN_SCORE: Score = 100;
pub const KNIGHT_SCORE: Score = 300;
pub const BISHOP_SCORE: Score = 320;
pub const ROOK_SCORE: Score = 500;
pub const QUEEN_SCORE: Score = 900;

impl Eval {
    fn mobility(&mut self, pos: &Position) -> Score {
        let mut white_knight_mobility = 0;
        let mut white_bishop_mobility = 0;
        let mut white_rook_mobility = 0;
        for knight in (pos.knights() & pos.white_pieces).squares() {
            white_knight_mobility += KNIGHT_MOBILITY
                [KNIGHT_ATTACKS[knight.0 as usize].popcount() as usize]
                - KNIGHT_MOBILITY_AVG;
        }

        for bishop in (pos.bishops() & pos.white_pieces).squares() {
            white_bishop_mobility += BISHOP_MOBILITY
                [get_bishop_attacks_from(bishop, pos.all_pieces).popcount() as usize]
                - BISHOP_MOBILITY_AVG;
        }

        for rook in (pos.rooks() & pos.white_pieces).squares() {
            white_rook_mobility += ROOK_MOBILITY
                [get_rook_attacks_from(rook, pos.all_pieces).popcount() as usize]
                - ROOK_MOBILITY_AVG;
        }

        let mut black_knight_mobility = 0;
        for knight in (pos.knights() & pos.black_pieces).squares() {
            black_knight_mobility += KNIGHT_MOBILITY
                [KNIGHT_ATTACKS[knight.0 as usize].popcount() as usize]
                - KNIGHT_MOBILITY_AVG;
        }

        let mut black_bishop_mobility = 0;
        for bishop in (pos.bishops() & pos.black_pieces).squares() {
            black_bishop_mobility += BISHOP_MOBILITY
                [get_bishop_attacks_from(bishop, pos.all_pieces).popcount() as usize]
                - BISHOP_MOBILITY_AVG;
        }

        let mut black_rook_mobility = 0;
        for rook in (pos.rooks() & pos.black_pieces).squares() {
            black_rook_mobility += ROOK_MOBILITY
                [get_rook_attacks_from(rook, pos.all_pieces).popcount() as usize]
                - ROOK_MOBILITY_AVG;
        }

        let white_mobility = white_knight_mobility + white_bishop_mobility + white_rook_mobility;
        let black_mobility = black_knight_mobility + black_bishop_mobility + black_rook_mobility;

        white_mobility - black_mobility
    }

    pub fn score(&mut self, pos: &Position, pawn_hash: Hash) -> Score {
        let mut score = 0;
        score += self.material.score();
        score += self.pst[1] - self.pst[0];
        score += self.positional.score(pos);
        score += self.mobility(pos);

        let phase = self.phase();
        let (king_mg, king_eg) = self.positional.king_safety(pos);
        score += (king_mg * phase + king_eg * (62 - phase)) / 62;
        let (pawns_mg, pawns_eg) = self.pawns(pos, pawn_hash);
        score += (pawns_mg * phase + pawns_eg * (62 - phase)) / 62;

        if pos.white_to_move {
            score
        } else {
            -score
        }
    }

    fn pawns(&mut self, pos: &Position, pawn_hash: Hash) -> (Score, Score) {
        let pawns = pos.bb[Piece::Pawn.index()];

        {
            let pawn_hash_entry = &self.pawn_table[pawn_hash as usize % PAWN_TABLE_NUM_ENTRIES];
            if pawn_hash_entry.pawns == pawns && pawn_hash_entry.color == pos.color & pawns {
                return (pawn_hash_entry.mg, pawn_hash_entry.eg);
            }
        }

        let (wmg, weg) = self.pawns_for_side(pos, true);
        let (bmg, beg) = self.pawns_for_side(pos, false);

        let pawn_hash_entry = self
            .pawn_table
            .get_mut(pawn_hash as usize % PAWN_TABLE_NUM_ENTRIES)
            .unwrap();
        pawn_hash_entry.pawns = pawns;
        pawn_hash_entry.color = pos.color & pawns;
        pawn_hash_entry.mg = wmg - bmg;
        pawn_hash_entry.eg = weg - beg;
        (wmg - bmg, weg - beg)
    }

    fn pawns_for_side(&mut self, pos: &Position, white: bool) -> (Score, Score) {
        let us = if white {
            pos.white_pieces
        } else {
            pos.black_pieces
        };
        let them = !us;
        let side = white as usize;

        const PASSER_ON_RANK_BONUS_EG: [Score; 8] = [0, 160, 80, 40, 20, 10, 10, 0];
        const PASSER_ON_RANK_BONUS_MG: [Score; 8] = [0, 60, 50, 40, 30, 20, 10, 0];

        let mut mg = 0;
        let mut eg = 0;
        for pawn in (pos.pawns() & us).squares() {
            let sq = pawn.0 as usize;
            let corridor = PAWN_CORRIDOR[side][sq];
            let file_forward = corridor & !(corridor.left(2) | corridor.right(2));
            let passed = (corridor & them & pos.pawns()).is_empty();
            let doubled = !(file_forward & us & pos.pawns()).is_empty();

            if passed && !doubled {
                let mut relative_rank = if white {
                    pawn.rank() as usize ^ 7
                } else {
                    pawn.rank() as usize
                };

                mg += PASSER_ON_RANK_BONUS_MG[relative_rank];
                eg += PASSER_ON_RANK_BONUS_EG[relative_rank];
            }
        }

        (mg, eg)
    }

    fn phase(&self) -> i16 {
        self.material.non_pawn_material()
    }

    pub fn make_move(&mut self, mov: Move, pos: &Position) {
        self.pst[pos.white_to_move as usize] -=
            pst(&PST[mov.piece.index()], pos.white_to_move, mov.from);

        if let Some(promotion_piece) = mov.promoted {
            self.pst[pos.white_to_move as usize] +=
                pst(&PST[promotion_piece.index()], pos.white_to_move, mov.to);
        } else {
            self.pst[pos.white_to_move as usize] +=
                pst(&PST[mov.piece.index()], pos.white_to_move, mov.to);
        }

        if let Some(captured_piece) = mov.captured {
            if mov.en_passant {
                self.pst[1 - pos.white_to_move as usize] -= pst(
                    &PST[Piece::Pawn.index()],
                    !pos.white_to_move,
                    mov.to.backward(pos.white_to_move, 1),
                );
            } else {
                self.pst[1 - pos.white_to_move as usize] -=
                    pst(&PST[captured_piece.index()], !pos.white_to_move, mov.to);
            }
        }

        match mov.piece {
            Piece::Pawn => {
                if pos.white_to_move {
                    self.positional.white_pawns_per_file[mov.from.file() as usize] -= 1;
                } else {
                    self.positional.black_pawns_per_file[mov.from.file() as usize] -= 1;
                }
            }
            _ => {}
        }

        match mov.captured {
            Some(Piece::Pawn) => {
                if pos.white_to_move {
                    self.positional.black_pawns_per_file[mov.to.file() as usize] -= 1;
                    self.material.black_pawns -= 1;
                } else {
                    self.positional.white_pawns_per_file[mov.to.file() as usize] -= 1;
                    self.material.white_pawns -= 1;
                }
            }
            Some(Piece::Knight) => {
                if pos.white_to_move {
                    self.material.black_knights -= 1;
                } else {
                    self.material.white_knights -= 1;
                }
            }
            Some(Piece::Bishop) => {
                if pos.white_to_move {
                    self.material.black_bishops -= 1;
                } else {
                    self.material.white_bishops -= 1;
                }
            }
            Some(Piece::Rook) => {
                if pos.white_to_move {
                    self.material.black_rooks -= 1;
                } else {
                    self.material.white_rooks -= 1;
                }
            }
            Some(Piece::Queen) => {
                if pos.white_to_move {
                    self.material.black_queens -= 1;
                } else {
                    self.material.white_queens -= 1;
                }
            }
            _ => {}
        }

        match mov.piece {
            Piece::Pawn => match mov.promoted {
                None => {
                    if pos.white_to_move {
                        self.positional.white_pawns_per_file[mov.to.file() as usize] += 1;
                    } else {
                        self.positional.black_pawns_per_file[mov.to.file() as usize] += 1;
                    }
                }
                Some(Piece::Knight) => {
                    if pos.white_to_move {
                        self.material.white_pawns -= 1;
                        self.material.white_knights += 1;
                    } else {
                        self.material.black_pawns -= 1;
                        self.material.black_knights += 1;
                    }
                }
                Some(Piece::Bishop) => {
                    if pos.white_to_move {
                        self.material.white_pawns -= 1;
                        self.material.white_bishops += 1;
                    } else {
                        self.material.black_pawns -= 1;
                        self.material.black_bishops += 1;
                    }
                }
                Some(Piece::Rook) => {
                    if pos.white_to_move {
                        self.material.white_pawns -= 1;
                        self.material.white_rooks += 1;
                    } else {
                        self.material.black_pawns -= 1;
                        self.material.black_rooks += 1;
                    }
                }
                Some(Piece::Queen) => {
                    if pos.white_to_move {
                        self.material.white_pawns -= 1;
                        self.material.white_queens += 1;
                    } else {
                        self.material.black_pawns -= 1;
                        self.material.black_queens += 1;
                    }
                }
                _ => {}
            },
            Piece::King => {
                if mov.to.0 == mov.from.0 + 2 {
                    // castle kingside
                    self.pst[pos.white_to_move as usize] -= pst(
                        &PST[Piece::Rook.index()],
                        pos.white_to_move,
                        mov.to.right(1),
                    );
                    self.pst[pos.white_to_move as usize] +=
                        pst(&PST[Piece::Rook.index()], pos.white_to_move, mov.to.left(1));
                } else if mov.from.0 == mov.to.0 + 2 {
                    // castle queenside
                    self.pst[pos.white_to_move as usize] -=
                        pst(&PST[Piece::Rook.index()], pos.white_to_move, mov.to.left(2));
                    self.pst[pos.white_to_move as usize] += pst(
                        &PST[Piece::Rook.index()],
                        pos.white_to_move,
                        mov.to.right(1),
                    );
                }
            }
            _ => {}
        }
    }

    pub fn unmake_move(&mut self, mov: Move, pos: &Position) {
        let unmaking_white_move = !pos.white_to_move;

        self.pst[1 - pos.white_to_move as usize] +=
            pst(&PST[mov.piece.index()], !pos.white_to_move, mov.from);

        if let Some(promotion_piece) = mov.promoted {
            self.pst[1 - pos.white_to_move as usize] -=
                pst(&PST[promotion_piece.index()], !pos.white_to_move, mov.to);
        } else {
            self.pst[1 - pos.white_to_move as usize] -=
                pst(&PST[mov.piece.index()], !pos.white_to_move, mov.to);
        }

        if let Some(captured_piece) = mov.captured {
            if mov.en_passant {
                self.pst[pos.white_to_move as usize] += pst(
                    &PST[Piece::Pawn.index()],
                    pos.white_to_move,
                    mov.to.backward(!pos.white_to_move, 1),
                );
            } else {
                self.pst[pos.white_to_move as usize] +=
                    pst(&PST[captured_piece.index()], pos.white_to_move, mov.to);
            }
        }

        match mov.piece {
            Piece::Pawn => {
                if unmaking_white_move {
                    self.positional.white_pawns_per_file[mov.from.file() as usize] += 1;
                } else {
                    self.positional.black_pawns_per_file[mov.from.file() as usize] += 1;
                }
            }
            Piece::King => {
                if mov.to.0 == mov.from.0 + 2 {
                    // castle kingside
                    self.pst[1 - pos.white_to_move as usize] += pst(
                        &PST[Piece::Rook.index()],
                        !pos.white_to_move,
                        mov.to.right(1),
                    );
                    self.pst[1 - pos.white_to_move as usize] -= pst(
                        &PST[Piece::Rook.index()],
                        !pos.white_to_move,
                        mov.to.left(1),
                    );
                } else if mov.from.0 == mov.to.0 + 2 {
                    // castle queenside
                    self.pst[1 - pos.white_to_move as usize] += pst(
                        &PST[Piece::Rook.index()],
                        !pos.white_to_move,
                        mov.to.left(2),
                    );
                    self.pst[1 - pos.white_to_move as usize] -= pst(
                        &PST[Piece::Rook.index()],
                        !pos.white_to_move,
                        mov.to.right(1),
                    );
                }
            }
            _ => {}
        }

        match mov.captured {
            Some(Piece::Pawn) => {
                if unmaking_white_move {
                    self.positional.black_pawns_per_file[mov.to.file() as usize] += 1;
                    self.material.black_pawns += 1;
                } else {
                    self.positional.white_pawns_per_file[mov.to.file() as usize] += 1;
                    self.material.white_pawns += 1;
                }
            }
            Some(Piece::Knight) => {
                if unmaking_white_move {
                    self.material.black_knights += 1;
                } else {
                    self.material.white_knights += 1;
                }
            }
            Some(Piece::Bishop) => {
                if unmaking_white_move {
                    self.material.black_bishops += 1;
                } else {
                    self.material.white_bishops += 1;
                }
            }
            Some(Piece::Rook) => {
                if unmaking_white_move {
                    self.material.black_rooks += 1;
                } else {
                    self.material.white_rooks += 1;
                }
            }
            Some(Piece::Queen) => {
                if unmaking_white_move {
                    self.material.black_queens += 1;
                } else {
                    self.material.white_queens += 1;
                }
            }
            _ => {}
        }

        if mov.piece == Piece::Pawn {
            match mov.promoted {
                None => {
                    if unmaking_white_move {
                        self.positional.white_pawns_per_file[mov.to.file() as usize] -= 1;
                    } else {
                        self.positional.black_pawns_per_file[mov.to.file() as usize] -= 1;
                    }
                }
                Some(Piece::Knight) => {
                    if unmaking_white_move {
                        self.material.white_pawns += 1;
                        self.material.white_knights -= 1;
                    } else {
                        self.material.black_pawns += 1;
                        self.material.black_knights -= 1;
                    }
                }
                Some(Piece::Bishop) => {
                    if unmaking_white_move {
                        self.material.white_pawns += 1;
                        self.material.white_bishops -= 1;
                    } else {
                        self.material.black_pawns += 1;
                        self.material.black_bishops -= 1;
                    }
                }
                Some(Piece::Rook) => {
                    if unmaking_white_move {
                        self.material.white_pawns += 1;
                        self.material.white_rooks -= 1;
                    } else {
                        self.material.black_pawns += 1;
                        self.material.black_rooks -= 1;
                    }
                }
                Some(Piece::Queen) => {
                    if unmaking_white_move {
                        self.material.white_pawns += 1;
                        self.material.white_queens -= 1;
                    } else {
                        self.material.black_pawns += 1;
                        self.material.black_queens -= 1;
                    }
                }
                _ => {}
            }
        }
    }
}

impl<'p> From<&'p Position> for Eval {
    fn from(pos: &Position) -> Eval {
        let mut pawn_table = Vec::with_capacity(PAWN_TABLE_NUM_ENTRIES);
        for _ in 0..PAWN_TABLE_NUM_ENTRIES {
            pawn_table.push(PawnHashEntry::default());
        }

        Eval {
            material: Material::from(pos),
            pst: init_pst_score(pos),
            positional: Positional::from(pos),
            pawn_table,
        }
    }
}

impl Material {
    pub fn is_draw(&self) -> bool {
        if self.white_pawns > 0 || self.white_rooks > 0 || self.white_queens > 0 {
            return false;
        }

        if self.black_pawns > 0 || self.black_rooks > 0 || self.black_queens > 0 {
            return false;
        }

        if self.white_bishops == 0 && self.white_knights == 0 {
            if self.black_bishops == 0 && self.black_knights < 3 {
                return true;
            }

            if self.black_bishops > 0 && self.black_bishops + self.black_knights > 1 {
                return false;
            }

            return true;
        }

        if self.black_bishops == 0 && self.black_knights == 0 {
            if self.white_bishops == 0 && self.white_knights < 3 {
                return true;
            }

            if self.white_bishops > 0 && self.white_bishops + self.white_knights > 1 {
                return false;
            }

            return true;
        }

        false
    }

    pub fn score(&self) -> Score {
        let mut white_material = 0;
        white_material += PAWN_SCORE * self.white_pawns as Score;
        white_material += KNIGHT_SCORE * self.white_knights as Score;
        white_material += BISHOP_SCORE * self.white_bishops as Score;
        white_material += ROOK_SCORE * self.white_rooks as Score;
        white_material += QUEEN_SCORE * self.white_queens as Score;

        let mut black_material = 0;
        black_material += PAWN_SCORE * self.black_pawns as Score;
        black_material += KNIGHT_SCORE * self.black_knights as Score;
        black_material += BISHOP_SCORE * self.black_bishops as Score;
        black_material += ROOK_SCORE * self.black_rooks as Score;
        black_material += QUEEN_SCORE * self.black_queens as Score;

        // encourage trading pieces if ahead in material or pawns if behind in material
        if white_material > black_material + 50 {
            white_material += 4 * self.white_pawns as Score;
            black_material -= 4 * self.black_pawns as Score;
        } else if black_material > white_material + 50 {
            black_material += 4 * self.black_pawns as Score;
            white_material -= 4 * self.white_pawns as Score;
        }

        if self.white_bishops > 1 {
            white_material += 40;
        }

        if self.black_bishops > 1 {
            black_material += 40;
        }

        white_material - black_material
    }

    pub fn non_pawn_material(&self) -> Score {
        let mut white_material = 0;
        white_material += 3 * self.white_knights as Score;
        white_material += 3 * self.white_bishops as Score;
        white_material += 5 * self.white_rooks as Score;
        white_material += 9 * self.white_queens as Score;

        let mut black_material = 0;
        black_material += 3 * self.black_knights as Score;
        black_material += 3 * self.black_bishops as Score;
        black_material += 5 * self.black_rooks as Score;
        black_material += 9 * self.black_queens as Score;

        white_material + black_material
    }
}

impl<'p> From<&'p Position> for Material {
    fn from(pos: &Position) -> Material {
        Material {
            white_pawns: (pos.white_pieces & pos.pawns()).popcount() as usize,
            white_knights: (pos.white_pieces & pos.knights()).popcount() as usize,
            white_bishops: (pos.white_pieces & pos.bishops()).popcount() as usize,
            white_rooks: (pos.white_pieces & pos.rooks()).popcount() as usize,
            white_queens: (pos.white_pieces & pos.queens()).popcount() as usize,
            black_pawns: (pos.black_pieces & pos.pawns()).popcount() as usize,
            black_knights: (pos.black_pieces & pos.knights()).popcount() as usize,
            black_bishops: (pos.black_pieces & pos.bishops()).popcount() as usize,
            black_rooks: (pos.black_pieces & pos.rooks()).popcount() as usize,
            black_queens: (pos.black_pieces & pos.queens()).popcount() as usize,
        }
    }
}

fn init_pst_score(pos: &Position) -> [Score; 2] {
    let mut white = 0;
    white += (pos.white_pieces & pos.pawns())
        .squares()
        .map(|sq| pst(&PST[Piece::Pawn.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces & pos.knights())
        .squares()
        .map(|sq| pst(&PST[Piece::Knight.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces & pos.bishops())
        .squares()
        .map(|sq| pst(&PST[Piece::Bishop.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces & pos.rooks())
        .squares()
        .map(|sq| pst(&PST[Piece::Rook.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces & pos.queens())
        .squares()
        .map(|sq| pst(&PST[Piece::Queen.index()], true, sq))
        .sum::<Score>();
    white += (pos.white_pieces & pos.kings())
        .squares()
        .map(|sq| pst(&PST[Piece::King.index()], true, sq))
        .sum::<Score>();

    let mut black = 0;
    black += (pos.black_pieces & pos.pawns())
        .squares()
        .map(|sq| pst(&PST[Piece::Pawn.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces & pos.knights())
        .squares()
        .map(|sq| pst(&PST[Piece::Knight.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces & pos.bishops())
        .squares()
        .map(|sq| pst(&PST[Piece::Bishop.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces & pos.rooks())
        .squares()
        .map(|sq| pst(&PST[Piece::Rook.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces & pos.queens())
        .squares()
        .map(|sq| pst(&PST[Piece::Queen.index()], false, sq))
        .sum::<Score>();
    black += (pos.black_pieces & pos.kings())
        .squares()
        .map(|sq| pst(&PST[Piece::King.index()], false, sq))
        .sum::<Score>();

    [black, white]
}

#[cfg_attr(rustfmt, rustfmt_skip)]
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

#[cfg_attr(rustfmt, rustfmt_skip)]
pub const KNIGHT_PST: [Score; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
pub const BISHOP_PST: [Score; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
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

#[cfg_attr(rustfmt, rustfmt_skip)]
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

#[cfg_attr(rustfmt, rustfmt_skip)]
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

impl Positional {
    fn score(&self, _pos: &Position) -> Score {
        let mut score = 0;
        let penalty = [0, 0, 25, 60, 90, 140, 200, 270];

        for &num_pawns in &self.white_pawns_per_file {
            score -= penalty[num_pawns];
        }

        for &num_pawns in &self.black_pawns_per_file {
            score += penalty[num_pawns];
        }

        score
    }

    pub fn king_safety(&self, pos: &Position) -> (Score, Score) {
        let (wmg, weg) = self.king_safety_for_side(pos, true);
        let (bmg, beg) = self.king_safety_for_side(pos, false);
        (wmg - bmg, weg - beg)
    }

    fn king_safety_for_side(&self, pos: &Position, white: bool) -> (Score, Score) {
        let us = if white {
            pos.white_pieces
        } else {
            pos.black_pieces
        };
        let them = !us;

        #[cfg_attr(rustfmt, rustfmt_skip)]
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
        if !(king_file & pos.rooks() & them).is_empty() {
            index += 1;
        }

        let mg_penalty = (index * index) as Score;
        let eg_penalty = CENTER_DISTANCE[king_sq.0 as usize];
        (-mg_penalty, -5 * eg_penalty)
    }
}

impl<'p> From<&'p Position> for Positional {
    fn from(pos: &Position) -> Positional {
        let mut white_pawns_per_file = [0; 8];
        let mut black_pawns_per_file = [0; 8];

        for pawn in (pos.pawns() & pos.white_pieces).squares() {
            white_pawns_per_file[pawn.file() as usize] += 1;
        }

        for pawn in (pos.pawns() & pos.black_pieces).squares() {
            black_pawns_per_file[pawn.file() as usize] += 1;
        }

        Positional {
            white_pawns_per_file,
            black_pawns_per_file,
        }
    }
}

const KNIGHT_MOBILITY: [Score; 9] = [-200, -90, 0, 40, 70, 90, 100, 105, 108];
const KNIGHT_MOBILITY_AVG: Score = 25;

const BISHOP_MOBILITY: [Score; 14] = [
    -200, -90, 0, 40, 70, 90, 100, 105, 108, 110, 112, 114, 116, 118,
];
const BISHOP_MOBILITY_AVG: Score = 57;

const ROOK_MOBILITY: [Score; 15] = [
    -200, -90, 0, 40, 70, 90, 100, 105, 108, 110, 112, 114, 116, 118, 120,
];
const ROOK_MOBILITY_AVG: Score = 61;
