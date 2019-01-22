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
use crate::bitboard::*;
use crate::eval::*;
use crate::hash::*;
use crate::movegen::*;
use crate::position::*;
use crate::search::*;

use std::cmp;

#[repr(align(64))]
pub struct Bucket([TTEntry; NUM_CLUSTERS]);

pub struct TT {
    table: Vec<Bucket>,
    bitmask: u64,
    generation: u8,
}

pub type Bound = u8;
pub const LOWER_BOUND: Bound = 1;
pub const UPPER_BOUND: Bound = 2;
pub const EXACT_BOUND: Bound = LOWER_BOUND | UPPER_BOUND;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TTScore(Score);

impl TTScore {
    pub fn to_score(self, ply: Ply) -> Score {
        if self.0 < -MATE_SCORE + MAX_PLY {
            self.0 + ply
        } else if self.0 > MATE_SCORE - MAX_PLY {
            self.0 - ply
        } else {
            self.0
        }
    }

    pub fn from_score(score: Score, ply: Ply) -> TTScore {
        if score < -MATE_SCORE + MAX_PLY {
            TTScore(score - ply)
        } else if score > MATE_SCORE - MAX_PLY {
            TTScore(score + ply)
        } else {
            TTScore(score)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct TTEntry {
    key: Hash,             // 8 byte
    pub best_move: TTMove, // 2 byte
    pub depth: Depth,      // 2 byte
    pub score: TTScore,    // 2 byte
    pub bound: Bound,      // 1 byte
    generation: u8,        // 1 byte
}

impl Default for TTEntry {
    fn default() -> Self {
        TTEntry {
            key: 0,
            depth: 0,
            score: TTScore(0),
            best_move: TTMove { from: 0, to: 0 },
            bound: 0,
            generation: 0,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct TTMove {
    from: u8, // bits 0-5: from positions, bit 6: capture, bit 7: en passant
    to: u8,   // bits 0-5: to position, bits 6-7 promotion piece
}

const SQUARE_MASK: u8 = 0b0011_1111;
const CAPTURE_FLAG: u8 = 0b0100_0000;
const EN_PASSANT_FLAG: u8 = 0b1000_0000;
const PROMOTION_MASK: u8 = 0b1100_0000;
const PROMOTION_QUEEN: u8 = 0b1100_0000;
const PROMOTION_ROOK: u8 = 0b1000_0000;
const PROMOTION_BISHOP: u8 = 0b0100_0000;
const PROMOTION_KNIGHT: u8 = 0b0000_0000;

impl TTMove {
    fn is_null(self) -> bool {
        unsafe { ::std::mem::transmute::<TTMove, u16>(self) == 0 }
    }

    // Expands this `TTMove` to a `Move`value.
    pub fn expand(self, pos: &Position) -> Option<Move> {
        let mut result = Move {
            from: Square(self.from & SQUARE_MASK),
            to: Square(self.to & SQUARE_MASK),
            piece: pos.find_piece(Square(self.from & SQUARE_MASK))?,
            captured: None,
            promoted: None,
            en_passant: self.from & EN_PASSANT_FLAG > 0,
        };

        if self.from & CAPTURE_FLAG > 0 {
            if result.en_passant {
                result.captured = Some(Piece::Pawn);
            } else {
                result.captured = pos.find_piece(result.to);
                if result.captured.is_none() {
                    // This move is supposed to be a capture but isn't
                    return None;
                }
            }
        }

        if result.piece == Piece::Pawn && (result.to.rank() == 0 || result.to.rank() == 7) {
            match self.to & PROMOTION_MASK {
                PROMOTION_QUEEN => result.promoted = Some(Piece::Queen),
                PROMOTION_ROOK => result.promoted = Some(Piece::Rook),
                PROMOTION_BISHOP => result.promoted = Some(Piece::Bishop),
                PROMOTION_KNIGHT => result.promoted = Some(Piece::Knight),
                _ => {}
            }
        }

        Some(result)
    }
}

impl From<Move> for TTMove {
    fn from(mov: Move) -> TTMove {
        let mut result = TTMove {
            from: mov.from.0 & SQUARE_MASK,
            to: mov.to.0 & SQUARE_MASK,
        };

        if mov.captured.is_some() {
            result.from |= CAPTURE_FLAG;
        }

        if mov.en_passant {
            result.from |= EN_PASSANT_FLAG;
        }

        match mov.promoted {
            Some(Piece::Queen) => result.to |= PROMOTION_QUEEN,
            Some(Piece::Rook) => result.to |= PROMOTION_ROOK,
            Some(Piece::Bishop) => result.to |= PROMOTION_BISHOP,
            Some(Piece::Knight) => result.to |= PROMOTION_KNIGHT,
            _ => {}
        }

        result
    }
}

const NUM_CLUSTERS: usize = 4;

impl TT {
    pub fn new(bits: u64) -> Self {
        let bitmask = (1 << bits) - 1;
        let mut table = Vec::with_capacity(1 << bits);
        for _ in 0..(1 << bits) {
            table.push(Bucket([TTEntry::default(); NUM_CLUSTERS]));
        }

        TT {
            table,
            bitmask,
            generation: 0,
        }
    }

    pub fn usage(&self) -> u64 {
        let n = cmp::min(300, self.table.len());
        let total = n * NUM_CLUSTERS;
        let mut usage = 0;
        for bucket in self.table.iter().take(n) {
            for &entry in &bucket.0 {
                if !entry.best_move.is_null() && entry.generation == self.generation {
                    usage += 1;
                }
            }
        }
        1000 * usage / total as u64
    }

    pub fn next_generation(&mut self) {
        self.generation = self.generation.wrapping_add(1);
    }

    pub fn insert(
        &mut self,
        hash: Hash,
        depth: Depth,
        score: TTScore,
        best_move: Move,
        bound: Bound,
    ) {
        let mut replace_age = None;
        let mut age_depth = 255;
        let mut replace_depth = None;
        let mut lowest_depth = 255;
        let mut replace = NUM_CLUSTERS - 1;

        {
            let entries = unsafe { self.table.get_unchecked((hash & self.bitmask) as usize).0 };
            for (i, entry) in entries.into_iter().enumerate() {
                if entry.best_move.is_null() {
                    replace = i;
                    replace_age = None;
                    replace_depth = None;
                    break;
                }

                if entry.key == hash {
                    replace = i;
                    replace_age = None;
                    replace_depth = None;
                    break;
                }

                if self.generation != entry.generation && entry.depth < age_depth {
                    age_depth = entry.depth;
                    replace_age = Some(i);
                }

                if entry.depth < lowest_depth {
                    replace_depth = Some(i);
                    lowest_depth = entry.depth;
                }
            }
        }

        if let Some(i) = replace_age {
            replace = i;
        } else if let Some(i) = replace_depth {
            replace = i;
        }

        unsafe {
            self.table
                .get_unchecked_mut((hash & self.bitmask) as usize)
                .0[replace] = TTEntry {
                key: hash,
                depth,
                score,
                best_move: TTMove::from(best_move),
                bound,
                generation: self.generation,
            }
        };
    }

    pub fn get(&mut self, hash: Hash) -> Option<TTEntry> {
        for entry in unsafe {
            &mut self
                .table
                .get_unchecked_mut((hash & self.bitmask) as usize)
                .0
        } {
            if entry.best_move.is_null() {
                break;
            }

            if entry.key == hash {
                entry.generation = self.generation;
                return Some(*entry);
            }
        }

        None
    }
}
