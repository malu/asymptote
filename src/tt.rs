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
use crate::bitboard::*;
use crate::eval::*;
use crate::hash::*;
use crate::movegen::*;
use crate::position::*;
use crate::search::*;

use std::cmp;
use std::sync::atomic::{AtomicU64, Ordering};

pub struct TT {
    table: Vec<Bucket>,
    bitmask: u64,
    generation: u8,
}

impl TT {
    pub fn new(bits: u64) -> Self {
        let bitmask = (1 << bits) - 1;
        let mut table = Vec::with_capacity(1 << bits);
        for _ in 0..(1 << bits) {
            table.push(Bucket::default());
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
            for atomic_entry in &bucket.0 {
                let entry = atomic_entry.read();
                if entry.generation == self.generation {
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
        &self,
        hash: Hash,
        depth: Depth,
        score: TTScore,
        best_move: Option<Move>,
        bound: Bound,
        eval: Option<Score>,
    ) {
        let mut replace_age = None;
        let mut age_depth = Depth::max_value();
        let mut replace_depth = None;
        let mut lowest_depth = Depth::max_value();
        let mut replace = 0;

        {
            let entries = unsafe { &self.table.get_unchecked((hash & self.bitmask) as usize).0 };
            for (i, atomic_entry) in entries.iter().enumerate() {
                let entry = atomic_entry.read();
                if entry.key == (hash >> 32) as u32 {
                    if bound != EXACT_BOUND && depth < entry.depth - 3 * INC_PLY {
                        return;
                    }

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

        let mut flags = 0;
        if eval.is_some() {
            flags |= FLAG_HAS_SCORE;
        }

        if best_move.is_some() {
            flags |= FLAG_HAS_MOVE;
        }

        unsafe {
            self.table.get_unchecked((hash & self.bitmask) as usize).0[replace].write(&TTEntry {
                key: (hash >> 32) as u32,
                depth,
                score,
                best_move: best_move.map_or(TTMove { from: 0, to: 0 }, TTMove::from),
                bound,
                generation: self.generation,
                eval: eval.unwrap_or(0),
                flags,
                _pad: 0,
            })
        };
    }

    pub fn get(&self, hash: Hash) -> Option<TTEntry> {
        for atomic_entry in unsafe { &self.table.get_unchecked((hash & self.bitmask) as usize).0 } {
            let mut entry = atomic_entry.read();
            if entry.key == (hash >> 32) as u32 {
                entry.generation = self.generation;
                atomic_entry.write(&entry);
                return Some(entry);
            }
        }

        None
    }
}

#[repr(align(64))]
pub struct Bucket([AtomicU128; NUM_CLUSTERS]);
const NUM_CLUSTERS: usize = 4;

impl Default for Bucket {
    fn default() -> Self {
        Bucket([
            AtomicU128::default(),
            AtomicU128::default(),
            AtomicU128::default(),
            AtomicU128::default(),
        ])
    }
}

const FLAG_HAS_SCORE: u8 = 0x1;
const FLAG_HAS_MOVE: u8 = 0x2;

struct AtomicU128(AtomicU64, AtomicU64);

impl AtomicU128 {
    fn read(&self) -> TTEntry {
        let a = self.0.load(Ordering::Relaxed);
        let b = self.1.load(Ordering::Relaxed);
        (a, a ^ b).into()
    }

    fn write(&self, entry: &TTEntry) {
        let (a, b): (u64, u64) = entry.into();
        self.0.store(a, Ordering::Relaxed);
        self.1.store(a ^ b, Ordering::Relaxed);
    }
}

impl Default for AtomicU128 {
    fn default() -> Self {
        AtomicU128(AtomicU64::default(), AtomicU64::default())
    }
}

#[repr(align(16))]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct TTEntry {
    key: u32,              // 4 byte
    pub best_move: TTMove, // 2 byte
    pub depth: Depth,      // 2 byte
    pub score: TTScore,    // 2 byte
    pub eval: Score,       // 2 byte
    pub bound: Bound,      // 1 byte
    generation: u8,        // 1 byte
    flags: u8,             // 1 byte
    _pad: u8,              // 1 byte
}

impl TTEntry {
    pub fn get_eval(&self) -> Option<Score> {
        if self.flags & FLAG_HAS_SCORE > 0 {
            Some(self.eval)
        } else {
            None
        }
    }

    pub fn has_move(&self) -> bool {
        self.flags & FLAG_HAS_MOVE > 0
    }
}

impl From<(u64, u64)> for TTEntry {
    fn from(v: (u64, u64)) -> Self {
        unsafe { std::mem::transmute(v) }
    }
}

impl From<&TTEntry> for (u64, u64) {
    fn from(v: &TTEntry) -> Self {
        unsafe { std::mem::transmute(*v) }
    }
}

impl Default for TTEntry {
    fn default() -> Self {
        TTEntry {
            key: 0,
            depth: 0,
            score: TTScore(0),
            eval: 0,
            best_move: TTMove { from: 0, to: 0 },
            bound: 0,
            generation: 0,
            flags: 0,
            _pad: 0,
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
    // Expands this `TTMove` to a `Move`value.
    pub fn expand(self, pos: &Position) -> Option<Move> {
        let mut result = Move {
            from: Square::from(self.from & SQUARE_MASK),
            to: Square::from(self.to & SQUARE_MASK),
            piece: pos.find_piece(Square::from(self.from & SQUARE_MASK))?,
            captured: None,
            promoted: None,
            en_passant: self.from & EN_PASSANT_FLAG > 0,
        };

        if self.from & CAPTURE_FLAG > 0 {
            if result.en_passant {
                result.captured = Some(Piece::Pawn);
            } else {
                result.captured = pos.find_piece(result.to);
                result.captured?;
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
        let from: u8 = mov.from.into();
        let to: u8 = mov.to.into();
        let mut result = TTMove {
            from: from & SQUARE_MASK,
            to: to & SQUARE_MASK,
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

pub type Bound = u8;
pub const LOWER_BOUND: Bound = 1;
pub const UPPER_BOUND: Bound = 2;
pub const EXACT_BOUND: Bound = LOWER_BOUND | UPPER_BOUND;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_size_of_structs() {
        assert_eq!(::std::mem::size_of::<TTEntry>(), 16);
        assert_eq!(::std::mem::size_of::<Bucket>(), 64);
    }

    #[test]
    fn test_align_of_structs() {
        assert_eq!(::std::mem::align_of::<TTEntry>(), 16);
        assert_eq!(::std::mem::align_of::<Bucket>(), 64);
    }
}
