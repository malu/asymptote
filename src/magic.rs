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
use crate::bitboard::*;
use crate::types::SquareMap;
use rand::{prelude::*, prng::ChaChaRng};

const SHIFT_MASK: u64 = 0xF8_00_00_00_00_00_00_00;

pub fn initialize_magics() {
    let offset = initialize_bishop_attacks(0);
    initialize_rook_attacks(offset);
}

pub static mut MAGIC_TABLE: [Bitboard; 156_800] = [Bitboard(0); 156_800];
pub static mut BISHOP_ATTACKS: SquareMap<Magic> = SquareMap::from_array(
    [Magic {
        magic: 0,
        mask: Bitboard(0),
        offset: 0,
    }; 64],
);
pub static mut ROOK_ATTACKS: SquareMap<Magic> = SquareMap::from_array(
    [Magic {
        magic: 0,
        mask: Bitboard(0),
        offset: 0,
    }; 64],
);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Magic {
    magic: u64,
    mask: Bitboard,
    offset: u32,
}

impl Magic {
    pub fn index(&self, occupied: Bitboard) -> usize {
        let shift = self.magic.wrapping_shr(56) as u32;
        self.offset as usize
            + ((occupied & self.mask).0.wrapping_mul(self.magic)).wrapping_shr(shift) as usize
    }
}

fn initialize_bishop_attacks(offset: usize) -> usize {
    let border = FILE_A | FILE_H | RANK_1 | RANK_8;

    let mut seed = [0; 32];
    seed[0] = 1;
    for i in 1..32 {
        seed[i] = (((i * i) + seed[i - 1] as usize) % 256) as u8;
    }
    let mut rng = ChaChaRng::from_seed(seed);

    let mut offset = offset;

    for sq in 0..64 {
        let from = Square::from(sq);
        let mask = bishop_from(from, Bitboard::from(0)) & !border;
        let bits = mask.popcount() as u64;
        let shift = 64 - bits;

        let mut occ = Bitboard::from(0);
        let mut size = 0;

        let mut occupancy = Vec::with_capacity(1 << size);
        let mut reference = Vec::with_capacity(1 << size);

        loop {
            occupancy.push(occ);
            reference.push(bishop_from(from, occ));
            size += 1;
            occ = Bitboard::from(occ.0.wrapping_sub(mask.0)) & mask;
            if occ.is_empty() {
                break;
            }
        }

        // search for magics
        let mut magic = Magic {
            magic: sparse_random(&mut rng) & !SHIFT_MASK | shift.wrapping_shl(56),
            mask,
            offset: offset as u32,
        };

        let mut last_used = Vec::with_capacity(size);
        for _ in 0..size {
            last_used.push(0);
        }

        let mut tries = 1;

        'search_magic: loop {
            for i in 0..size {
                let index = magic.index(occupancy[i]);
                let magic_table_entry = unsafe { MAGIC_TABLE[index] };
                if magic_table_entry != reference[i] && last_used[index - offset] == tries {
                    // retry
                    magic.magic = sparse_random(&mut rng) & !SHIFT_MASK | shift.wrapping_shl(56);
                    tries += 1;
                    continue 'search_magic;
                }
                unsafe {
                    MAGIC_TABLE[index] = reference[i];
                }
                last_used[index - offset] = tries;
            }

            break;
        }

        unsafe {
            BISHOP_ATTACKS[from] = magic;
        }
        offset += size;
    }

    offset
}

fn bishop_from(from: Square, blockers: Bitboard) -> Bitboard {
    let empty = !blockers;

    let mut propagators_ne = empty;
    let mut propagators_se = empty;
    let mut propagators_sw = empty;
    let mut propagators_nw = empty;
    let mut reachable_ne = Bitboard::from(0);
    let mut reachable_se = Bitboard::from(0);
    let mut reachable_sw = Bitboard::from(0);
    let mut reachable_nw = Bitboard::from(0);
    reachable_ne |= from;
    reachable_se |= from;
    reachable_sw |= from;
    reachable_nw |= from;

    reachable_ne |= reachable_ne.forward(true, 1).right(1) & propagators_ne;
    propagators_ne &= propagators_ne.forward(true, 1).right(1);
    reachable_ne |= reachable_ne.forward(true, 2).right(2) & propagators_ne;
    propagators_ne &= propagators_ne.forward(true, 2).right(2);
    reachable_ne |= reachable_ne.forward(true, 4).right(4) & propagators_ne;

    reachable_se |= reachable_se.backward(true, 1).right(1) & propagators_se;
    propagators_se &= propagators_se.backward(true, 1).right(1);
    reachable_se |= reachable_se.backward(true, 2).right(2) & propagators_se;
    propagators_se &= propagators_se.backward(true, 2).right(2);
    reachable_se |= reachable_se.backward(true, 4).right(4) & propagators_se;

    reachable_sw |= reachable_sw.backward(true, 1).left(1) & propagators_sw;
    propagators_sw &= propagators_sw.backward(true, 1).left(1);
    reachable_sw |= reachable_sw.backward(true, 2).left(2) & propagators_sw;
    propagators_sw &= propagators_sw.backward(true, 2).left(2);
    reachable_sw |= reachable_sw.backward(true, 4).left(4) & propagators_sw;

    reachable_nw |= reachable_nw.forward(true, 1).left(1) & propagators_nw;
    propagators_nw &= propagators_nw.forward(true, 1).left(1);
    reachable_nw |= reachable_nw.forward(true, 2).left(2) & propagators_nw;
    propagators_nw &= propagators_nw.forward(true, 2).left(2);
    reachable_nw |= reachable_nw.forward(true, 4).left(4) & propagators_nw;

    reachable_ne.forward(true, 1).right(1)
        | reachable_se.backward(true, 1).right(1)
        | reachable_sw.backward(true, 1).left(1)
        | reachable_nw.forward(true, 1).left(1)
}

fn initialize_rook_attacks(offset: usize) -> usize {
    let border_files = FILE_A | FILE_H;
    let border_ranks = RANK_1 | RANK_8;

    let mut seed = [0; 32];
    seed[0] = 3;
    for i in 1..32 {
        seed[i] = (((i * i) + seed[i - 1] as usize) % 256) as u8;
    }
    let mut rng = ChaChaRng::from_seed(seed);

    let mut offset = offset;

    for sq in 0..64 {
        let from = Square::from(sq);
        let mask = (FILES[from.file() as usize] & !border_ranks)
            ^ (RANKS[from.rank() as usize] & !border_files);
        let bits = mask.popcount() as u64;
        let shift = 64 - bits;

        let mut occ = Bitboard::from(0);
        let mut size = 0;

        let mut occupancy = Vec::with_capacity(1 << bits);
        let mut reference = Vec::with_capacity(1 << bits);

        loop {
            occupancy.push(occ);
            reference.push(rook_from(from, occ));
            size += 1;
            occ = Bitboard::from(occ.0.wrapping_sub(mask.0)) & mask;
            if occ.is_empty() {
                break;
            }
        }

        // search for magics
        let mut magic = Magic {
            magic: sparse_random(&mut rng) & !SHIFT_MASK | shift.wrapping_shl(56),
            mask,
            offset: offset as u32,
        };

        let mut last_used = Vec::with_capacity(size);
        for _ in 0..size {
            last_used.push(0);
        }

        let mut tries = 1;

        'search_magic: loop {
            for i in 0..size {
                let index = magic.index(occupancy[i]);
                let magic_table_entry = unsafe { MAGIC_TABLE[index] };
                if magic_table_entry != reference[i] && last_used[index - offset] == tries {
                    // retry
                    magic.magic = sparse_random(&mut rng) & !SHIFT_MASK | shift.wrapping_shl(56);
                    tries += 1;
                    continue 'search_magic;
                }
                unsafe {
                    MAGIC_TABLE[index] = reference[i];
                }
                last_used[index - offset] = tries;
            }

            break;
        }

        unsafe {
            ROOK_ATTACKS[from] = magic;
        }
        offset += size;
    }

    offset
}

fn rook_from(from: Square, blockers: Bitboard) -> Bitboard {
    let empty = !blockers;

    let mut propagators_north = empty;
    let mut propagators_south = empty;
    let mut propagators_west = empty;
    let mut propagators_east = empty;
    let mut reachable_north = Bitboard::from(0);
    let mut reachable_south = Bitboard::from(0);
    let mut reachable_west = Bitboard::from(0);
    let mut reachable_east = Bitboard::from(0);
    reachable_north |= from;
    reachable_south |= from;
    reachable_west |= from;
    reachable_east |= from;

    reachable_north |= reachable_north.forward(true, 1) & propagators_north;
    propagators_north &= propagators_north.forward(true, 1);
    reachable_north |= reachable_north.forward(true, 2) & propagators_north;
    propagators_north &= propagators_north.forward(true, 2);
    reachable_north |= reachable_north.forward(true, 4) & propagators_north;

    reachable_south |= reachable_south.backward(true, 1) & propagators_south;
    propagators_south &= propagators_south.backward(true, 1);
    reachable_south |= reachable_south.backward(true, 2) & propagators_south;
    propagators_south &= propagators_south.backward(true, 2);
    reachable_south |= reachable_south.backward(true, 4) & propagators_south;

    reachable_west |= reachable_west.left(1) & propagators_west;
    propagators_west &= propagators_west.left(1);
    reachable_west |= reachable_west.left(2) & propagators_west;
    propagators_west &= propagators_west.left(2);
    reachable_west |= reachable_west.left(4) & propagators_west;

    reachable_east |= reachable_east.right(1) & propagators_east;
    propagators_east &= propagators_east.right(1);
    reachable_east |= reachable_east.right(2) & propagators_east;
    propagators_east &= propagators_east.right(2);
    reachable_east |= reachable_east.right(4) & propagators_east;

    reachable_north.forward(true, 1)
        | reachable_south.backward(true, 1)
        | reachable_west.left(1)
        | reachable_east.right(1)
}

fn sparse_random(rng: &mut ChaChaRng) -> u64 {
    rng.gen::<u64>() & rng.gen::<u64>() & rng.gen::<u64>()
}

