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
use std::mem;

use crate::bitboard::Square;
use crate::rand::{Fill, Xoshiro};

pub struct SquareMap<T> {
    data: [T; 64],
}

impl<T> SquareMap<T> {
    pub const fn from_array(data: [T; 64]) -> SquareMap<T> {
        SquareMap { data }
    }
}

impl<T: Copy> Copy for SquareMap<T> {}

impl<T: Clone> Clone for SquareMap<T> {
    fn clone(&self) -> Self {
        SquareMap {
            data: self.data.clone(),
        }
    }
}

impl<T> Fill for SquareMap<T>
where
    T: Fill,
{
    fn fill(&mut self, rng: &mut Xoshiro) {
        self.data.fill(rng)
    }
}

impl<T: Copy + Default> Default for SquareMap<T> {
    fn default() -> Self {
        SquareMap {
            data: [Default::default(); 64],
        }
    }
}

impl<T> From<[T; 64]> for SquareMap<T> {
    fn from(data: [T; 64]) -> SquareMap<T> {
        SquareMap { data }
    }
}

impl<T> std::ops::Index<Square> for SquareMap<T> {
    type Output = T;

    fn index(&self, sq: Square) -> &Self::Output {
        unsafe {
            let i = mem::transmute::<Square, u8>(sq);
            self.data.get_unchecked(i as usize)
        }
    }
}

impl<T> std::ops::IndexMut<Square> for SquareMap<T> {
    fn index_mut(&mut self, sq: Square) -> &mut Self::Output {
        unsafe {
            let i = mem::transmute::<Square, u8>(sq);
            self.data.get_unchecked_mut(i as usize)
        }
    }
}
