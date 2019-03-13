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
use crate::movegen::*;
use crate::search::*;
use crate::types::SquareMap;

#[derive(Default)]
pub struct History {
    piece_to: [[SquareMap<i64>; 6]; 2],
}

impl History {
    pub fn get_score(&self, white: bool, mov: Move) -> i64 {
        self.piece_to[white as usize][mov.piece.index()][mov.to]
    }

    pub fn increase_score(&mut self, white: bool, mov: Move, depth: Depth) {
        let d = i64::from(depth / INC_PLY);

        self.piece_to[white as usize][mov.piece.index()][mov.to] += d * d;
    }

    pub fn decrease_score(&mut self, white: bool, moves: &[Option<Move>], depth: Depth) {
        let d = i64::from(depth / INC_PLY);

        for mov in moves {
            let mov = mov.unwrap();
            self.piece_to[white as usize][mov.piece.index()][mov.to] -= d * d;
        }
    }

    pub fn rescale(&mut self) {
        for stm in 0..2 {
            for piece in 0..6 {
                for hist in self.piece_to[stm][piece].iter_mut() {
                    *hist /= 256;
                }
            }
        }
    }
}
