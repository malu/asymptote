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
use crate::movegen::*;
use crate::search::*;
use crate::types::SquareMap;

const DECAY: i32 = 512;

#[derive(Clone, Default)]
pub struct History {
    piece_to: [SquareMap<SquareMap<i16>>; 2],
    pub last_best_reply: [[SquareMap<Option<Move>>; 6]; 2],
}

impl History {
    pub fn get_score(&self, white: bool, mov: Move) -> i16 {
        self.piece_to[white as usize][mov.from][mov.to]
    }

    fn change_score(&mut self, white: bool, depth: Depth, mov: Move) {
        // keep the sign of depth, but square the absolute value
        let change = depth * depth.abs();
        let change = change.clamp(-400, 400);

        let entry = &mut self.piece_to[white as usize][mov.from][mov.to];
        let decay = *entry as i32 * change.abs() as i32 / DECAY;
        *entry += 32 * change - decay as i16;
    }

    pub fn increase_score(&mut self, white: bool, mov: Move, depth: Depth) {
        let d = depth / INC_PLY;
        self.change_score(white, d, mov);
    }

    pub fn decrease_score(&mut self, white: bool, moves: &[Option<Move>], depth: Depth) {
        let d = depth / INC_PLY;

        for mov in moves {
            let mov = mov.unwrap();
            self.change_score(white, -d, mov);
        }
    }
}
