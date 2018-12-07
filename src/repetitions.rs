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
use crate::hash::Hash;

pub struct Repetitions {
    past_positions: Vec<Vec<Hash>>,
    index: usize,
}

impl Repetitions {
    pub fn new(capacity: usize) -> Self {
        let mut past_positions = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            past_positions.push(Vec::with_capacity(100));
        }

        past_positions[0].push(0);

        Repetitions {
            past_positions,
            index: 0,
        }
    }

    pub fn irreversible_move(&mut self) {
        self.index += 1;
        if self.index >= self.past_positions.len() {
            self.past_positions.push(Vec::with_capacity(100));
        }
    }

    pub fn push_position(&mut self, hash: Hash) {
        self.past_positions[self.index].push(hash);
    }

    pub fn pop_position(&mut self) {
        self.past_positions[self.index].pop();
        if self.past_positions[self.index].is_empty() {
            self.index -= 1;
        }
    }

    pub fn has_repeated(&self) -> bool {
        let current = self.past_positions[self.index].last().unwrap();
        self.past_positions[self.index]
            .iter()
            .take(self.past_positions[self.index].len() - 1)
            .any(|h| h == current)
    }
}
