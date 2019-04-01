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
use crate::search::Ply;

#[derive(Clone)]
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

    pub fn clear(&mut self) {
        self.past_positions.iter_mut().for_each(|vec| vec.clear());
        self.past_positions[0].push(0);
        self.index = 0;
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

    pub fn has_repeated(&self, ply: Ply) -> bool {
        let current = self.past_positions[self.index].last().unwrap();
        let repeated_since_root = self.past_positions[self.index]
            .iter()
            .rev()
            .take(ply as usize + 1)
            .step_by(2)
            .skip(1)
            .any(|h| h == current);

        if repeated_since_root {
            return true;
        }

        let repeated_twice_before_root = self.past_positions[self.index]
            .iter()
            .rev()
            .skip(ply as usize)
            .skip(ply as usize % 2) // skip once more if not root side-to-move
            .step_by(2)
            .filter(|&h| h == current)
            .skip(1)
            .next().is_some();

        repeated_twice_before_root
    }
}
