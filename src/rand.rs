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

pub struct Xoshiro {
    state: [u64; 4],
}

impl Xoshiro {
    pub fn new(state: [u64; 4]) -> Self {
        Xoshiro { state }
    }

    pub fn gen(&mut self) -> u64 {
        let result = self.state[1].wrapping_mul(5).rotate_left(7).wrapping_mul(9);
        let t = self.state[1] << 17;

        self.state[2] ^= self.state[0];
        self.state[3] ^= self.state[1];
        self.state[1] ^= self.state[2];
        self.state[0] ^= self.state[3];

        self.state[2] ^= t;
        self.state[3] = self.state[3].rotate_left(45);

        result
    }

    pub fn gen_sparse<const N: usize>(&mut self) -> u64 {
        if N == 0 {
            return 0;
        }

        let mut result = self.gen();
        for _ in 1..N {
            result &= self.gen();
        }

        result
    }

    pub fn fill<T: Fill>(&mut self, v: &mut T) {
        v.fill(self)
    }
}

pub trait Fill {
    fn fill(&mut self, rand: &mut Xoshiro);
}

impl Fill for u64 {
    fn fill(&mut self, rand: &mut Xoshiro) {
        *self = rand.gen();
    }
}

impl<const N: usize, T: Fill> Fill for [T; N] {
    fn fill(&mut self, rand: &mut Xoshiro) {
        self.iter_mut().for_each(|v| v.fill(rand))
    }
}
