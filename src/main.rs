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
#![warn(clippy::option_map_unwrap_or)]
#![warn(clippy::option_map_unwrap_or_else)]
#![warn(clippy::result_map_unwrap_or_else)]
#![warn(clippy::single_match_else)]
#![warn(clippy::unseparated_literal_suffix)]
#![warn(clippy::used_underscore_binding)]
#![warn(clippy::clone_on_ref_ptr)]
#![warn(clippy::multiple_inherent_impl)]

mod bench;
mod bitboard;
mod eval;
mod hash;
mod history;
mod movegen;
mod movepick;
pub mod position;
mod repetitions;
mod search;
mod time;
mod tt;
mod uci;

use std::env::args;

use crate::position::STARTING_POSITION;
use crate::search::Search;
use crate::uci::*;

fn main() {
    let mut args = args().skip(1);
    if let Some(cmd) = args.next() {
        match cmd.as_ref() {
            "perft" => {
                let mut search = Search::new(STARTING_POSITION);
                let i = if let Some(n) = args.next() {
                    n.parse().unwrap()
                } else {
                    4
                };
                search.perft(i);
            }
            "go" => {
                let mut search = Search::new(STARTING_POSITION);
                println!("bestmove {}", search.root().to_algebraic());
            }
            _ => {}
        }
    } else {
        let mut uci = UCI::new();
        uci.run();
    }
}
