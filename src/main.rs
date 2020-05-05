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
mod fathom;
mod hash;
mod history;
mod magic;
mod movegen;
mod movepick;
pub mod position;
mod repetitions;
mod search;
mod search_controller;
mod syzygy;
mod time;
mod tt;
#[cfg(feature = "tune")]
mod tune;
mod types;
mod uci;

use crate::bench::run_benchmark;
use crate::magic::initialize_magics;
use crate::uci::*;

fn main() {
    initialize_magics();
    if std::env::args().nth(1) == Some(String::from("bench")) {
        run_benchmark(
            std::env::args()
                .nth(2)
                .and_then(|depth| depth.parse::<i16>().ok())
                .unwrap_or(12),
            std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
        );
        return;
    }

    let mut uci = UCI::new();
    uci.run();
}
