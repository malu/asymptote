#![cfg_attr(feature = "cargo-clippy", warn(option_map_unwrap_or))]
#![cfg_attr(feature = "cargo-clippy", warn(option_map_unwrap_or_else))]
#![cfg_attr(feature = "cargo-clippy", warn(result_map_unwrap_or_else))]
#![cfg_attr(feature = "cargo-clippy", warn(single_match_else))]
#![cfg_attr(feature = "cargo-clippy", warn(unseparated_literal_suffix))]
#![cfg_attr(feature = "cargo-clippy", warn(used_underscore_binding))]

#![cfg_attr(feature = "cargo-clippy", warn(clone_on_ref_ptr))]
#![cfg_attr(feature = "cargo-clippy", warn(multiple_inherent_impl))]

#[macro_use]
extern crate lazy_static;
extern crate rand;

mod bitboard;
mod eval;
mod hash;
mod movegen;
mod movepick;
mod position;
mod search;
mod tt;
mod uci;

use std::env::args;

use position::STARTING_POSITION;
use search::Search;
use uci::*;

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
