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
    match args.next() {
        Some(cmd) => match cmd.as_ref() {
            "perft" => {
                let mut search = Search::new(STARTING_POSITION);
                let i = match args.next() {
                    Some(n) => n.parse().unwrap(),
                    _ => 4,
                };
                search.perft(i);
            }
            "go" => {
                let mut search = Search::new(STARTING_POSITION);
                println!("bestmove {}", search.root().to_algebraic());
            }
            _ => {}
        },
        None => {
            let mut uci = UCI::new();
            uci.run();
        }
    }
}
