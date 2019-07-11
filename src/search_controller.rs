/* Asymptote, a UCI chess engine
   Copyright (C) 2018-2019  Maximilian Lupke

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
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{self, Arc};

use crossbeam::thread;

use crate::hash::Hasher;
use crate::movegen::{Move, MoveGenerator};
use crate::position::{Position, STARTING_POSITION};
use crate::repetitions::Repetitions;
use crate::search::{Search, INC_PLY};
use crate::time::TimeControl;
use crate::tt::{self, TT};
use crate::uci::{GoParams, UciCommand};

#[derive(Copy, Clone, Debug)]
pub struct PersistentOptions {
    hash_bits: u64,
    pub show_pv_board: bool,
    threads: usize,
    pub move_overhead: u64,
}

impl Default for PersistentOptions {
    fn default() -> Self {
        PersistentOptions {
            hash_bits: 14,
            show_pv_board: false,
            threads: 1,
            move_overhead: 10,
        }
    }
}

pub struct SearchController {
    abort: Arc<AtomicBool>,
    node_count: Arc<AtomicUsize>,
    hasher: Hasher,
    options: PersistentOptions,
    position: Position,
    time_control: TimeControl,
    tt: TT,
    repetitions: Repetitions,
}

impl SearchController {
    pub fn new(position: Position, abort: Arc<AtomicBool>) -> SearchController {
        let mut hasher = Hasher::new();
        hasher.from_position(&position);
        SearchController {
            abort,
            node_count: Arc::new(AtomicUsize::new(0)),
            hasher,
            options: PersistentOptions::default(),
            position,
            time_control: TimeControl::Infinite,
            tt: TT::new(14),
            repetitions: Repetitions::new(100),
        }
    }

    pub fn get_best_move(&mut self) -> Move {
        self.node_count.store(0, Ordering::SeqCst);
        self.tt.next_generation();

        let threads = self.options.threads;
        let tt = self.tt.share();

        let mut main_thread = Search::new(
            Arc::clone(&self.abort),
            Arc::clone(&self.node_count),
            self.hasher.clone(),
            self.options,
            self.position.clone(),
            self.time_control,
            &tt,
            self.repetitions.clone(),
        );

        thread::scope(|s| {
            main_thread.prepare_search();

            for id in 1..threads {
                let mut thread = main_thread.clone();
                thread.id = id;
                thread.set_time_control(TimeControl::Infinite);
                s.spawn(move |_| thread.iterative_deepening());
            }

            main_thread.iterative_deepening()
        })
        .unwrap()
    }

    pub fn get_node_count(&self) -> u64 {
        self.node_count.load(Ordering::SeqCst) as u64
    }

    pub fn make_move(&mut self, mov: Move) {
        self.hasher.make_move(&self.position, mov);
        self.position.make_move(mov);

        if self.position.details.halfmove == 0 {
            self.repetitions.irreversible_move();
        }
        self.repetitions.push_position(self.hasher.get_hash());
    }

    pub fn resize_tt(&mut self, bits: u64) {
        self.options.hash_bits = bits;
        self.tt = TT::new(self.options.hash_bits);
    }

    pub fn set_time_control(&mut self, tc: TimeControl) {
        self.time_control = tc;
    }

    pub fn looping(&mut self, main_rx: sync::mpsc::Receiver<UciCommand>) {
        for cmd in main_rx {
            match cmd {
                UciCommand::Unknown(cmd) => eprintln!("Unknown command: {}", cmd),
                UciCommand::UciNewGame => self.handle_ucinewgame(),
                UciCommand::Uci => self.handle_uci(),
                UciCommand::IsReady => self.handle_isready(),
                UciCommand::SetOption(name, value) => self.handle_setoption(name, value),
                UciCommand::Position(pos, moves) => self.handle_position(pos, moves),
                UciCommand::Go(params) => self.handle_go(params),
                UciCommand::ShowMoves => self.handle_showmoves(),
                UciCommand::Debug => self.handle_d(),
                UciCommand::TT => self.handle_tt(),
                UciCommand::Perft(depth) => self.handle_perft(depth),
                _ => eprintln!("Unexpected uci command"),
            }
        }
    }

    fn handle_ucinewgame(&mut self) {
        self.position = STARTING_POSITION;
        self.hasher.from_position(&self.position);
        self.tt = TT::new(self.options.hash_bits);
        self.repetitions = Repetitions::new(100);
        self.repetitions.push_position(self.hasher.get_hash());
    }

    fn handle_uci(&mut self) {
        println!("id name Asymptote 0.6");
        println!("id author Maximilian Lupke");
        println!("option name Hash type spin default 1 min 0 max 2048");
        println!("option name Threads type spin default 1 min 1 max 64");
        println!("option name ShowPVBoard type check default false");
        println!("option name MoveOverhead type spin default 10 min 0");
        self.handle_ucinewgame();
        println!("uciok");
    }

    fn handle_isready(&self) {
        println!("readyok");
    }

    fn handle_go(&mut self, params: GoParams) {
        self.time_control = params.time_control;
        let bestmove = self.get_best_move();
        println!("bestmove {}", bestmove.to_algebraic());
    }

    fn handle_position(&mut self, pos: Position, moves: Vec<String>) {
        self.position = pos;
        self.hasher.from_position(&self.position);

        self.repetitions.clear();
        self.repetitions.push_position(self.hasher.get_hash());

        for mov in &moves {
            let mov = Move::from_algebraic(&self.position, mov);
            self.make_move(mov);
        }
    }

    fn handle_setoption(&mut self, name: String, value: String) {
        match name.as_ref() {
            "hash" => {
                if let Ok(mb) = value.parse::<usize>() {
                    let hash_buckets = 1024 * 1024 * mb / 64; // 64 bytes per hash bucket
                    let power_of_two = (hash_buckets + 1).next_power_of_two() / 2;
                    let bits = power_of_two.trailing_zeros();
                    self.tt = TT::new(u64::from(bits));
                    self.options.hash_bits = u64::from(bits);
                } else {
                    eprintln!("Unable to parse value '{}' as integer", value);
                }
            }
            "threads" => {
                if let Ok(threads) = value.parse::<usize>() {
                    self.options.threads = threads;
                } else {
                    eprintln!("Unable to parse value '{}' as integer", value);
                }
            }
            "showpvboard" => {
                self.options.show_pv_board = value.eq_ignore_ascii_case("true");
            }
            "moveoverhead" => {
                if let Ok(move_overhead) = value.parse::<u64>() {
                    self.options.move_overhead = move_overhead;
                } else {
                    eprintln!("Unable to parse value '{}' as integer", value);
                }
            }
            _ => {
                eprintln!("Unrecognized option {}", name);
            }
        }
    }

    fn handle_showmoves(&mut self) {
        println!("Pseudo-legal moves");
        for mov in MoveGenerator::from(&self.position).all_moves() {
            print!("{} ", mov.to_algebraic());
        }
        println!("\n");

        println!("Legal moves");
        for mov in MoveGenerator::from(&self.position).all_moves() {
            if self.position.move_is_legal(mov) {
                print!("{} ", mov.to_algebraic());
            }
        }
        println!();
    }

    fn handle_d(&self) {
        self.position.print("");
    }

    fn handle_tt(&mut self) {
        println!("Current hash: 0x{:0>8x}", self.hasher.get_hash());
        let tt = self.tt.get(self.hasher.get_hash());
        if let Some(tt) = tt {
            if let Some(best_move) = tt.best_move.expand(&self.position) {
                println!("Best move: {}", best_move.to_algebraic());
                print!("Score:     ");
                if tt.bound == tt::EXACT_BOUND {
                    println!("= {:?}", tt.score);
                } else if tt.bound & tt::LOWER_BOUND > 0 {
                    println!("> {:?}", tt.score);
                } else {
                    println!("< {:?}", tt.score);
                }
                println!("Depth:     {} ({} plies)", tt.depth, tt.depth / INC_PLY);
            } else {
                println!("No TT entry.");
            }
        } else {
            println!("No TT entry.");
        }
    }

    fn handle_perft(&mut self, depth: usize) {
        let tt = self.tt.share();
        let mut thread = Search::new(
            Arc::clone(&self.abort),
            Arc::clone(&self.node_count),
            self.hasher.clone(),
            self.options,
            self.position.clone(),
            self.time_control,
            &tt,
            self.repetitions.clone(),
        );
        thread.perft(depth);
    }
}
