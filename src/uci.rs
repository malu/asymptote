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
use crate::bench::*;
use crate::movegen::*;
use crate::position::*;
use crate::search::*;
use crate::time::*;
use crate::tt::*;

use std::io::{self, BufRead};

pub struct UCI {
    search: Search,
    options: PersistentOptions,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum UciCommand {
    Unknown(String),
    UciNewGame,
    Uci,
    IsReady,
    SetOption(String, String),
    Position(Position, Vec<String>),
    Go(GoParams),
    Quit,
    Bench,
    ShowMoves,
    Debug,
    TT,
    History(Option<String>),
    Perft(usize),
}

struct PersistentOptions {
    hash_bits: u64,
}

impl Default for PersistentOptions {
    fn default() -> Self {
        PersistentOptions { hash_bits: 14 }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct GoParams {
    time_control: TimeControl,
}

impl UCI {
    pub fn new() -> UCI {
        initialize_magics();
        UCI {
            search: Search::new(STARTING_POSITION),
            options: PersistentOptions::default(),
        }
    }

    pub fn run(&mut self) {
        let stdin = io::stdin();
        let lock = stdin.lock();
        for line in lock.lines() {
            let line = line.unwrap();

            let cmd = UciCommand::from(line.as_ref());
            if cmd == UciCommand::Quit {
                return;
            }

            self.handle_uci_cmd(cmd);
        }
    }

    fn handle_uci_cmd(&mut self, cmd: UciCommand) {
        match cmd {
            UciCommand::Unknown(cmd) => eprintln!("Unknown command: {}", cmd),
            UciCommand::UciNewGame => self.handle_ucinewgame(),
            UciCommand::Uci => self.handle_uci(),
            UciCommand::IsReady => self.handle_isready(),
            UciCommand::SetOption(name, value) => self.handle_setoption(name, value),
            UciCommand::Position(pos, moves) => self.handle_position(pos, moves),
            UciCommand::Go(params) => self.handle_go(params),
            UciCommand::Quit => {}
            UciCommand::Bench => self.handle_bench(),
            UciCommand::ShowMoves => self.handle_showmoves(),
            UciCommand::Debug => self.handle_d(),
            UciCommand::TT => self.handle_tt(),
            UciCommand::History(mov) => self.handle_history(mov),
            UciCommand::Perft(depth) => self.handle_perft(depth),
        }
    }

    fn handle_ucinewgame(&mut self) {
        self.search = Search::new(STARTING_POSITION);
        self.search.resize_tt(self.options.hash_bits);
    }

    fn handle_uci(&mut self) {
        self.search = Search::new(STARTING_POSITION);
        println!("id name Asymptote v0.4.2");
        println!("id author Maximilian Lupke");
        println!("option name Hash type spin default 1 min 0 max 2048");
        println!("option name ShowPVBoard type check default false");
        println!("uciok");
    }

    fn handle_isready(&self) {
        println!("readyok");
    }

    fn handle_go(&mut self, params: GoParams) {
        self.search.time_control = params.time_control;
        let mov = self.search.root();
        println!("bestmove {}", mov.to_algebraic());
    }

    fn handle_position(&mut self, pos: Position, moves: Vec<String>) {
        self.search.position = pos;
        self.search.redo_eval();

        for (i, mov) in moves.iter().enumerate() {
            if i < self.search.made_moves.len()
                && mov == self.search.made_moves[i].to_algebraic().as_str()
            {
                continue;
            }

            let mov = Move::from_algebraic(&self.search.position, &mov);
            self.search.make_move(mov);
        }
    }

    fn handle_setoption(&mut self, name: String, value: String) {
        match name.as_ref() {
            "hash" => {
                if let Ok(mb) = value.parse::<usize>() {
                    let hash_buckets = 1024 * 1024 * mb / 64; // 64 bytes per hash bucket
                    let power_of_two = (hash_buckets + 1).next_power_of_two() / 2;
                    let bits = power_of_two.trailing_zeros();
                    self.search.resize_tt(u64::from(bits));
                    self.options.hash_bits = u64::from(bits);
                } else {
                    eprintln!("Unable to parse value '{}' as integer", value);
                }
            }
            "showpvboard" => {
                self.search.show_pv_board = value.eq_ignore_ascii_case("true");
            }
            _ => {
                eprintln!("Unrecognized option {}", name);
            }
        }
    }

    fn handle_bench(&self) {
        run_benchmark(12);
    }

    fn handle_showmoves(&mut self) {
        println!("Pseudo-legal moves");
        for mov in MoveGenerator::from(&self.search.position).all_moves() {
            print!("{} ", mov.to_algebraic());
        }
        println!("\n");

        println!("Legal moves");
        for mov in MoveGenerator::from(&self.search.position).all_moves() {
            self.search.internal_make_move(mov, 0);
            if self.search.position.move_was_legal(mov) {
                print!("{} ", mov.to_algebraic());
            }
            self.search.internal_unmake_move(mov, 0);
        }
        println!();
    }

    fn handle_d(&self) {
        self.search.position.print("");
    }

    fn handle_tt(&mut self) {
        println!("Current hash: 0x{:0>64x}", self.search.hasher.get_hash());
        let tt = self
            .search
            .tt
            .borrow_mut()
            .get(self.search.hasher.get_hash());
        if let Some(tt) = tt {
            if let Some(best_move) = tt.best_move.expand(&self.search.position) {
                println!("Best move: {}", best_move.to_algebraic());
                print!("Score:     ");
                if tt.bound == EXACT_BOUND {
                    println!("= {:?}", tt.score);
                } else if tt.bound & LOWER_BOUND > 0 {
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

    fn handle_history(&self, mov: Option<String>) {
        match mov.map(|m| Move::from_algebraic(&self.search.position, &m)) {
            Some(mov) => {
                let score = self
                    .search
                    .history
                    .borrow()
                    .get_score(self.search.position.white_to_move, mov);
                println!("History score: {}", score);
            }
            None => {
                let mg = MoveGenerator::from(&self.search.position);
                let history = self.search.history.borrow();
                let mut moves = Vec::new();
                mg.quiet_moves(&mut moves);
                let mut moves = moves
                    .into_iter()
                    .map(|mov| {
                        (
                            mov,
                            history.get_score(self.search.position.white_to_move, mov),
                        )
                    })
                    .collect::<Vec<_>>();
                moves.sort_by_key(|(_, hist)| -hist);
                for (mov, hist) in moves {
                    println!("{} {:>8}", mov.to_algebraic(), hist);
                }
            }
        }
    }

    fn handle_perft(&mut self, depth: usize) {
        self.search.perft(depth);
    }
}

impl<'a> From<&'a str> for UciCommand {
    fn from(line: &str) -> Self {
        if line.starts_with("ucinewgame") {
            UciCommand::UciNewGame
        } else if line.starts_with("setoption") {
            let mut words = line.split_whitespace();
            assert!(words.next() == Some("setoption"));
            assert!(words.next() == Some("name"));
            let mut name_parts = Vec::new();
            let mut value_parts = Vec::new();

            // parse option name
            while let Some(word) = words.next() {
                if word == "value" {
                    break;
                } else {
                    name_parts.push(word);
                }
            }

            while let Some(word) = words.next() {
                value_parts.push(word);
            }

            let mut name = name_parts
                .into_iter()
                .fold(String::new(), |name, part| name + part);
            name.make_ascii_lowercase();
            let value = value_parts
                .into_iter()
                .fold(String::new(), |name, part| name + part);

            UciCommand::SetOption(name, value)
        } else if line.starts_with("uci") {
            UciCommand::Uci
        } else if line.starts_with("isready") {
            UciCommand::IsReady
        } else if line.starts_with("go") {
            let params = GoParams::from(line);
            UciCommand::Go(params)
        } else if line.starts_with("position") {
            let pos;
            let fen = line.trim_left_matches("position ");
            if fen.starts_with("startpos") {
                pos = STARTING_POSITION;
            } else {
                pos = Position::from(fen.trim_left_matches("fen"));
            }

            let mut moves = Vec::new();
            if line.contains("moves") {
                if let Some(moves_) = line.split_terminator("moves ").nth(1) {
                    for mov in moves_.split_whitespace() {
                        moves.push(String::from(mov));
                    }
                }
            }
            UciCommand::Position(pos, moves)
        } else if line.starts_with("quit") {
            UciCommand::Quit
        } else if line.starts_with("bench") {
            UciCommand::Bench
        } else if line.starts_with("showmoves") {
            UciCommand::ShowMoves
        } else if line == "d" {
            UciCommand::Debug
        } else if line == "tt" {
            UciCommand::TT
        } else if line.starts_with("history") {
            let mov = line
                .split_whitespace()
                .nth(1).map(String::from);
            UciCommand::History(mov)
        } else if line.starts_with("perft") {
            let depth = line.split_whitespace().nth(1).and_then(|d| d.parse().ok()).unwrap_or(6);
            UciCommand::Perft(depth)
        } else {
            UciCommand::Unknown(line.to_owned())
        }
    }
}

impl<'a> From<&'a str> for GoParams {
    fn from(s: &str) -> Self {
        let mut result = GoParams {
            time_control: TimeControl::Infinite,
        };

        let mut wtime: Option<u64> = None;
        let mut btime: Option<u64> = None;
        let mut winc: Option<u64> = None;
        let mut binc: Option<u64> = None;
        let mut movestogo: Option<u64> = None;

        let mut split = s.split_whitespace();
        while let Some(s) = split.next() {
            if s == "movetime" {
                result.time_control =
                    TimeControl::FixedMillis(split.next().unwrap().parse().unwrap());
                return result;
            } else if s == "infinite" {
                result.time_control = TimeControl::Infinite;
                return result;
            } else if s == "nodes" {
                result.time_control =
                    TimeControl::FixedNodes(split.next().unwrap().parse().unwrap());
                return result;
            } else if s == "depth" {
                result.time_control =
                    TimeControl::FixedDepth(split.next().unwrap().parse().unwrap());
                return result;
            } else if s == "wtime" {
                wtime = split.next().unwrap().parse().ok();
            } else if s == "btime" {
                btime = split.next().unwrap().parse().ok();
            } else if s == "winc" {
                winc = split.next().unwrap().parse().ok();
            } else if s == "binc" {
                binc = split.next().unwrap().parse().ok();
            } else if s == "movestogo" {
                movestogo = split.next().unwrap().parse().ok();
            }
        }

        GoParams {
            time_control: TimeControl::Variable {
                wtime: wtime.unwrap(),
                btime: btime.unwrap(),
                winc,
                binc,
                movestogo,
            },
        }
    }
}
