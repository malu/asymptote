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

use std::io::{self, BufRead};
use std::sync;
use std::thread;

pub struct UCI {
    _main_thread: thread::JoinHandle<()>,
    main_thread_tx: sync::mpsc::Sender<UciCommand>,
    stop_tx: sync::mpsc::Sender<()>,
}

pub enum UciCommand {
    Unknown(String),
    UciNewGame(sync::mpsc::Sender<()>, sync::mpsc::Receiver<()>),
    Uci(sync::mpsc::Sender<()>, sync::mpsc::Receiver<()>),
    IsReady,
    SetOption(String, String),
    Position(Position, Vec<String>),
    Go(GoParams),
    Quit,
    Stop,
    Bench,
    ShowMoves,
    Debug,
    TT,
    History(Option<String>),
    Perft(usize),
}

pub struct GoParams {
    pub time_control: TimeControl,
}

impl UCI {
    pub fn new() -> UCI {
        initialize_magics();
        let (main_tx, main_rx) = sync::mpsc::channel();
        let (stop_tx, stop_rx) = sync::mpsc::channel();
        UCI {
            _main_thread: thread::spawn(move || {
                Search::new(STARTING_POSITION, stop_rx).looping(main_rx)
            }),
            main_thread_tx: main_tx,
            stop_tx,
        }
    }

    pub fn run(&mut self) {
        let stdin = io::stdin();
        let lock = stdin.lock();
        for line in lock.lines() {
            let line = line.unwrap();

            let cmd = UciCommand::from(line.as_ref());

            // Some commands are handled here instead of by the search
            match cmd {
                UciCommand::Quit => return,
                UciCommand::Stop => {
                    self.stop_tx.send(()).unwrap();
                }
                UciCommand::Uci(tx, rx) => {
                    self.stop_tx = tx.clone();
                    self.main_thread_tx.send(UciCommand::Uci(tx, rx)).unwrap();
                }
                UciCommand::UciNewGame(tx, rx) => {
                    self.stop_tx = tx.clone();
                    self.main_thread_tx.send(UciCommand::UciNewGame(tx, rx)).unwrap();
                }
                UciCommand::Bench => {
                    let (_, rx) = sync::mpsc::channel();
                    run_benchmark(12, rx);
                }
                cmd => {
                    self.main_thread_tx.send(cmd).unwrap();
                }
            }
        }
    }
}

impl<'a> From<&'a str> for UciCommand {
    fn from(line: &str) -> Self {
        if line.starts_with("ucinewgame") {
            let (tx, rx) = sync::mpsc::channel();
            UciCommand::UciNewGame(tx, rx)
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

            for word in words {
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
            let (tx, rx) = sync::mpsc::channel();
            UciCommand::Uci(tx, rx)
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
        } else if line == "stop" {
            UciCommand::Stop
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
