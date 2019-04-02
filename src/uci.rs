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
use crate::bench::*;
use crate::position::*;
use crate::search_controller::SearchController;
use crate::time::*;

#[cfg(feature = "tune")]
use crate::tune::*;

use std::io::{self, BufRead};
use std::sync;
use std::thread;

pub struct UCI {
    _main_thread: thread::JoinHandle<()>,
    main_thread_tx: sync::mpsc::Sender<UciCommand>,
    abort: sync::Arc<sync::atomic::AtomicBool>,
}

pub enum UciCommand {
    Unknown(String),
    UciNewGame,
    Uci,
    IsReady,
    SetOption(String, String),
    Position(Position, Vec<String>),
    Go(GoParams),
    Quit,
    Stop,
    Bench,
    Tune(String),
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
        let (main_tx, main_rx) = sync::mpsc::channel();
        let abort = sync::Arc::new(sync::atomic::AtomicBool::new(false));
        UCI {
            abort: sync::Arc::clone(&abort),
            _main_thread: thread::spawn(move || {
                SearchController::new(STARTING_POSITION, abort).looping(main_rx)
            }),
            main_thread_tx: main_tx,
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
                    self.abort.store(true, sync::atomic::Ordering::SeqCst);
                }
                UciCommand::Uci => {
                    self.main_thread_tx.send(UciCommand::Uci).unwrap();
                }
                UciCommand::UciNewGame => {
                    self.main_thread_tx.send(UciCommand::UciNewGame).unwrap();
                }
                UciCommand::Bench => {
                    run_benchmark(12, sync::Arc::clone(&self.abort));
                }
                UciCommand::Tune(filename) => {
                    tune(&filename);
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
            UciCommand::Uci
        } else if line.starts_with("isready") {
            UciCommand::IsReady
        } else if line.starts_with("go") {
            let params = GoParams::from(line);
            UciCommand::Go(params)
        } else if line.starts_with("position") {
            let pos;
            let fen = line.trim_start_matches("position ");
            if fen.starts_with("startpos") {
                pos = STARTING_POSITION;
            } else {
                pos = Position::from(fen.trim_start_matches("fen"));
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
        } else if line.starts_with("tune") {
            UciCommand::Tune(line[5..].to_owned())
        } else if line.starts_with("showmoves") {
            UciCommand::ShowMoves
        } else if line == "d" {
            UciCommand::Debug
        } else if line == "tt" {
            UciCommand::TT
        } else if line.starts_with("history") {
            let mov = line.split_whitespace().nth(1).map(String::from);
            UciCommand::History(mov)
        } else if line.starts_with("perft") {
            let depth = line
                .split_whitespace()
                .nth(1)
                .and_then(|d| d.parse().ok())
                .unwrap_or(6);
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

#[cfg(feature = "tune")]
fn tune(path: &str) {
    let mut traces;
    if path.ends_with(".pgn") {
        traces = pgn_to_positions(path)
            .map(|(r, pos)| CompactTrace::from(Trace::from_position(r, pos)))
            .collect::<Vec<_>>();
    } else if path.ends_with(".fen") {
        traces = fens_to_positions(path)
            .map(|(r, pos)| CompactTrace::from(Trace::from_position(r, pos)))
            .collect::<Vec<_>>();
    } else if path.ends_with(".epd") {
        traces = epd_to_positions(path)
            .map(|(r, pos)| CompactTrace::from(Trace::from_position(r, pos)))
            .collect::<Vec<_>>();
    } else {
        eprintln!("Unsupported format");
        return;
    }
    let mut params = Parameters::default();
    println!("# positions: {:>8}", traces.len());
    params.compute_optimal_k(&traces);
    println!("Optimized K: {:>8.6}", params.k);

    let initial_error = params.total_error(&traces);
    let mut last_printed_error = initial_error;
    let mut best = initial_error;
    println!("Error      : {:>8.6}", last_printed_error);

    let mut f = 1.;

    for i in 1.. {
        shuffle_traces(&mut traces);
        params.step(&traces, f);

        let error = params.total_error(&traces);
        if i % 20 == 0 {
            params.print_weights();
            println!(
                "Error      : {:>8.6}  ({:>8.6}%)  (total {:>8.6}%)",
                error,
                100. * (error - last_printed_error) / last_printed_error,
                100. * (error - initial_error) / initial_error
            );
            last_printed_error = error;
            if error > best {
                f /= 2.;
                if f < 0.000001 {
                    break;
                }
                println!("Decreased learning rate. Now: {:.6}", f);
            }

            best = error;
        }
    }
    println!("Done");
    let error = params.total_error(&traces);
    println!(
        "Total error: {:>8.6}  ({:>8.6}%)",
        error,
        100. * (error - initial_error) / initial_error
    );
}

#[cfg(feature = "tune")]
fn shuffle_traces(traces: &mut [CompactTrace]) {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let n = traces.len();

    for _ in 0..n {
        let a = rng.gen::<usize>() % n;
        let b = rng.gen::<usize>() % n;
        traces.swap(a, b);
    }
}

#[cfg(not(feature = "tune"))]
fn tune(_path: &str) {
    println!("This binary was not compiled with tuning support.");
}
