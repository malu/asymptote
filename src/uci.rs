use movegen::*;
use position::*;
use search::*;

use std::io::{self, BufRead};
pub struct UCI {
    search: Search,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct UciGoParams {
    stop_condition: StopCondition,
}

impl<'a> From<&'a str> for UciGoParams {
    fn from(s: &str) -> Self {
        let mut result = UciGoParams {
            stop_condition: StopCondition::Infinite,
        };

        let mut wtime: Option<u64> = None;
        let mut btime: Option<u64> = None;
        let mut winc: Option<u64> = None;
        let mut binc: Option<u64> = None;
        let mut movestogo: Option<u64> = None;

        let mut split = s.split_whitespace();
        while let Some(s) = split.next() {
            if s == "movetime" {
                result.stop_condition = StopCondition::TimePerMove {
                    millis: split.next().unwrap().parse().unwrap(),
                };
                return result;
            } else if s == "infinite" {
                result.stop_condition = StopCondition::Infinite;
                return result;
            } else if s == "nodes" {
                result.stop_condition =
                    StopCondition::Nodes(split.next().unwrap().parse().unwrap());
                return result;
            } else if s == "depth" {
                result.stop_condition =
                    StopCondition::Depth(split.next().unwrap().parse().unwrap());
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

        UciGoParams {
            stop_condition: StopCondition::Variable {
                wtime: wtime.unwrap(),
                btime: btime.unwrap(),
                winc,
                binc,
                movestogo,
            },
        }
    }
}

impl UCI {
    pub fn new() -> UCI {
        initialize_magics();
        UCI {
            search: Search::new(STARTING_POSITION),
        }
    }

    pub fn run(&mut self) {
        let stdin = io::stdin();
        let lock = stdin.lock();
        for line in lock.lines() {
            let line = line.unwrap();
            if line.starts_with("ucinewgame") {
                self.search = Search::new(STARTING_POSITION);
            } else if line.starts_with("uci") {
                self.search = Search::new(STARTING_POSITION);
                println!("id name rChess");
                println!("id author M. Lupke");
                println!("uciok");
            } else if line.starts_with("isready") {
                println!("readyok");
            } else if line.starts_with("go") {
                let go_params = UciGoParams::from(line.as_ref());
                self.search.stop_condition = go_params.stop_condition;
                let mov = self.search.root();
                println!("bestmove {}", mov.to_algebraic());
            } else if line.starts_with("position") {
                if !line.contains("moves") {
                    let fen = line.trim_left_matches("position ");
                    if fen.starts_with("startpos") {
                        self.search.position = STARTING_POSITION;
                        self.search.redo_eval();
                    } else {
                        self.search.position = Position::from(fen);
                        self.search.redo_eval();
                    }
                }

                if let Some(moves) = line.split_terminator("moves ").nth(1) {
                    let mut i = 0;
                    for mov in moves.split_whitespace() {
                        if i < self.search.made_moves.len()
                            && self.search.made_moves[i].to_algebraic() == mov
                        {
                            i += 1;
                            continue;
                        }

                        let mov = Move::from_algebraic(self.search.position, mov);
                        self.search.make_move(mov);
                        i += 1;
                    }

                    /*
                    if i < self.search.made_moves.len() {
                        for _ in i..self.search.made_moves.len() {
                            self.search.unmake_move();
                        }
                    }
                    */
                }
            } else if line.starts_with("quit") {
                return;
            } else if line.starts_with("showboard") {
                self.search.position.print("");
            } else if line.starts_with("showmoves") {
                println!("Pseudo-legal moves");
                for mov in MoveGenerator::from(self.search.position).all_moves() {
                    print!("{} ", mov.to_algebraic());
                }
                println!("\n");

                println!("Legal moves");
                for mov in MoveGenerator::from(self.search.position).all_moves() {
                    self.search.internal_make_move(mov, 0);
                    if self.search.position.move_was_legal(mov) {
                        print!("{} ", mov.to_algebraic());
                    }
                    self.search.internal_unmake_move(mov);
                }
                println!();
            } else if line == "d" {
                self.search.position.print("");
            } else if line == "tt" {
                let tt = self.search
                    .tt
                    .borrow_mut()
                    .get(self.search.made_moves.len(), self.search.hasher.get_hash());
                if let Some(tt) = tt {
                    tt.best_move
                        .expand(self.search.position)
                        .iter()
                        .for_each(|&mov| println!("{:?}", mov.to_algebraic()));
                    println!("{:?}", tt);
                }
            } else if line.starts_with("perft") {
                self.search
                    .perft(line.split_whitespace().nth(1).unwrap().parse().unwrap());
            } else {
                eprintln!("{}", line);
            }
        }
    }
}
