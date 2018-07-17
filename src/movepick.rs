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
use std::cell::RefCell;
use std::rc::Rc;

use eval::*;
use hash::*;
use movegen::*;
use position::*;
use search::*;
use tt::*;

pub struct MovePicker {
    tt: Rc<RefCell<TT>>,
    hash: Hash,
    position: Position,
    excluded: Vec<Move>,
    stage: usize,
    moves: Vec<Move>,
    scores: Vec<Score>,
    index: usize,
    ply_details: Rc<RefCell<PlyDetails>>,
    history: Rc<RefCell<History>>,
    skip_quiets: bool,
}

#[derive(Clone)]
enum Stage {
    TTMove,
    GenerateGoodCaptures,
    GoodCaptures,
    GenerateKillers,
    Killers,
    GenerateQuietMoves,
    QuietMoves,
    GenerateBadCaptures,
    BadCaptures,
    End,
}

const STAGE_ORDER: &[Stage] = &[
    // 0
    Stage::TTMove,
    Stage::GenerateGoodCaptures,
    Stage::GoodCaptures,
    Stage::GenerateKillers,
    Stage::Killers,
    Stage::GenerateQuietMoves,
    Stage::QuietMoves,
    Stage::GenerateBadCaptures,
    Stage::BadCaptures,
    Stage::End,
    // 10
    Stage::GenerateGoodCaptures,
    Stage::GoodCaptures,
    Stage::End,
    // 13
    Stage::GenerateGoodCaptures,
    Stage::GoodCaptures,
    Stage::GenerateBadCaptures,
    Stage::BadCaptures,
    Stage::GenerateQuietMoves,
    Stage::QuietMoves,
    Stage::End,
];

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MoveType {
    TTMove,
    GoodCapture,
    Killer,
    Quiet,
    BadCapture,
}

impl MovePicker {
    pub fn new(
        position: Position,
        tt: Rc<RefCell<TT>>,
        hash: Hash,
        ply_details: Rc<RefCell<PlyDetails>>,
        history: Rc<RefCell<History>>,
    ) -> Self {
        MovePicker {
            history,
            tt,
            hash,
            position,
            excluded: Vec::new(),
            stage: 0,
            moves: Vec::new(),
            scores: Vec::new(),
            index: 0,
            ply_details,
            skip_quiets: false,
        }
    }

    pub fn qsearch(
        position: Position,
        tt: Rc<RefCell<TT>>,
        hash: Hash,
        ply_details: Rc<RefCell<PlyDetails>>,
        history: Rc<RefCell<History>>,
    ) -> Self {
        MovePicker {
            history,
            tt,
            hash,
            position,
            excluded: Vec::new(),
            stage: 10,
            moves: Vec::new(),
            scores: Vec::new(),
            index: 0,
            ply_details,
            skip_quiets: false,
        }
    }

    pub fn qsearch_in_check(
        position: Position,
        tt: Rc<RefCell<TT>>,
        hash: Hash,
        ply_details: Rc<RefCell<PlyDetails>>,
        history: Rc<RefCell<History>>,
    ) -> Self {
        MovePicker {
            history,
            tt,
            hash,
            position,
            excluded: Vec::new(),
            stage: 13,
            moves: Vec::new(),
            scores: Vec::new(),
            index: 0,
            ply_details,
            skip_quiets: false,
        }
    }

    pub fn has_tt_move(&self) -> bool {
        if let Some(ttentry) = self.tt.borrow_mut().get(self.hash) {
            if let Some(mov) = ttentry.best_move.expand(&self.position) {
                return MoveGenerator::from(&self.position).is_legal(mov);
            }
        }

        false
    }

    pub fn skip_quiets(&mut self, skip_quiets: bool) {
        self.skip_quiets = skip_quiets;
    }

    fn get_move(&mut self) -> Move {
        let mut best_score = self.scores[self.index];
        let mut best_index = self.index;
        for i in self.index + 1..self.moves.len() {
            if self.scores[i] > best_score {
                best_score = self.scores[i];
                best_index = i;
            }
        }

        self.moves.swap(self.index, best_index);
        self.scores.swap(self.index, best_index);
        let mov = self.moves[self.index];
        self.index += 1;
        mov
    }
}

impl Iterator for MovePicker {
    type Item = (MoveType, Move);

    fn next(&mut self) -> Option<Self::Item> {
        match STAGE_ORDER[self.stage] {
            Stage::TTMove => {
                self.stage += 1;
                if let Some(ttentry) = self.tt.borrow_mut().get(self.hash) {
                    if let Some(mov) = ttentry.best_move.expand(&self.position) {
                        if MoveGenerator::from(&self.position).is_legal(mov) {
                            self.excluded.push(mov);
                            return Some((MoveType::TTMove, mov));
                        }
                    }
                }
                self.next()
            }
            Stage::GenerateGoodCaptures => {
                let (moves, scores) = MoveGenerator::from(&self.position).good_captures();
                self.moves = moves;
                self.scores = scores;
                self.index = 0;
                self.stage += 1;
                self.next()
            }
            Stage::GoodCaptures => {
                if self.index < self.moves.len() {
                    let mov = self.get_move();
                    if self.excluded.contains(&mov) {
                        self.next()
                    } else {
                        Some((MoveType::GoodCapture, mov))
                    }
                } else {
                    self.stage += 1;
                    self.next()
                }
            }
            Stage::GenerateKillers => {
                if self.skip_quiets {
                    self.stage += 1;
                    return self.next();
                }

                self.moves = self.ply_details
                    .borrow()
                    .killers_moves
                    .into_iter()
                    .filter(|m| m.is_some())
                    .map(|m| m.unwrap())
                    .filter(|&m| MoveGenerator::from(&self.position).is_legal(m))
                    .collect();
                self.scores = self.moves.iter().map(|_| 0).collect();
                self.index = 0;
                self.stage += 1;
                self.next()
            }
            Stage::Killers => {
                if self.skip_quiets {
                    self.stage += 1;
                    return self.next();
                }

                if self.index < self.moves.len() {
                    let mov = self.get_move();
                    if self.excluded.contains(&mov) {
                        self.next()
                    } else {
                        self.excluded.push(mov);
                        Some((MoveType::Killer, mov))
                    }
                } else {
                    self.stage += 1;
                    self.next()
                }
            }
            Stage::GenerateQuietMoves => {
                if self.skip_quiets {
                    self.stage += 1;
                    return self.next();
                }

                self.moves = MoveGenerator::from(&self.position).quiet_moves();
                {
                    let wtm = self.position.white_to_move as usize;
                    let history = self.history.borrow();
                    self.scores = self.moves
                        .iter()
                        .map(|mov| history[wtm][mov.from.0 as usize][mov.to.0 as usize] as Score)
                        .collect();
                }
                self.index = 0;
                self.stage += 1;
                self.next()
            }
            Stage::QuietMoves => {
                if self.skip_quiets {
                    self.stage += 1;
                    return self.next();
                }

                if self.index < self.moves.len() {
                    let mov = self.get_move();
                    if self.excluded.contains(&mov) {
                        self.next()
                    } else {
                        Some((MoveType::Quiet, mov))
                    }
                } else {
                    self.stage += 1;
                    self.next()
                }
            }
            Stage::GenerateBadCaptures => {
                let (moves, scores) = MoveGenerator::from(&self.position).bad_captures();
                self.moves = moves;
                self.scores = scores;
                self.index = 0;
                self.stage += 1;
                self.next()
            }
            Stage::BadCaptures => {
                if self.index < self.moves.len() {
                    let mov = self.get_move();
                    if self.excluded.contains(&mov) {
                        self.next()
                    } else {
                        Some((MoveType::BadCapture, mov))
                    }
                } else {
                    self.stage += 1;
                    self.next()
                }
            }
            Stage::End => None,
        }
    }
}
