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

use crate::history::*;
use crate::movegen::*;
use crate::position::*;

pub struct MovePickerAllocations {
    excluded: Vec<Move>,
    moves: Vec<Move>,
    scores: Vec<i64>,
}

impl Default for MovePickerAllocations {
    fn default() -> Self {
        MovePickerAllocations {
            excluded: Vec::with_capacity(8),
            moves: Vec::with_capacity(128),
            scores: Vec::with_capacity(128),
        }
    }
}

pub struct MovePicker<'a> {
    ttmove: Option<Move>,
    position: Position,
    excluded: &'a mut Vec<Move>,
    stage: usize,
    stages: &'a [Stage],
    moves: &'a mut Vec<Move>,
    scores: &'a mut Vec<i64>,
    index: usize,
    killers: [Option<Move>; 2],
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
}

const ALPHA_BETA_STAGES: &[Stage] = &[
    Stage::TTMove,
    Stage::GenerateGoodCaptures,
    Stage::GoodCaptures,
    Stage::GenerateKillers,
    Stage::Killers,
    Stage::GenerateQuietMoves,
    Stage::QuietMoves,
    Stage::GenerateBadCaptures,
    Stage::BadCaptures,
];

const QUIESCENCE_STAGES: &[Stage] = &[
    Stage::GenerateGoodCaptures,
    Stage::GoodCaptures,
];

const QUIESCENCE_CHECK_STAGES: &[Stage] = &[
    Stage::GenerateGoodCaptures,
    Stage::GoodCaptures,
    Stage::GenerateBadCaptures,
    Stage::BadCaptures,
    Stage::GenerateQuietMoves,
    Stage::QuietMoves,
];

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MoveType {
    TTMove,
    GoodCapture,
    Killer,
    Quiet,
    BadCapture,
}

impl<'a> MovePicker<'a> {
    pub fn new(
        position: Position,
        ttmove: Option<Move>,
        killers: [Option<Move>; 2],
        history: Rc<RefCell<History>>,
        allocations: &'a mut MovePickerAllocations,
    ) -> Self {
        allocations.excluded.clear();
        allocations.moves.clear();
        allocations.scores.clear();

        MovePicker {
            history,
            ttmove,
            position,
            excluded: &mut allocations.excluded,
            stage: 0,
            stages: ALPHA_BETA_STAGES,
            moves: &mut allocations.moves,
            scores: &mut allocations.scores,
            index: 0,
            killers,
            skip_quiets: false,
        }
    }

    pub fn qsearch(
        position: Position,
        killers: [Option<Move>; 2],
        history: Rc<RefCell<History>>,
        allocations: &'a mut MovePickerAllocations,
    ) -> Self {
        allocations.excluded.clear();
        allocations.moves.clear();
        allocations.scores.clear();

        MovePicker {
            history,
            ttmove: None,
            position,
            excluded: &mut allocations.excluded,
            stage: 0,
            stages: QUIESCENCE_STAGES,
            moves: &mut allocations.moves,
            scores: &mut allocations.scores,
            index: 0,
            killers,
            skip_quiets: false,
        }
    }

    pub fn qsearch_in_check(
        position: Position,
        killers: [Option<Move>; 2],
        history: Rc<RefCell<History>>,
        allocations: &'a mut MovePickerAllocations,
    ) -> Self {
        allocations.excluded.clear();
        allocations.moves.clear();
        allocations.scores.clear();

        MovePicker {
            history,
            ttmove: None,
            position,
            excluded: &mut allocations.excluded,
            stage: 0,
            stages: QUIESCENCE_CHECK_STAGES,
            moves: &mut allocations.moves,
            scores: &mut allocations.scores,
            index: 0,
            killers,
            skip_quiets: false,
        }
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

impl<'a> Iterator for MovePicker<'a> {
    type Item = (MoveType, Move);

    fn next(&mut self) -> Option<Self::Item> {
        if self.stage >= self.stages.len() {
            return None;
        }

        match self.stages[self.stage] {
            Stage::TTMove => {
                self.stage += 1;
                if let Some(mov) = self.ttmove {
                    self.excluded.push(mov);
                    return Some((MoveType::TTMove, mov));
                }
                self.next()
            }
            Stage::GenerateGoodCaptures => {
                MoveGenerator::from(&self.position)
                    .good_captures(&mut self.moves, &mut self.scores);
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

                self.moves.clear();
                self.scores.clear();
                {
                    let pos = &self.position;
                    self.moves.extend(
                        self.killers
                            .into_iter()
                            .flatten()
                            .filter(|&&m| MoveGenerator::from(pos).is_legal(m)),
                    );
                    self.scores.extend(self.moves.iter().map(|_| 0));
                }
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

                MoveGenerator::from(&self.position).quiet_moves(&mut self.moves);
                {
                    let history = self.history.borrow();
                    let wtm = self.position.white_to_move;
                    self.scores.clear();
                    self.scores
                        .extend(self.moves.iter().map(|&mov| history.get_score(wtm, mov)));
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
                MoveGenerator::from(&self.position).bad_captures(&mut self.moves, &mut self.scores);
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
        }
    }
}
