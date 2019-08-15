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
use crate::history::*;
use crate::movegen::*;
use crate::position::*;

pub struct MovePicker<'a> {
    ttmove: Option<Move>,
    excluded: ShortMoveList,
    stage: usize,
    stages: &'a [Stage],
    moves: MoveList,
    scores: ScoreList,
    bad_moves: MoveList,
    bad_scores: ScoreList,
    index: usize,
    killers: [Option<Move>; 2],
    skip_quiets: bool,
    previous_move: Option<Move>,
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

const QUIESCENCE_STAGES: &[Stage] = &[Stage::GenerateGoodCaptures, Stage::GoodCaptures];

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
        ttmove: Option<Move>,
        killers: [Option<Move>; 2],
        previous_move: Option<Move>,
    ) -> Self {
        MovePicker {
            ttmove,
            excluded: ShortMoveList::new(),
            stage: 0,
            stages: ALPHA_BETA_STAGES,
            moves: MoveList::new(),
            scores: ScoreList::new(),
            bad_moves: MoveList::new(),
            bad_scores: ScoreList::new(),
            index: 0,
            killers,
            skip_quiets: false,
            previous_move,
        }
    }

    pub fn qsearch(position: &Position) -> Self {
        let stages = if position.in_check() {
            QUIESCENCE_CHECK_STAGES
        } else {
            QUIESCENCE_STAGES
        };

        MovePicker {
            ttmove: None,
            excluded: ShortMoveList::new(),
            stage: 0,
            stages,
            moves: MoveList::new(),
            scores: ScoreList::new(),
            bad_moves: MoveList::new(),
            bad_scores: ScoreList::new(),
            index: 0,
            killers: [None; 2],
            skip_quiets: false,
            previous_move: None,
        }
    }

    pub fn add_excluded_move(&mut self, mov: Move) {
        self.excluded.push(mov);
    }

    pub fn skip_quiets(&mut self, skip_quiets: bool) {
        self.skip_quiets = skip_quiets;
    }

    fn get_move(&mut self) -> Option<Move> {
        // Iterator::max_by_key chooses the last maximal element, but we want
        // the first (for no particular reason other than that's what was done
        // before). Hence we reverse the iterator first.
        let (best_index, _) = self
            .scores
            .iter()
            .enumerate()
            .skip(self.index)
            .rev()
            .max_by_key(|(_, &score)| score)?;

        self.moves.swap(self.index, best_index);
        self.scores.swap(self.index, best_index);
        let mov = self.moves[self.index];
        self.index += 1;
        Some(mov)
    }

    pub fn next(&mut self, position: &Position, history: &History) -> Option<(MoveType, Move)> {
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
                self.next(position, history)
            }
            Stage::GenerateGoodCaptures => {
                self.moves.clear();
                self.scores.clear();
                self.bad_moves.clear();
                self.bad_scores.clear();

                MoveGenerator::from(position).good_captures(
                    &mut self.moves,
                    &mut self.scores,
                    &mut self.bad_moves,
                    &mut self.bad_scores,
                );
                self.index = 0;
                self.stage += 1;
                self.next(position, history)
            }
            Stage::GoodCaptures => {
                if let Some(mov) = self.get_move() {
                    if self.excluded.contains(&mov) {
                        self.next(position, history)
                    } else {
                        Some((MoveType::GoodCapture, mov))
                    }
                } else {
                    self.stage += 1;
                    self.next(position, history)
                }
            }
            Stage::GenerateKillers => {
                if self.skip_quiets {
                    self.stage += 1;
                    return self.next(position, history);
                }

                self.moves.clear();
                self.scores.clear();
                {
                    self.moves.extend(
                        self.killers
                            .into_iter()
                            .flatten()
                            .filter(|&&m| position.move_is_pseudo_legal(m))
                            .map(|&m| m),
                    );
                    if let Some(prev_move) = self.previous_move {
                        if prev_move.is_quiet() {
                            self.moves.extend(
                                history.last_best_reply[position.white_to_move as usize]
                                    [prev_move.piece.index()][prev_move.to]
                                    .iter()
                                    .filter(|&&m| position.move_is_pseudo_legal(m))
                                    .map(|&m| m),
                            );
                        }
                    }
                    self.scores.extend(self.moves.iter().map(|_| 0));
                }
                self.index = 0;
                self.stage += 1;
                self.next(position, history)
            }
            Stage::Killers => {
                if self.skip_quiets {
                    self.stage += 1;
                    return self.next(position, history);
                }

                if let Some(mov) = self.get_move() {
                    if self.excluded.contains(&mov) {
                        self.next(position, history)
                    } else {
                        self.excluded.push(mov);
                        Some((MoveType::Killer, mov))
                    }
                } else {
                    self.stage += 1;
                    self.next(position, history)
                }
            }
            Stage::GenerateQuietMoves => {
                if self.skip_quiets {
                    self.stage += 1;
                    return self.next(position, history);
                }

                self.moves.clear();
                self.scores.clear();

                MoveGenerator::from(position).quiet_moves(&mut self.moves);
                {
                    let wtm = position.white_to_move;
                    self.scores
                        .extend(self.moves.iter().map(|&mov| history.get_score(wtm, mov)));
                }
                self.index = 0;
                self.stage += 1;
                self.next(position, history)
            }
            Stage::QuietMoves => {
                if self.skip_quiets {
                    self.stage += 1;
                    return self.next(position, history);
                }

                if let Some(mov) = self.get_move() {
                    if self.excluded.contains(&mov) {
                        self.next(position, history)
                    } else {
                        Some((MoveType::Quiet, mov))
                    }
                } else {
                    self.stage += 1;
                    self.next(position, history)
                }
            }
            Stage::GenerateBadCaptures => {
                std::mem::swap(&mut self.moves, &mut self.bad_moves);
                std::mem::swap(&mut self.scores, &mut self.bad_scores);
                self.index = 0;
                self.stage += 1;
                self.next(position, history)
            }
            Stage::BadCaptures => {
                if let Some(mov) = self.get_move() {
                    if self.excluded.contains(&mov) {
                        self.next(position, history)
                    } else {
                        Some((MoveType::BadCapture, mov))
                    }
                } else {
                    self.stage += 1;
                    self.next(position, history)
                }
            }
        }
    }
}
