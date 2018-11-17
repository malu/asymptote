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
use std::time;

use eval::*;
use hash::*;
use history::*;
use movegen::*;
use movepick::*;
use position::*;
use repetitions::Repetitions;
use tt::*;

pub type Ply = i16;
pub type Depth = i16;

pub const INC_PLY: Depth = 64;
pub const MAX_PLY: Ply = 128 - 1;

const FUTILITY_POSITIONAL_MARGIN: Score = 300;

const LMR_MAX_DEPTH: Depth = 8 * INC_PLY;
const LMR_MOVES: [usize; (LMR_MAX_DEPTH / INC_PLY) as usize + 1] = [255, 255, 3, 5, 5, 7, 7, 9, 9];

pub struct Search {
    stack: Vec<Rc<RefCell<PlyDetails>>>,
    history: Rc<RefCell<History>>,
    pub position: Position,
    eval: Eval,
    started_at: time::Instant,
    pub stop_condition: StopCondition,
    pub hasher: Hasher,
    visited_nodes: u64,
    pub tt: Rc<RefCell<TT>>,
    pv: Vec<Vec<Option<Move>>>,
    max_ply_searched: Ply,
    repetitions: Repetitions,
    pub made_moves: Vec<Move>,
    searching_for_white: bool,
    pub show_pv_board: bool,

    mp_excluded: Vec<Rc<RefCell<Vec<Move>>>>,
    mp_moves: Vec<Rc<RefCell<Vec<Move>>>>,
    mp_scores: Vec<Rc<RefCell<Vec<Score>>>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum StopCondition {
    Infinite,
    TimePerMove {
        millis: u64,
    },
    Depth(Depth),
    Nodes(u64),
    Variable {
        wtime: u64,
        btime: u64,
        winc: Option<u64>,
        binc: Option<u64>,
        movestogo: Option<u64>,
    },
}

#[derive(Copy, Clone, Debug, Default)]
pub struct PlyDetails {
    irreversible_details: IrreversibleDetails,
    current_move: Option<Move>,
    pub killers_moves: [Option<Move>; 2],
}

impl Search {
    pub fn new(position: Position) -> Self {
        let mut pv = Vec::with_capacity(MAX_PLY as usize);
        let mut stack = Vec::with_capacity(MAX_PLY as usize);
        let mut mp_excluded = Vec::with_capacity(MAX_PLY as usize);
        let mut mp_moves = Vec::with_capacity(MAX_PLY as usize);
        let mut mp_scores = Vec::with_capacity(MAX_PLY as usize);
        for i in 0..MAX_PLY as usize {
            pv.push(vec![None; MAX_PLY as usize - i + 1]);
            stack.push(Rc::new(RefCell::new(PlyDetails::default())));
            mp_excluded.push(Rc::new(RefCell::new(Vec::with_capacity(8))));
            mp_moves.push(Rc::new(RefCell::new(Vec::with_capacity(128))));
            mp_scores.push(Rc::new(RefCell::new(Vec::with_capacity(128))));
        }

        Search {
            history: Rc::new(RefCell::new(History::default())),
            stack,
            eval: Eval::from(&position),
            position,
            started_at: time::Instant::now(),
            stop_condition: StopCondition::Infinite,
            hasher: Hasher::new(),
            tt: Rc::new(RefCell::new(TT::new(14))),
            visited_nodes: 0,
            pv,
            max_ply_searched: 0,
            repetitions: Repetitions::new(100),
            made_moves: Vec::new(),
            searching_for_white: true,
            show_pv_board: false,

            mp_excluded,
            mp_moves,
            mp_scores,
        }
    }

    pub fn root(&mut self) -> Move {
        self.searching_for_white = self.position.white_to_move;
        self.started_at = time::Instant::now();
        self.visited_nodes = 0;
        self.tt.borrow_mut().next_generation();
        self.pv
            .iter_mut()
            .for_each(|pv| pv.iter_mut().for_each(|i| *i = None));

        {
            let mut history = self.history.borrow_mut();
            *history = History::default();
        }

        let mut moves = MoveGenerator::from(&self.position)
            .all_moves()
            .into_iter()
            .map(|mov| (mov, 0))
            .collect::<Vec<_>>();

        let mut last_score = 0;
        if let Some(ttentry) = self.tt.borrow_mut().get(self.hasher.get_hash()) {
            let mut swap_with = 0;
            let ttmove = ttentry.best_move.expand(&self.position);
            for (i, &mov) in moves.iter().enumerate() {
                if Some(mov.0) == ttmove {
                    if ttentry.bound == EXACT_BOUND {
                        last_score = ttentry.score.to_score(0);
                    }
                    swap_with = i;
                    break;
                }
            }

            moves.swap(0, swap_with);
        }

        let mut alpha = -Score::max_value();
        let mut beta;
        let mut max_depth = 0;
        'deepening: for d in 1_i16.. {
            if !self.start_another_iteration(d) {
                break;
            }
            let depth = d * INC_PLY;
            max_depth = ::std::cmp::max(max_depth, depth);

            self.max_ply_searched = 0;
            let mut increased_alpha = false;
            let mut best_move_index;
            let mut num_moves;

            let mut delta = 50;
            alpha = ::std::cmp::max(last_score - delta, -MATE_SCORE);
            beta = ::std::cmp::min(last_score + delta, MATE_SCORE);
            'aspiration: loop {
                let mut best_score = -Score::max_value();
                best_move_index = 0;
                num_moves = 0;
                let mut beta_cutoff = false;
                'try_moves: for (i, &mut (mov, ref mut subtree_size)) in moves.iter_mut().enumerate() {
                    self.internal_make_move(mov, 0);
                    if !self.position.move_was_legal(mov) {
                        self.internal_unmake_move(mov, 0);
                        continue;
                    }

                    num_moves += 1;

                    let mut new_depth = depth - INC_PLY;
                    if self.position.in_check() {
                        new_depth += INC_PLY;
                    }

                    let num_nodes_before = self.visited_nodes;
                    let value;
                    if num_moves == 1 {
                        value = self.search_pv(1, -beta, -alpha, new_depth).map(|v| -v);
                    } else {
                        let value_zw = self.search_zw(1, -alpha, new_depth).map(|v| -v);
                        if value_zw.is_some() && value_zw.unwrap() > alpha {
                            value = self.search_pv(1, -beta, -alpha, new_depth).map(|v| -v);
                        } else {
                            value = value_zw;
                        }
                    }

                    *subtree_size = ((self.visited_nodes - num_nodes_before) / 2) as i64;

                    self.internal_unmake_move(mov, 0);

                    match value {
                        None => {
                            if increased_alpha {
                                break 'try_moves;
                            } else {
                                break 'deepening;
                            }
                        }
                        Some(value) => {
                            if value > best_score {
                                best_score = value;
                                if value > alpha {
                                    increased_alpha = true;
                                    alpha = value;
                                    best_move_index = i;

                                    if value >= beta {
                                        beta_cutoff = true;
                                        delta += delta / 2;
                                        beta = ::std::cmp::min(beta + delta, MATE_SCORE);
                                        break 'try_moves;
                                    }

                                    self.add_pv_move(mov, 0);

                                    if depth > 5 * INC_PLY {
                                        self.uci_info(depth, alpha);
                                    }
                                }
                            }
                        }
                    }
                }

                if best_move_index > 0 {
                    let best_move = moves[best_move_index];
                    moves.insert(0, best_move);
                    moves.remove(best_move_index + 1);
                }

                if !increased_alpha {
                    delta += delta / 2;
                    alpha = ::std::cmp::max(best_score - delta, -MATE_SCORE);
                } else if beta_cutoff {
                    continue 'aspiration;
                } else {
                    last_score = alpha;
                    break 'aspiration;
                }
            }

            let num_pseudo_legal_moves = moves.len();
            moves[1..num_pseudo_legal_moves].sort_by_key(|&(_, subtree_size)| -subtree_size);
            self.uci_info(depth, alpha);

            if num_moves == 1 {
                return moves[0].0;
            }
        }

        self.tt.borrow_mut().insert(
            self.hasher.get_hash(),
            max_depth,
            TTScore::from_score(alpha, 0),
            moves[0].0,
            EXACT_BOUND,
        );

        moves[0].0
    }

    pub fn search_pv(
        &mut self,
        ply: Ply,
        alpha: Score,
        beta: Score,
        depth: Depth,
    ) -> Option<Score> {
        if self.should_stop() {
            return None;
        }

        self.visited_nodes += 1;
        self.max_ply_searched = ::std::cmp::max(ply, self.max_ply_searched);

        // Check if there is a draw by insufficient mating material or threefold repetition.
        if self.is_draw(ply) {
            return Some(0);
        }

        // Check if the fifty moves rule applies and if so, return the apropriate score.
        if let Some(score) = self.fifty_moves_rule(ply) {
            return Some(score);
        }

        if ply == MAX_PLY {
            return Some(self.eval.score(&self.position, self.hasher.get_pawn_hash()));
        }

        if MATE_SCORE - ply < alpha {
            return Some(alpha);
        }

        if -MATE_SCORE + ply > beta {
            return Some(beta);
        }

        if depth < INC_PLY {
            self.visited_nodes -= 1;
            return self.qsearch(ply, alpha, beta, depth);
        }

        if depth >= 4 * INC_PLY && !self.has_tt_move() {
            self.search_pv(ply, alpha, beta, depth - 2 * INC_PLY);
            self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
        }

        let mp_excluded = Rc::clone(&self.mp_excluded[ply as usize]);
        let mut mp_excluded = mp_excluded.borrow_mut();
        let mp_moves = Rc::clone(&self.mp_moves[ply as usize]);
        let mut mp_moves = mp_moves.borrow_mut();
        let mp_scores = Rc::clone(&self.mp_scores[ply as usize]);
        let mut mp_scores = mp_scores.borrow_mut();
        let moves = MovePicker::new(
            self.position.clone(),
            Rc::clone(&self.tt),
            self.hasher.get_hash(),
            Rc::clone(&self.stack[ply as usize]),
            Rc::clone(&self.history),
            &mut mp_excluded,
            &mut mp_moves,
            &mut mp_scores,
        );

        let mut alpha = alpha;
        let mut increased_alpha = false;

        let previous_move = self.stack[ply as usize - 1].borrow().current_move;
        let mut best_move = None;
        let mut best_score = -Score::max_value();

        let mut num_moves = 0;
        for (mtype, mov) in moves {
            self.internal_make_move(mov, ply);
            if !self.position.move_was_legal(mov) {
                self.internal_unmake_move(mov, ply);
                continue;
            }

            num_moves += 1;

            let mut extension = 0;
            if mtype == MoveType::TTMove && ply < 80 {
                extension += INC_PLY / 4;
            }

            let check = self.position.in_check();
            if check {
                extension += INC_PLY;
            }

            if let Some(previous_move) = previous_move {
                if previous_move.to == mov.to {
                    extension += INC_PLY;
                }
            }

            let mut new_depth = depth - INC_PLY + extension;

            let mut value;
            if num_moves == 1 {
                value = self.search_pv(ply + 1, -beta, -alpha, new_depth)
                    .map(|v| -v);
            } else {
                value = self.search_zw(ply + 1, -alpha, new_depth).map(|v| -v);
                if value.is_some() && value.unwrap() > alpha {
                    value = self.search_pv(ply + 1, -beta, -alpha, new_depth)
                        .map(|v| -v);
                }
            }
            self.internal_unmake_move(mov, ply);

            match value {
                None => {
                    if increased_alpha {
                        self.tt.borrow_mut().insert(
                            self.hasher.get_hash(),
                            depth,
                            TTScore::from_score(alpha, ply),
                            best_move.unwrap(),
                            LOWER_BOUND,
                        );
                    }

                    self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
                    return None;
                }
                Some(value) => {
                    if value > best_score {
                        best_score = value;
                        best_move = Some(mov);
                        if value > alpha {
                            increased_alpha = true;
                            alpha = value;
                            self.add_pv_move(mov, ply);
                            if value >= beta {
                                if mov.is_quiet() {
                                    self.update_quiet_stats(mov, ply, depth);
                                }

                                self.tt.borrow_mut().insert(
                                    self.hasher.get_hash(),
                                    depth,
                                    TTScore::from_score(value, ply),
                                    mov,
                                    LOWER_BOUND,
                                );
                                self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
                                return Some(value);
                            }
                        }
                    }
                }
            }
        }

        if num_moves == 0 {
            if self.position.in_check() {
                return Some(-MATE_SCORE + ply);
            } else {
                return Some(0);
            }
        }

        let tt_bound = if increased_alpha {
            EXACT_BOUND
        } else {
            UPPER_BOUND
        };
        self.tt.borrow_mut().insert(
            self.hasher.get_hash(),
            depth,
            TTScore::from_score(best_score, ply),
            best_move.unwrap(),
            tt_bound,
        );

        Some(best_score)
    }

    pub fn search_zw(&mut self, ply: Ply, beta: Score, depth: Depth) -> Option<Score> {
        if self.should_stop() {
            return None;
        }

        // Check if there is a draw by insufficient mating material or threefold repetition.
        if self.is_draw(ply) {
            return Some(0);
        }

        // Check if the fifty moves rule applies and if so, return the apropriate score.
        if let Some(score) = self.fifty_moves_rule(ply) {
            return Some(score);
        }

        if ply == MAX_PLY {
            return Some(self.eval.score(&self.position, self.hasher.get_pawn_hash()));
        }

        let alpha = beta - 1;
        self.visited_nodes += 1;
        self.max_ply_searched = ::std::cmp::max(ply, self.max_ply_searched);

        let in_check = self.position.in_check();

        if let Some(ttentry) = self.tt.borrow_mut().get(self.hasher.get_hash()) {
            let check_move_legality = |mov| MoveGenerator::from(&self.position).is_legal(mov);
            if ttentry.depth >= depth
                && ttentry
                    .best_move
                    .expand(&self.position)
                    .map_or(false, check_move_legality)
            {
                let score = ttentry.score.to_score(ply);

                if score >= beta && ttentry.bound & LOWER_BOUND > 0 {
                    return Some(score);
                }

                if score <= alpha && ttentry.bound & UPPER_BOUND > 0 {
                    return Some(score);
                }

                if ttentry.bound & EXACT_BOUND == EXACT_BOUND {
                    return Some(score);
                }
            }
        }

        if depth < INC_PLY {
            self.visited_nodes -= 1;
            return self.qsearch(ply, alpha, beta, depth);
        }

        let eval = self.eval.score(&self.position, self.hasher.get_pawn_hash());
        // Nullmove
        if !in_check && self.eval.material.non_pawn_material() > 0 && eval >= beta {
            let r = 2;
            self.internal_make_nullmove(ply);
            let score = self.search_zw(ply + 1, -alpha, depth - INC_PLY - r * INC_PLY)
                .map(|v| -v);
            self.internal_unmake_nullmove(ply);
            match score {
                None => return None,
                Some(score) => {
                    if score >= beta {
                        return Some(beta);
                    }
                }
            }
        }

        let mut best_score = -Score::max_value();
        let mut best_move = None;

        let mut pruned;

        // If the futility limit is positive, a move has to gain material or else it gets pruned.
        // Therefore we can skip all quiet moves since they don't change material.
        let futility_skip_quiets = !in_check && depth < 3*INC_PLY && alpha > -MATE_SCORE + MAX_PLY && alpha > eval + FUTILITY_POSITIONAL_MARGIN;

        let previous_move = self.stack[ply as usize - 1].borrow().current_move;
        let nullmove_reply = previous_move == None;

        // Internal deepening
        if depth >= 6 * INC_PLY && !self.has_tt_move() {
            self.search_zw(ply, beta, depth / 2);
        }

        let mp_excluded = Rc::clone(&self.mp_excluded[ply as usize]);
        let mut mp_excluded = mp_excluded.borrow_mut();
        let mp_moves = Rc::clone(&self.mp_moves[ply as usize]);
        let mut mp_moves = mp_moves.borrow_mut();
        let mp_scores = Rc::clone(&self.mp_scores[ply as usize]);
        let mut mp_scores = mp_scores.borrow_mut();

        let mut moves = MovePicker::new(
            self.position.clone(),
            Rc::clone(&self.tt),
            self.hasher.get_hash(),
            Rc::clone(&self.stack[ply as usize]),
            Rc::clone(&self.history),
            &mut mp_excluded,
            &mut mp_moves,
            &mut mp_scores,
        );
        moves.skip_quiets(futility_skip_quiets);
        pruned = futility_skip_quiets;

        let mut num_moves = 0;
        for (mtype, mov) in moves {
            self.internal_make_move(mov, ply);
            if !self.position.move_was_legal(mov) {
                self.internal_unmake_move(mov, ply);
                continue;
            }

            let check = self.position.in_check();

            // Futility pruning
            if !check {
                let capture_value = mov.captured.map_or(0, Piece::value);
                let promotion_value = mov.promoted
                    .map_or(0, |piece| piece.value() - Piece::Pawn.value());

                if eval + 200*((depth / INC_PLY + 1) / 2) + capture_value + promotion_value < alpha {
                    pruned = true;
                    self.internal_unmake_move(mov, ply);
                    continue;
                }
            }

            num_moves += 1;

            let mut extension = 0;
            let mut reduction = 0;

            if check {
                extension += INC_PLY;
            }

            if let Some(previous_move) = previous_move {
                if previous_move.to == mov.to {
                    extension += INC_PLY / 2;
                }
            }

            // Reduce quiet responses to a null move one ply. They are unlikely to produce a
            // cutoff.
            if nullmove_reply && mtype == MoveType::Quiet {
                reduction += INC_PLY;
            }

            if extension <= 0 {
                if depth <= LMR_MAX_DEPTH
                    && num_moves > LMR_MOVES[(depth / INC_PLY) as usize]
                    && mtype != MoveType::GoodCapture
                    && mtype != MoveType::Killer
                    && !check
                    && !in_check
                {
                    reduction += INC_PLY;
                }
            }

            let mut new_depth = depth - INC_PLY + extension - reduction;

            let mut value = self.search_zw(ply + 1, -alpha, new_depth).map(|v| -v);
            let mut research = false;
            if value.is_some() {
                if value.unwrap() > alpha && reduction > 0 {
                    new_depth += reduction;
                    research = true;
                }

                if research {
                    value = self.search_zw(ply + 1, -alpha, new_depth).map(|v| -v);
                }
            }

            self.internal_unmake_move(mov, ply);

            if value == None {
                return None;
            }

            if value.unwrap() > best_score {
                best_score = value.unwrap();
                best_move = Some(mov);
                if value.unwrap() >= beta {
                    if mov.is_quiet() {
                        self.update_quiet_stats(mov, ply, depth);
                    }

                    self.tt.borrow_mut().insert(
                        self.hasher.get_hash(),
                        depth,
                        TTScore::from_score(best_score, ply),
                        mov,
                        LOWER_BOUND,
                    );
                    return value;
                }
            }
        }

        if num_moves == 0 {
            if pruned {
                return Some(alpha);
            } else if self.position.in_check() {
                return Some(-MATE_SCORE + ply);
            } else {
                return Some(0);
            }
        }

        self.tt.borrow_mut().insert(
            self.hasher.get_hash(),
            depth,
            TTScore::from_score(best_score, ply),
            best_move.unwrap(),
            UPPER_BOUND,
        );
        Some(best_score)
    }

    pub fn qsearch(&mut self, ply: Ply, alpha: Score, beta: Score, depth: Depth) -> Option<Score> {
        if self.should_stop() {
            return None;
        }

        self.visited_nodes += 1;

        let in_check = self.position.in_check();
        let mut depth = depth;

        if in_check {
            depth += INC_PLY;
        }

        let mut alpha = alpha;
        if !in_check {
            let eval = self.eval.score(&self.position, self.hasher.get_pawn_hash());
            if eval >= beta {
                return Some(eval);
            }

            if alpha < eval {
                alpha = eval;
            }
        }

        let mp_excluded = Rc::clone(&self.mp_excluded[ply as usize]);
        let mut mp_excluded = mp_excluded.borrow_mut();
        let mp_moves = Rc::clone(&self.mp_moves[ply as usize]);
        let mut mp_moves = mp_moves.borrow_mut();
        let mp_scores = Rc::clone(&self.mp_scores[ply as usize]);
        let mut mp_scores = mp_scores.borrow_mut();

        let moves = if in_check {
            MovePicker::qsearch_in_check(
                self.position.clone(),
                Rc::clone(&self.tt),
                self.hasher.get_hash(),
                Rc::clone(&self.stack[ply as usize]),
                Rc::clone(&self.history),
                &mut mp_excluded,
                &mut mp_moves,
                &mut mp_scores,
            )
        } else {
            MovePicker::qsearch(
                self.position.clone(),
                Rc::clone(&self.tt),
                self.hasher.get_hash(),
                Rc::clone(&self.stack[ply as usize]),
                Rc::clone(&self.history),
                &mut mp_excluded,
                &mut mp_moves,
                &mut mp_scores,
            )
        };

        let mut num_moves = 0;
        for (_mtype, mov) in moves {
            self.internal_make_move(mov, ply);
            if !self.position.move_was_legal(mov) {
                self.internal_unmake_move(mov, ply);
                continue;
            }
            num_moves += 1;

            let value = self.qsearch(ply + 1, -beta, -alpha, depth - INC_PLY)
                .map(|v| -v);
            self.internal_unmake_move(mov, ply);

            match value {
                None => return None,
                Some(score) => {
                    if score > alpha {
                        alpha = score;
                        if score >= beta {
                            return value;
                        }
                    }
                }
            }
        }

        if num_moves == 0 && in_check {
            return Some(-MATE_SCORE + ply);
        }

        Some(alpha)
    }

    /// Checks for draws by fifty moves rule.
    ///
    /// Returns `None` if the halfmove clock did not reach move 100 yet.
    /// Returns the mate score for `ply` if checkmate and a draw score otherwise.
    fn fifty_moves_rule(&mut self, ply: Ply) -> Option<Score> {
        if self.position.details.halfmove == 100 {
            if self.checkmate() {
                return Some(-MATE_SCORE + ply);
            } else {
                return Some(0);
            }
        }

        None
    }

    fn checkmate(&mut self) -> bool {
        if !self.position.in_check() {
            return false;
        }

        let mp_excluded = Rc::clone(&self.mp_excluded[0]);
        let mut mp_excluded = mp_excluded.borrow_mut();
        let mp_moves = Rc::clone(&self.mp_moves[0]);
        let mut mp_moves = mp_moves.borrow_mut();
        let mp_scores = Rc::clone(&self.mp_scores[0]);
        let mut mp_scores = mp_scores.borrow_mut();

        let moves = MovePicker::new(
            self.position.clone(),
            Rc::clone(&self.tt),
            self.hasher.get_hash(),
            Rc::clone(&self.stack[MAX_PLY as usize - 1]),
            Rc::clone(&self.history),
            &mut mp_excluded,
            &mut mp_moves,
            &mut mp_scores,
        );

        for (_, mov) in moves {
            self.internal_make_move(mov, MAX_PLY - 1);
            if self.position.move_was_legal(mov) {
                self.internal_unmake_move(mov, MAX_PLY - 1);
                return false;
            }
            self.internal_unmake_move(mov, MAX_PLY - 1);
        }

        true
    }

    pub fn has_tt_move(&self) -> bool {
        if let Some(ttentry) = self.tt.borrow_mut().get(self.hasher.get_hash()) {
            if let Some(mov) = ttentry.best_move.expand(&self.position) {
                return MoveGenerator::from(&self.position).is_legal(mov);
            }
        }

        false
    }

    fn uci_info(&self, d: Depth, alpha: Score) {
        let elapsed = time::Instant::now() - self.started_at;
        let millis = elapsed.as_secs() as u32 * 1000 + elapsed.subsec_millis();
        let score_str = if alpha.abs() >= MATE_SCORE - MAX_PLY {
            if alpha < 0 {
                format!("mate {}", -MATE_SCORE - alpha)
            } else {
                format!("mate {}", -alpha + MATE_SCORE)
            }
        } else {
            format!("cp {}", alpha)
        };
        let mut pos = self.position.clone();
        print!(
            "info depth {} seldepth {} nodes {} score {} time {} hashfull {} pv ",
            d / INC_PLY,
            self.max_ply_searched,
            self.visited_nodes,
            score_str,
            millis,
            self.tt.borrow().usage()
        );
        for mov in self.pv[0].iter().take_while(|o| o.is_some()) {
            print!("{} ", mov.unwrap().to_algebraic());
            pos.make_move(mov.unwrap());
        }
        println!();

        if self.show_pv_board {
            pos.print("info string ");
        }
    }

    fn update_quiet_stats(&mut self, mov: Move, ply: Ply, depth: Depth) {
        assert!(mov.is_quiet());

        self.history.borrow_mut().increase_score(self.position.white_to_move, mov, depth);

        let killers = &mut self.stack[ply as usize].borrow_mut().killers_moves;

        if killers[0] != Some(mov) {
            killers[1] = killers[0];
            killers[0] = Some(mov);
        }
    }

    fn add_pv_move(&mut self, mov: Move, ply: Ply) {
        self.pv[ply as usize][0] = Some(mov);
        for i in 0..MAX_PLY - ply - 1 {
            self.pv[ply as usize][1 + i as usize] = self.pv[1 + ply as usize][i as usize];

            if self.pv[1 + ply as usize][i as usize] == None {
                for j in i + 1..MAX_PLY - ply - 1 {
                    self.pv[ply as usize][j as usize] = None;
                }
                break;
            }
            self.pv[1 + ply as usize][i as usize] = None;
        }
    }

    fn start_another_iteration(&mut self, d: Depth) -> bool {
        if d == MAX_PLY {
            return false;
        }

        match self.stop_condition {
            StopCondition::Depth(stop_d) => d <= stop_d,
            StopCondition::TimePerMove { millis } => {
                let now = time::Instant::now();
                let elapsed = now - self.started_at;
                let elapsed_millis = elapsed.as_secs() * 1000 + u64::from(elapsed.subsec_millis());
                elapsed_millis + 50 <= millis
            }
            StopCondition::Variable {
                wtime,
                btime,
                winc,
                binc,
                movestogo,
            } => {
                let now = time::Instant::now();
                let elapsed = now - self.started_at;
                let elapsed_millis = elapsed.as_secs() * 1000 + u64::from(elapsed.subsec_millis());

                let time = if self.searching_for_white {
                    wtime
                } else {
                    btime
                };
                let inc = if self.searching_for_white { winc } else { binc }.unwrap_or(0);
                let movestogo = movestogo.unwrap_or(40);
                elapsed_millis <= ::std::cmp::min(time, time / movestogo + inc) / 2
            }
            _ => true,
        }
    }

    fn should_stop(&mut self) -> bool {
        match self.stop_condition {
            StopCondition::Infinite => false,
            StopCondition::Nodes(nodes) => self.visited_nodes >= nodes,
            StopCondition::Depth(_) => false, /* handled in start_another_iteration */
            StopCondition::TimePerMove { millis } => {
                if self.visited_nodes & 0x7F == 0 {
                    let now = time::Instant::now();
                    let elapsed = now - self.started_at;
                    let elapsed_millis =
                        elapsed.as_secs() * 1000 + u64::from(elapsed.subsec_millis());
                    return elapsed_millis + 20 > millis;
                }
                false
            }
            StopCondition::Variable {
                wtime,
                btime,
                winc,
                binc,
                movestogo,
            } => {
                if self.visited_nodes & 0x7F == 0 {
                    let now = time::Instant::now();
                    let elapsed = now - self.started_at;
                    let elapsed_millis =
                        elapsed.as_secs() * 1000 + u64::from(elapsed.subsec_millis());

                    let time = if self.searching_for_white {
                        wtime
                    } else {
                        btime
                    };
                    let inc = if self.searching_for_white { winc } else { binc }.unwrap_or(0);
                    let movestogo = ::std::cmp::min(10, movestogo.unwrap_or(10));
                    return elapsed_millis >= ::std::cmp::min(time - 10, time / movestogo + inc);
                }
                false
            }
        }
    }

    fn is_draw(&self, ply: Ply) -> bool {
        let last_move = self.stack[ply as usize - 1].borrow().current_move;
        if last_move.and_then(|mov| mov.captured).is_some() {
            self.eval.material.is_draw()
        } else if last_move.is_some() && last_move.unwrap().piece != Piece::Pawn {
            self.repetitions.has_repeated()
        } else {
            false
        }
    }

    pub fn perft(&mut self, depth: usize) {
        let start = time::Instant::now();

        let mut num_moves = 0;
        let moves = MoveGenerator::from(&self.position).all_moves();

        if depth > 0 {
            for mov in moves {
                self.internal_make_move(mov, depth as Ply);
                if self.position.move_was_legal(mov) {
                    let perft = self.internal_perft(depth - 1);
                    num_moves += perft;
                    println!("{}: {}", mov.to_algebraic(), perft);
                }
                self.internal_unmake_move(mov, depth as Ply);
            }
        }

        let elapsed = time::Instant::now() - start;

        println!();
        println!("Nodes searched: {}", num_moves);
        println!(
            "Took {} ms",
            elapsed.as_secs() as u32 * 1000 + elapsed.subsec_millis()
        );
        println!();
    }

    fn internal_perft(&mut self, depth: usize) -> usize {
        if depth == 0 {
            return 1;
        }

        let mut num_moves = 0;
        let moves = MoveGenerator::from(&self.position).all_moves();

        for mov in moves {
            self.internal_make_move(mov, depth as Ply);
            if self.position.move_was_legal(mov) {
                num_moves += self.internal_perft(depth - 1);
            }
            self.internal_unmake_move(mov, depth as Ply);
        }

        num_moves
    }

    pub fn make_move(&mut self, mov: Move) {
        self.internal_make_move(mov, 0);
        self.made_moves.push(mov);
    }

    pub fn internal_make_move(&mut self, mov: Move, ply: Ply) {
        self.stack[ply as usize].borrow_mut().irreversible_details = self.position.details;
        self.stack[ply as usize].borrow_mut().current_move = Some(mov);

        self.hasher.make_move(&self.position, mov);
        self.eval.make_move(mov, &self.position);
        self.position.make_move(mov);

        if self.position.details.halfmove == 0 {
            self.repetitions.irreversible_move();
        }
        self.repetitions.push_position(self.hasher.get_hash());
    }

    fn internal_make_nullmove(&mut self, ply: Ply) {
        self.stack[ply as usize].borrow_mut().irreversible_details = self.position.details;
        self.stack[ply as usize].borrow_mut().current_move = None;

        self.hasher.make_nullmove(&self.position);
        self.position.make_nullmove();
    }

    fn internal_unmake_nullmove(&mut self, ply: Ply) {
        let irreversible = self.stack[ply as usize].borrow().irreversible_details;
        self.hasher.unmake_nullmove(&self.position, irreversible);
        self.position.unmake_nullmove(irreversible);
    }

    pub fn redo_eval(&mut self) {
        self.eval = Eval::from(&self.position);
    }

    /*
    pub fn unmake_move(&mut self) {
        let mov = self.made_moves.pop().unwrap();
        self.internal_unmake_move(mov);
    }
    */

    pub fn internal_unmake_move(&mut self, mov: Move, ply: Ply) {
        let irreversible = self.stack[ply as usize].borrow().irreversible_details;
        self.hasher.unmake_move(&self.position, mov, irreversible);
        self.repetitions.pop_position();
        self.eval.unmake_move(mov, &self.position);
        self.position.unmake_move(mov, irreversible);
    }

    pub fn resize_tt(&mut self, bits: u64) {
        self.tt = Rc::new(RefCell::new(TT::new(bits)));
    }
}
