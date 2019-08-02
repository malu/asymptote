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
use std::cmp;
use std::sync;

use crate::eval::*;
use crate::hash::*;
use crate::history::*;
use crate::movegen::*;
use crate::movepick::*;
use crate::position::*;
use crate::repetitions::Repetitions;
use crate::search_controller::PersistentOptions;
use crate::time::*;
use crate::tt::*;

pub type Ply = i16;
pub type Depth = i16;

pub const INC_PLY: Depth = 64;
pub const MAX_PLY: Ply = 128;

const CHECK_EXTENSION_DEPTH: Depth = 3 * INC_PLY;
const FUTILITY_MARGIN: Score = 200;
const HISTORY_PRUNING_DEPTH: Depth = 2 * INC_PLY;
const HISTORY_PRUNING_THRESHOLD: i64 = 0;
const LMR_DEPTH: Depth = 3 * INC_PLY;
const SEE_PRUNING_DEPTH: Depth = 5 * INC_PLY;
const SEE_PRUNING_MARGIN_CAPTURE: Score = -25;
const SEE_PRUNING_MARGIN_QUIET: Score = -100;
const STATIC_BETA_DEPTH: Depth = 5 * INC_PLY;
const STATIC_BETA_MARGIN: Score = 128;
const QS_FUTILITY_MARGIN: Score = 200;

#[derive(Clone)]
pub struct Search<'a> {
    pub id: usize,

    stack: [PlyDetails; MAX_PLY as usize],
    history: History,
    position: Position,
    eval: Eval,
    time_control: TimeControl,
    time_manager: TimeManager,
    hasher: Hasher,
    pub visited_nodes: u64,
    tt: &'a SharedTT<'a>,
    pv: Vec<Vec<Option<Move>>>,
    max_ply_searched: Ply,
    repetitions: Repetitions,
    options: PersistentOptions,

    quiets: [[Option<Move>; 256]; MAX_PLY as usize],
}

#[derive(Copy, Clone, Debug, Default)]
pub struct PlyDetails {
    irreversible_details: IrreversibleDetails,
    current_move: Option<Move>,
    pub killers_moves: [Option<Move>; 2],
}

impl<'a> Search<'a> {
    pub fn new(
        abort: sync::Arc<sync::atomic::AtomicBool>,
        hasher: Hasher,
        options: PersistentOptions,
        position: Position,
        time_control: TimeControl,
        tt: &'a SharedTT<'a>,
        repetitions: Repetitions,
    ) -> Search {
        let mut pv = Vec::with_capacity(MAX_PLY as usize);
        for i in 0..MAX_PLY as usize {
            pv.push(vec![None; MAX_PLY as usize - i + 1]);
        }

        Search {
            id: 0,

            history: History::default(),
            stack: [PlyDetails::default(); MAX_PLY as usize],
            eval: Eval::from(&position),
            time_control,
            time_manager: TimeManager::new(&position, time_control, options.move_overhead, abort),
            position,
            hasher,
            tt,
            visited_nodes: 0,
            pv,
            max_ply_searched: 0,
            repetitions,
            options,

            quiets: [[None; 256]; MAX_PLY as usize],
        }
    }

    pub fn prepare_search(&mut self) {
        self.set_time_control(self.time_control);
        self.pv
            .iter_mut()
            .for_each(|pv| pv.iter_mut().for_each(|i| *i = None));
    }

    pub fn iterative_deepening(&mut self) -> Move {
        let mut moves = MoveList::new();
        MoveGenerator::from(&self.position).all_moves(&mut moves);
        let mut moves = moves
            .into_iter()
            .filter(|&mov| self.position.move_is_legal(mov))
            .map(|mov| (mov, 0))
            .collect::<arrayvec::ArrayVec<[(Move, i64); 256]>>();

        if moves.len() == 1 {
            return moves[0].0;
        }

        let mut last_score = 0;
        if let Some(ttentry) = self.tt.get(self.hasher.get_hash()) {
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

        for d in 1_i16.. {
            if d >= MAX_PLY || !self.time_manager.start_another_iteration(d) {
                break;
            }
            let depth = d * INC_PLY;

            self.max_ply_searched = 0;

            if let Some(best_score) = self.aspiration(last_score, &mut moves, depth) {
                if d >= 4 && best_score < last_score {
                    self.time_manager.fail_low(best_score - last_score);
                }
                last_score = best_score;
                moves[1..].sort_by_key(|&(_, subtree_size)| -subtree_size);
            }

            self.uci_info(depth, last_score, EXACT_BOUND);
        }

        if self.id == 0 {
            self.time_manager
                .abort
                .store(true, std::sync::atomic::Ordering::SeqCst);
        }

        moves[0].0
    }

    fn aspiration(
        &mut self,
        last_score: Score,
        moves: &mut arrayvec::ArrayVec<[(Move, i64); 256]>,
        depth: Depth,
    ) -> Option<Score> {
        let mut delta = 30;
        let mut alpha = cmp::max(last_score - delta, -MATE_SCORE);
        let mut beta = cmp::min(last_score + delta, MATE_SCORE);

        loop {
            let (score, index) = self.search_root(moves, alpha, beta, depth)?;
            if index > 0 {
                let best_move = moves[index];
                moves.insert(0, best_move);
                moves.remove(index + 1);
            }

            delta += delta / 2;
            if score >= beta {
                beta = cmp::min(MATE_SCORE, score + delta);
                if self.time_manager.elapsed_millis() > 5000 {
                    self.uci_info(depth, score, LOWER_BOUND);
                }
            } else if score <= alpha {
                alpha = cmp::max(score - delta, -MATE_SCORE);
                if self.time_manager.elapsed_millis() > 5000 {
                    self.uci_info(depth, score, UPPER_BOUND);
                }
            } else {
                return Some(score);
            }
        }
    }

    fn search_root(
        &mut self,
        moves: &mut [(Move, i64)],
        alpha: Score,
        beta: Score,
        depth: Depth,
    ) -> Option<(Score, usize)> {
        let mut alpha = alpha;
        let mut best_score = -Score::max_value();
        let mut best_move_index = 0;
        let mut increased_alpha = false;
        for (i, &mut (mov, ref mut subtree_size)) in moves.iter_mut().enumerate() {
            self.internal_make_move(mov, 0);

            let mut new_depth = depth - INC_PLY;
            if self.position.in_check() {
                new_depth += INC_PLY;
            }

            let num_nodes_before = self.visited_nodes;
            let value;
            if i == 0 {
                value = self.search(1, -beta, -alpha, new_depth, true).map(|v| -v);
            } else {
                let value_zw = self
                    .search(1, -alpha - 1, -alpha, new_depth, false)
                    .map(|v| -v);
                if Some(alpha) < value_zw {
                    value = self.search(1, -beta, -alpha, new_depth, true).map(|v| -v);
                } else {
                    value = value_zw;
                }
            }

            *subtree_size = ((self.visited_nodes - num_nodes_before) / 2) as i64;

            self.internal_unmake_move(mov, 0);

            match value {
                None => {
                    if increased_alpha {
                        return Some((best_score, best_move_index));
                    } else {
                        return None;
                    }
                }
                Some(value) => {
                    if value > best_score {
                        best_score = value;
                        best_move_index = i;
                        if value > alpha {
                            alpha = value;
                            increased_alpha = true;
                            self.add_pv_move(mov, 0);

                            if value >= beta {
                                break;
                            }

                            if self.time_manager.elapsed_millis() > 1000 {
                                self.uci_info(depth, alpha, EXACT_BOUND);
                            }
                        }
                    }
                }
            }
        }

        Some((best_score, best_move_index))
    }

    pub fn search(
        &mut self,
        ply: Ply,
        alpha: Score,
        beta: Score,
        depth: Depth,
        is_pv: bool,
    ) -> Option<Score> {
        if self.time_manager.should_stop(self.visited_nodes) {
            return None;
        }

        self.visited_nodes += 1;
        self.max_ply_searched = cmp::max(ply, self.max_ply_searched);

        // Mate distance pruning
        let mdp_alpha = if alpha > -MATE_SCORE + ply {
            alpha
        } else {
            -MATE_SCORE + ply
        };
        let mdp_beta = if beta < MATE_SCORE - ply - 1 {
            beta
        } else {
            MATE_SCORE - ply - 1
        };
        if mdp_alpha >= mdp_beta {
            return Some(mdp_alpha);
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

        let (mut ttentry, mut ttmove) = self.get_tt_entry();

        if let Some(ttentry) = ttentry {
            let score = ttentry.score.to_score(ply);

            if is_pv {
                // In PV nodes we only cutoff on TT hits if we would drop into quiescence search otherwise.
                // Otherwise we would get shorter principal variations as output.
                if depth < INC_PLY && ttentry.bound & EXACT_BOUND == EXACT_BOUND {
                    return Some(score);
                }
            } else {
                if ttentry.depth >= depth || depth < INC_PLY {
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
        }

        if depth < INC_PLY {
            self.visited_nodes -= 1;
            return self.qsearch(ply, alpha, beta, 0);
        }

        let previous_move = self.stack[ply as usize - 1].current_move;
        let nullmove_reply = previous_move == None;
        let in_check = !nullmove_reply && self.position.in_check();

        let mut eval = None;
        let mut skip_quiets = false;
        if !is_pv {
            eval = Some(self.eval.score(&self.position, self.hasher.get_pawn_hash()));
        }

        if let Some(eval) = eval {
            skip_quiets = !in_check
                && depth < 3 * INC_PLY
                && eval + FUTILITY_MARGIN * (depth / INC_PLY) < alpha;

            // Static beta pruning
            //
            // Prune nodes at shallow depth if current evaluation is above beta by
            // a large (depth-dependent) margin.
            if !in_check
                && depth < STATIC_BETA_DEPTH
                && eval - STATIC_BETA_MARGIN * (depth / INC_PLY) > beta
            {
                return Some(beta);
            }

            // Nullmove pruning
            //
            // Prune nodes that are so good that we could pass without the opponent
            // catching up.
            if !in_check && self.eval.phase() > 0 && eval >= beta {
                let r = INC_PLY + depth / 4;
                self.internal_make_nullmove(ply);
                let score = self
                    .search(ply + 1, -alpha - 1, -alpha, depth - INC_PLY - r, false)
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
        }

        // Internal iterative deepening
        // If we don't get a previous best move from the TT, do a reduced-depth search first to get one.
        if ttmove.is_none() {
            if is_pv && depth >= 4 * INC_PLY {
                self.search(ply, alpha, beta, depth - 2 * INC_PLY, true);
                self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
                let (ttentry_, ttmove_) = self.get_tt_entry();
                ttentry = ttentry_;
                ttmove = ttmove_;
            } else if !is_pv && depth >= 6 * INC_PLY {
                self.search(ply, alpha, beta, depth / 2, false);
                let (ttentry_, ttmove_) = self.get_tt_entry();
                ttentry = ttentry_;
                ttmove = ttmove_;
            }
        }

        let previous_move = self.stack[ply as usize - 1].current_move;
        let mut moves = MovePicker::new(
            ttmove,
            self.stack[ply as usize].killers_moves,
            previous_move,
        );

        // Futility pruning
        //
        // If we are too far behind at shallow depth, then don't search moves
        // which don't change material.
        moves.skip_quiets(skip_quiets);

        // We have to remember whether we pruned any moves to avoid returing
        // invalid (stale)mate scores.
        let mut pruned = skip_quiets;

        let mut alpha = alpha;
        let mut increased_alpha = false;
        let mut best_score = -Score::max_value();
        let mut best_move = None;
        let mut num_moves = 0;
        let mut num_quiets = 0;
        while let Some((mtype, mov)) = moves.next(&self.position, &self.history) {
            if !self.position.move_is_legal(mov) {
                continue;
            }

            let check = self.position.move_will_check(mov);

            // Prunings
            if let Some(eval) = eval {
                if best_score > -MATE_SCORE + MAX_PLY {
                    // Futility pruning
                    if !in_check
                        && !check
                        && depth < 9 * INC_PLY
                        && mtype == MoveType::Quiet
                        && eval + 64 + 64 * (depth / INC_PLY) < alpha
                    {
                        pruned = true;
                        continue;
                    }

                    // History leaf pruning
                    //
                    // Do not play moves with negative history score if at very low
                    // depth.
                    if depth < HISTORY_PRUNING_DEPTH
                        && mtype == MoveType::Quiet
                        && self.history.get_score(self.position.white_to_move, mov)
                            < HISTORY_PRUNING_THRESHOLD
                    {
                        pruned = true;

                        // We can skip the remaining quiet moves because quiet moves
                        // are ordered by history score.
                        moves.skip_quiets(true);
                        continue;
                    }

                    // Static exchange evaluation pruning
                    //
                    // Do not play very bad moves at shallow depths.
                    // Does not trigger for winning or equal tactical moves
                    // (MoveType::GoodCapture) because those are assumed to have a
                    // non-negative static exchange evaluation.
                    if depth < SEE_PRUNING_DEPTH && !check && !in_check {
                        if mtype == MoveType::BadCapture
                            && !self.position.see(
                                mov,
                                SEE_PRUNING_MARGIN_CAPTURE * (depth / INC_PLY) * (depth / INC_PLY),
                            )
                        {
                            pruned = true;
                            continue;
                        } else if mtype == MoveType::Quiet
                            && !self
                                .position
                                .see(mov, SEE_PRUNING_MARGIN_QUIET * (depth / INC_PLY))
                        {
                            pruned = true;
                            continue;
                        }
                    }
                }
            }

            let mut extension = 0;

            if check {
                // We only extend checks which satify at least one of the
                // following conditions:
                // * node is near horizon
                // * move is the hash move from a previous search
                // * move does not lose material
                if depth < CHECK_EXTENSION_DEPTH
                    || mtype == MoveType::GoodCapture
                    || mtype == MoveType::TTMove
                    // Filter tactically bad moves. They wouldn't pass the SEE
                    // test anyway.
                    || mtype != MoveType::BadCapture && self.position.see(mov, 0)
                {
                    extension += INC_PLY;
                }
            }

            // Recapture extension
            if let Some(previous_move) = previous_move {
                if previous_move.to == mov.to {
                    extension += if is_pv { INC_PLY } else { INC_PLY / 2 };
                }
            }

            let mut reduction = 0;
            if nullmove_reply && mtype == MoveType::Quiet {
                reduction += INC_PLY;
            }

            if depth >= LMR_DEPTH && mtype == MoveType::Quiet && best_score > -MATE_SCORE + MAX_PLY
            {
                reduction += lmr_reduction(depth, num_moves, is_pv);

                if check {
                    reduction -= INC_PLY;
                }

                if in_check {
                    reduction -= INC_PLY;
                }
            };

            extension = cmp::min(extension, INC_PLY);
            let new_depth = depth - INC_PLY + extension;
            reduction = cmp::max(0, cmp::min(reduction, new_depth - INC_PLY));

            self.internal_make_move(mov, ply);

            num_moves += 1;
            if mov.is_quiet() {
                self.quiets[ply as usize][num_quiets] = Some(mov);
                num_quiets += 1;
            }

            let mut value = Some(Score::max_value());
            if is_pv {
                if num_moves > 1 {
                    value = self
                        .search(ply + 1, -alpha - 1, -alpha, new_depth - reduction, false)
                        .map(|v| -v);
                }

                if reduction > 0 && Some(alpha) < value {
                    value = self
                        .search(ply + 1, -alpha - 1, -alpha, new_depth, false)
                        .map(|v| -v);
                }

                if Some(alpha) < value {
                    value = self
                        .search(ply + 1, -beta, -alpha, new_depth, true)
                        .map(|v| -v);
                }
            } else {
                value = self
                    .search(ply + 1, -alpha - 1, -alpha, new_depth - reduction, false)
                    .map(|v| -v);

                if reduction > 0 && Some(alpha) < value {
                    value = self
                        .search(ply + 1, -beta, -alpha, new_depth, false)
                        .map(|v| -v);
                }
            }

            self.internal_unmake_move(mov, ply);

            match value {
                None => {
                    if is_pv && increased_alpha {
                        self.tt.insert(
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
                    }

                    if value > alpha {
                        increased_alpha = true;
                        alpha = value;

                        if is_pv {
                            self.add_pv_move(mov, ply);
                        }
                    }

                    if value >= beta {
                        if mov.is_quiet() {
                            self.update_quiet_stats(mov, ply, depth, num_quiets - 1);
                        }

                        if is_pv {
                            self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
                        }

                        break;
                    }
                }
            }
        }

        if let Some(best_move) = best_move {
            let tt_bound = if best_score >= beta {
                LOWER_BOUND
            } else if increased_alpha {
                EXACT_BOUND
            } else {
                UPPER_BOUND
            };

            self.tt.insert(
                self.hasher.get_hash(),
                depth,
                TTScore::from_score(best_score, ply),
                best_move,
                tt_bound,
            );

            Some(best_score)
        } else {
            // No best move => no legal moves
            if pruned {
                Some(alpha)
            } else if self.position.in_check() {
                Some(-MATE_SCORE + ply)
            } else {
                // Stalemate
                Some(0)
            }
        }
    }

    pub fn qsearch(&mut self, ply: Ply, alpha: Score, beta: Score, depth: Depth) -> Option<Score> {
        if self.time_manager.should_stop(self.visited_nodes) {
            return None;
        }

        if ply == MAX_PLY {
            return Some(self.eval.score(&self.position, self.hasher.get_pawn_hash()));
        }

        self.visited_nodes += 1;

        let in_check = self.position.in_check();
        let mut alpha = alpha;

        let eval = if in_check {
            // Don't do any cutoffs or prunings when in check.
            None
        } else {
            let e = self.eval.score(&self.position, self.hasher.get_pawn_hash());
            if e >= beta {
                return Some(e);
            }

            if alpha < e {
                alpha = e;
            }

            Some(e)
        };

        if depth == 0 {
            let (ttentry, _ttmove) = self.get_tt_entry();
            if let Some(ttentry) = ttentry {
                let score = ttentry.score.to_score(ply);

                if alpha + 1 < beta {
                    if score >= beta && ttentry.bound & LOWER_BOUND > 0 {
                        return Some(score);
                    }

                    if score <= alpha && ttentry.bound & UPPER_BOUND > 0 {
                        return Some(score);
                    }
                }

                if ttentry.bound & EXACT_BOUND == EXACT_BOUND {
                    return Some(score);
                }
            }
        }

        let mut depth = depth;

        if in_check {
            depth += INC_PLY;
        }

        let mut moves = MovePicker::qsearch(&self.position);

        let mut best_move = None;
        let mut best_score = -MATE_SCORE;

        let mut num_moves = 0;
        while let Some((_mtype, mov)) = moves.next(&self.position, &self.history) {
            if !self.position.move_is_legal(mov) {
                continue;
            }

            if let Some(eval) = eval {
                let capture = mov.captured.map_or(0, Piece::value);
                let promote = mov.promoted.map_or(0, |p| p.value() - Piece::Pawn.value());
                let mscore = capture + promote;

                if eval + mscore + QS_FUTILITY_MARGIN < alpha && !self.position.move_will_check(mov)
                {
                    continue;
                }
            }

            self.internal_make_move(mov, ply);
            num_moves += 1;

            let value = self
                .qsearch(ply + 1, -beta, -alpha, depth - INC_PLY)
                .map(|v| -v);
            self.internal_unmake_move(mov, ply);

            match value {
                None => return None,
                Some(score) => {
                    if score > best_score {
                        best_score = score;
                        best_move = Some(mov);
                        if score > alpha {
                            alpha = score;
                            if score >= beta {
                                break;
                            }
                        }
                    }
                }
            }
        }

        if num_moves == 0 {
            if in_check {
                return Some(-MATE_SCORE + ply);
            } else {
                return Some(alpha);
            }
        }

        let score = if best_score >= beta {
            best_score
        } else {
            alpha
        };
        if depth == 0 || depth == INC_PLY && in_check {
            let bound = if best_score >= beta {
                LOWER_BOUND
            } else {
                UPPER_BOUND
            };
            self.tt.insert(
                self.hasher.get_hash(),
                0,
                TTScore::from_score(score, ply),
                best_move.unwrap(),
                bound,
            );
        }
        Some(score)
    }

    fn get_tt_entry(&mut self) -> (Option<TTEntry>, Option<Move>) {
        if let Some(ttentry) = self.tt.get(self.hasher.get_hash()) {
            let mov = ttentry
                .best_move
                .expand(&self.position)
                .filter(|&mov| self.position.move_is_pseudo_legal(mov));
            (mov.map(|_| ttentry), mov)
        } else {
            (None, None)
        }
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

        let mut moves = MovePicker::new(None, [None; 2], None);

        while let Some((_, mov)) = moves.next(&self.position, &self.history) {
            if self.position.move_is_legal(mov) {
                return false;
            }
        }

        true
    }

    fn uci_info(&self, d: Depth, alpha: Score, bound: Bound) {
        if self.id > 0 {
            return;
        }

        let elapsed = self.time_manager.elapsed_millis();
        let score_str = if alpha.abs() >= MATE_SCORE - MAX_PLY {
            if alpha < 0 {
                format!("mate {}", -(MATE_SCORE + alpha) / 2)
            } else {
                format!("mate {}", (MATE_SCORE - alpha + 1) / 2)
            }
        } else {
            format!("cp {}", alpha)
        };

        let score_str = if bound == LOWER_BOUND {
            format!("{} lowerbound", score_str)
        } else if bound == UPPER_BOUND {
            format!("{} upperbound", score_str)
        } else {
            score_str
        };

        let mut pos = self.position.clone();
        let estimated_nodes = self.visited_nodes * self.options.threads as u64;
        print!(
            "info depth {} seldepth {} nodes {} nps {} score {} time {} hashfull {} pv ",
            d / INC_PLY,
            self.max_ply_searched,
            estimated_nodes,
            1000 * estimated_nodes / cmp::max(1, elapsed),
            score_str,
            elapsed,
            self.tt.usage()
        );
        for mov in self.pv[0]
            .iter()
            .cloned()
            .take_while(Option::is_some)
            .flatten()
        {
            print!("{} ", mov.to_algebraic());
            pos.make_move(mov);
        }
        println!();

        if self.options.show_pv_board {
            pos.print("info string ");
        }
    }

    fn update_quiet_stats(&mut self, mov: Move, ply: Ply, depth: Depth, num_failed_quiets: usize) {
        assert!(mov.is_quiet());

        self.history
            .increase_score(self.position.white_to_move, mov, depth);
        self.history.decrease_score(
            self.position.white_to_move,
            &self.quiets[ply as usize][0..num_failed_quiets],
            depth,
        );

        if let Some(previous_move) = self.stack[ply as usize - 1].current_move {
            if previous_move.is_quiet() {
                self.history.last_best_reply[self.position.white_to_move as usize]
                    [previous_move.piece.index()][previous_move.to] = Some(mov);
            } else {
                self.history.last_best_reply[self.position.white_to_move as usize]
                    [previous_move.piece.index()][previous_move.to] = None;
            }
        }

        let killers = &mut self.stack[ply as usize].killers_moves;

        if killers[0] != Some(mov) {
            killers[1] = killers[0];
            killers[0] = Some(mov);
        }
    }

    fn add_pv_move(&mut self, mov: Move, ply: Ply) {
        let ply = ply as usize;
        self.pv[ply][0] = Some(mov);
        for i in 0..MAX_PLY as usize - ply - 1 {
            self.pv[ply][1 + i] = self.pv[1 + ply][i];

            if self.pv[1 + ply][i] == None {
                for j in i + 1..MAX_PLY as usize - ply - 1 {
                    self.pv[ply][j] = None;
                }
                break;
            }
            self.pv[1 + ply][i] = None;
        }
    }

    fn is_draw(&self, ply: Ply) -> bool {
        if let Some(last_move) = self.stack[ply as usize - 1].current_move {
            if last_move.captured.is_some() {
                return self.eval.is_material_draw();
            } else if last_move.piece != Piece::Pawn {
                return self.repetitions.has_repeated(ply);
            }
        }

        false
    }

    pub fn perft(&mut self, depth: usize) {
        self.time_manager
            .update(&self.position, TimeControl::Infinite);

        let mut num_moves = 0;

        let mut moves = MoveList::new();
        MoveGenerator::from(&self.position).all_moves(&mut moves);

        if depth > 0 {
            for &mov in &moves {
                if !self.position.move_is_legal(mov) {
                    continue;
                }

                self.internal_make_move(mov, depth as Ply);
                let perft = self.internal_perft(depth - 1);
                num_moves += perft;
                println!("{}: {}", mov.to_algebraic(), perft);
                self.internal_unmake_move(mov, depth as Ply);
            }
        }

        let elapsed = self.time_manager.elapsed_millis();

        println!();
        println!("Nodes searched: {}", num_moves);
        println!("Took {} ms", elapsed);
        println!();
    }

    fn internal_perft(&mut self, depth: usize) -> usize {
        if depth == 0 {
            return 1;
        }

        let mut num_moves = 0;
        let mut moves = MoveList::new();
        MoveGenerator::from(&self.position).all_moves(&mut moves);

        for &mov in &moves {
            if !self.position.move_is_legal(mov) {
                continue;
            }

            self.internal_make_move(mov, depth as Ply);
            num_moves += self.internal_perft(depth - 1);
            self.internal_unmake_move(mov, depth as Ply);
        }

        num_moves
    }

    pub fn internal_make_move(&mut self, mov: Move, ply: Ply) {
        let current_ply = &mut self.stack[ply as usize];
        current_ply.irreversible_details = self.position.details;
        current_ply.current_move = Some(mov);
        if ply + 2 < MAX_PLY {
            self.stack[2 + ply as usize].killers_moves = [None; 2];
        }

        self.hasher.make_move(&self.position, mov);
        self.eval.make_move(mov, &self.position);
        self.position.make_move(mov);

        if self.position.details.halfmove == 0 {
            self.repetitions.irreversible_move();
        }
        self.repetitions.push_position(self.hasher.get_hash());
    }

    fn internal_make_nullmove(&mut self, ply: Ply) {
        self.stack[ply as usize].irreversible_details = self.position.details;
        self.stack[ply as usize].current_move = None;

        self.hasher.make_nullmove(&self.position);
        self.position.make_nullmove();
        self.repetitions.push_position(self.hasher.get_hash());
    }

    fn internal_unmake_nullmove(&mut self, ply: Ply) {
        let irreversible = self.stack[ply as usize].irreversible_details;
        self.hasher.unmake_nullmove(&self.position, irreversible);
        self.repetitions.pop_position();
        self.position.unmake_nullmove(irreversible);
    }

    pub fn internal_unmake_move(&mut self, mov: Move, ply: Ply) {
        let irreversible = self.stack[ply as usize].irreversible_details;
        self.hasher.unmake_move(&self.position, mov, irreversible);
        self.repetitions.pop_position();
        self.eval.unmake_move(mov, &self.position);
        self.position.unmake_move(mov, irreversible);
    }

    pub fn set_time_control(&mut self, tc: TimeControl) {
        self.time_control = tc;
        self.time_manager.update(&self.position, self.time_control);
    }
}

fn lmr_reduction(depth: Depth, move_count: i16, pv: bool) -> Depth {
    let r = (depth / INC_PLY + move_count) / 8 - pv as i16;
    r * INC_PLY
}
