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
use std::cmp;
use std::sync;

use crate::eval::*;
use crate::hash::*;
use crate::history::*;
use crate::movegen::*;
use crate::movepick::*;
use crate::position::*;
use crate::repetitions::Repetitions;
use crate::time::*;
use crate::tt::*;
use crate::uci::*;

pub type Ply = i16;
pub type Depth = i16;

pub const INC_PLY: Depth = 64;
pub const MAX_PLY: Ply = 128;

const FUTILITY_MARGIN: Score = 200;

const LMR_MAX_DEPTH: Depth = 9 * INC_PLY;
const LMR_MOVES: [usize; (LMR_MAX_DEPTH / INC_PLY) as usize] = [255, 255, 3, 5, 5, 7, 7, 9, 9];

#[derive(Copy, Clone, Debug)]
struct PersistentOptions {
    hash_bits: u64,
    show_pv_board: bool,
}

impl Default for PersistentOptions {
    fn default() -> Self {
        PersistentOptions {
            hash_bits: 14,
            show_pv_board: false,
        }
    }
}

pub struct Search {
    stack: [PlyDetails; MAX_PLY as usize],
    history: History,
    position: Position,
    eval: Eval,
    pub time_control: TimeControl,
    pub time_manager: TimeManager,
    hasher: Hasher,
    pub visited_nodes: u64,
    tt: TT,
    pv: Vec<Vec<Option<Move>>>,
    max_ply_searched: Ply,
    repetitions: Repetitions,
    made_moves: Vec<Move>,
    options: PersistentOptions,

    mp_allocations: Vec<MovePickerAllocations>,
    quiets: [[Option<Move>; 256]; MAX_PLY as usize],
}

#[derive(Copy, Clone, Debug, Default)]
pub struct PlyDetails {
    irreversible_details: IrreversibleDetails,
    current_move: Option<Move>,
    pub killers_moves: [Option<Move>; 2],
}

impl Search {
    pub fn new(position: Position, abort: sync::Arc<sync::atomic::AtomicBool>) -> Self {
        let mut pv = Vec::with_capacity(MAX_PLY as usize);
        let stack = [PlyDetails::default(); MAX_PLY as usize];
        let mut mp_allocations = Vec::with_capacity(MAX_PLY as usize);
        for i in 0..MAX_PLY as usize {
            pv.push(vec![None; MAX_PLY as usize - i + 1]);
            mp_allocations.push(MovePickerAllocations::default());
        }

        Search {
            history: History::default(),
            stack,
            eval: Eval::from(&position),
            time_control: TimeControl::Infinite,
            time_manager: TimeManager::new(&position, TimeControl::Infinite, abort),
            position,
            hasher: Hasher::new(),
            tt: TT::new(14),
            visited_nodes: 0,
            pv,
            max_ply_searched: 0,
            repetitions: Repetitions::new(100),
            made_moves: Vec::new(),
            options: PersistentOptions::default(),

            mp_allocations,
            quiets: [[None; 256]; MAX_PLY as usize],
        }
    }

    pub fn root(&mut self) -> Move {
        self.time_manager.update(&self.position, self.time_control);
        self.visited_nodes = 0;
        self.tt.next_generation();
        self.pv
            .iter_mut()
            .for_each(|pv| pv.iter_mut().for_each(|i| *i = None));

        self.history.rescale();

        let mut moves = MoveGenerator::from(&self.position)
            .all_moves()
            .into_iter()
            .filter(|&mov| self.position.move_is_legal(mov))
            .map(|mov| (mov, 0))
            .collect::<Vec<_>>();

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

        let mut max_depth = 0;
        for d in 1_i16.. {
            if !self.time_manager.start_another_iteration(d) {
                break;
            }
            let depth = d * INC_PLY;
            max_depth = cmp::max(max_depth, depth);

            self.max_ply_searched = 0;

            if let Some(best_score) = self.aspiration(last_score, &mut moves, depth) {
                last_score = best_score;
                moves[1..].sort_by_key(|&(_, subtree_size)| -subtree_size);
            }

            self.uci_info(depth, last_score, EXACT_BOUND);
        }

        moves[0].0
    }

    fn aspiration(
        &mut self,
        last_score: Score,
        moves: &mut Vec<(Move, i64)>,
        depth: Depth,
    ) -> Option<Score> {
        let mut delta = 50;
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

                            if value >= beta {
                                break;
                            }

                            self.add_pv_move(mov, 0);

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

    pub fn search_pv(
        &mut self,
        ply: Ply,
        alpha: Score,
        beta: Score,
        depth: Depth,
    ) -> Option<Score> {
        if self.time_manager.should_stop(self.visited_nodes) {
            return None;
        }

        self.visited_nodes += 1;
        self.max_ply_searched = cmp::max(ply, self.max_ply_searched);

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

        // Mate Distance Pruning
        // If we found a shorter mating sequence, do not search this move further.
        if MATE_SCORE - ply < alpha {
            return Some(alpha);
        }

        let (mut ttentry, mut ttmove) = self.get_tt_entry();

        if let Some(ttentry) = ttentry {
            let score = ttentry.score.to_score(ply);

            // In PV nodes we only cutoff on TT hits if we would drop into quiescence search otherwise.
            // Otherwise we would get shorter principal variations as output.
            if depth < INC_PLY {
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
            return self.qsearch(ply, alpha, beta, 0);
        }

        // Internal iterative deepening
        // If we don't get a previous best move from the TT, do a reduced-depth search first to get one.
        if depth >= 4 * INC_PLY && ttmove.is_none() {
            self.search_pv(ply, alpha, beta, depth - 2 * INC_PLY);
            self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
            let (ttentry_, ttmove_) = self.get_tt_entry();
            ttentry = ttentry_;
            ttmove = ttmove_;
        }

        let mut mp_allocations = self.mp_allocations.pop().unwrap();

        let mut moves = MovePicker::new(
            self.position.clone(),
            ttmove,
            self.stack[ply as usize].killers_moves,
            &mut mp_allocations,
        );

        let mut alpha = alpha;
        let mut increased_alpha = false;

        let previous_move = self.stack[ply as usize - 1].current_move;
        let mut best_move = None;
        let mut best_score = -Score::max_value();

        let mut num_moves = 0;
        let mut num_quiets = 0;
        while let Some((_mtype, mov)) = moves.next(&self.history) {
            if !self.position.move_is_legal(mov) {
                continue;
            }

            let check = self.position.move_will_check(mov);

            self.internal_make_move(mov, ply);

            num_moves += 1;
            if mov.is_quiet() {
                self.quiets[ply as usize][num_quiets] = Some(mov);
                num_quiets += 1;
            }

            let mut extension = 0;
            // Check extension
            if check {
                extension += INC_PLY;
            }

            // Recapture extension
            if let Some(previous_move) = previous_move {
                if previous_move.to == mov.to {
                    extension += INC_PLY;
                }
            }

            extension = cmp::min(extension, INC_PLY);
            let new_depth = depth - INC_PLY + extension;

            let mut value = if num_moves > 1 {
                self.search_zw(ply + 1, -alpha, new_depth).map(|v| -v)
            } else {
                Some(-Score::max_value())
            };

            if num_moves == 1 || value.map_or(false, |value| value > alpha) {
                value = self
                    .search_pv(ply + 1, -beta, -alpha, new_depth)
                    .map(|v| -v);
            }

            self.internal_unmake_move(mov, ply);

            match value {
                None => {
                    if increased_alpha {
                        self.tt.insert(
                            self.hasher.get_hash(),
                            depth,
                            TTScore::from_score(alpha, ply),
                            best_move.unwrap(),
                            LOWER_BOUND,
                        );
                    }

                    self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
                    self.mp_allocations.push(mp_allocations);
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
                                    self.update_quiet_stats(mov, ply, depth, num_quiets - 1);
                                }

                                self.tt.insert(
                                    self.hasher.get_hash(),
                                    depth,
                                    TTScore::from_score(value, ply),
                                    mov,
                                    LOWER_BOUND,
                                );
                                self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
                                self.mp_allocations.push(mp_allocations);
                                return Some(value);
                            }
                        }
                    }
                }
            }
        }

        self.mp_allocations.push(mp_allocations);
        if let Some(best_move) = best_move {
            let tt_bound = if increased_alpha {
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
            if self.position.in_check() {
                Some(-MATE_SCORE + ply)
            } else {
                // Stalemate
                Some(0)
            }
        }
    }

    pub fn search_zw(&mut self, ply: Ply, beta: Score, depth: Depth) -> Option<Score> {
        if self.time_manager.should_stop(self.visited_nodes) {
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
        self.max_ply_searched = cmp::max(ply, self.max_ply_searched);

        let previous_move = self.stack[ply as usize - 1].current_move;
        let nullmove_reply = previous_move == None;

        let in_check = !nullmove_reply && self.position.in_check();

        let (mut ttentry, mut ttmove) = self.get_tt_entry();

        if let Some(ttentry) = ttentry {
            let score = ttentry.score.to_score(ply);

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

        // Do a quiescent search if we have no depth left.
        if depth < INC_PLY {
            self.visited_nodes -= 1;
            return self.qsearch(ply, alpha, beta, 0);
        }

        let eval = self.eval.score(&self.position, self.hasher.get_pawn_hash());

        // Static beta pruning
        //
        // Prune nodes at shallow depth if current evaluation is above beta by
        // a large (depth-dependent) margin.
        if !in_check && depth < 5 * INC_PLY && eval - 128 * (depth / INC_PLY) > beta {
            return Some(beta);
        }

        // Nullmove pruning
        //
        // Prune nodes that are so good that we could pass without the opponent
        // catching up.
        if !in_check && self.eval.phase() > 0 && eval >= beta {
            let r = 2;
            self.internal_make_nullmove(ply);
            let score = self
                .search_zw(ply + 1, -alpha, depth - INC_PLY - r * INC_PLY)
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

        // Internal deepening
        //
        // If we do not have a previous best move for this node, try to find
        // one first.
        if depth >= 6 * INC_PLY && ttmove.is_none() {
            self.search_zw(ply, beta, depth / 2);
            let (ttentry_, ttmove_) = self.get_tt_entry();
            ttentry = ttentry_;
            ttmove = ttmove_;
        }

        let mut mp_allocations = self.mp_allocations.pop().unwrap();

        let mut moves = MovePicker::new(
            self.position.clone(),
            ttmove,
            self.stack[ply as usize].killers_moves,
            &mut mp_allocations,
        );

        // We have to remember whether we pruned any moves to avoid returing
        // invalid (stale)mate scores.
        let mut pruned = false;

        // Futility pruning
        //
        // If we are too far behind at shallow depth, then don't search moves
        // which don't change material.
        if !in_check && depth < 3 * INC_PLY && eval + FUTILITY_MARGIN * (depth / INC_PLY) < alpha {
            moves.skip_quiets(true);
            pruned = true;
        }

        let mut num_moves = 0;
        let mut num_quiets = 0;
        while let Some((mtype, mov)) = moves.next(&self.history) {
            if !self.position.move_is_legal(mov) {
                continue;
            }

            let check = self.position.move_will_check(mov);
            // Futility pruning
            if !check {
                let capture_value = mov.captured.map_or(0, Piece::value);
                let promotion_value = mov
                    .promoted
                    .map_or(0, |piece| piece.value() - Piece::Pawn.value());

                if eval + 200 * ((depth / INC_PLY + 1) / 2) + capture_value + promotion_value
                    < alpha
                {
                    pruned = true;
                    continue;
                }
            }

            self.internal_make_move(mov, ply);

            num_moves += 1;

            if mov.is_quiet() {
                self.quiets[ply as usize][num_quiets] = Some(mov);
                num_quiets += 1;
            }

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

            if extension <= 0
                && depth < LMR_MAX_DEPTH
                && num_moves > LMR_MOVES[(depth / INC_PLY) as usize]
                && mtype != MoveType::GoodCapture
                && mtype != MoveType::Killer
                && !check
                && !in_check
            {
                reduction += INC_PLY;
            }

            extension = cmp::min(extension, INC_PLY);
            let new_depth = depth - INC_PLY + extension;

            // If we drop into qsearch anyway, don't reduce.
            if new_depth < INC_PLY {
                reduction = 0;
            }

            let mut value = self
                .search_zw(ply + 1, -alpha, new_depth - reduction)
                .map(|v| -v);
            if value.is_some() && value.unwrap() > alpha && reduction > 0 {
                value = self.search_zw(ply + 1, -alpha, new_depth).map(|v| -v);
            }

            self.internal_unmake_move(mov, ply);

            if let Some(value) = value {
                if value > best_score {
                    best_score = value;
                    best_move = Some(mov);
                }

                if value >= beta {
                    if mov.is_quiet() {
                        self.update_quiet_stats(mov, ply, depth, num_quiets - 1);
                    }

                    self.tt.insert(
                        self.hasher.get_hash(),
                        depth,
                        TTScore::from_score(best_score, ply),
                        mov,
                        LOWER_BOUND,
                    );
                    self.mp_allocations.push(mp_allocations);
                    return Some(value);
                }
            } else {
                self.mp_allocations.push(mp_allocations);
                return None;
            }
        }

        self.mp_allocations.push(mp_allocations);

        if num_moves == 0 {
            if pruned {
                return Some(alpha);
            } else if self.position.in_check() {
                return Some(-MATE_SCORE + ply);
            } else {
                return Some(0);
            }
        }

        self.tt.insert(
            self.hasher.get_hash(),
            depth,
            TTScore::from_score(best_score, ply),
            best_move.unwrap(),
            UPPER_BOUND,
        );
        Some(best_score)
    }

    pub fn qsearch(&mut self, ply: Ply, alpha: Score, beta: Score, depth: Depth) -> Option<Score> {
        if self.time_manager.should_stop(self.visited_nodes) {
            return None;
        }

        self.visited_nodes += 1;

        let in_check = self.position.in_check();
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

        let (ttentry, _ttmove) = self.get_tt_entry();

        if depth == 0 {
            if let Some(ttentry) = ttentry {
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

        let mut depth = depth;

        if in_check {
            depth += INC_PLY;
        }

        let mut mp_allocations = self.mp_allocations.pop().unwrap();

        let mut moves = if in_check {
            MovePicker::qsearch_in_check(self.position.clone(), &mut mp_allocations)
        } else {
            MovePicker::qsearch(self.position.clone(), &mut mp_allocations)
        };

        let mut best_move = None;
        let mut best_score = -MATE_SCORE;

        let mut num_moves = 0;
        while let Some((_mtype, mov)) = moves.next(&self.history) {
            if !self.position.move_is_legal(mov) {
                continue;
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
                                if depth == 0 || depth == INC_PLY && in_check {
                                    self.tt.insert(
                                        self.hasher.get_hash(),
                                        0,
                                        TTScore::from_score(score, ply),
                                        mov,
                                        LOWER_BOUND,
                                    );
                                }
                                self.mp_allocations.push(mp_allocations);
                                return value;
                            }
                        }
                    }
                }
            }
        }

        self.mp_allocations.push(mp_allocations);
        if num_moves == 0 {
            if in_check {
                return Some(-MATE_SCORE + ply);
            } else {
                return Some(alpha);
            }
        }

        if depth == 0 || depth == INC_PLY && in_check {
            self.tt.insert(
                self.hasher.get_hash(),
                0,
                TTScore::from_score(alpha, ply),
                best_move.unwrap(),
                UPPER_BOUND,
            );
        }
        Some(alpha)
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

        let mp_allocations = &mut self.mp_allocations[0];

        let mut moves = MovePicker::new(self.position.clone(), None, [None; 2], mp_allocations);

        while let Some((_, mov)) = moves.next(&self.history) {
            if self.position.move_is_legal(mov) {
                return false;
            }
        }

        true
    }

    fn uci_info(&self, d: Depth, alpha: Score, bound: Bound) {
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
        print!(
            "info depth {} seldepth {} nodes {} nps {} score {} time {} hashfull {} pv ",
            d / INC_PLY,
            self.max_ply_searched,
            self.visited_nodes,
            1000 * self.visited_nodes / cmp::max(1, elapsed),
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

        let killers = &mut self.stack[ply as usize].killers_moves;

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

    fn is_draw(&self, ply: Ply) -> bool {
        if let Some(last_move) = self.stack[ply as usize - 1].current_move {
            if last_move.captured.is_some() {
                return self.eval.is_material_draw();
            } else if last_move.piece != Piece::Pawn {
                return self.repetitions.has_repeated();
            }
        }

        false
    }

    pub fn perft(&mut self, depth: usize) {
        self.time_manager
            .update(&self.position, TimeControl::Infinite);

        let mut num_moves = 0;
        let moves = MoveGenerator::from(&self.position).all_moves();

        if depth > 0 {
            for mov in moves {
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
        let moves = MoveGenerator::from(&self.position).all_moves();

        for mov in moves {
            if !self.position.move_is_legal(mov) {
                continue;
            }

            self.internal_make_move(mov, depth as Ply);
            num_moves += self.internal_perft(depth - 1);
            self.internal_unmake_move(mov, depth as Ply);
        }

        num_moves
    }

    pub fn make_move(&mut self, mov: Move) {
        self.internal_make_move(mov, 0);
        self.made_moves.push(mov);
    }

    pub fn internal_make_move(&mut self, mov: Move, ply: Ply) {
        let current_ply = &mut self.stack[ply as usize];
        current_ply.irreversible_details = self.position.details;
        current_ply.current_move = Some(mov);

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
        let irreversible = self.stack[ply as usize].irreversible_details;
        self.hasher.unmake_move(&self.position, mov, irreversible);
        self.repetitions.pop_position();
        self.eval.unmake_move(mov, &self.position);
        self.position.unmake_move(mov, irreversible);
    }

    pub fn resize_tt(&mut self, bits: u64) {
        self.tt = TT::new(bits);
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
                UciCommand::History(mov) => self.handle_history(mov),
                UciCommand::Perft(depth) => self.handle_perft(depth),
                _ => eprintln!("Unexpected uci command"),
            }
        }
    }

    fn handle_ucinewgame(&mut self) {
        let options = self.options;
        *self = Search::new(
            STARTING_POSITION,
            sync::Arc::clone(&self.time_manager.abort),
        );
        self.options = options;
        self.resize_tt(self.options.hash_bits);
    }

    fn handle_uci(&mut self) {
        let options = self.options;
        *self = Search::new(
            STARTING_POSITION,
            sync::Arc::clone(&self.time_manager.abort),
        );
        self.options = options;
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
        self.time_control = params.time_control;
        let mov = self.root();
        println!("bestmove {}", mov.to_algebraic());
    }

    fn handle_position(&mut self, pos: Position, moves: Vec<String>) {
        pos.print("");
        println!("{:?}", moves);
        self.position = pos;
        self.redo_eval();

        for ref mov in &moves {
            let mov = Move::from_algebraic(&self.position, &mov);
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
                    self.resize_tt(u64::from(bits));
                    self.options.hash_bits = u64::from(bits);
                } else {
                    eprintln!("Unable to parse value '{}' as integer", value);
                }
            }
            "showpvboard" => {
                self.options.show_pv_board = value.eq_ignore_ascii_case("true");
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
        println!("Current hash: 0x{:0>64x}", self.hasher.get_hash());
        let tt = self.tt.get(self.hasher.get_hash());
        if let Some(tt) = tt {
            if let Some(best_move) = tt.best_move.expand(&self.position) {
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
        match mov.map(|m| Move::from_algebraic(&self.position, &m)) {
            Some(mov) => {
                let score = self.history.get_score(self.position.white_to_move, mov);
                println!("History score: {}", score);
            }
            None => {
                let mg = MoveGenerator::from(&self.position);
                let mut moves = Vec::new();
                mg.quiet_moves(&mut moves);
                let mut moves = moves
                    .into_iter()
                    .map(|mov| {
                        (
                            mov,
                            self.history.get_score(self.position.white_to_move, mov),
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
        self.perft(depth);
    }
}
