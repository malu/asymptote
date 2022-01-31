/* Asymptote, a UCI chess engine
   Copyright (C) 2018-2020  Maximilian Lupke

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
#[cfg(feature = "fathom")]
use crate::fathom;
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
const LMP_MAX_DEPTH: Depth = 5 * INC_PLY;
const LMP_MOVES: [i16; (LMP_MAX_DEPTH / INC_PLY) as usize] = [0, 4, 8, 16, 32];

#[derive(Clone)]
pub struct Search<'a> {
    pub id: usize,
    position: Position,

    // Required for (efficient) search
    stack: [PlyDetails; MAX_PLY as usize],
    history: History,
    eval: Eval,
    hasher: Hasher,
    tt: &'a TT,
    repetitions: Repetitions,

    // Time Management
    time_control: TimeControl,
    time_manager: TimeManager,

    // Statistics and nice-to-haves
    pub visited_nodes: u64,
    tb_hits: u64,
    max_ply_searched: Ply,
    pv: Vec<Vec<Option<Move>>>,

    // Misc
    options: PersistentOptions,
    quiets: [[Option<Move>; 256]; MAX_PLY as usize],
    lmr: [[Depth; 64]; 64],
}

#[derive(Copy, Clone, Debug, Default)]
pub struct PlyDetails {
    irreversible_details: IrreversibleDetails,
    current_move: Option<Move>,
    pub killers_moves: [Option<Move>; 2],
    exclude_move: Option<Move>,
    hash: Hash,
    pawn_hash: Hash,
}

impl<'a> Search<'a> {
    pub fn new(
        abort: sync::Arc<sync::atomic::AtomicBool>,
        hasher: Hasher,
        options: PersistentOptions,
        position: Position,
        time_control: TimeControl,
        tt: &'a TT,
        repetitions: Repetitions,
    ) -> Search<'a> {
        let mut pv = Vec::with_capacity(MAX_PLY as usize);
        for i in 0..MAX_PLY as usize {
            pv.push(vec![None; MAX_PLY as usize - i + 1]);
        }

        let mut lmr = [[0; 64]; 64];
        for d in 2..64 {
            for m in 1..64 {
                let dd = d as f32;
                let mm = m as f32;
                let rr = 0.25 + dd.ln() * mm.ln() / 2.;
                let r = rr as Depth;
                lmr[d][m] = r * INC_PLY;
            }
        }

        Search {
            id: 0,
            position: position.clone(),

            stack: [PlyDetails::default(); MAX_PLY as usize],
            history: History::default(),
            eval: Eval::from(&position),
            hasher,
            tt,
            repetitions,

            time_control,
            time_manager: TimeManager::new(&position, time_control, options.move_overhead, abort),

            visited_nodes: 0,
            tb_hits: 0,
            max_ply_searched: 0,
            pv,

            options,
            quiets: [[None; 256]; MAX_PLY as usize],
            lmr,
        }
    }

    pub fn prepare_search(&mut self) {
        self.set_time_control(self.time_control);
        self.pv
            .iter_mut()
            .for_each(|pv| pv.iter_mut().for_each(|i| *i = None));
        self.stack[0].hash = self.hasher.get_hash();
        self.stack[0].pawn_hash = self.hasher.get_pawn_hash();
    }

    pub fn iterative_deepening(&mut self) -> Move {
        let mut moves = MoveList::new();
        MoveGenerator::from(&self.position).all_moves(&mut moves);
        let mut moves = moves
            .into_iter()
            .filter(|&mov| self.position.move_is_legal(mov))
            .map(|mov| (mov, 0))
            .collect::<arrayvec::ArrayVec<(Move, i64), 256>>();

        if moves.len() == 1 {
            return moves[0].0;
        }

        #[cfg(feature = "fathom")]
        {
            if self.id == 0 {
                let state = (&self.position).into();
                if let Some(probe_result) = unsafe { fathom::probe_root(&state) } {
                    let dtz = probe_result.dtz as Score;

                    let score;
                    let bound;

                    match probe_result.wdl {
                        fathom::Wdl::Loss => {
                            score = -MATE_SCORE + MAX_PLY + dtz + 1;
                            bound = UPPER_BOUND;
                        }
                        fathom::Wdl::Win => {
                            score = MATE_SCORE - MAX_PLY - dtz - 1;
                            bound = LOWER_BOUND;
                        }
                        _ => {
                            score = 0;
                            bound = EXACT_BOUND;
                        }
                    }

                    let best_move = probe_result.best_move;
                    for (mov, _) in &moves {
                        let from: u8 = mov.from.into();
                        let to: u8 = mov.to.into();
                        let promotion = match mov.promoted {
                            Some(Piece::Bishop) => Some(fathom::PromotionPiece::Bishop),
                            Some(Piece::Knight) => Some(fathom::PromotionPiece::Knight),
                            Some(Piece::Rook) => Some(fathom::PromotionPiece::Rook),
                            Some(Piece::Queen) => Some(fathom::PromotionPiece::Queen),
                            _ => None,
                        };
                        if from as u32 == best_move.from
                            && to as u32 == best_move.to
                            && mov.en_passant == best_move.en_passant
                            && promotion == best_move.promotes
                        {
                            self.uci_info((MAX_PLY - 1) * INC_PLY, score, bound);

                            self.time_manager
                                .abort
                                .store(true, std::sync::atomic::Ordering::SeqCst);

                            return mov.clone();
                        }
                    }
                }
            }
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
        moves: &mut [(Move, i64)],
        depth: Depth,
    ) -> Option<Score> {
        let mut delta = 30;
        let mut alpha = cmp::max(last_score - delta, -MATE_SCORE);
        let mut beta = cmp::min(last_score + delta, MATE_SCORE);

        loop {
            let (score, index) = self.search_root(moves, alpha, beta, depth)?;
            (&mut moves[0..index + 1]).rotate_right(1);

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
            if self.time_manager.elapsed_millis() > 10000 {
                self.uci_curmove_info(i, mov);
            }

            // We already filtered all illegal moves. No need to check for move
            // legality here.
            self.make_move(Some(mov), 0);

            let mut new_depth = depth - INC_PLY;
            if self.position.in_check() {
                new_depth += INC_PLY;
            }

            let num_nodes_before = self.visited_nodes;
            let mut value = Some(Score::max_value());
            if i > 0 {
                value = self.search(1, -alpha - 1, -alpha, new_depth).map(|v| -v);
            }

            if Some(alpha) < value {
                value = self.search(1, -beta, -alpha, new_depth).map(|v| -v);
            }

            *subtree_size = ((self.visited_nodes - num_nodes_before) / 2) as i64;

            self.unmake_move(Some(mov), 0);

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
                    }

                    if value > alpha {
                        alpha = value;
                        increased_alpha = true;
                        self.add_pv_move(mov, 0);
                    }

                    if value >= beta {
                        break;
                    }

                    if value > alpha && self.time_manager.elapsed_millis() > 1000 {
                        self.uci_info(depth, alpha, EXACT_BOUND);
                    }
                }
            }
        }

        Some((best_score, best_move_index))
    }

    pub fn search(&mut self, ply: Ply, alpha: Score, beta: Score, depth: Depth) -> Option<Score> {
        if self.time_manager.should_stop() {
            return None;
        }

        self.visited_nodes += 1;
        self.max_ply_searched = cmp::max(ply, self.max_ply_searched);
        let is_pv = alpha + 1 != beta;

        // Mate distance pruning
        let mdp_alpha = cmp::max(alpha, -MATE_SCORE + ply);
        let mdp_beta = cmp::min(beta, MATE_SCORE - ply - 1);
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

        let has_excluded_move = self.stack[ply as usize].exclude_move.is_some();
        if let Some(mov) = self.stack[ply as usize].exclude_move {
            self.hasher.toggle_singular(mov);
        }
        let hash = self.hasher.get_hash();
        let (mut ttentry, mut ttmove) = self.get_tt_entry(hash);
        if let Some(mov) = self.stack[ply as usize].exclude_move {
            self.hasher.toggle_singular(mov);
        }

        let mut eval = None;
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

            if !is_pv {
                eval = ttentry.get_eval();
            }
        }

        if depth < INC_PLY {
            self.visited_nodes -= 1;
            return self.qsearch(ply, alpha, beta, 0);
        }

        // Look up position from Syzygy tablebases
        //
        // Since those only contain positions which can be the result of a
        // capture, the half move clock has to the zero and there cannot be an
        // en passant square (since that results from a pawn push rather than a
        // capture). Also, the TBs do not contain positions with castling
        // rights, so exclude those as well.
        #[cfg(feature = "fathom")]
        {
            if self.position.details.halfmove == 0
                && self.position.details.en_passant == 255
                && self.position.details.castling == 0
                && !has_excluded_move
            {
                let piece_count = self.position.all_pieces.popcount();
                let max_pieces = unsafe { fathom::max_pieces() };
                if piece_count < max_pieces
                    || piece_count <= max_pieces && depth >= self.options.syzygy_probe_depth
                {
                    let state = (&self.position).into();
                    if let Some(wdl) = unsafe { fathom::probe_wdl(&state) } {
                        self.tb_hits += 1;
                        let value;
                        let bound;
                        match wdl {
                            fathom::Wdl::Loss => {
                                value = -MATE_SCORE + MAX_PLY + ply + 1;
                                bound = UPPER_BOUND;
                            }
                            fathom::Wdl::Win => {
                                value = MATE_SCORE - MAX_PLY - ply - 1;
                                bound = UPPER_BOUND;
                            }
                            _ => {
                                value = 0;
                                bound = EXACT_BOUND;
                            }
                        }

                        if bound == EXACT_BOUND
                            || (bound == UPPER_BOUND && value <= alpha)
                            || (bound == LOWER_BOUND && value >= beta)
                        {
                            self.tt.insert(
                                hash,
                                (MAX_PLY - 1) * INC_PLY,
                                TTScore::from_score(value, ply),
                                None,
                                bound,
                                None,
                            );

                            return Some(value);
                        }
                    }
                }
            }
        }

        if eval.is_none() && !is_pv {
            eval = Some(self.eval.score(&self.position, self.hasher.get_pawn_hash()));
        }

        let previous_move = self.stack[ply as usize - 1].current_move;
        let nullmove_reply = previous_move == None;
        let in_check = !nullmove_reply && self.position.in_check();
        let mut skip_quiets = false;

        if let Some(eval) = eval {
            skip_quiets = !in_check
                && !has_excluded_move
                && depth < 3 * INC_PLY
                && eval + FUTILITY_MARGIN * (depth / INC_PLY) < alpha;

            // Static beta pruning
            //
            // Prune nodes at shallow depth if current evaluation is above beta by
            // a large (depth-dependent) margin.
            if !in_check
                && !has_excluded_move
                && depth < STATIC_BETA_DEPTH
                && eval - STATIC_BETA_MARGIN * (depth / INC_PLY) > beta
            {
                return Some(beta);
            }

            // Nullmove pruning
            //
            // Prune nodes that are so good that we could pass without the opponent
            // catching up.
            if !has_excluded_move && !in_check && self.eval.phase() > 0 && eval >= beta {
                let r = INC_PLY + depth / 4 + cmp::min(2 * INC_PLY, (eval - beta) / 2);
                self.make_move(None, ply);
                let score = self
                    .search(ply + 1, -alpha - 1, -alpha, depth - INC_PLY - r)
                    .map(|v| -v);
                self.unmake_move(None, ply);
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
                self.search(ply, alpha, beta, depth - 2 * INC_PLY);
                self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
                let (ttentry_, ttmove_) = self.get_tt_entry(hash);
                ttentry = ttentry_;
                ttmove = ttmove_;
            } else if !is_pv && depth >= 6 * INC_PLY {
                self.search(ply, alpha, beta, depth / 2);
                let (ttentry_, ttmove_) = self.get_tt_entry(hash);
                ttentry = ttentry_;
                ttmove = ttmove_;
            }
        }

        // ProbCut
        // If a capture or promotion leads to a beta cutoff in a reduced-depth search with increased beta bound, we
        // assume it will also lead to a cutoff in a full-depth search with the original beta bound.
        if !in_check && !is_pv && !has_excluded_move && depth >= 6 * INC_PLY {
            let mut moves = MovePicker::qsearch(&self.position);

            let probcut_beta = beta + 100;

            while let Some((_, mov)) = moves.next(&self.position, &self.history) {
                if !self.position.move_is_legal(mov) {
                    continue;
                }

                self.make_move(Some(mov), ply);
                let value = self
                    .search(
                        ply + 1,
                        -probcut_beta,
                        -probcut_beta + 1,
                        depth - 4 * INC_PLY,
                    )
                    .map(|v| -v);
                self.unmake_move(Some(mov), ply);

                match value {
                    None => return None,
                    Some(value) => {
                        if value >= probcut_beta {
                            return Some(value);
                        }
                    }
                }
            }
        }

        let mut moves = MovePicker::new(
            ttmove,
            self.stack[ply as usize].killers_moves,
            previous_move,
        );

        if let Some(excluded_move) = self.stack[ply as usize].exclude_move {
            moves.add_excluded_move(excluded_move);
        }

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
        let mut num_moves_searched = 0;
        let mut num_quiet_moves_searched = 0;
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
                        // We can skip the remaining quiet moves because quiet moves
                        // are ordered by history score.
                        moves.skip_quiets(true);
                        pruned = true;
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
                        }

                        if mtype == MoveType::Quiet
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

            if let (Some(ttentry), Some(ttmove)) = (ttentry, ttmove) {
                if mtype == MoveType::TTMove
                    && extension < INC_PLY
                    && self.is_singular(ttentry, ttmove, depth, ply)
                {
                    extension += INC_PLY;
                }
            }

            if depth < LMP_MAX_DEPTH
                && !is_pv
                && !in_check
                && !check
                && mtype == MoveType::Quiet
                && best_score > -MATE_SCORE + MAX_PLY
                && num_moves_searched > LMP_MOVES[(depth / INC_PLY) as usize]
            {
                moves.skip_quiets(true);
                pruned = true;
                continue;
            }

            let mut reduction = 0;
            if nullmove_reply && mtype == MoveType::Quiet {
                reduction += INC_PLY;
            }

            if depth >= LMR_DEPTH && mtype == MoveType::Quiet && best_score > -MATE_SCORE + MAX_PLY
            {
                let d = (depth / INC_PLY) as usize;
                let m = num_moves_searched as usize;
                reduction += self.lmr[cmp::min(d, 63)][cmp::min(m, 63)];

                if check {
                    reduction -= INC_PLY;
                }

                if in_check {
                    reduction -= INC_PLY;
                }

                if is_pv {
                    reduction -= INC_PLY;
                }

                // Last two moves are reversible
                if self.position.details.halfmove > 2 && ply >= 2 {
                    if let Some(last_move) = self.stack[ply as usize - 2].current_move {
                        if last_move.from == mov.to && last_move.to == mov.from {
                            reduction += INC_PLY;
                        }
                    }
                }
            };

            extension = cmp::min(extension, INC_PLY);
            let new_depth = depth - INC_PLY + extension;
            reduction = cmp::max(0, cmp::min(reduction, new_depth - INC_PLY));

            self.make_move(Some(mov), ply);

            let mut value = Some(Score::max_value());
            if !(is_pv && num_moves_searched == 0) {
                value = self
                    .search(ply + 1, -alpha - 1, -alpha, new_depth - reduction)
                    .map(|v| -v);
            }

            if Some(alpha) < value && reduction > 0 {
                value = self
                    .search(ply + 1, -alpha - 1, -alpha, new_depth)
                    .map(|v| -v);
            }

            if Some(alpha) < value && is_pv {
                value = self.search(ply + 1, -beta, -alpha, new_depth).map(|v| -v);
            }

            self.unmake_move(Some(mov), ply);

            num_moves_searched += 1;
            if mov.is_quiet() {
                self.quiets[ply as usize][num_quiet_moves_searched] = Some(mov);
                num_quiet_moves_searched += 1;
            }

            match value {
                None => {
                    if is_pv && increased_alpha {
                        self.tt.insert(
                            hash,
                            depth,
                            TTScore::from_score(alpha, ply),
                            best_move,
                            LOWER_BOUND,
                            eval,
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
                            self.update_quiet_stats(mov, ply, depth, num_quiet_moves_searched - 1);
                        }

                        if is_pv {
                            self.pv[ply as usize].iter_mut().for_each(|mov| *mov = None);
                        }

                        break;
                    }
                }
            }
        }

        if best_move.is_none() {
            // No best move => no legal moves
            if pruned {
                return Some(alpha);
            } else if self.position.in_check() {
                return Some(-MATE_SCORE + ply);
            } else {
                // Stalemate
                return Some(0);
            }
        }

        let tt_bound = if best_score >= beta {
            LOWER_BOUND
        } else if increased_alpha {
            EXACT_BOUND
        } else {
            UPPER_BOUND
        };

        self.tt.insert(
            hash,
            depth,
            TTScore::from_score(best_score, ply),
            best_move,
            tt_bound,
            eval,
        );

        Some(best_score)
    }

    fn is_singular(&mut self, ttentry: TTEntry, ttmove: Move, depth: Depth, ply: Ply) -> bool {
        if depth < 8 * INC_PLY {
            return false;
        }

        if ttentry.bound == UPPER_BOUND {
            return false;
        }

        if ttentry.depth + 2 * INC_PLY < depth {
            return false;
        }

        if self.stack[ply as usize].exclude_move.is_some() {
            return false;
        }

        let ttscore = ttentry.score.to_score(ply);
        let alpha = ttscore - 2 * (depth / INC_PLY);
        let beta = alpha + 1;

        self.stack[ply as usize].exclude_move = Some(ttmove);
        let singular_score = self.search(ply, alpha, beta, depth / 2);
        self.stack[ply as usize].exclude_move = None;

        match singular_score {
            None => false,
            Some(score) => score < beta,
        }
    }

    fn qsearch(&mut self, ply: Ply, alpha: Score, beta: Score, depth: Depth) -> Option<Score> {
        if self.time_manager.should_stop() {
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
            let hash = self.hasher.get_hash();
            let (ttentry, _ttmove) = self.get_tt_entry(hash);
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

        let mut moves = MovePicker::qsearch(&self.position);

        let mut best_move = None;
        let mut best_score = -MATE_SCORE;

        let mut num_moves_searched = 0;
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

            self.make_move(Some(mov), ply);

            let value = self
                .qsearch(ply + 1, -beta, -alpha, depth - INC_PLY)
                .map(|v| -v);
            self.unmake_move(Some(mov), ply);

            num_moves_searched += 1;

            match value {
                None => return None,
                Some(score) => {
                    if score > best_score {
                        best_score = score;
                        best_move = Some(mov);
                    }

                    if score > alpha {
                        alpha = score;
                    }

                    if score >= beta {
                        break;
                    }
                }
            }
        }

        if num_moves_searched == 0 {
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

        if depth == 0 {
            let bound = if best_score >= beta {
                LOWER_BOUND
            } else {
                UPPER_BOUND
            };

            self.tt.insert(
                self.hasher.get_hash(),
                0,
                TTScore::from_score(score, ply),
                best_move,
                bound,
                eval,
            );
        }

        Some(score)
    }

    fn get_tt_entry(&mut self, hash: Hash) -> (Option<TTEntry>, Option<Move>) {
        if let Some(ttentry) = self.tt.get(hash) {
            if !ttentry.has_move() {
                return (Some(ttentry), None);
            }

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
        let estimated_tb_hits = self.tb_hits * self.options.threads as u64;
        print!(
            "info depth {} seldepth {} nodes {} nps {} tbhits {} score {} time {} hashfull {} pv ",
            d / INC_PLY,
            self.max_ply_searched,
            estimated_nodes,
            1000 * estimated_nodes / cmp::max(1, elapsed),
            estimated_tb_hits,
            score_str,
            elapsed,
            self.tt.usage(),
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

    fn uci_curmove_info(&self, i: usize, mov: Move) {
        if self.id > 0 {
            return;
        }

        let elapsed = self.time_manager.elapsed_millis();
        let estimated_nodes = self.visited_nodes * self.options.threads as u64;
        let estimated_tb_hits = self.tb_hits * self.options.threads as u64;
        println!(
            "info currmove {} currmovenumber {} nodes {} nps {} tbhits {} time {} hashfull {}",
            mov.to_algebraic(),
            i + 1, // currmovenumber should start at 1
            estimated_nodes,
            1000 * estimated_nodes / cmp::max(1, elapsed),
            estimated_tb_hits,
            elapsed,
            self.tt.usage(),
        );
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
            if last_move.captured.is_some() || last_move.promoted.is_some() {
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

                self.make_move(Some(mov), depth as Ply);
                let perft = self.internal_perft(depth - 1);
                num_moves += perft;
                println!("{}: {}", mov.to_algebraic(), perft);
                self.unmake_move(Some(mov), depth as Ply);
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

            self.make_move(Some(mov), depth as Ply);
            num_moves += self.internal_perft(depth - 1);
            self.unmake_move(Some(mov), depth as Ply);
        }

        num_moves
    }

    fn make_move(&mut self, mov: Option<Move>, ply: Ply) {
        let white_move = self.position.white_to_move;

        let current_ply = &mut self.stack[ply as usize];
        current_ply.irreversible_details = self.position.details;
        current_ply.current_move = mov;

        if ply + 2 < MAX_PLY {
            self.stack[2 + ply as usize].killers_moves = [None; 2];
        }

        if let Some(mov) = mov {
            self.hasher.make_move(&self.position, mov);
            self.eval.make_move(mov, white_move);
            self.position.make_move(mov);
        } else {
            self.hasher.make_nullmove(&self.position);
            self.position.make_nullmove();
        }

        if self.position.details.halfmove == 0 {
            self.repetitions.irreversible_move();
        }
        self.repetitions.push_position(self.hasher.get_hash());

        let next_ply = &mut self.stack[1 + ply as usize];
        next_ply.hash = self.hasher.get_hash();
        next_ply.pawn_hash = self.hasher.get_pawn_hash();
    }

    fn unmake_move(&mut self, mov: Option<Move>, ply: Ply) {
        let white_move = !self.position.white_to_move;

        let prev_ply = &self.stack[ply as usize];
        let irreversible = prev_ply.irreversible_details;

        if let Some(mov) = mov {
            self.eval.unmake_move(mov, white_move);
            self.position.unmake_move(mov, irreversible);
            self.hasher.set(prev_ply.hash, prev_ply.pawn_hash);
        } else {
            self.position.unmake_nullmove(irreversible);
            self.hasher.set(prev_ply.hash, prev_ply.pawn_hash);
        }

        self.repetitions.pop_position();
    }

    pub fn set_time_control(&mut self, tc: TimeControl) {
        self.time_control = tc;
        self.time_manager.update(&self.position, self.time_control);
    }
}
