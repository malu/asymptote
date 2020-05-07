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
use crate::bitboard::*;
#[cfg(feature = "fathom")]
use crate::fathom::BoardState;
use crate::movegen::*;

/// Bit indicating if white can castle kingside.
pub const CASTLE_WHITE_KSIDE: u8 = 0x1;

/// Bit indicating if white can castle queenside.
pub const CASTLE_WHITE_QSIDE: u8 = 0x2;

/// Bit indicating if black can castle kingside.
pub const CASTLE_BLACK_KSIDE: u8 = 0x4;

/// Bit indicating if black can castle queenside.
pub const CASTLE_BLACK_QSIDE: u8 = 0x8;

/// A `Position` holds all information to completely describe a chess position.
///
/// Position does not implement Copy because moving of Copy types always involves a memcpy and we
/// want to avoid that.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Position {
    /// The color of the piece occupying the respective square, if any. A set bit corresponds to
    /// the white side.
    pub color: Bitboard,

    /// Bitboard of each piece type on the board. A set bit means a piece occupies the respective square. Index by `bb[Piece::index()]`.
    pub bb: [Bitboard; 6],

    /// Bitboard of all pieces of a single color. `[Black, White]`.
    pub pieces: [Bitboard; 2],

    /// Whether it is white's tunr to move.
    pub white_to_move: bool,

    /// Number of the current full move. The first moves of white and black belong to the first
    /// full move. Not strictly necessary for correct play.
    pub fullmove: u16,

    /// The irreversible details of thsi position.
    pub details: IrreversibleDetails,

    /// A bitboard of all pieces on the board.
    pub all_pieces: Bitboard,

    /// The squares the [black, white] king is occupying.
    /// Could be calculate from the `pieces` bitboard, but cached here for speed.
    pub king_sq: [Square; 2],
}

/// Some not easily reverted changes in a position.
///
/// Some details (en passant, castling rights and current halfmove clock) whose changes can not
/// be undone easily and therefore are kept in a stack of past values.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub struct IrreversibleDetails {
    /// All pieces currently checking the king.
    pub checkers: Bitboard,

    /// Number of moves of both players since the last capture or pawn moves. Used for checking for
    /// a draw by the 50 moves rule (draw if halfmove = 100 and side to move has at least one legal
    /// move).
    pub halfmove: u8,

    /// This is the file of the target square of a possible en passant capture. If there is no such
    /// capture possible this variable is set to 255.
    pub en_passant: u8,

    /// Possible castling moves for both sides.
    pub castling: u8,
}

impl Position {
    pub fn pawns(&self) -> Bitboard {
        self.bb[Piece::Pawn.index()]
    }

    pub fn knights(&self) -> Bitboard {
        self.bb[Piece::Knight.index()]
    }

    pub fn bishops(&self) -> Bitboard {
        self.bb[Piece::Bishop.index()]
    }

    pub fn rooks(&self) -> Bitboard {
        self.bb[Piece::Rook.index()]
    }

    pub fn queens(&self) -> Bitboard {
        self.bb[Piece::Queen.index()]
    }

    pub fn kings(&self) -> Bitboard {
        self.bb[Piece::King.index()]
    }

    pub fn king_sq(&self, white: bool) -> Square {
        self.king_sq[white as usize]
    }

    pub fn white_pieces(&self) -> Bitboard {
        self.pieces[1]
    }

    pub fn black_pieces(&self) -> Bitboard {
        self.pieces[0]
    }

    pub fn us(&self, white: bool) -> Bitboard {
        self.pieces[white as usize]
    }

    pub fn them(&self, white: bool) -> Bitboard {
        self.pieces[1 - white as usize]
    }

    pub fn see(&self, mov: Move, threshold: i16) -> bool {
        let mut score = mov.captured.map_or(0, Piece::see_value)
            + mov
                .promoted
                .map_or(0, |p| p.see_value() - Piece::Pawn.see_value())
            - threshold;

        // Even if the opponent cannot recapture, this move does not surpass the threshold.
        if score < 0 {
            return false;
        }

        let next_victim = mov.promoted.unwrap_or(mov.piece);
        // Even if the opponent recaptures and we cannot, this move surpasses the threshold.
        if score - next_victim.see_value() >= 0 {
            return true;
        }

        let mut white = !self.white_to_move;
        let mut occupancy = self.all_pieces & !(mov.from.to_bb() | mov.to.to_bb());
        if mov.en_passant {
            occupancy ^= mov.to.backward(self.white_to_move, 1);
        }

        let promotion = mov.to.rank() == 0 || mov.to.rank() == 7;

        let to_bb = mov.to.to_bb();
        let bq = self.bishops() | self.queens();
        let rq = self.rooks() | self.queens();

        let mut attackers = Bitboard::from(0);
        attackers |= (to_bb.left(1) | to_bb.right(1)).backward(false, 1)
            & self.pawns()
            & self.black_pieces();
        attackers |=
            (to_bb.left(1) | to_bb.right(1)).backward(true, 1) & self.pawns() & self.white_pieces();
        attackers |= KNIGHT_ATTACKS[mov.to] & self.knights();
        attackers |= get_bishop_attacks_from(mov.to, occupancy) & bq;
        attackers |= get_rook_attacks_from(mov.to, occupancy) & rq;
        attackers |= KING_ATTACKS[mov.to] & self.kings();
        attackers &= occupancy;

        if next_victim == Piece::King {
            // SEE test is successful if king cannot be recaptured since currently score >= 0 (see above)
            return (self.them(self.white_to_move) & attackers).is_empty();
        }

        let mut next_victim_value = next_victim.see_value();

        while (attackers & self.us(white)).at_least_one() {
            let us = self.us(white) & attackers;
            if !promotion && (us & self.pawns()).at_least_one() {
                score = -score - 1 + next_victim_value;
                next_victim_value = Piece::Pawn.see_value();

                let lsb_bb = (us & self.pawns()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |= get_bishop_attacks_from(mov.to, occupancy) & bq;
            } else if (us & self.knights()).at_least_one() {
                score = -score - 1 + next_victim_value;
                next_victim_value = Piece::Knight.see_value();

                let lsb_bb = (us & self.knights()).lsb_bb();
                occupancy ^= lsb_bb;
            } else if (us & self.bishops()).at_least_one() {
                score = -score - 1 + next_victim_value;
                next_victim_value = Piece::Bishop.see_value();

                let lsb_bb = (us & self.bishops()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |= get_bishop_attacks_from(mov.to, occupancy) & bq;
            } else if (us & self.rooks()).at_least_one() {
                score = -score - 1 + next_victim_value;
                next_victim_value = Piece::Rook.see_value();

                let lsb_bb = (us & self.rooks()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |= get_rook_attacks_from(mov.to, occupancy) & rq;
            } else if promotion && (us & self.pawns()).at_least_one() {
                score = -score - 1 + next_victim_value + Piece::Queen.see_value()
                    - Piece::Pawn.see_value();
                next_victim_value = Piece::Queen.see_value();

                let lsb_bb = (us & self.pawns()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |= get_bishop_attacks_from(mov.to, occupancy) & bq;
            } else if (us & self.queens()).at_least_one() {
                score = -score - 1 + next_victim_value;
                next_victim_value = Piece::Queen.see_value();

                let lsb_bb = (us & self.queens()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |= get_bishop_attacks_from(mov.to, occupancy) & bq
                    | get_rook_attacks_from(mov.to, occupancy) & rq;
            } else if (us & self.kings()).at_least_one() {
                score = -score - 1 + next_victim_value;

                // Do not need to update attackers because we will stop now,
                // either because there are no more captures or the king would
                // be recaptured.

                // Capture wasn't enough or king gets recaptured -> side-to-move lost exchange
                if score < 0 || (self.them(white) & attackers).at_least_one() {
                    return self.white_to_move != white;
                }

                // side-to-move won exchange
                return self.white_to_move == white;
            } else {
                // no more captures
                unreachable!();
            }

            if score < 0 {
                break;
            }

            white = !white;
            attackers &= occupancy;
        }

        // The side which made the last move (which also pushed the score to at
        // least 0) is successful, since the other side cannot recapture.
        self.white_to_move != white
    }

    fn is_attacked(&self, sq: Square) -> bool {
        let them = self.them(self.white_to_move);
        let mg = MoveGenerator::from(self);
        let bishop_attacks: Bitboard =
            get_bishop_attacks_from(sq, self.all_pieces) & (self.bishops() | self.queens()) & them;
        if bishop_attacks.at_least_one() {
            return true;
        }

        let rook_attacks: Bitboard =
            get_rook_attacks_from(sq, self.all_pieces) & (self.rooks() | self.queens()) & them;
        if rook_attacks.at_least_one() {
            return true;
        }

        let knight_attacks: Bitboard = mg.knight_from(sq) & self.knights() & them;
        if knight_attacks.at_least_one() {
            return true;
        }

        let pawn_right: bool = (self.pawns() & them)
            .backward(self.white_to_move, 1)
            .left(1)
            & sq;
        if pawn_right {
            return true;
        }

        let pawn_left: bool = (self.pawns() & them)
            .backward(self.white_to_move, 1)
            .right(1)
            & sq;
        if pawn_left {
            return true;
        }

        if (mg.king_from(sq) & self.kings() & them).at_least_one() {
            return true;
        }

        false
    }

    /// Checks whether the current side to move is in check.
    pub fn in_check(&self) -> bool {
        self.details.checkers.at_least_one()
    }

    pub fn move_is_legal(&self, mov: Move) -> bool {
        let mut all_pieces = self.all_pieces;
        let mut king = self.king_sq(self.white_to_move);
        let mut them = self.them(self.white_to_move) & all_pieces;

        if mov.piece == Piece::King {
            king = mov.to;

            if mov.is_queenside_castle() {
                // Queenside castling
                if self.is_attacked(mov.from) || self.is_attacked(mov.from.left(1)) {
                    return false;
                }

                all_pieces ^= mov.from;
                all_pieces ^= mov.to;

                // Rook movement
                all_pieces ^= mov.to.left(2);
                all_pieces ^= mov.to.right(1);
            } else if mov.is_kingside_castle() {
                // Kingside castling
                if self.is_attacked(mov.from) || self.is_attacked(mov.from.right(1)) {
                    return false;
                }

                all_pieces ^= mov.from;
                all_pieces ^= mov.to;

                // Rook movement
                all_pieces ^= mov.to.right(1);
                all_pieces ^= mov.to.left(1);
            } else {
                all_pieces ^= mov.from;

                if mov.captured.is_none() {
                    all_pieces ^= mov.to;
                } else {
                    them ^= mov.to;
                }
            }
        } else if mov.en_passant {
            all_pieces ^= mov.from;
            all_pieces ^= mov.to;
            all_pieces ^= mov.to.backward(self.white_to_move, 1);
            them ^= mov.to.backward(self.white_to_move, 1);
        } else if mov.captured.is_some() {
            all_pieces ^= mov.from;
            them ^= mov.to;
        } else {
            all_pieces ^= mov.from;
            all_pieces ^= mov.to;
        }

        if (KNIGHT_ATTACKS[king] & them & self.knights()).at_least_one() {
            return false;
        }

        if (KING_ATTACKS[king] & them & self.kings()).at_least_one() {
            return false;
        }

        if (get_bishop_attacks_from(king, all_pieces) & them & (self.queens() | self.bishops()))
            .at_least_one()
        {
            return false;
        }

        if (get_rook_attacks_from(king, all_pieces) & them & (self.queens() | self.rooks()))
            .at_least_one()
        {
            return false;
        }

        let their_pawns = self.pawns() & them;
        if (their_pawns.left(1) | their_pawns.right(1)).backward(self.white_to_move, 1) & king {
            return false;
        }

        true
    }

    pub fn move_will_check(&self, mov: Move) -> bool {
        let us = self.us(self.white_to_move);
        let mut all_pieces = self.all_pieces;
        let mut pawns = self.pawns() & us;
        let mut knights = self.knights() & us;
        let mut bishops = (self.bishops() | self.queens()) & us;
        let mut rooks = (self.rooks() | self.queens()) & us;

        all_pieces ^= mov.from;
        all_pieces |= mov.to;

        match mov.piece {
            Piece::Pawn => {
                pawns ^= mov.from;
                pawns |= mov.to;

                if mov.en_passant {
                    all_pieces ^= mov.to.backward(self.white_to_move, 1);
                }

                if let Some(piece) = mov.promoted {
                    pawns ^= mov.to;
                    match piece {
                        Piece::Knight => knights |= mov.to,
                        Piece::Bishop => bishops |= mov.to,
                        Piece::Rook => rooks |= mov.to,
                        Piece::Queen => {
                            bishops |= mov.to;
                            rooks |= mov.to;
                        }
                        _ => unreachable!("Illegal promotion"),
                    }
                }
            }
            Piece::Knight => {
                knights ^= mov.from;
                knights |= mov.to;
            }
            Piece::Bishop => {
                bishops ^= mov.from;
                bishops |= mov.to;
            }
            Piece::Rook => {
                rooks ^= mov.from;
                rooks |= mov.to;
            }
            Piece::Queen => {
                bishops ^= mov.from;
                bishops |= mov.to;
                rooks ^= mov.from;
                rooks |= mov.to;
            }
            Piece::King => {
                if mov.is_kingside_castle() {
                    rooks ^= mov.to.right(1);
                    rooks |= mov.to.left(1);
                    all_pieces ^= mov.to.right(1);
                    all_pieces |= mov.to.left(1);
                } else if mov.is_queenside_castle() {
                    rooks ^= mov.to.left(2);
                    rooks |= mov.to.right(1);
                    all_pieces ^= mov.to.left(2);
                    all_pieces |= mov.to.right(1);
                }
            }
        }

        let their_king = self.king_sq(!self.white_to_move);
        if (KNIGHT_ATTACKS[their_king] & knights).at_least_one() {
            return true;
        }

        if (get_bishop_attacks_from(their_king, all_pieces) & bishops).at_least_one() {
            return true;
        }

        if (get_rook_attacks_from(their_king, all_pieces) & rooks).at_least_one() {
            return true;
        }

        if (pawns.left(1) | pawns.right(1)).forward(self.white_to_move, 1) & their_king {
            return true;
        }

        false
    }

    /// Applies `mov` to the current board position.
    pub fn make_move(&mut self, mov: Move) {
        let them = self.them(self.white_to_move);
        let rank2 = if self.white_to_move { 1 } else { 6 };
        let rank4 = if self.white_to_move { 3 } else { 4 };

        self.details.en_passant = 255;
        if mov.piece == Piece::Pawn
            && mov.from.rank() == rank2
            && mov.to.rank() == rank4
            && ((them & self.pawns()).left(1) | (them & self.pawns()).right(1)) & mov.to
        {
            self.details.en_passant = mov.from.file();
        }

        self.details.halfmove += 1;

        self.bb[mov.piece.index()] ^= mov.from;

        if let Some(piece) = mov.captured {
            self.details.halfmove = 0;

            if mov.en_passant {
                self.bb[Piece::Pawn.index()] ^= mov.to.backward(self.white_to_move, 1);
                if !self.white_to_move {
                    self.color ^= mov.to.backward(self.white_to_move, 1);
                }
            } else {
                self.bb[piece.index()] ^= mov.to;
                if !self.white_to_move {
                    self.color ^= mov.to;
                }
            }
        }

        if let Some(piece) = mov.promoted {
            self.bb[piece.index()] ^= mov.to;
        } else {
            self.bb[mov.piece.index()] ^= mov.to;
        }

        match mov.piece {
            Piece::Pawn => {
                self.details.halfmove = 0;
            }
            Piece::King => {
                self.king_sq[self.white_to_move as usize] = mov.to;
                if mov.is_kingside_castle() {
                    // castle kingside
                    self.bb[Piece::Rook.index()] ^= mov.to.right(1);
                    self.bb[Piece::Rook.index()] ^= mov.to.left(1);
                    if self.white_to_move {
                        self.color ^= mov.to.right(1);
                        self.color ^= mov.to.left(1);
                    }
                } else if mov.is_queenside_castle() {
                    // castle queenside
                    self.bb[Piece::Rook.index()] ^= mov.to.left(2);
                    self.bb[Piece::Rook.index()] ^= mov.to.right(1);
                    if self.white_to_move {
                        self.color ^= mov.to.left(2);
                        self.color ^= mov.to.right(1);
                    }
                }

                if self.white_to_move {
                    self.details.castling &= !(CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE);
                } else {
                    self.details.castling &= !(CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE);
                }
            }
            _ => {}
        }

        if mov.from == SQUARE_A1 || mov.to == SQUARE_A1 {
            self.details.castling &= !CASTLE_WHITE_QSIDE;
        }

        if mov.from == SQUARE_H1 || mov.to == SQUARE_H1 {
            self.details.castling &= !CASTLE_WHITE_KSIDE;
        }

        if mov.from == SQUARE_A8 || mov.to == SQUARE_A8 {
            self.details.castling &= !CASTLE_BLACK_QSIDE;
        }

        if mov.from == SQUARE_H8 || mov.to == SQUARE_H8 {
            self.details.castling &= !CASTLE_BLACK_KSIDE;
        }

        if self.white_to_move {
            self.color ^= mov.to;
            self.color ^= mov.from;
        } else {
            self.fullmove += 1;
        }

        self.white_to_move = !self.white_to_move;
        self.all_pieces = self.pawns()
            | self.knights()
            | self.bishops()
            | self.rooks()
            | self.queens()
            | self.kings();
        self.pieces[1] = self.all_pieces & self.color;
        self.pieces[0] = self.all_pieces & !self.color;

        self.update_checkers();
    }

    /// Undoes a previously made move (by `self.make_move(mov)`).
    pub fn unmake_move(&mut self, mov: Move, irreversible_details: IrreversibleDetails) {
        self.details = irreversible_details;
        self.white_to_move = !self.white_to_move;
        let unmaking_white_move = self.white_to_move;

        if unmaking_white_move {
            self.color ^= mov.from;
            self.color ^= mov.to;
        } else {
            self.fullmove -= 1;
        }

        self.bb[mov.piece.index()] ^= mov.from;

        if let Some(piece) = mov.captured {
            if mov.en_passant {
                self.bb[Piece::Pawn.index()] ^= mov.to.backward(unmaking_white_move, 1);
                if !unmaking_white_move {
                    self.color ^= mov.to.backward(unmaking_white_move, 1);
                }
            } else {
                self.bb[piece.index()] ^= mov.to;
                if !unmaking_white_move {
                    self.color ^= mov.to;
                }
            }
        }

        if let Some(piece) = mov.promoted {
            self.bb[piece.index()] ^= mov.to;
        } else {
            self.bb[mov.piece.index()] ^= mov.to;
        }

        if mov.piece == Piece::King {
            self.king_sq[unmaking_white_move as usize] = mov.from;
            if mov.is_kingside_castle() {
                self.bb[Piece::Rook.index()] ^= mov.to.right(1);
                self.bb[Piece::Rook.index()] ^= mov.to.left(1);
                if unmaking_white_move {
                    self.color ^= mov.to.right(1);
                    self.color ^= mov.to.left(1);
                }
            } else if mov.is_queenside_castle() {
                self.bb[Piece::Rook.index()] ^= mov.to.left(2);
                self.bb[Piece::Rook.index()] ^= mov.to.right(1);
                if unmaking_white_move {
                    self.color ^= mov.to.left(2);
                    self.color ^= mov.to.right(1);
                }
            }
        }

        self.all_pieces = self.pawns()
            | self.knights()
            | self.bishops()
            | self.rooks()
            | self.queens()
            | self.kings();
        self.pieces[1] = self.all_pieces & self.color;
        self.pieces[0] = self.all_pieces & !self.color;
    }

    /// Applies a null move (no move, just change side to move) allowing one side to make two
    /// consecutive moves.
    pub fn make_nullmove(&mut self) {
        self.white_to_move = !self.white_to_move;
        self.details.checkers = Bitboard::from(0);
        self.details.en_passant = 255;
        self.details.halfmove += 1;
    }

    /// Undoes a previous null move.
    pub fn unmake_nullmove(&mut self, irreversible_details: IrreversibleDetails) {
        self.white_to_move = !self.white_to_move;
        self.details = irreversible_details;
    }

    /// Finds the piece type occupying `at`.
    pub fn find_piece(&self, at: Square) -> Option<Piece> {
        if !(self.all_pieces & at) {
            return None;
        }

        if self.pawns() & at {
            Some(Piece::Pawn)
        } else if self.knights() & at {
            Some(Piece::Knight)
        } else if self.bishops() & at {
            Some(Piece::Bishop)
        } else if self.rooks() & at {
            Some(Piece::Rook)
        } else if self.queens() & at {
            Some(Piece::Queen)
        } else if self.kings() & at {
            Some(Piece::King)
        } else {
            None
        }
    }

    pub fn move_is_pseudo_legal(&self, mov: Move) -> bool {
        let us = self.us(self.white_to_move);

        // Check piece actually belongs to us
        if !(us & mov.from) {
            return false;
        }

        // Check target square is not occupied by us
        if us & mov.to {
            return false;
        }

        // Check the moving piece is correct
        if self.find_piece(mov.from) != Some(mov.piece) {
            return false;
        }

        // Check the captured piece is correct
        if self.find_piece(mov.to) != mov.captured && !mov.en_passant {
            return false;
        }

        // Check for en passant and promotion only when it's a pawn move
        if mov.piece != Piece::Pawn && (mov.en_passant || mov.promoted.is_some()) {
            return false;
        }

        match mov.piece {
            Piece::Pawn => {
                if mov.en_passant {
                    if self.details.en_passant == 255 {
                        return false;
                    }

                    let ep_capturers_rank = 3 + self.white_to_move as u8;
                    let ep_square = Square::file_rank(self.details.en_passant, ep_capturers_rank);
                    let their_pawns = self.pawns() & !us;
                    return mov.to == ep_square.forward(self.white_to_move, 1)
                        && their_pawns & ep_square;
                }

                let mut possible_targets = mov.from.forward(self.white_to_move, 1).to_bb();
                if mov.captured.is_some() {
                    possible_targets |= possible_targets.left(1);
                    possible_targets |= possible_targets.right(1);
                    possible_targets ^= mov.from.forward(self.white_to_move, 1);
                } else {
                    let skip_rank = if self.white_to_move { RANK_3 } else { RANK_6 };
                    possible_targets |= (possible_targets & skip_rank & !self.all_pieces)
                        .forward(self.white_to_move, 1);
                    possible_targets &= !self.all_pieces;
                }

                if !(possible_targets & mov.to) {
                    return false;
                }

                if (RANK_1 | RANK_8) & mov.to {
                    return mov.promoted.is_some();
                }

                true
            }
            Piece::Knight => KNIGHT_ATTACKS[mov.from] & mov.to,
            Piece::Bishop => get_bishop_attacks_from(mov.from, self.all_pieces) & mov.to,
            Piece::Rook => get_rook_attacks_from(mov.from, self.all_pieces) & mov.to,
            Piece::Queen => {
                (get_bishop_attacks_from(mov.from, self.all_pieces)
                    | get_rook_attacks_from(mov.from, self.all_pieces))
                    & mov.to
            }
            Piece::King => {
                if mov.is_kingside_castle() {
                    if self.white_to_move {
                        return (self.details.castling & CASTLE_WHITE_KSIDE) > 0
                            && (self.all_pieces & Bitboard::from(0x00_00_00_00_00_00_00_60))
                                .is_empty()
                            && (self.rooks() & us & SQUARE_H1);
                    } else {
                        return (self.details.castling & CASTLE_BLACK_KSIDE) > 0
                            && (self.all_pieces & Bitboard::from(0x60_00_00_00_00_00_00_00))
                                .is_empty()
                            && (self.rooks() & us & SQUARE_H8);
                    }
                }

                if mov.is_queenside_castle() {
                    if self.white_to_move {
                        return (self.details.castling & CASTLE_WHITE_QSIDE) > 0
                            && (self.all_pieces & Bitboard::from(0x00_00_00_00_00_00_00_0E))
                                .is_empty()
                            && (self.rooks() & us & SQUARE_A1);
                    } else {
                        return (self.details.castling & CASTLE_BLACK_QSIDE) > 0
                            && (self.all_pieces & Bitboard::from(0x0E_00_00_00_00_00_00_00))
                                .is_empty()
                            && (self.rooks() & us & SQUARE_A8);
                    }
                }

                KING_ATTACKS[mov.from] & mov.to
            }
        }
    }

    fn update_checkers(&mut self) {
        let them = self.them(self.white_to_move);
        let king = self.king_sq(self.white_to_move);

        self.details.checkers = Bitboard::from(0);
        self.details.checkers |= (king.to_bb().left(1) | king.to_bb().right(1))
            .forward(self.white_to_move, 1)
            & them
            & self.pawns();
        self.details.checkers |= KNIGHT_ATTACKS[king] & them & self.knights();
        self.details.checkers |= get_bishop_attacks_from(king, self.all_pieces)
            & them
            & (self.bishops() | self.queens());
        self.details.checkers |=
            get_rook_attacks_from(king, self.all_pieces) & them & (self.rooks() | self.queens());
    }

    /// Prints the board state.
    pub fn print(&self, pre: &str) {
        println!("{}     a b c d e f g h", pre);
        println!("{}   +-----------------+", pre);
        for rank in 0..8 {
            print!("{} {} | ", pre, 8 - rank);
            for file in 0..8 {
                let sq = Square::file_rank(file, 7 - rank);
                match self.find_piece(sq) {
                    Some(Piece::Pawn) => {
                        if self.color & sq {
                            print!("P ");
                        } else {
                            print!("p ");
                        }
                    }
                    Some(Piece::Knight) => {
                        if self.color & sq {
                            print!("N ");
                        } else {
                            print!("n ");
                        }
                    }
                    Some(Piece::Bishop) => {
                        if self.color & sq {
                            print!("B ");
                        } else {
                            print!("b ");
                        }
                    }
                    Some(Piece::Rook) => {
                        if self.color & sq {
                            print!("R ");
                        } else {
                            print!("r ");
                        }
                    }
                    Some(Piece::Queen) => {
                        if self.color & sq {
                            print!("Q ");
                        } else {
                            print!("q ");
                        }
                    }
                    Some(Piece::King) => {
                        if self.color & sq {
                            print!("K ");
                        } else {
                            print!("k ");
                        }
                    }
                    None => {
                        if self.color & sq {
                            print!("# ");
                        } else if (rank + file) % 2 == 1 {
                            print!(". ");
                        } else {
                            print!("  ");
                        }
                    }
                }
            }
            if 8 - rank == 1 {
                if self.white_to_move {
                    println!("|  White to move");
                } else {
                    println!("|  Black to move");
                }
            } else if 8 - rank == 5 {
                println!("|  Castling rights:");
            } else if 8 - rank == 4 {
                print!("|  ");
                if self.details.castling & CASTLE_WHITE_KSIDE > 0 {
                    print!("K");
                }

                if self.details.castling & CASTLE_WHITE_QSIDE > 0 {
                    print!("Q");
                }

                if self.details.castling & CASTLE_BLACK_KSIDE > 0 {
                    print!("k");
                }

                if self.details.castling & CASTLE_BLACK_QSIDE > 0 {
                    print!("q");
                }

                println!();
            } else {
                println!("|");
            }
        }
        println!("{}   +-----------------+", pre);
    }
}

impl<'a> From<&'a str> for Position {
    fn from(fen: &'a str) -> Position {
        let mut pos = Position {
            color: Bitboard::from(0x0),
            bb: [Bitboard::from(0x0); 6],
            pieces: [Bitboard::from(0x0); 2],
            details: IrreversibleDetails {
                checkers: Bitboard::from(0),
                en_passant: 255,
                castling: CASTLE_WHITE_KSIDE
                    | CASTLE_WHITE_QSIDE
                    | CASTLE_BLACK_KSIDE
                    | CASTLE_BLACK_QSIDE,
                halfmove: 0,
            },
            white_to_move: true,
            fullmove: 1,

            all_pieces: Bitboard::from(0x0),

            // probably wrong but need to initialize value
            king_sq: [SQUARE_E8, SQUARE_E1],
        };

        let mut split = fen.split(' ').filter(|s| !s.is_empty());

        let mut file = 0;
        let mut rank = 7;
        for c in split.next().unwrap().chars() {
            let sq;
            let piece;
            let white;
            match c {
                'P' => {
                    piece = Piece::Pawn;
                    white = true;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'N' => {
                    piece = Piece::Knight;
                    white = true;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'B' => {
                    piece = Piece::Bishop;
                    white = true;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'R' => {
                    piece = Piece::Rook;
                    white = true;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'Q' => {
                    piece = Piece::Queen;
                    white = true;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'K' => {
                    piece = Piece::King;
                    white = true;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'p' => {
                    piece = Piece::Pawn;
                    white = false;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'n' => {
                    piece = Piece::Knight;
                    white = false;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'b' => {
                    piece = Piece::Bishop;
                    white = false;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'r' => {
                    piece = Piece::Rook;
                    white = false;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'q' => {
                    piece = Piece::Queen;
                    white = false;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                'k' => {
                    piece = Piece::King;
                    white = false;
                    sq = Square::file_rank(file, rank);
                    file += 1;
                }
                '/' => {
                    file = 0;
                    rank -= 1;
                    continue;
                }
                '1' => {
                    file += 1;
                    continue;
                }
                '2' => {
                    file += 2;
                    continue;
                }
                '3' => {
                    file += 3;
                    continue;
                }
                '4' => {
                    file += 4;
                    continue;
                }
                '5' => {
                    file += 5;
                    continue;
                }
                '6' => {
                    file += 6;
                    continue;
                }
                '7' => {
                    file += 7;
                    continue;
                }
                '8' => {
                    file += 8;
                    continue;
                }
                x => {
                    panic!("Unexpected character in fen position: {}", x);
                }
            }

            pos.bb[piece.index()] ^= sq;
            pos.pieces[white as usize] ^= sq;
            pos.color = pos.pieces[1];
        }

        pos.all_pieces = pos.white_pieces() | pos.black_pieces();

        if split.next().unwrap() == "b" {
            pos.white_to_move = false;
        }

        pos.details.castling = 0;
        for c in split.next().unwrap().chars() {
            match c {
                '-' => break,
                'K' => pos.details.castling |= CASTLE_WHITE_KSIDE,
                'Q' => pos.details.castling |= CASTLE_WHITE_QSIDE,
                'k' => pos.details.castling |= CASTLE_BLACK_KSIDE,
                'q' => pos.details.castling |= CASTLE_BLACK_QSIDE,
                x => panic!("Unexpected character in fen castling: {}", x),
            }
        }

        if let Some(en_passant_sq) = split.next() {
            if en_passant_sq != "-" {
                pos.details.en_passant = match en_passant_sq.chars().nth(0) {
                    Some('a') => 0,
                    Some('b') => 1,
                    Some('c') => 2,
                    Some('d') => 3,
                    Some('e') => 4,
                    Some('f') => 5,
                    Some('g') => 6,
                    Some('h') => 7,
                    Some(x) => panic!("Unexpected character in fen en passant: {}", x),
                    None => panic!("Expected character for fen en passant"),
                }
            }
        }

        let halfmove: u8 = split.next().and_then(|n| n.parse().ok()).unwrap_or(0);
        let fullmove: u16 = split.next().and_then(|n| n.parse().ok()).unwrap_or(1);

        pos.details.halfmove = halfmove;
        pos.fullmove = fullmove;

        pos.king_sq[0] = (pos.kings() & pos.black_pieces()).squares().next().unwrap();
        pos.king_sq[1] = (pos.kings() & pos.white_pieces()).squares().next().unwrap();

        pos.update_checkers();

        pos
    }
}

#[cfg(feature = "fathom")]
impl Into<BoardState> for &Position {
    fn into(self) -> BoardState {
        let en_passant = if self.details.en_passant == 255 {
            0
        } else if self.white_to_move {
            5 * 8 + self.details.en_passant
        } else {
            // black to move
            3 * 8 + self.details.en_passant
        };

        BoardState {
            white: self.white_pieces().0,
            black: self.black_pieces().0,
            kings: self.kings().0,
            queens: self.queens().0,
            rooks: self.rooks().0,
            bishops: self.bishops().0,
            knights: self.knights().0,
            pawns: self.pawns().0,
            halfmove_clock: self.details.halfmove as u32,
            castling: self.details.castling as u32, // castling bits coincide luckily
            en_passant: en_passant as u32,
            white_to_move: self.white_to_move,
        }
    }
}

/// The starting position in standadrd chess.
pub const STARTING_POSITION: Position = Position {
    color: STARTING_COLOR,
    bb: [
        STARTING_PAWNS,
        STARTING_KNIGHTS,
        STARTING_BISHOPS,
        STARTING_ROOKS,
        STARTING_QUEENS,
        STARTING_KINGS,
    ],
    pieces: [STARTING_BLACK, STARTING_COLOR],
    details: IrreversibleDetails {
        checkers: Bitboard(0),
        en_passant: 255,
        castling: CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE | CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE,
        halfmove: 0,
    },
    white_to_move: true,
    fullmove: 1,

    all_pieces: STARTING_ALL,

    king_sq: [SQUARE_E8, SQUARE_E1],
};

#[cfg(test)]
mod tests {
    use crate::position::*;
    #[test]
    fn test_parse_start_fen() {
        let start_by_fen =
            Position::from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(start_by_fen, STARTING_POSITION);
    }
}
