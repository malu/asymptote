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
use bitboard::*;
use eval::*;
use movegen::*;

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

    /// Bitboard of all pawns on the board. A set bit means a pawn occupies the respective square.
    pub pawns: Bitboard,

    /// Bitboard of all bishops on the board. A set bit means a bishop occupies the respective square.
    pub bishops: Bitboard,

    /// Bitboard of all knights on the board. A set bit means a knight occupies the respective square.
    pub knights: Bitboard,

    /// Bitboard of all rooks on the board. A set bit means a rook occupies the respective square.
    pub rooks: Bitboard,

    /// Bitboard of all queens on the board. A set bit means a queen occupies the respective square.
    pub queens: Bitboard,

    /// Bitboard of all kings on the board. A set bit means a king occupies the respective square.
    pub kings: Bitboard,

    /// Whether it is white's tunr to move.
    pub white_to_move: bool,

    /// Number of the current full move. The first moves of white and black belong to the first
    /// full move. Not strictly necessary for correct play.
    pub fullmove: usize,

    /// The irreversible details of thsi position.
    pub details: IrreversibleDetails,

    /// A bitboard of all pieces on the board.
    pub all_pieces: Bitboard,

    /// A bitboard of all white pieces on the board.
    pub white_pieces: Bitboard,

    /// A bitboard of all black pieces on the board.
    pub black_pieces: Bitboard,
}

/// Some not easily reverted changes in a position.
///
/// Some details (en passant, castling rights and current halfmove clock) whose changes can not
/// be undone easily and therefore are kept in a stack of past values.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct IrreversibleDetails {
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
    /// Static exchange evaluation
    pub fn see(&mut self, mov: Move) -> i16 {
        let ep = self.details.en_passant;
        self.make_capture(mov);
        let piece_value = match mov.captured {
            Some(Piece::Pawn) => PAWN_SCORE,
            Some(Piece::Knight) => KNIGHT_SCORE,
            Some(Piece::Bishop) => BISHOP_SCORE,
            Some(Piece::Rook) => ROOK_SCORE,
            Some(Piece::Queen) => QUEEN_SCORE,
            Some(Piece::King) => 10000,
            None => 0,
        };
        let piece_after_move = mov.promoted.unwrap_or(mov.piece);
        let value = piece_value - self.see_square(mov.to, piece_after_move);
        self.unmake_capture(mov);
        self.details.en_passant = ep;

        value
    }

    fn see_square(&mut self, sq: Square, occupier: Piece) -> i16 {
        let mut value = 0;
        let captures = self.get_cheapest_captures(sq);

        let capture_value = match occupier {
            Piece::Pawn => 100,
            Piece::Knight => 300,
            Piece::Bishop => 320,
            Piece::Rook => 500,
            Piece::Queen => 900,
            Piece::King => 10000,
        };

        let ep = self.details.en_passant;
        for capture in captures {
            self.make_capture(capture);

            value = ::std::cmp::max(value, capture_value - self.see_square(sq, capture.piece));
            self.unmake_capture(capture);
            self.details.en_passant = ep;

            if value >= capture_value {
                break;
            }
        }

        value
    }

    fn make_capture(&mut self, mov: Move) {
        match mov.captured {
            Some(Piece::Pawn) => {
                if mov.en_passant {
                    self.pawns ^= mov.to.backward(self.white_to_move, 1);
                    if !self.white_to_move {
                        self.color ^= mov.to.backward(self.white_to_move, 1)
                    }
                } else {
                    self.pawns ^= mov.to;
                }
            }
            Some(Piece::Knight) => {
                self.knights ^= mov.to;
            }
            Some(Piece::Bishop) => {
                self.bishops ^= mov.to;
            }
            Some(Piece::Rook) => {
                self.rooks ^= mov.to;
            }
            Some(Piece::Queen) => {
                self.queens ^= mov.to;
            }
            Some(Piece::King) => {
                self.kings ^= mov.to;
            }
            _ => {}
        }

        match mov.piece {
            Piece::Pawn => {
                self.pawns ^= mov.from;
                self.details.halfmove = 0;
                match mov.promoted {
                    Some(Piece::Queen) => {
                        self.queens |= mov.to;
                    }
                    Some(Piece::Knight) => {
                        self.knights |= mov.to;
                    }
                    Some(Piece::Rook) => {
                        self.rooks |= mov.to;
                    }
                    Some(Piece::Bishop) => {
                        self.bishops |= mov.to;
                    }
                    Some(x) => {
                        panic!("Invalid promotion: {:?}", x);
                    }
                    None => {
                        self.pawns |= mov.to;
                        if mov.from.forward(self.white_to_move, 2) == mov.to {
                            self.details.en_passant = mov.from.file();
                        }
                    }
                }
            }
            Piece::Knight => {
                self.knights ^= mov.from;
                self.knights |= mov.to;
            }
            Piece::Bishop => {
                self.bishops ^= mov.from;
                self.bishops |= mov.to;
            }
            Piece::Rook => {
                self.rooks ^= mov.from;
                self.rooks |= mov.to;
            }
            Piece::Queen => {
                self.queens ^= mov.from;
                self.queens |= mov.to;
            }
            Piece::King => {
                self.kings ^= mov.from;
                self.kings |= mov.to;
            }
        }

        if self.white_to_move {
            self.color |= mov.to;
            self.color ^= mov.from;
        } else if self.color & mov.to {
            self.color ^= mov.to;
        }
        self.white_to_move = !self.white_to_move;
        self.all_pieces =
            self.pawns | self.knights | self.bishops | self.rooks | self.queens | self.kings;
        self.white_pieces = self.all_pieces & self.color;
        self.black_pieces = self.all_pieces & !self.white_pieces;
    }

    fn unmake_capture(&mut self, mov: Move) {
        self.white_to_move = !self.white_to_move;
        let unmaking_white_move = self.white_to_move;

        if unmaking_white_move {
            self.color |= mov.from;
            self.color ^= mov.to;
        }

        match mov.piece {
            Piece::Pawn => {
                self.pawns |= mov.from;
                match mov.promoted {
                    Some(Piece::Queen) => {
                        self.queens ^= mov.to;
                    }
                    Some(Piece::Knight) => {
                        self.knights ^= mov.to;
                    }
                    Some(Piece::Rook) => {
                        self.rooks ^= mov.to;
                    }
                    Some(Piece::Bishop) => {
                        self.bishops ^= mov.to;
                    }
                    Some(x) => {
                        panic!("Invalid promotion: {:?}", x);
                    }
                    None => {
                        self.pawns ^= mov.to;
                    }
                }
            }
            Piece::Knight => {
                self.knights ^= mov.to;
                self.knights |= mov.from;
            }
            Piece::Bishop => {
                self.bishops ^= mov.to;
                self.bishops |= mov.from;
            }
            Piece::Rook => {
                self.rooks ^= mov.to;
                self.rooks |= mov.from;
            }
            Piece::Queen => {
                self.queens ^= mov.to;
                self.queens |= mov.from;
            }
            Piece::King => {
                self.kings ^= mov.to;
                self.kings |= mov.from;
            }
        }

        match mov.captured {
            Some(Piece::Pawn) => {
                if mov.en_passant {
                    self.pawns |= mov.to.backward(unmaking_white_move, 1);
                    if !unmaking_white_move {
                        self.color |= mov.to.backward(unmaking_white_move, 1);
                    }
                } else {
                    self.pawns |= mov.to;
                    if !unmaking_white_move {
                        self.color |= mov.to;
                    }
                }
            }
            Some(Piece::Knight) => {
                self.knights |= mov.to;
                if !unmaking_white_move {
                    self.color |= mov.to;
                }
            }
            Some(Piece::Bishop) => {
                self.bishops |= mov.to;
                if !unmaking_white_move {
                    self.color |= mov.to;
                }
            }
            Some(Piece::Rook) => {
                self.rooks |= mov.to;
                if !unmaking_white_move {
                    self.color |= mov.to;
                }
            }
            Some(Piece::Queen) => {
                self.queens |= mov.to;
                if !unmaking_white_move {
                    self.color |= mov.to;
                }
            }
            Some(Piece::King) => {
                self.kings |= mov.to;
                if !unmaking_white_move {
                    self.color |= mov.to;
                }
            }
            _ => {}
        }

        self.all_pieces =
            self.pawns | self.knights | self.bishops | self.rooks | self.queens | self.kings;
        self.white_pieces = self.all_pieces & self.color;
        self.black_pieces = self.all_pieces & !self.white_pieces;
    }

    fn get_cheapest_captures(&mut self, sq: Square) -> Vec<Move> {
        let mg = MoveGenerator::from(self as &Position);
        let mut captures = Vec::with_capacity(8);
        let targets = sq.to_bb();
        mg.pawn(targets, &mut captures);
        if !captures.is_empty() {
            return captures;
        }

        mg.knight(targets, &mut captures);
        if !captures.is_empty() {
            return captures;
        }

        mg.bishop(targets, &mut captures);
        if !captures.is_empty() {
            return captures;
        }

        mg.rook(targets, &mut captures);
        if !captures.is_empty() {
            return captures;
        }

        mg.queen(targets, &mut captures);
        if !captures.is_empty() {
            return captures;
        }

        let us = if self.white_to_move {
            self.white_pieces
        } else {
            self.black_pieces
        };
        if !(self.kings & us).is_empty() {
            mg.king(targets, &mut captures);
        }
        captures
    }

    fn is_attacked(&self, sq: Square) -> bool {
        let them = if self.white_to_move {
            self.black_pieces
        } else {
            self.white_pieces
        };

        let mg = MoveGenerator::from(self);
        let bishop_attacks: Bitboard =
            get_bishop_attacks_from(sq, self.all_pieces) & (self.bishops | self.queens) & them;
        if !bishop_attacks.is_empty() {
            return true;
        }

        let rook_attacks: Bitboard =
            get_rook_attacks_from(sq, self.all_pieces) & (self.rooks | self.queens) & them;
        if !rook_attacks.is_empty() {
            return true;
        }

        let knight_attacks: Bitboard = mg.knight_from(sq) & self.knights & them;
        if !knight_attacks.is_empty() {
            return true;
        }

        let pawn_right: bool = (self.pawns & them).backward(self.white_to_move, 1).left(1) & sq;
        if pawn_right {
            return true;
        }

        let pawn_left: bool = (self.pawns & them).backward(self.white_to_move, 1).right(1) & sq;
        if pawn_left {
            return true;
        }

        if !(mg.king_from(sq) & self.kings & them).is_empty() {
            return true;
        }

        false
    }

    /// Checks whether the current side to move is in check.
    pub fn in_check(&self) -> bool {
        let us = if self.white_to_move {
            self.white_pieces
        } else {
            self.black_pieces
        };
        let king = (us & self.kings).squares().nth(0).unwrap();
        self.is_attacked(king)
    }

    /// Checks whether `prev_move` (which has to be already made by `self.make_move(prev_move)`) was
    /// legal.
    pub fn move_was_legal(&mut self, prev_move: Move) -> bool {
        self.white_to_move = !self.white_to_move;

        if self.in_check() {
            self.white_to_move = !self.white_to_move;
            return false;
        }

        if prev_move.piece == Piece::King && prev_move.to.0 == prev_move.from.0 + 2 {
            // kside castling
            let legal =
                !self.is_attacked(prev_move.from) && !self.is_attacked(prev_move.from.right(1));
            self.white_to_move = !self.white_to_move;
            return legal;
        } else if prev_move.piece == Piece::King && prev_move.from.0 == prev_move.to.0 + 2 {
            // qside castling
            let legal =
                !self.is_attacked(prev_move.from) && !self.is_attacked(prev_move.from.left(1));
            self.white_to_move = !self.white_to_move;
            return legal;
        }

        self.white_to_move = !self.white_to_move;
        true
    }

    /// Applies `mov` to the current board position.
    pub fn make_move(&mut self, mov: Move) {
        let them = if self.white_to_move {
            self.black_pieces
        } else {
            self.white_pieces
        };
        let rank2 = if self.white_to_move { RANK_2 } else { RANK_7 };
        let rank4 = if self.white_to_move { RANK_4 } else { RANK_5 };

        self.details.en_passant = 255;
        if self.pawns & rank2 & mov.from
            && rank4 & mov.to
            && ((them & self.pawns).left(1) | (them & self.pawns).right(1)) & mov.to
        {
            self.details.en_passant = mov.from.file();
        }

        self.details.halfmove += 1;

        match mov.captured {
            Some(Piece::Pawn) => {
                if mov.en_passant {
                    self.pawns ^= mov.to.backward(self.white_to_move, 1);
                    if !self.white_to_move {
                        self.color ^= mov.to.backward(self.white_to_move, 1)
                    }
                } else {
                    self.pawns ^= mov.to;
                }
                self.details.halfmove = 0;
            }
            Some(Piece::Knight) => {
                self.knights ^= mov.to;
                self.details.halfmove = 0;
            }
            Some(Piece::Bishop) => {
                self.bishops ^= mov.to;
                self.details.halfmove = 0;
            }
            Some(Piece::Rook) => {
                self.rooks ^= mov.to;
                self.details.halfmove = 0;
            }
            Some(Piece::Queen) => {
                self.queens ^= mov.to;
                self.details.halfmove = 0;
            }
            _ => {}
        }

        match mov.piece {
            Piece::Pawn => {
                self.pawns ^= mov.from;
                self.details.halfmove = 0;
                match mov.promoted {
                    Some(Piece::Queen) => {
                        self.queens |= mov.to;
                    }
                    Some(Piece::Knight) => {
                        self.knights |= mov.to;
                    }
                    Some(Piece::Rook) => {
                        self.rooks |= mov.to;
                    }
                    Some(Piece::Bishop) => {
                        self.bishops |= mov.to;
                    }
                    Some(x) => {
                        panic!("Invalid promotion: {:?}", x);
                    }
                    None => {
                        self.pawns |= mov.to;
                    }
                }
            }
            Piece::Knight => {
                self.knights ^= mov.from;
                self.knights |= mov.to;
            }
            Piece::Bishop => {
                self.bishops ^= mov.from;
                self.bishops |= mov.to;
            }
            Piece::Rook => {
                self.rooks ^= mov.from;
                self.rooks |= mov.to;

                if self.white_to_move {
                    if mov.from.0 == 0 {
                        // A1
                        self.details.castling &= !CASTLE_WHITE_QSIDE;
                    } else if mov.from.0 == 7 {
                        // H1
                        self.details.castling &= !CASTLE_WHITE_KSIDE;
                    }
                } else {
                    if mov.from.0 == 56 {
                        // A8
                        self.details.castling &= !CASTLE_BLACK_QSIDE;
                    } else if mov.from.0 == 63 {
                        // H8
                        self.details.castling &= !CASTLE_BLACK_KSIDE;
                    }
                }
            }
            Piece::Queen => {
                self.queens ^= mov.from;
                self.queens |= mov.to;
            }
            Piece::King => {
                self.kings ^= mov.from;
                self.kings |= mov.to;

                if mov.to.0 == mov.from.0 + 2 {
                    // castle kingside
                    self.rooks ^= mov.to.right(1);
                    self.rooks |= mov.to.left(1);
                    if self.white_to_move {
                        self.color ^= mov.to.right(1);
                        self.color |= mov.to.left(1);
                    }
                } else if mov.from.0 == mov.to.0 + 2 {
                    // castle queenside
                    self.rooks ^= mov.to.left(2);
                    self.rooks |= mov.to.right(1);
                    if self.white_to_move {
                        self.color ^= mov.to.left(2);
                        self.color |= mov.to.right(1);
                    }
                }

                if self.white_to_move {
                    self.details.castling &= !(CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE);
                } else {
                    self.details.castling &= !(CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE);
                }
            }
        }

        if self.white_to_move {
            self.color |= mov.to;
            self.color ^= mov.from;
        } else {
            if self.color & mov.to {
                self.color ^= mov.to;
            }

            self.fullmove += 1;
        }
        self.white_to_move = !self.white_to_move;
        self.all_pieces =
            self.pawns | self.knights | self.bishops | self.rooks | self.queens | self.kings;
        self.white_pieces = self.all_pieces & self.color;
        self.black_pieces = self.all_pieces & !self.white_pieces;
    }

    /// Undoes a previously made move (by `self.make_move(mov)`).
    pub fn unmake_move(&mut self, mov: Move, irreversible_details: IrreversibleDetails) {
        self.details = irreversible_details;
        self.white_to_move = !self.white_to_move;
        let unmaking_white_move = self.white_to_move;

        if unmaking_white_move {
            self.color |= mov.from;
            self.color ^= mov.to;
        } else {
            self.fullmove -= 1;
        }

        match mov.piece {
            Piece::Pawn => {
                self.pawns |= mov.from;
                match mov.promoted {
                    Some(Piece::Queen) => {
                        self.queens ^= mov.to;
                    }
                    Some(Piece::Knight) => {
                        self.knights ^= mov.to;
                    }
                    Some(Piece::Rook) => {
                        self.rooks ^= mov.to;
                    }
                    Some(Piece::Bishop) => {
                        self.bishops ^= mov.to;
                    }
                    Some(x) => {
                        panic!("Invalid promotion: {:?}", x);
                    }
                    None => {
                        self.pawns ^= mov.to;
                    }
                }
            }
            Piece::Knight => {
                self.knights ^= mov.to;
                self.knights |= mov.from;
            }
            Piece::Bishop => {
                self.bishops ^= mov.to;
                self.bishops |= mov.from;
            }
            Piece::Rook => {
                self.rooks ^= mov.to;
                self.rooks |= mov.from;
            }
            Piece::Queen => {
                self.queens ^= mov.to;
                self.queens |= mov.from;
            }
            Piece::King => {
                self.kings ^= mov.to;
                self.kings |= mov.from;

                if mov.to.0 == mov.from.0 + 2 {
                    // castle kingside
                    self.rooks |= mov.to.right(1);
                    self.rooks ^= mov.to.left(1);
                    if unmaking_white_move {
                        self.color |= mov.to.right(1);
                        self.color ^= mov.to.left(1);
                    }
                } else if mov.from.0 == mov.to.0 + 2 {
                    // castle queenside
                    self.rooks |= mov.to.left(2);
                    self.rooks ^= mov.to.right(1);
                    if unmaking_white_move {
                        self.color |= mov.to.left(2);
                        self.color ^= mov.to.right(1);
                    }
                }
            }
        }

        match mov.captured {
            Some(Piece::Pawn) => {
                if mov.en_passant {
                    self.pawns |= mov.to.backward(unmaking_white_move, 1);
                    if !unmaking_white_move {
                        self.color |= mov.to.backward(unmaking_white_move, 1);
                    }
                } else {
                    self.pawns |= mov.to;
                    if !unmaking_white_move {
                        self.color |= mov.to;
                    }
                }
            }
            Some(Piece::Knight) => {
                self.knights |= mov.to;
                if !unmaking_white_move {
                    self.color |= mov.to;
                }
            }
            Some(Piece::Bishop) => {
                self.bishops |= mov.to;
                if !unmaking_white_move {
                    self.color |= mov.to;
                }
            }
            Some(Piece::Rook) => {
                self.rooks |= mov.to;
                if !unmaking_white_move {
                    self.color |= mov.to;
                }
            }
            Some(Piece::Queen) => {
                self.queens |= mov.to;
                if !unmaking_white_move {
                    self.color |= mov.to;
                }
            }
            _ => {}
        }

        self.all_pieces =
            self.pawns | self.knights | self.bishops | self.rooks | self.queens | self.kings;
        self.white_pieces = self.all_pieces & self.color;
        self.black_pieces = self.all_pieces & !self.white_pieces;
    }

    /// Applies a null move (no move, just change side to move) allowing one side to make two
    /// consecutive moves.
    pub fn make_nullmove(&mut self) {
        self.white_to_move = !self.white_to_move;
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
        if self.pawns & at {
            Some(Piece::Pawn)
        } else if self.knights & at {
            Some(Piece::Knight)
        } else if self.bishops & at {
            Some(Piece::Bishop)
        } else if self.rooks & at {
            Some(Piece::Rook)
        } else if self.queens & at {
            Some(Piece::Queen)
        } else if self.kings & at {
            Some(Piece::King)
        } else {
            None
        }
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
            pawns: Bitboard::from(0x0),
            bishops: Bitboard::from(0x0),
            knights: Bitboard::from(0x0),
            rooks: Bitboard::from(0x0),
            queens: Bitboard::from(0x0),
            kings: Bitboard::from(0x0),
            details: IrreversibleDetails {
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
            white_pieces: Bitboard::from(0x0),
            black_pieces: Bitboard::from(0x0),
        };

        let mut split = fen.split(' ');
        assert!(split.next() == Some("fen"));

        let mut file = 0;
        let mut rank = 7;
        for c in split.next().unwrap().chars() {
            let sq = Square::file_rank(file, rank);
            match c {
                'P' => {
                    pos.pawns |= sq;
                    pos.color |= sq;
                    pos.white_pieces |= sq;
                    file += 1;
                }
                'N' => {
                    pos.knights |= sq;
                    pos.color |= sq;
                    pos.white_pieces |= sq;
                    file += 1;
                }
                'B' => {
                    pos.bishops |= sq;
                    pos.color |= sq;
                    pos.white_pieces |= sq;
                    file += 1;
                }
                'R' => {
                    pos.rooks |= sq;
                    pos.color |= sq;
                    pos.white_pieces |= sq;
                    file += 1;
                }
                'Q' => {
                    pos.queens |= sq;
                    pos.color |= sq;
                    pos.white_pieces |= sq;
                    file += 1;
                }
                'K' => {
                    pos.kings |= sq;
                    pos.color |= sq;
                    pos.white_pieces |= sq;
                    file += 1;
                }
                'p' => {
                    pos.pawns |= sq;
                    pos.black_pieces |= sq;
                    file += 1;
                }
                'n' => {
                    pos.knights |= sq;
                    pos.black_pieces |= sq;
                    file += 1;
                }
                'b' => {
                    pos.bishops |= sq;
                    pos.black_pieces |= sq;
                    file += 1;
                }
                'r' => {
                    pos.rooks |= sq;
                    pos.black_pieces |= sq;
                    file += 1;
                }
                'q' => {
                    pos.queens |= sq;
                    pos.black_pieces |= sq;
                    file += 1;
                }
                'k' => {
                    pos.kings |= sq;
                    pos.black_pieces |= sq;
                    file += 1;
                }
                '/' => {
                    file = 0;
                    rank -= 1;
                }
                '1' => {
                    file += 1;
                }
                '2' => {
                    file += 2;
                }
                '3' => {
                    file += 3;
                }
                '4' => {
                    file += 4;
                }
                '5' => {
                    file += 5;
                }
                '6' => {
                    file += 6;
                }
                '7' => {
                    file += 7;
                }
                '8' => {
                    file += 8;
                }
                x => {
                    panic!("Unexpected character in fen position: {}", x);
                }
            }
        }

        pos.all_pieces = pos.white_pieces | pos.black_pieces;

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

        let halfmove: u8 = split.next().unwrap().parse().unwrap();
        let fullmove: usize = split.next().unwrap().parse().unwrap();

        pos.details.halfmove = halfmove;
        pos.fullmove = fullmove;

        pos
    }
}

/// The starting position in standadrd chess.
pub const STARTING_POSITION: Position = Position {
    color: STARTING_COLOR,
    pawns: STARTING_PAWNS,
    bishops: STARTING_BISHOPS,
    knights: STARTING_KNIGHTS,
    rooks: STARTING_ROOKS,
    queens: STARTING_QUEENS,
    kings: STARTING_KINGS,
    details: IrreversibleDetails {
        en_passant: 255,
        castling: CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE | CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE,
        halfmove: 0,
    },
    white_to_move: true,
    fullmove: 1,

    all_pieces: STARTING_ALL,
    white_pieces: STARTING_COLOR,
    black_pieces: STARTING_BLACK,
};

#[cfg(test)]
mod tests {
    use position::*;
    #[test]
    fn test_parse_start_fen() {
        let start_by_fen =
            Position::from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(start_by_fen, STARTING_POSITION);
    }
}
