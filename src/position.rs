use bitboard::*;
use eval::*;
use movegen::*;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Position {
    pub color: Bitboard,
    pub pawns: Bitboard,
    pub bishops: Bitboard,
    pub knights: Bitboard,
    pub rooks: Bitboard,
    pub queens: Bitboard,
    pub kings: Bitboard,
    pub en_passant: u8,
    pub castling: u8,
    pub white_to_move: bool,
    pub halfmove: usize,
    pub fullmove: usize,

    pub all_pieces: Bitboard,
    pub white_pieces: Bitboard,
    pub black_pieces: Bitboard,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct IrreversibleDetails {
    pub halfmove: usize,
    pub en_passant: u8,
    pub castling: u8,
}

impl Position {
    pub fn see(&mut self, mov: Move) -> i16 {
        let ep = self.en_passant;
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
        self.en_passant = ep;

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

        let ep = self.en_passant;
        for capture in captures {
            self.make_capture(capture);

            value = ::std::cmp::max(value, capture_value - self.see_square(sq, capture.piece));
            self.unmake_capture(capture);
            self.en_passant = ep;

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
                self.halfmove = 0;
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
                            self.en_passant = mov.from.file();
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
        let mg = MoveGenerator::from(*self);
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

        let mg = MoveGenerator::from(*self);
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

    pub fn in_check(&self) -> bool {
        let us = if self.white_to_move {
            self.white_pieces
        } else {
            self.black_pieces
        };
        let king = (us & self.kings).squares().nth(0).unwrap();
        self.is_attacked(king)
    }

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

    pub fn make_move(&mut self, mov: Move) {
        let them = if self.white_to_move {
            self.black_pieces
        } else {
            self.white_pieces
        };
        let rank2 = if self.white_to_move { RANK_2 } else { RANK_7 };
        let rank4 = if self.white_to_move { RANK_4 } else { RANK_5 };

        self.en_passant = 255;
        if self.pawns & rank2 & mov.from
            && rank4 & mov.to
            && ((them & self.pawns).left(1) | (them & self.pawns).right(1)) & mov.to
        {
            self.en_passant = mov.from.file();
        }

        self.halfmove += 1;

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
                self.halfmove = 0;
            }
            Some(Piece::Knight) => {
                self.knights ^= mov.to;
                self.halfmove = 0;
            }
            Some(Piece::Bishop) => {
                self.bishops ^= mov.to;
                self.halfmove = 0;
            }
            Some(Piece::Rook) => {
                self.rooks ^= mov.to;
                self.halfmove = 0;
            }
            Some(Piece::Queen) => {
                self.queens ^= mov.to;
                self.halfmove = 0;
            }
            _ => {}
        }

        match mov.piece {
            Piece::Pawn => {
                self.pawns ^= mov.from;
                self.halfmove = 0;
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
                        self.castling &= !CASTLE_WHITE_QSIDE;
                    } else if mov.from.0 == 7 {
                        // H1
                        self.castling &= !CASTLE_WHITE_KSIDE;
                    }
                } else {
                    if mov.from.0 == 56 {
                        // A8
                        self.castling &= !CASTLE_BLACK_QSIDE;
                    } else if mov.from.0 == 63 {
                        // H8
                        self.castling &= !CASTLE_BLACK_KSIDE;
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
                    self.castling &= !(CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE);
                } else {
                    self.castling &= !(CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE);
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

    pub fn unmake_move(&mut self, mov: Move, irreversible_details: IrreversibleDetails) {
        self.en_passant = irreversible_details.en_passant;
        self.castling = irreversible_details.castling;
        self.halfmove = irreversible_details.halfmove;
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

    pub fn make_nullmove(&mut self) {
        self.white_to_move = !self.white_to_move;
        self.en_passant = 255;
        self.halfmove += 1;
    }

    pub fn unmake_nullmove(&mut self, irreversible_details: IrreversibleDetails) {
        self.white_to_move = !self.white_to_move;
        self.en_passant = irreversible_details.en_passant;
        self.castling = irreversible_details.castling;
        self.halfmove = irreversible_details.halfmove;
    }

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
                if self.castling & CASTLE_WHITE_KSIDE > 0 {
                    print!("K");
                }

                if self.castling & CASTLE_WHITE_QSIDE > 0 {
                    print!("Q");
                }

                if self.castling & CASTLE_BLACK_KSIDE > 0 {
                    print!("k");
                }

                if self.castling & CASTLE_BLACK_QSIDE > 0 {
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
            en_passant: 255,
            castling: CASTLE_WHITE_KSIDE
                | CASTLE_WHITE_QSIDE
                | CASTLE_BLACK_KSIDE
                | CASTLE_BLACK_QSIDE,
            white_to_move: true,
            fullmove: 1,
            halfmove: 0,

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

        pos.castling = 0;
        for c in split.next().unwrap().chars() {
            match c {
                '-' => break,
                'K' => pos.castling |= CASTLE_WHITE_KSIDE,
                'Q' => pos.castling |= CASTLE_WHITE_QSIDE,
                'k' => pos.castling |= CASTLE_BLACK_KSIDE,
                'q' => pos.castling |= CASTLE_BLACK_QSIDE,
                x => panic!("Unexpected character in fen castling: {}", x),
            }
        }

        if let Some(en_passant_sq) = split.next() {
            if en_passant_sq != "-" {
                pos.en_passant = match en_passant_sq.chars().nth(0) {
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

        let halfmove: usize = split.next().unwrap().parse().unwrap();
        let fullmove: usize = split.next().unwrap().parse().unwrap();

        pos.halfmove = halfmove;
        pos.fullmove = fullmove;

        pos
    }
}

pub const CASTLE_WHITE_KSIDE: u8 = 0x1;
pub const CASTLE_WHITE_QSIDE: u8 = 0x2;
pub const CASTLE_BLACK_KSIDE: u8 = 0x4;
pub const CASTLE_BLACK_QSIDE: u8 = 0x8;

pub const STARTING_POSITION: Position = Position {
    color: STARTING_COLOR,
    pawns: STARTING_PAWNS,
    bishops: STARTING_BISHOPS,
    knights: STARTING_KNIGHTS,
    rooks: STARTING_ROOKS,
    queens: STARTING_QUEENS,
    kings: STARTING_KINGS,
    en_passant: 255,
    castling: CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE | CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE,
    white_to_move: true,
    fullmove: 1,
    halfmove: 0,

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
