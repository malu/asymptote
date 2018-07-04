use rand::{prelude::*, prng::ChaChaRng};

use bitboard::*;
use movegen::*;
use position::*;

pub type Hash = u64;

pub struct Hasher {
    color: [Hash; 64],
    pawns: [Hash; 64],
    knights: [Hash; 64],
    bishops: [Hash; 64],
    rooks: [Hash; 64],
    queens: [Hash; 64],
    kings: [Hash; 64],
    white_to_move: Hash,
    en_passant: [Hash; 8],
    castle: [Hash; 16],

    hash: Hash,
}

impl Hasher {
    pub fn new() -> Self {
        let mut seed = [0; 32];
        seed[0] = 1;
        seed[1] = 1;
        seed[2] = 2;
        seed[3] = 3;
        seed[4] = 5;
        seed[5] = 8;
        seed[6] = 13;
        seed[7] = 21;
        seed[8] = 34;
        seed[9] = 55;
        seed[10] = 89;
        seed[11] = 144;
        seed[12] = 233;
        seed[13] = 1;
        seed[14] = 2;
        seed[15] = 4;
        seed[16] = 8;
        seed[17] = 16;
        seed[18] = 32;
        seed[19] = 64;
        seed[20] = 128;
        seed[21] = 1;
        seed[22] = 2;
        seed[23] = 6;
        seed[24] = 24;
        seed[25] = 120;
        seed[26] = 2;
        seed[27] = 3;
        seed[28] = 5;
        seed[29] = 7;
        seed[30] = 11;
        seed[31] = 13;

        let mut rng = ChaChaRng::from_seed(seed);
        let mut hasher = Hasher {
            color: [0; 64],
            pawns: [0; 64],
            knights: [0; 64],
            bishops: [0; 64],
            rooks: [0; 64],
            queens: [0; 64],
            kings: [0; 64],
            white_to_move: 0,
            en_passant: [0; 8],
            castle: [0; 16],

            hash: 0,
        };

        rng.fill(&mut hasher.color);
        rng.fill(&mut hasher.pawns);
        rng.fill(&mut hasher.knights);
        rng.fill(&mut hasher.bishops);
        rng.fill(&mut hasher.rooks);
        rng.fill(&mut hasher.queens);
        rng.fill(&mut hasher.kings);
        hasher.white_to_move = rng.gen();
        rng.fill(&mut hasher.en_passant);
        rng.fill(&mut hasher.castle);

        hasher
    }

    pub fn get_hash(&self) -> Hash {
        self.hash
    }

    pub fn make_move(&mut self, pos: &Position, mov: Move) {
        let rank2 = if pos.white_to_move { RANK_2 } else { RANK_7 };
        let rank4 = if pos.white_to_move { RANK_4 } else { RANK_5 };
        let them = if pos.white_to_move {
            pos.black_pieces
        } else {
            pos.white_pieces
        };

        if pos.details.en_passant != 255 {
            self.hash ^= self.en_passant[pos.details.en_passant as usize];
        }
        if pos.pawns & rank2 & mov.from
            && rank4 & mov.to
            && ((pos.pawns & them).right(1) & mov.to || (pos.pawns & them).left(1) & mov.to)
        {
            self.hash ^= self.en_passant[mov.from.file() as usize];
        }

        let mut castling = pos.details.castling;
        self.hash ^= self.castle[castling as usize];

        match mov.captured {
            Some(Piece::Pawn) => {
                if mov.en_passant {
                    self.hash ^= self.pawns[mov.to.backward(pos.white_to_move, 1).0 as usize];
                    if !pos.white_to_move {
                        self.hash ^= self.color[mov.to.backward(pos.white_to_move, 1).0 as usize];
                    }
                } else {
                    self.hash ^= self.pawns[mov.to.0 as usize];
                }
            }
            Some(Piece::Knight) => {
                self.hash ^= self.knights[mov.to.0 as usize];
            }
            Some(Piece::Bishop) => {
                self.hash ^= self.bishops[mov.to.0 as usize];
            }
            Some(Piece::Rook) => {
                self.hash ^= self.rooks[mov.to.0 as usize];
            }
            Some(Piece::Queen) => {
                self.hash ^= self.queens[mov.to.0 as usize];
            }
            _ => {}
        }

        match mov.piece {
            Piece::Pawn => {
                self.hash ^= self.pawns[mov.from.0 as usize];
                match mov.promoted {
                    Some(Piece::Queen) => {
                        self.hash ^= self.queens[mov.to.0 as usize];
                    }
                    Some(Piece::Knight) => {
                        self.hash ^= self.knights[mov.to.0 as usize];
                    }
                    Some(Piece::Rook) => {
                        self.hash ^= self.rooks[mov.to.0 as usize];
                    }
                    Some(Piece::Bishop) => {
                        self.hash ^= self.bishops[mov.to.0 as usize];
                    }
                    Some(x) => {
                        panic!("Invalid promotion: {:?}", x);
                    }
                    None => {
                        self.hash ^= self.pawns[mov.to.0 as usize];
                    }
                }
            }
            Piece::Knight => {
                self.hash ^= self.knights[mov.from.0 as usize];
                self.hash ^= self.knights[mov.to.0 as usize];
            }
            Piece::Bishop => {
                self.hash ^= self.bishops[mov.from.0 as usize];
                self.hash ^= self.bishops[mov.to.0 as usize];
            }
            Piece::Rook => {
                self.hash ^= self.rooks[mov.from.0 as usize];
                self.hash ^= self.rooks[mov.to.0 as usize];

                if pos.white_to_move {
                    if mov.from.0 == 0 {
                        // A1
                        castling &= CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE | CASTLE_WHITE_KSIDE;
                    } else if mov.from.0 == 7 {
                        // H1
                        castling &= CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE | CASTLE_WHITE_QSIDE;
                    }
                } else {
                    if mov.from.0 == 56 {
                        // A8
                        castling &= CASTLE_BLACK_KSIDE | CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE;
                    } else if mov.from.0 == 63 {
                        // H8
                        castling &= CASTLE_BLACK_QSIDE | CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE;
                    }
                }
            }
            Piece::Queen => {
                self.hash ^= self.queens[mov.from.0 as usize];
                self.hash ^= self.queens[mov.to.0 as usize];
            }
            Piece::King => {
                if mov.to.0 == mov.from.0 + 2 {
                    // castle kingside
                    self.hash ^= self.rooks[mov.to.right(1).0 as usize];
                    self.hash ^= self.rooks[mov.to.left(1).0 as usize];
                    if pos.white_to_move {
                        self.hash ^= self.color[mov.to.right(1).0 as usize];
                        self.hash ^= self.color[mov.to.left(1).0 as usize];
                    }
                } else if mov.from.0 == mov.to.0 + 2 {
                    // castle queenside
                    self.hash ^= self.rooks[mov.to.left(2).0 as usize];
                    self.hash ^= self.rooks[mov.to.right(1).0 as usize];
                    if pos.white_to_move {
                        self.hash ^= self.color[mov.to.left(2).0 as usize];
                        self.hash ^= self.color[mov.to.right(1).0 as usize];
                    }
                }

                self.hash ^= self.kings[mov.from.0 as usize];
                self.hash ^= self.kings[mov.to.0 as usize];

                if pos.white_to_move {
                    castling &= CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE;
                } else {
                    castling &= CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE;
                }
            }
        }

        if pos.white_to_move {
            self.hash ^= self.color[mov.to.0 as usize];
            self.hash ^= self.color[mov.from.0 as usize];
        } else {
            if pos.color & mov.to {
                self.hash ^= self.color[mov.to.0 as usize];
            }
        }
        self.hash ^= self.castle[castling as usize];
        self.hash ^= self.white_to_move;
    }

    pub fn unmake_move(
        &mut self,
        pos: &Position,
        mov: Move,
        irreversible_details: IrreversibleDetails,
    ) {
        self.hash ^= self.white_to_move;
        if pos.details.en_passant != 255 {
            self.hash ^= self.en_passant[pos.details.en_passant as usize];
        }
        if irreversible_details.en_passant != 255 {
            self.hash ^= self.en_passant[irreversible_details.en_passant as usize];
        }
        self.hash ^= self.castle[pos.details.castling as usize];
        self.hash ^= self.castle[irreversible_details.castling as usize];
        let unmaking_white_move = !pos.white_to_move;

        if unmaking_white_move {
            self.hash ^= self.color[mov.from.0 as usize];
            self.hash ^= self.color[mov.to.0 as usize];
        } else if pos.color & mov.from {
            self.hash ^= self.color[mov.from.0 as usize];
        }

        match mov.piece {
            Piece::Pawn => {
                self.hash ^= self.pawns[mov.from.0 as usize];
                match mov.promoted {
                    Some(Piece::Queen) => {
                        self.hash ^= self.queens[mov.to.0 as usize];
                    }
                    Some(Piece::Knight) => {
                        self.hash ^= self.knights[mov.to.0 as usize];
                    }
                    Some(Piece::Rook) => {
                        self.hash ^= self.rooks[mov.to.0 as usize];
                    }
                    Some(Piece::Bishop) => {
                        self.hash ^= self.bishops[mov.to.0 as usize];
                    }
                    Some(x) => {
                        panic!("Invalid promotion: {:?}", x);
                    }
                    None => {
                        self.hash ^= self.pawns[mov.to.0 as usize];
                    }
                }
            }
            Piece::Knight => {
                self.hash ^= self.knights[mov.to.0 as usize];
                self.hash ^= self.knights[mov.from.0 as usize];
            }
            Piece::Bishop => {
                self.hash ^= self.bishops[mov.to.0 as usize];
                self.hash ^= self.bishops[mov.from.0 as usize];
            }
            Piece::Rook => {
                self.hash ^= self.rooks[mov.to.0 as usize];
                self.hash ^= self.rooks[mov.from.0 as usize];
            }
            Piece::Queen => {
                self.hash ^= self.queens[mov.to.0 as usize];
                self.hash ^= self.queens[mov.from.0 as usize];
            }
            Piece::King => {
                self.hash ^= self.kings[mov.to.0 as usize];
                self.hash ^= self.kings[mov.from.0 as usize];

                if mov.to.0 == mov.from.0 + 2 {
                    // castle kingside
                    self.hash ^= self.rooks[mov.to.right(1).0 as usize];
                    self.hash ^= self.rooks[mov.to.left(1).0 as usize];
                    if unmaking_white_move {
                        self.hash ^= self.color[mov.to.right(1).0 as usize];
                        self.hash ^= self.color[mov.to.left(1).0 as usize];
                    }
                } else if mov.from.0 == mov.to.0 + 2 {
                    // castle queenside
                    self.hash ^= self.rooks[mov.to.left(2).0 as usize];
                    self.hash ^= self.rooks[mov.to.right(1).0 as usize];
                    if unmaking_white_move {
                        self.hash ^= self.color[mov.to.left(2).0 as usize];
                        self.hash ^= self.color[mov.to.right(1).0 as usize];
                    }
                }
            }
        }

        match mov.captured {
            Some(Piece::Pawn) => {
                if mov.en_passant {
                    self.hash ^= self.pawns[mov.to.backward(!pos.white_to_move, 1).0 as usize];
                    if !unmaking_white_move {
                        self.hash ^= self.color[mov.to.backward(!pos.white_to_move, 1).0 as usize];
                    }
                } else {
                    self.hash ^= self.pawns[mov.to.0 as usize];
                    if !unmaking_white_move {
                        self.hash ^= self.color[mov.to.0 as usize];
                    }
                }
            }
            Some(Piece::Knight) => {
                self.hash ^= self.knights[mov.to.0 as usize];
                if !unmaking_white_move {
                    self.hash ^= self.color[mov.to.0 as usize];
                }
            }
            Some(Piece::Bishop) => {
                self.hash ^= self.bishops[mov.to.0 as usize];
                if !unmaking_white_move {
                    self.hash ^= self.color[mov.to.0 as usize];
                }
            }
            Some(Piece::Rook) => {
                self.hash ^= self.rooks[mov.to.0 as usize];
                if !unmaking_white_move {
                    self.hash ^= self.color[mov.to.0 as usize];
                }
            }
            Some(Piece::Queen) => {
                self.hash ^= self.queens[mov.to.0 as usize];
                if !unmaking_white_move {
                    self.hash ^= self.color[mov.to.0 as usize];
                }
            }
            _ => {}
        }
    }

    pub fn make_nullmove(&mut self, pos: &Position) {
        self.hash ^= self.white_to_move;
        if pos.details.en_passant != 255 {
            self.hash ^= self.en_passant[pos.details.en_passant as usize];
        }
    }

    pub fn unmake_nullmove(&mut self, pos: &Position, irreversible_details: IrreversibleDetails) {
        self.hash ^= self.white_to_move;
        if pos.details.en_passant != 255 {
            self.hash ^= self.en_passant[pos.details.en_passant as usize];
        }
        if irreversible_details.en_passant != 255 {
            self.hash ^= self.en_passant[irreversible_details.en_passant as usize];
        }
        self.hash ^= self.castle[pos.details.castling as usize];
        self.hash ^= self.castle[irreversible_details.castling as usize];
    }
}
