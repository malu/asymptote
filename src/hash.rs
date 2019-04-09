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
use rand::{prelude::*, prng::ChaChaRng};

use crate::bitboard::*;
use crate::movegen::*;
use crate::position::*;
use crate::types::SquareMap;

pub type Hash = u64;

#[derive(Clone)]
pub struct Hasher {
    color: SquareMap<Hash>,
    hashes: [SquareMap<Hash>; 6],
    white_to_move: Hash,
    en_passant: [Hash; 8],
    castle: [Hash; 16],

    hash: Hash,
    pawn_hash: Hash,
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
            color: SquareMap::default(),
            hashes: [SquareMap::default(); 6],
            white_to_move: 0,
            en_passant: [0; 8],
            castle: [0; 16],

            hash: 0,
            pawn_hash: 0,
        };

        rng.fill(&mut hasher.color);
        rng.fill(&mut hasher.hashes[0]);
        rng.fill(&mut hasher.hashes[1]);
        rng.fill(&mut hasher.hashes[2]);
        rng.fill(&mut hasher.hashes[3]);
        rng.fill(&mut hasher.hashes[4]);
        rng.fill(&mut hasher.hashes[5]);
        hasher.white_to_move = rng.gen();
        rng.fill(&mut hasher.en_passant);
        rng.fill(&mut hasher.castle);

        hasher.from_position(&STARTING_POSITION);

        hasher
    }

    pub fn get_hash(&self) -> Hash {
        self.hash
    }

    pub fn get_pawn_hash(&self) -> Hash {
        self.pawn_hash
    }

    pub fn from_position(&mut self, pos: &Position) {
        self.hash = 0;
        if pos.white_to_move {
            self.hash ^= self.white_to_move;
        }

        if pos.details.en_passant != 255 {
            self.hash ^= self.en_passant[pos.details.en_passant as usize];
        }

        self.hash ^= self.castle[pos.details.castling as usize];

        for sq in pos.white_pieces().squares() {
            self.hash ^= self.color[sq];
        }

        for sq in pos.pawns().squares() {
            self.hash ^= self.hashes[Piece::Pawn.index()][sq];
            self.pawn_hash ^= self.hashes[Piece::Pawn.index()][sq];
        }

        for sq in pos.knights().squares() {
            self.hash ^= self.hashes[Piece::Knight.index()][sq];
        }

        for sq in pos.bishops().squares() {
            self.hash ^= self.hashes[Piece::Bishop.index()][sq];
        }

        for sq in pos.rooks().squares() {
            self.hash ^= self.hashes[Piece::Rook.index()][sq];
        }

        for sq in pos.queens().squares() {
            self.hash ^= self.hashes[Piece::Queen.index()][sq];
        }

        for sq in pos.kings().squares() {
            self.hash ^= self.hashes[Piece::King.index()][sq];
        }
    }

    pub fn make_move(&mut self, pos: &Position, mov: Move) {
        let them = pos.them(pos.white_to_move);
        let rank2 = if pos.white_to_move { 1 } else { 6 };
        let rank4 = if pos.white_to_move { 3 } else { 4 };

        if pos.details.en_passant != 255 {
            self.hash ^= self.en_passant[pos.details.en_passant as usize];
        }

        if mov.piece == Piece::Pawn
            && mov.from.rank() == rank2
            && mov.to.rank() == rank4
            && ((them & pos.pawns()).left(1) | (them & pos.pawns()).right(1)) & mov.to
        {
            self.hash ^= self.en_passant[mov.from.file() as usize];
        }

        // Update Pawn Hash
        if mov.captured == Some(Piece::Pawn) {
            if mov.en_passant {
                self.pawn_hash ^=
                    self.hashes[Piece::Pawn.index()][mov.to.backward(pos.white_to_move, 1)];
                if !pos.white_to_move {
                    self.pawn_hash ^= self.color[mov.to.backward(pos.white_to_move, 1)];
                }
            } else {
                self.pawn_hash ^= self.hashes[Piece::Pawn.index()][mov.to];

                if !pos.white_to_move {
                    self.pawn_hash ^= self.color[mov.to];
                }
            }
        }

        if mov.piece == Piece::Pawn {
            self.pawn_hash ^= self.hashes[Piece::Pawn.index()][mov.from];
            if pos.white_to_move {
                self.pawn_hash ^= self.color[mov.from];
            }

            if mov.promoted.is_none() {
                self.pawn_hash ^= self.hashes[Piece::Pawn.index()][mov.to];
                if pos.white_to_move {
                    self.pawn_hash ^= self.color[mov.to];
                }
            }
        }

        let mut castling = pos.details.castling;
        self.hash ^= self.castle[castling as usize];

        self.hash ^= self.hashes[mov.piece.index()][mov.from];

        if let Some(piece) = mov.captured {
            if mov.en_passant {
                self.hash ^=
                    self.hashes[Piece::Pawn.index()][mov.to.backward(pos.white_to_move, 1)];
                if !pos.white_to_move {
                    self.hash ^= self.color[mov.to.backward(pos.white_to_move, 1)];
                }
            } else {
                self.hash ^= self.hashes[piece.index()][mov.to];
                if !pos.white_to_move {
                    self.hash ^= self.color[mov.to];
                }
            }
        }

        if let Some(piece) = mov.promoted {
            self.hash ^= self.hashes[piece.index()][mov.to];
        } else {
            self.hash ^= self.hashes[mov.piece.index()][mov.to];
        }

        if mov.piece == Piece::King {
            if mov.from.right(2) == mov.to {
                // castle kingside
                self.hash ^= self.hashes[Piece::Rook.index()][mov.to.right(1)];
                self.hash ^= self.hashes[Piece::Rook.index()][mov.to.left(1)];
                if pos.white_to_move {
                    self.hash ^= self.color[mov.to.right(1)];
                    self.hash ^= self.color[mov.to.left(1)];
                }
            } else if mov.from.left(2) == mov.to {
                // castle queenside
                self.hash ^= self.hashes[Piece::Rook.index()][mov.to.left(2)];
                self.hash ^= self.hashes[Piece::Rook.index()][mov.to.right(1)];
                if pos.white_to_move {
                    self.hash ^= self.color[mov.to.left(2)];
                    self.hash ^= self.color[mov.to.right(1)];
                }
            }

            if pos.white_to_move {
                castling &= CASTLE_BLACK_KSIDE | CASTLE_BLACK_QSIDE;
            } else {
                castling &= CASTLE_WHITE_KSIDE | CASTLE_WHITE_QSIDE;
            }
        }

        if mov.from == SQUARE_A1 || mov.to == SQUARE_A1 {
            castling &= !CASTLE_WHITE_QSIDE;
        }

        if mov.from == SQUARE_H1 || mov.to == SQUARE_H1 {
            castling &= !CASTLE_WHITE_KSIDE;
        }

        if mov.from == SQUARE_A8 || mov.to == SQUARE_A8 {
            castling &= !CASTLE_BLACK_QSIDE;
        }

        if mov.from == SQUARE_H8 || mov.to == SQUARE_H8 {
            castling &= !CASTLE_BLACK_KSIDE;
        }

        if pos.white_to_move {
            self.hash ^= self.color[mov.to];
            self.hash ^= self.color[mov.from];
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

        // Update Pawn Hash
        if mov.captured == Some(Piece::Pawn) {
            if mov.en_passant {
                self.pawn_hash ^=
                    self.hashes[Piece::Pawn.index()][mov.to.backward(unmaking_white_move, 1)];
                if !unmaking_white_move {
                    self.pawn_hash ^= self.color[mov.to.backward(unmaking_white_move, 1)];
                }
            } else {
                self.pawn_hash ^= self.hashes[Piece::Pawn.index()][mov.to];

                if !unmaking_white_move {
                    self.pawn_hash ^= self.color[mov.to];
                }
            }
        }

        if mov.piece == Piece::Pawn {
            self.pawn_hash ^= self.hashes[Piece::Pawn.index()][mov.from];
            if unmaking_white_move {
                self.pawn_hash ^= self.color[mov.from];
            }

            if mov.promoted.is_none() {
                self.pawn_hash ^= self.hashes[Piece::Pawn.index()][mov.to];
                if unmaking_white_move {
                    self.pawn_hash ^= self.color[mov.to];
                }
            }
        }

        if unmaking_white_move {
            self.hash ^= self.color[mov.from];
            self.hash ^= self.color[mov.to];
        }

        self.hash ^= self.hashes[mov.piece.index()][mov.from];

        if let Some(piece) = mov.captured {
            if mov.en_passant {
                self.hash ^=
                    self.hashes[Piece::Pawn.index()][mov.to.backward(unmaking_white_move, 1)];
                if !unmaking_white_move {
                    self.hash ^= self.color[mov.to.backward(unmaking_white_move, 1)];
                }
            } else {
                self.hash ^= self.hashes[piece.index()][mov.to];
                if !unmaking_white_move {
                    self.hash ^= self.color[mov.to];
                }
            }
        }

        if let Some(piece) = mov.promoted {
            self.hash ^= self.hashes[piece.index()][mov.to];
        } else {
            self.hash ^= self.hashes[mov.piece.index()][mov.to];
        }

        if mov.piece == Piece::King {
            if mov.from.right(2) == mov.to {
                // castle kingside
                self.hash ^= self.hashes[Piece::Rook.index()][mov.to.right(1)];
                self.hash ^= self.hashes[Piece::Rook.index()][mov.to.left(1)];
                if unmaking_white_move {
                    self.hash ^= self.color[mov.to.right(1)];
                    self.hash ^= self.color[mov.to.left(1)];
                }
            } else if mov.from.left(2) == mov.to {
                // castle queenside
                self.hash ^= self.hashes[Piece::Rook.index()][mov.to.left(2)];
                self.hash ^= self.hashes[Piece::Rook.index()][mov.to.right(1)];
                if unmaking_white_move {
                    self.hash ^= self.color[mov.to.left(2)];
                    self.hash ^= self.color[mov.to.right(1)];
                }
            }
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
