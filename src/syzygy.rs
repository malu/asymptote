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

use crate::bitboard::{Bitboard, Square};
use crate::movegen::{Move, Piece};
use crate::position::*;

use std::path::Path;

use shakmaty::FromSetup;
use shakmaty_syzygy::Tablebase;

pub struct Syzygy {
    tablebase: Tablebase<shakmaty::Chess>,
}

impl Syzygy {
    pub fn new() -> Self {
        Syzygy {
            tablebase: Tablebase::new(),
        }
    }

    pub fn add_directory<P: AsRef<Path>>(&mut self, path: P) {
        let _ = self.tablebase.add_directory(path);
    }

    pub fn get_max_pieces(&self) -> usize {
        <shakmaty::Chess as shakmaty_syzygy::Syzygy>::MAX_PIECES
    }

    pub fn dtz(&self, position: &Position) -> Option<shakmaty_syzygy::Dtz> {
        let chess = Self::construct_chess(position)?;
        self.tablebase.probe_dtz(&chess).ok()
    }

    pub fn wdl(&self, position: &Position) -> Option<shakmaty_syzygy::Wdl> {
        let chess = Self::construct_chess(position)?;
        self.tablebase.probe_wdl(&chess).ok()
    }

    pub fn best_move(&self, position: &Position) -> Option<(shakmaty::Move, shakmaty_syzygy::Dtz)> {
        let chess = Self::construct_chess(position)?;
        self.tablebase.best_move(&chess).ok().flatten()
    }

    fn construct_chess(position: &Position) -> Option<shakmaty::Chess> {
        let position_with_board = PositionWithBoard::from(position);
        shakmaty::Chess::from_setup(&position_with_board).ok()
    }
}

struct PositionWithBoard<'a> {
    position: &'a Position,
    board: shakmaty::Board,
}

impl<'a> From<&'a Position> for PositionWithBoard<'a> {
    fn from(position: &'a Position) -> Self {
        let mut board = shakmaty::Board::empty();

        let add_piece_type = |board: &mut shakmaty::Board,
                              bitboard: Bitboard,
                              color: shakmaty::Color,
                              role: shakmaty::Role| {
            board.extend(bitboard.squares().map(|sq| {
                let sq: u8 = sq.into();
                (
                    shakmaty::Square::new(sq as u32),
                    shakmaty::Piece { color, role },
                )
            }));
        };

        add_piece_type(
            &mut board,
            position.pawns() & position.white_pieces(),
            shakmaty::Color::White,
            shakmaty::Role::Pawn,
        );

        add_piece_type(
            &mut board,
            position.knights() & position.white_pieces(),
            shakmaty::Color::White,
            shakmaty::Role::Knight,
        );

        add_piece_type(
            &mut board,
            position.bishops() & position.white_pieces(),
            shakmaty::Color::White,
            shakmaty::Role::Bishop,
        );

        add_piece_type(
            &mut board,
            position.rooks() & position.white_pieces(),
            shakmaty::Color::White,
            shakmaty::Role::Rook,
        );

        add_piece_type(
            &mut board,
            position.queens() & position.white_pieces(),
            shakmaty::Color::White,
            shakmaty::Role::Queen,
        );

        add_piece_type(
            &mut board,
            position.kings() & position.white_pieces(),
            shakmaty::Color::White,
            shakmaty::Role::King,
        );

        add_piece_type(
            &mut board,
            position.pawns() & position.black_pieces(),
            shakmaty::Color::Black,
            shakmaty::Role::Pawn,
        );

        add_piece_type(
            &mut board,
            position.knights() & position.black_pieces(),
            shakmaty::Color::Black,
            shakmaty::Role::Knight,
        );

        add_piece_type(
            &mut board,
            position.bishops() & position.black_pieces(),
            shakmaty::Color::Black,
            shakmaty::Role::Bishop,
        );

        add_piece_type(
            &mut board,
            position.rooks() & position.black_pieces(),
            shakmaty::Color::Black,
            shakmaty::Role::Rook,
        );

        add_piece_type(
            &mut board,
            position.queens() & position.black_pieces(),
            shakmaty::Color::Black,
            shakmaty::Role::Queen,
        );

        add_piece_type(
            &mut board,
            position.kings() & position.black_pieces(),
            shakmaty::Color::Black,
            shakmaty::Role::King,
        );

        PositionWithBoard { position, board }
    }
}

impl<'a> shakmaty::Setup for PositionWithBoard<'a> {
    fn board(&self) -> &shakmaty::Board {
        &self.board
    }

    fn pockets(&self) -> Option<&shakmaty::Material> {
        None
    }

    fn turn(&self) -> shakmaty::Color {
        if self.position.white_to_move {
            shakmaty::Color::White
        } else {
            shakmaty::Color::Black
        }
    }

    fn castling_rights(&self) -> shakmaty::Bitboard {
        let mut bb = shakmaty::Bitboard::default();

        if self.position.details.castling & CASTLE_WHITE_QSIDE > 0 {
            bb.set(shakmaty::Square::A1, true);
        }

        if self.position.details.castling & CASTLE_WHITE_KSIDE > 0 {
            bb.set(shakmaty::Square::H1, true);
        }

        if self.position.details.castling & CASTLE_BLACK_QSIDE > 0 {
            bb.set(shakmaty::Square::A8, true);
        }

        if self.position.details.castling & CASTLE_BLACK_KSIDE > 0 {
            bb.set(shakmaty::Square::H8, true);
        }

        bb
    }

    fn ep_square(&self) -> Option<shakmaty::Square> {
        if self.position.details.en_passant >= 8 {
            return None;
        }

        let file = shakmaty::File::new(self.position.details.en_passant as u32);
        let rank = if self.position.white_to_move {
            shakmaty::Rank::Sixth
        } else {
            shakmaty::Rank::Third
        };

        Some(shakmaty::Square::from_coords(file, rank))
    }

    fn remaining_checks(&self) -> Option<&shakmaty::RemainingChecks> {
        None
    }

    fn halfmoves(&self) -> u32 {
        self.position.details.halfmove as u32
    }

    fn fullmoves(&self) -> u32 {
        self.position.fullmove as u32
    }
}

impl From<shakmaty::Move> for Move {
    fn from(mov: shakmaty::Move) -> Self {
        match mov {
            shakmaty::Move::Normal {
                role,
                from,
                capture,
                to,
                promotion,
            } => Move {
                from: from.into(),
                to: to.into(),
                piece: role.into(),
                captured: capture.map(Into::into),
                promoted: promotion.map(Into::into),
                en_passant: false,
            },
            shakmaty::Move::EnPassant { from, to } => Move {
                from: from.into(),
                to: to.into(),
                piece: Piece::Pawn,
                captured: Some(Piece::Pawn),
                promoted: None,
                en_passant: true,
            },
            shakmaty::Move::Castle { king, rook } => {
                let kingside = king.file() < rook.file();
                let king: Square = king.into();
                let rook: Square = rook.into();
                if kingside {
                    Move {
                        from: king,
                        to: rook.left(1),
                        piece: Piece::King,
                        captured: None,
                        promoted: None,
                        en_passant: false,
                    }
                } else {
                    Move {
                        from: king,
                        to: rook.right(2),
                        piece: Piece::King,
                        captured: None,
                        promoted: None,
                        en_passant: false,
                    }
                }
            }
            shakmaty::Move::Put { .. } => unimplemented!(),
        }
    }
}

impl From<shakmaty::Square> for Square {
    fn from(sq: shakmaty::Square) -> Self {
        Square::from(u8::from(sq))
    }
}

impl From<shakmaty::Role> for Piece {
    fn from(role: shakmaty::Role) -> Self {
        match role {
            shakmaty::Role::Pawn => Piece::Pawn,
            shakmaty::Role::Knight => Piece::Knight,
            shakmaty::Role::Bishop => Piece::Bishop,
            shakmaty::Role::Rook => Piece::Rook,
            shakmaty::Role::Queen => Piece::Queen,
            shakmaty::Role::King => Piece::King,
        }
    }
}
