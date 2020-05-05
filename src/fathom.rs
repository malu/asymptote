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

use std::{
    convert::{TryFrom, TryInto},
    ffi::CString,
    path::Path,
};

// pub const CASTLE_WHITE_KINGSIDE: u8 = 0x1;
// pub const CASTLE_WHITE_QUEENSIDE: u8 = 0x2;
// pub const CASTLE_BLACK_KINGSIDE: u8 = 0x4;
// pub const CASTLE_BLACK_QUEENSIDE: u8 = 0x8;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct BoardState {
    pub white: u64,
    pub black: u64,
    pub kings: u64,
    pub queens: u64,
    pub rooks: u64,
    pub bishops: u64,
    pub knights: u64,
    pub pawns: u64,
    pub halfmove_clock: u32,
    pub castling: u32,
    pub en_passant: u32,
    pub white_to_move: bool,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum Wdl {
    Loss,
    BlessedLoss,
    Draw,
    CursedWin,
    Win,
}

const TB_LOSS: u32 = 0;
const TB_BLESSED_LOSS: u32 = 1;
const TB_DRAW: u32 = 2;
const TB_CURSED_WIN: u32 = 3;
const TB_WIN: u32 = 4;
const TB_FAILED: u32 = 0xFFFFFFFF;

impl TryFrom<u32> for Wdl {
    type Error = ();
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            TB_LOSS => Ok(Wdl::Loss),
            TB_BLESSED_LOSS => Ok(Wdl::BlessedLoss),
            TB_DRAW => Ok(Wdl::Draw),
            TB_CURSED_WIN => Ok(Wdl::CursedWin),
            TB_WIN => Ok(Wdl::Win),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum PromotionPiece {
    Knight,
    Bishop,
    Rook,
    Queen,
}

impl TryFrom<u32> for PromotionPiece {
    type Error = ();
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            TB_PROMOTES_KNIGHT => Ok(PromotionPiece::Knight),
            TB_PROMOTES_BISHOP => Ok(PromotionPiece::Bishop),
            TB_PROMOTES_ROOK => Ok(PromotionPiece::Rook),
            TB_PROMOTES_QUEEN => Ok(PromotionPiece::Queen),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Move {
    pub from: u32,
    pub to: u32,
    pub promotes: Option<PromotionPiece>,
    pub en_passant: bool,
}

pub struct ProbeResult {
    pub wdl: Wdl,
    pub dtz: u32,
    pub best_move: Move,
}

pub unsafe fn init<P: AsRef<Path>>(path: P) -> bool {
    let pathref = path.as_ref();
    let pathstr = match pathref.to_str() {
        Some(s) => s,
        None => return false,
    };
    let c_string = match CString::new(pathstr) {
        Ok(s) => s,
        Err(_) => return false,
    };

    return c::tb_init(c_string.as_ptr());
}

pub unsafe fn max_pieces() -> usize {
    c::TB_LARGEST as usize
}

pub unsafe fn probe_wdl(board: &BoardState) -> Option<Wdl> {
    let result = c::tb_probe_wdl_wrapper(
        board.white,
        board.black,
        board.kings,
        board.queens,
        board.rooks,
        board.bishops,
        board.knights,
        board.pawns,
        board.halfmove_clock,
        board.castling,
        board.en_passant,
        board.white_to_move as u8,
    );

    result.try_into().ok()
}

// const TB_PROMOTES_NONE: u32 = 0;
const TB_PROMOTES_QUEEN: u32 = 1;
const TB_PROMOTES_ROOK: u32 = 2;
const TB_PROMOTES_BISHOP: u32 = 3;
const TB_PROMOTES_KNIGHT: u32 = 4;

const TB_RESULT_WDL_MASK: u32 = 0x0000000F;
const TB_RESULT_TO_MASK: u32 = 0x000003F0;
const TB_RESULT_FROM_MASK: u32 = 0x0000FC00;
const TB_RESULT_PROMOTES_MASK: u32 = 0x00070000;
const TB_RESULT_EP_MASK: u32 = 0x00080000;
const TB_RESULT_DTZ_MASK: u32 = 0xFFF00000;
const TB_RESULT_WDL_SHIFT: u32 = 0;
const TB_RESULT_TO_SHIFT: u32 = 4;
const TB_RESULT_FROM_SHIFT: u32 = 10;
const TB_RESULT_PROMOTES_SHIFT: u32 = 16;
const TB_RESULT_EP_SHIFT: u32 = 19;
const TB_RESULT_DTZ_SHIFT: u32 = 20;

fn tb_get_wdl(res: u32) -> Option<Wdl> {
    ((res & TB_RESULT_WDL_MASK) >> TB_RESULT_WDL_SHIFT)
        .try_into()
        .ok()
}

fn tb_get_to(res: u32) -> u32 {
    (res & TB_RESULT_TO_MASK) >> TB_RESULT_TO_SHIFT
}

fn tb_get_from(res: u32) -> u32 {
    (res & TB_RESULT_FROM_MASK) >> TB_RESULT_FROM_SHIFT
}

fn tb_get_promotes(res: u32) -> Option<PromotionPiece> {
    ((res & TB_RESULT_PROMOTES_MASK) >> TB_RESULT_PROMOTES_SHIFT)
        .try_into()
        .ok()
}

fn tb_get_ep(res: u32) -> bool {
    (res & TB_RESULT_EP_MASK) >> TB_RESULT_EP_SHIFT > 0
}

fn tb_get_dtz(res: u32) -> u32 {
    (res & TB_RESULT_DTZ_MASK) >> TB_RESULT_DTZ_SHIFT
}

pub unsafe fn probe_root(board: &BoardState) -> Option<ProbeResult> {
    let result = c::tb_probe_root_wrapper(
        board.white,
        board.black,
        board.kings,
        board.queens,
        board.rooks,
        board.bishops,
        board.knights,
        board.pawns,
        board.halfmove_clock,
        board.castling,
        board.en_passant,
        board.white_to_move as u8,
    );

    if result == TB_FAILED {
        return None;
    }

    let wdl = tb_get_wdl(result)?;
    let dtz = tb_get_dtz(result);
    let best_move = Move {
        from: tb_get_from(result),
        to: tb_get_to(result),
        promotes: tb_get_promotes(result),
        en_passant: tb_get_ep(result),
    };

    Some(ProbeResult {
        best_move: best_move,
        wdl,
        dtz,
    })
}

mod c {
    extern "C" {
        pub static TB_LARGEST: u32;
        pub fn tb_init(filename: *const i8) -> bool;
        pub fn tb_probe_wdl_wrapper(
            white: u64,
            black: u64,
            kings: u64,
            queens: u64,
            rooks: u64,
            bishops: u64,
            knights: u64,
            pawns: u64,
            rule50: u32,
            castling: u32,
            ep: u32,
            turn: u8,
        ) -> u32;

        pub fn tb_probe_root_wrapper(
            white: u64,
            black: u64,
            kings: u64,
            queens: u64,
            rooks: u64,
            bishops: u64,
            knights: u64,
            pawns: u64,
            rule50: u32,
            castling: u32,
            ep: u32,
            turn: u8,
        ) -> u32;
    }
}
