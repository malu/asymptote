use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, Shr};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Bitboard(pub u64);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Square(pub u8);

pub struct SquareIterator {
    bb: Bitboard,
}

// =========================================================
// Bitboard impls
// =========================================================
impl Bitboard {
    pub fn forward(&self, white_to_move: bool, ranks: u8) -> Self {
        if white_to_move {
            Bitboard(self.0 << (8 * ranks))
        } else {
            Bitboard(self.0 >> (8 * ranks))
        }
    }

    pub fn backward(&self, white_to_move: bool, ranks: u8) -> Self {
        self.forward(!white_to_move, ranks)
    }

    pub fn right(&self, files: u8) -> Self {
        (*self & LEFT_FILES[8 - files as usize]) << files
    }

    pub fn left(&self, files: u8) -> Self {
        (*self & RIGHT_FILES[8 - files as usize]) >> files
    }

    pub fn popcount(&self) -> usize {
        self.0.count_ones() as usize
    }

    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub fn squares(self) -> SquareIterator {
        SquareIterator { bb: self }
    }

    pub fn print(&self) {
        for rank in 0..8 {
            for file in 0..8 {
                let sq = Square::file_rank(file, 7 - rank);
                if *self & sq {
                    print!("#");
                } else {
                    print!("_");
                }
            }
            println!();
        }
    }
}

impl From<u64> for Bitboard {
    fn from(bb: u64) -> Self {
        Bitboard(bb)
    }
}

impl BitAnd for Bitboard {
    type Output = Self;
    fn bitand(self, other: Self) -> Self {
        Bitboard(self.0 & other.0)
    }
}

impl BitAnd<Square> for Bitboard {
    type Output = bool;
    fn bitand(self, other: Square) -> bool {
        self.0 & (1 << other.0) > 0
    }
}

impl BitOr for Bitboard {
    type Output = Self;
    fn bitor(self, other: Self) -> Self {
        Bitboard(self.0 | other.0)
    }
}

impl BitXor for Bitboard {
    type Output = Self;
    fn bitxor(self, other: Self) -> Self {
        Bitboard(self.0 ^ other.0)
    }
}

impl BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, other: Self) {
        self.0 |= other.0;
    }
}

impl BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, other: Self) {
        self.0 &= other.0;
    }
}

impl BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, other: Self) {
        self.0 ^= other.0;
    }
}

impl BitOrAssign<Square> for Bitboard {
    fn bitor_assign(&mut self, other: Square) {
        *self |= other.to_bb();
    }
}

impl BitAndAssign<Square> for Bitboard {
    fn bitand_assign(&mut self, other: Square) {
        *self &= other.to_bb();
    }
}

impl BitXorAssign<Square> for Bitboard {
    fn bitxor_assign(&mut self, other: Square) {
        *self ^= other.to_bb();
    }
}

impl Not for Bitboard {
    type Output = Self;
    fn not(self) -> Self {
        Bitboard(!self.0)
    }
}

impl Shl<u8> for Bitboard {
    type Output = Self;
    fn shl(self, shift: u8) -> Self {
        Bitboard(self.0 << shift)
    }
}

impl Shr<u8> for Bitboard {
    type Output = Self;
    fn shr(self, shift: u8) -> Self {
        Bitboard(self.0 >> shift)
    }
}

// =========================================================
// Square impls
// =========================================================
impl Square {
    pub fn to_bb(&self) -> Bitboard {
        Bitboard(1 << self.0)
    }

    pub fn file_rank(file: u8, rank: u8) -> Self {
        Square(rank * 8 + file)
    }

    pub fn forward(&self, white_to_move: bool, ranks: u8) -> Self {
        // TODO: Are these possible overflows dangerous?
        if white_to_move {
            Square(self.0 + (8 * ranks))
        } else {
            Square(self.0 - (8 * ranks))
        }
    }

    pub fn backward(&self, white_to_move: bool, ranks: u8) -> Self {
        self.forward(!white_to_move, ranks)
    }

    pub fn right(&self, files: u8) -> Self {
        // TODO: Are these possible overflows dangerous?
        // FIXME: Yes, they are...
        Square(self.0 + files)
    }

    pub fn left(&self, files: u8) -> Self {
        // TODO: Are these possible overflows dangerous?
        // FIXME: Yes, they are...
        Square(self.0 - files)
    }

    pub fn rank(&self) -> u8 {
        self.0 >> 3
    }

    pub fn file(&self) -> u8 {
        self.0 & 0x7
    }
}

// =========================================================
// SquareIterator impls
// =========================================================
impl Iterator for SquareIterator {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bb.0 > 0 {
            let sq = Square(self.bb.0.trailing_zeros() as u8);
            self.bb.0 &= self.bb.0 - 1;
            return Some(sq);
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let popcount = self.bb.popcount();
        (popcount, Some(popcount))
    }
}

// =========================================================
// Constants
// =========================================================
pub const FILE_A: Bitboard = Bitboard(0x01_01_01_01_01_01_01_01);
pub const FILE_B: Bitboard = Bitboard(0x02_02_02_02_02_02_02_02);
pub const FILE_C: Bitboard = Bitboard(0x04_04_04_04_04_04_04_04);
pub const FILE_D: Bitboard = Bitboard(0x08_08_08_08_08_08_08_08);
pub const FILE_E: Bitboard = Bitboard(0x10_10_10_10_10_10_10_10);
pub const FILE_F: Bitboard = Bitboard(0x20_20_20_20_20_20_20_20);
pub const FILE_G: Bitboard = Bitboard(0x40_40_40_40_40_40_40_40);
pub const FILE_H: Bitboard = Bitboard(0x80_80_80_80_80_80_80_80);
pub const FILES: [Bitboard; 8] = [
    FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H,
];

pub const RANK_1: Bitboard = Bitboard(0x00_00_00_00_00_00_00_FF);
pub const RANK_2: Bitboard = Bitboard(0x00_00_00_00_00_00_FF_00);
pub const RANK_3: Bitboard = Bitboard(0x00_00_00_00_00_FF_00_00);
pub const RANK_4: Bitboard = Bitboard(0x00_00_00_00_FF_00_00_00);
pub const RANK_5: Bitboard = Bitboard(0x00_00_00_FF_00_00_00_00);
pub const RANK_6: Bitboard = Bitboard(0x00_00_FF_00_00_00_00_00);
pub const RANK_7: Bitboard = Bitboard(0x00_FF_00_00_00_00_00_00);
pub const RANK_8: Bitboard = Bitboard(0xFF_00_00_00_00_00_00_00);
pub const RANKS: [Bitboard; 8] = [
    RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8,
];

pub const STARTING_COLOR: Bitboard = Bitboard(0x00_00_00_00_00_00_FF_FF);
pub const STARTING_PAWNS: Bitboard = Bitboard(0x00_FF_00_00_00_00_FF_00);
pub const STARTING_KNIGHTS: Bitboard = Bitboard(0x42_00_00_00_00_00_00_42);
pub const STARTING_BISHOPS: Bitboard = Bitboard(0x24_00_00_00_00_00_00_24);
pub const STARTING_ROOKS: Bitboard = Bitboard(0x81_00_00_00_00_00_00_81);
pub const STARTING_QUEENS: Bitboard = Bitboard(0x08_00_00_00_00_00_00_08);
pub const STARTING_KINGS: Bitboard = Bitboard(0x10_00_00_00_00_00_00_10);
pub const STARTING_ALL: Bitboard = Bitboard(0xFF_FF_00_00_00_00_FF_FF);
pub const STARTING_BLACK: Bitboard = Bitboard(0xFF_FF_00_00_00_00_00_00);

pub const EN_PASSANT_FILES: [Bitboard; 8] = [
    FILE_B,
    Bitboard(0x05_05_05_05_05_05_05_05), // FILE_A | FILE_C,
    Bitboard(0x0A_0A_0A_0A_0A_0A_0A_0A), // FILE_B | FILE_D,
    Bitboard(0x14_14_14_14_14_14_14_14), // FILE_C | FILE_E,
    Bitboard(0x28_28_28_28_28_28_28_28), // FILE_D | FILE_F,
    Bitboard(0x50_50_50_50_50_50_50_50), // FILE_E | FILE_G,
    Bitboard(0xA0_A0_A0_A0_A0_A0_A0_A0), // FILE_F | FILE_H,
    FILE_G,
];

pub const KNIGHT_ATTACKS: [Bitboard; 64] = [
    // rank 1
    Bitboard(0x00_00_00_00_00_02_04_00),
    Bitboard(0x00_00_00_00_00_05_08_00),
    Bitboard(0x00_00_00_00_00_0A_11_00),
    Bitboard(0x00_00_00_00_00_14_22_00),
    Bitboard(0x00_00_00_00_00_28_44_00),
    Bitboard(0x00_00_00_00_00_50_88_00),
    Bitboard(0x00_00_00_00_00_A0_10_00),
    Bitboard(0x00_00_00_00_00_40_20_00),
    // rank 2
    Bitboard(0x00_00_00_00_02_04_00_04),
    Bitboard(0x00_00_00_00_05_08_00_08),
    Bitboard(0x00_00_00_00_0A_11_00_11),
    Bitboard(0x00_00_00_00_14_22_00_22),
    Bitboard(0x00_00_00_00_28_44_00_44),
    Bitboard(0x00_00_00_00_50_88_00_88),
    Bitboard(0x00_00_00_00_A0_10_00_10),
    Bitboard(0x00_00_00_00_40_20_00_20),
    // rank 3
    Bitboard(0x00_00_00_02_04_00_04_02),
    Bitboard(0x00_00_00_05_08_00_08_05),
    Bitboard(0x00_00_00_0A_11_00_11_0A),
    Bitboard(0x00_00_00_14_22_00_22_14),
    Bitboard(0x00_00_00_28_44_00_44_28),
    Bitboard(0x00_00_00_50_88_00_88_50),
    Bitboard(0x00_00_00_A0_10_00_10_A0),
    Bitboard(0x00_00_00_40_20_00_20_40),
    // rank 4
    Bitboard(0x00_00_02_04_00_04_02_00),
    Bitboard(0x00_00_05_08_00_08_05_00),
    Bitboard(0x00_00_0A_11_00_11_0A_00),
    Bitboard(0x00_00_14_22_00_22_14_00),
    Bitboard(0x00_00_28_44_00_44_28_00),
    Bitboard(0x00_00_50_88_00_88_50_00),
    Bitboard(0x00_00_A0_10_00_10_A0_00),
    Bitboard(0x00_00_40_20_00_20_40_00),
    // rank 5
    Bitboard(0x00_02_04_00_04_02_00_00),
    Bitboard(0x00_05_08_00_08_05_00_00),
    Bitboard(0x00_0A_11_00_11_0A_00_00),
    Bitboard(0x00_14_22_00_22_14_00_00),
    Bitboard(0x00_28_44_00_44_28_00_00),
    Bitboard(0x00_50_88_00_88_50_00_00),
    Bitboard(0x00_A0_10_00_10_A0_00_00),
    Bitboard(0x00_40_20_00_20_40_00_00),
    // rank 6
    Bitboard(0x02_04_00_04_02_00_00_00),
    Bitboard(0x05_08_00_08_05_00_00_00),
    Bitboard(0x0A_11_00_11_0A_00_00_00),
    Bitboard(0x14_22_00_22_14_00_00_00),
    Bitboard(0x28_44_00_44_28_00_00_00),
    Bitboard(0x50_88_00_88_50_00_00_00),
    Bitboard(0xA0_10_00_10_A0_00_00_00),
    Bitboard(0x40_20_00_20_40_00_00_00),
    // rank 7
    Bitboard(0x04_00_04_02_00_00_00_00),
    Bitboard(0x08_00_08_05_00_00_00_00),
    Bitboard(0x11_00_11_0A_00_00_00_00),
    Bitboard(0x22_00_22_14_00_00_00_00),
    Bitboard(0x44_00_44_28_00_00_00_00),
    Bitboard(0x88_00_88_50_00_00_00_00),
    Bitboard(0x10_00_10_A0_00_00_00_00),
    Bitboard(0x20_00_20_40_00_00_00_00),
    // rank 8
    Bitboard(0x00_04_02_00_00_00_00_00),
    Bitboard(0x00_08_05_00_00_00_00_00),
    Bitboard(0x00_11_0A_00_00_00_00_00),
    Bitboard(0x00_22_14_00_00_00_00_00),
    Bitboard(0x00_44_28_00_00_00_00_00),
    Bitboard(0x00_88_50_00_00_00_00_00),
    Bitboard(0x00_10_A0_00_00_00_00_00),
    Bitboard(0x00_20_40_00_00_00_00_00),
];

pub const KING_ATTACKS: [Bitboard; 64] = [
    // rank 1
    Bitboard(0x00_00_00_00_00_00_03_02),
    Bitboard(0x00_00_00_00_00_00_07_05),
    Bitboard(0x00_00_00_00_00_00_0E_0A),
    Bitboard(0x00_00_00_00_00_00_1C_14),
    Bitboard(0x00_00_00_00_00_00_38_28),
    Bitboard(0x00_00_00_00_00_00_70_50),
    Bitboard(0x00_00_00_00_00_00_E0_A0),
    Bitboard(0x00_00_00_00_00_00_C0_40),
    // rank 2
    Bitboard(0x00_00_00_00_00_03_02_03),
    Bitboard(0x00_00_00_00_00_07_05_07),
    Bitboard(0x00_00_00_00_00_0E_0A_0E),
    Bitboard(0x00_00_00_00_00_1C_14_1C),
    Bitboard(0x00_00_00_00_00_38_28_38),
    Bitboard(0x00_00_00_00_00_70_50_70),
    Bitboard(0x00_00_00_00_00_E0_A0_E0),
    Bitboard(0x00_00_00_00_00_C0_40_C0),
    // rank 3
    Bitboard(0x00_00_00_00_03_02_03_00),
    Bitboard(0x00_00_00_00_07_05_07_00),
    Bitboard(0x00_00_00_00_0E_0A_0E_00),
    Bitboard(0x00_00_00_00_1C_14_1C_00),
    Bitboard(0x00_00_00_00_38_28_38_00),
    Bitboard(0x00_00_00_00_70_50_70_00),
    Bitboard(0x00_00_00_00_E0_A0_E0_00),
    Bitboard(0x00_00_00_00_C0_40_C0_00),
    // rank 4
    Bitboard(0x00_00_00_03_02_03_00_00),
    Bitboard(0x00_00_00_07_05_07_00_00),
    Bitboard(0x00_00_00_0E_0A_0E_00_00),
    Bitboard(0x00_00_00_1C_14_1C_00_00),
    Bitboard(0x00_00_00_38_28_38_00_00),
    Bitboard(0x00_00_00_70_50_70_00_00),
    Bitboard(0x00_00_00_E0_A0_E0_00_00),
    Bitboard(0x00_00_00_C0_40_C0_00_00),
    // rank 5
    Bitboard(0x00_00_03_02_03_00_00_00),
    Bitboard(0x00_00_07_05_07_00_00_00),
    Bitboard(0x00_00_0E_0A_0E_00_00_00),
    Bitboard(0x00_00_1C_14_1C_00_00_00),
    Bitboard(0x00_00_38_28_38_00_00_00),
    Bitboard(0x00_00_70_50_70_00_00_00),
    Bitboard(0x00_00_E0_A0_E0_00_00_00),
    Bitboard(0x00_00_C0_40_C0_00_00_00),
    // rank 6
    Bitboard(0x00_03_02_03_00_00_00_00),
    Bitboard(0x00_07_05_07_00_00_00_00),
    Bitboard(0x00_0E_0A_0E_00_00_00_00),
    Bitboard(0x00_1C_14_1C_00_00_00_00),
    Bitboard(0x00_38_28_38_00_00_00_00),
    Bitboard(0x00_70_50_70_00_00_00_00),
    Bitboard(0x00_E0_A0_E0_00_00_00_00),
    Bitboard(0x00_C0_40_C0_00_00_00_00),
    // rank 7
    Bitboard(0x03_02_03_00_00_00_00_00),
    Bitboard(0x07_05_07_00_00_00_00_00),
    Bitboard(0x0E_0A_0E_00_00_00_00_00),
    Bitboard(0x1C_14_1C_00_00_00_00_00),
    Bitboard(0x38_28_38_00_00_00_00_00),
    Bitboard(0x70_50_70_00_00_00_00_00),
    Bitboard(0xE0_A0_E0_00_00_00_00_00),
    Bitboard(0xC0_40_C0_00_00_00_00_00),
    // rank 8
    Bitboard(0x02_03_00_00_00_00_00_00),
    Bitboard(0x05_07_00_00_00_00_00_00),
    Bitboard(0x0A_0E_00_00_00_00_00_00),
    Bitboard(0x14_1C_00_00_00_00_00_00),
    Bitboard(0x28_38_00_00_00_00_00_00),
    Bitboard(0x50_70_00_00_00_00_00_00),
    Bitboard(0xA0_E0_00_00_00_00_00_00),
    Bitboard(0x40_C0_00_00_00_00_00_00),
];

const LEFT_FILES: [Bitboard; 9] = [
    Bitboard(0x00_00_00_00_00_00_00_00),
    Bitboard(0x01_01_01_01_01_01_01_01),
    Bitboard(0x03_03_03_03_03_03_03_03),
    Bitboard(0x07_07_07_07_07_07_07_07),
    Bitboard(0x0F_0F_0F_0F_0F_0F_0F_0F),
    Bitboard(0x1F_1F_1F_1F_1F_1F_1F_1F),
    Bitboard(0x3F_3F_3F_3F_3F_3F_3F_3F),
    Bitboard(0x7F_7F_7F_7F_7F_7F_7F_7F),
    Bitboard(0xFF_FF_FF_FF_FF_FF_FF_FF),
];

const RIGHT_FILES: [Bitboard; 9] = [
    Bitboard(0x00_00_00_00_00_00_00_00),
    Bitboard(0x80_80_80_80_80_80_80_80),
    Bitboard(0xC0_C0_C0_C0_C0_C0_C0_C0),
    Bitboard(0xE0_E0_E0_E0_E0_E0_E0_E0),
    Bitboard(0xF0_F0_F0_F0_F0_F0_F0_F0),
    Bitboard(0xF8_F8_F8_F8_F8_F8_F8_F8),
    Bitboard(0xFC_FC_FC_FC_FC_FC_FC_FC),
    Bitboard(0xFE_FE_FE_FE_FE_FE_FE_FE),
    Bitboard(0xFF_FF_FF_FF_FF_FF_FF_FF),
];
