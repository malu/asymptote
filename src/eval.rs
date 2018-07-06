use bitboard::*;
use movegen::*;
use position::*;

#[derive(Debug)]
pub struct Eval {
    pub material: Material,
    pst: PST,
    positional: Positional,
}

#[derive(Debug)]
pub struct Material {
    white_pawns: usize,
    white_knights: usize,
    white_bishops: usize,
    white_rooks: usize,
    white_queens: usize,
    black_pawns: usize,
    black_knights: usize,
    black_bishops: usize,
    black_rooks: usize,
    black_queens: usize,
}

#[derive(Debug)]
struct PST {
    white_pawns: Score,
    black_pawns: Score,
    white_knights: Score,
    black_knights: Score,
}

#[derive(Debug)]
struct Positional {
    white_pawns_per_file: [usize; 8],
    black_pawns_per_file: [usize; 8],
}

pub type Score = i16;

pub const MATE_SCORE: Score = 20000;

pub const PAWN_SCORE: Score = 100;
pub const KNIGHT_SCORE: Score = 300;
pub const BISHOP_SCORE: Score = 320;
pub const ROOK_SCORE: Score = 500;
pub const QUEEN_SCORE: Score = 900;

impl Eval {
    fn mobility(&mut self, pos: Position) -> Score {
        let mut pos = pos;
        pos.white_to_move = true;
        let mg = MoveGenerator::from(pos);
        let mut white_knight_mobility = 0;
        for knight in (pos.knights & pos.white_pieces).squares() {
            white_knight_mobility +=
                KNIGHT_MOBILITY[mg.knight_from(knight).popcount() as usize] - KNIGHT_MOBILITY_AVG;
        }

        let mut white_bishop_mobility = 0;
        for bishop in (pos.bishops & pos.white_pieces).squares() {
            white_bishop_mobility += BISHOP_MOBILITY
                [get_bishop_attacks_from(bishop, pos.all_pieces).popcount() as usize]
                - BISHOP_MOBILITY_AVG;
        }

        let mut white_rook_mobility = 0;
        for rook in (pos.rooks & pos.white_pieces).squares() {
            white_rook_mobility += ROOK_MOBILITY
                [get_rook_attacks_from(rook, pos.all_pieces).popcount() as usize]
                - ROOK_MOBILITY_AVG;
        }

        pos.white_to_move = false;
        let mg = MoveGenerator::from(pos);
        let mut black_knight_mobility = 0;
        for knight in (pos.knights & pos.black_pieces).squares() {
            black_knight_mobility +=
                KNIGHT_MOBILITY[mg.knight_from(knight).popcount() as usize] - KNIGHT_MOBILITY_AVG;
        }

        let mut black_bishop_mobility = 0;
        for bishop in (pos.bishops & pos.black_pieces).squares() {
            black_bishop_mobility += BISHOP_MOBILITY
                [get_bishop_attacks_from(bishop, pos.all_pieces).popcount() as usize]
                - BISHOP_MOBILITY_AVG;
        }

        let mut black_rook_mobility = 0;
        for rook in (pos.rooks & pos.black_pieces).squares() {
            black_rook_mobility += ROOK_MOBILITY
                [get_rook_attacks_from(rook, pos.all_pieces).popcount() as usize]
                - ROOK_MOBILITY_AVG;
        }

        let white_mobility = white_knight_mobility + white_bishop_mobility + white_rook_mobility;
        let black_mobility = black_knight_mobility + black_bishop_mobility + black_rook_mobility;

        white_mobility - black_mobility
    }

    pub fn score(&mut self, pos: Position) -> Score {
        let mut score = 0;
        score += self.material.score();
        score += self.pst.score();
        score += self.positional.score(pos);
        score += self.mobility(pos);

        let phase = self.phase();
        let (king_mg, king_eg) = self.positional.king_safety(pos);
        score += (king_mg * phase + king_eg * (62 - phase)) / 62;
        let (pawns_mg, pawns_eg) = self.pawns(pos);
        score += (pawns_mg * phase + pawns_eg * (62 - phase)) / 62;

        if pos.white_to_move {
            score
        } else {
            -score
        }
    }

    fn pawns(&mut self, pos: Position) -> (Score, Score) {
        let (wmg, weg) = self.pawns_for_side(pos, true);
        let (bmg, beg) = self.pawns_for_side(pos, false);
        (wmg - bmg, weg - beg)
    }

    fn pawns_for_side(&mut self, pos: Position, white: bool) -> (Score, Score) {
        let us = if white {
            pos.white_pieces
        } else {
            pos.black_pieces
        };
        let them = !us;
        let side = white as usize;

        const PASSER_ON_RANK_BONUS_EG: [Score; 8] = [0, 160, 80, 40, 20, 10, 10, 0];
        const PASSER_ON_RANK_BONUS_MG: [Score; 8] = [0, 60, 50, 40, 30, 20, 10, 0];

        let mut mg = 0;
        let mut eg = 0;
        for pawn in (pos.pawns & us).squares() {
            let sq = pawn.0 as usize;
            let corridor = PAWN_CORRIDOR[side][sq];
            let file_forward = corridor & !(corridor.left(2) | corridor.right(2));
            let passed = (corridor & them & pos.pawns).is_empty();
            let doubled = !(file_forward & us & pos.pawns).is_empty();

            if passed && !doubled {
                let mut relative_rank = if white {
                    pawn.rank() as usize ^ 7
                } else {
                    pawn.rank() as usize
                };

                mg += PASSER_ON_RANK_BONUS_MG[relative_rank];
                eg += PASSER_ON_RANK_BONUS_EG[relative_rank];
            }
        }

        (mg, eg)
    }

    fn phase(&self) -> i16 {
        self.material.non_pawn_material()
    }

    pub fn make_move(&mut self, mov: Move, pos: Position) {
        match mov.piece {
            Piece::Pawn => {
                if pos.white_to_move {
                    self.positional.white_pawns_per_file[mov.from.file() as usize] -= 1;
                    self.pst.white_pawns -= pst(&PAWN_PST, true, mov.from);
                } else {
                    self.positional.black_pawns_per_file[mov.from.file() as usize] -= 1;
                    self.pst.black_pawns -= pst(&PAWN_PST, false, mov.from);
                }
            }
            Piece::Knight => {
                if pos.white_to_move {
                    self.pst.white_knights -= pst(&KNIGHT_PST, true, mov.from);
                    self.pst.white_knights += pst(&KNIGHT_PST, true, mov.to);
                } else {
                    self.pst.black_knights -= pst(&KNIGHT_PST, false, mov.from);
                    self.pst.black_knights += pst(&KNIGHT_PST, false, mov.to);
                }
            }
            _ => {}
        }

        match mov.captured {
            Some(Piece::Pawn) => {
                if pos.white_to_move {
                    self.positional.black_pawns_per_file[mov.to.file() as usize] -= 1;
                    self.material.black_pawns -= 1;
                    self.pst.black_pawns -= pst(&PAWN_PST, false, mov.to);
                } else {
                    self.positional.white_pawns_per_file[mov.to.file() as usize] -= 1;
                    self.material.white_pawns -= 1;
                    self.pst.white_pawns -= pst(&PAWN_PST, true, mov.to);
                }
            }
            Some(Piece::Knight) => {
                if pos.white_to_move {
                    self.material.black_knights -= 1;
                    self.pst.black_knights -= pst(&KNIGHT_PST, false, mov.to);
                } else {
                    self.material.white_knights -= 1;
                    self.pst.white_knights -= pst(&KNIGHT_PST, true, mov.to);
                }
            }
            Some(Piece::Bishop) => {
                if pos.white_to_move {
                    self.material.black_bishops -= 1;
                } else {
                    self.material.white_bishops -= 1;
                }
            }
            Some(Piece::Rook) => {
                if pos.white_to_move {
                    self.material.black_rooks -= 1;
                } else {
                    self.material.white_rooks -= 1;
                }
            }
            Some(Piece::Queen) => {
                if pos.white_to_move {
                    self.material.black_queens -= 1;
                } else {
                    self.material.white_queens -= 1;
                }
            }
            _ => {}
        }

        if mov.piece == Piece::Pawn {
            match mov.promoted {
                None => {
                    if pos.white_to_move {
                        self.positional.white_pawns_per_file[mov.to.file() as usize] += 1;
                        self.pst.white_pawns += pst(&PAWN_PST, true, mov.to);
                    } else {
                        self.positional.black_pawns_per_file[mov.to.file() as usize] += 1;
                        self.pst.black_pawns += pst(&PAWN_PST, false, mov.to);
                    }
                }
                Some(Piece::Knight) => {
                    if pos.white_to_move {
                        self.material.white_pawns -= 1;
                        self.material.white_knights += 1;
                        self.pst.white_knights += pst(&KNIGHT_PST, true, mov.to);
                    } else {
                        self.material.black_pawns -= 1;
                        self.material.black_knights += 1;
                        self.pst.black_knights += pst(&KNIGHT_PST, false, mov.to);
                    }
                }
                Some(Piece::Bishop) => {
                    if pos.white_to_move {
                        self.material.white_pawns -= 1;
                        self.material.white_bishops += 1;
                    } else {
                        self.material.black_pawns -= 1;
                        self.material.black_bishops += 1;
                    }
                }
                Some(Piece::Rook) => {
                    if pos.white_to_move {
                        self.material.white_pawns -= 1;
                        self.material.white_rooks += 1;
                    } else {
                        self.material.black_pawns -= 1;
                        self.material.black_rooks += 1;
                    }
                }
                Some(Piece::Queen) => {
                    if pos.white_to_move {
                        self.material.white_pawns -= 1;
                        self.material.white_queens += 1;
                    } else {
                        self.material.black_pawns -= 1;
                        self.material.black_queens += 1;
                    }
                }
                _ => {}
            }
        }
    }

    pub fn unmake_move(&mut self, mov: Move, pos: Position) {
        let unmaking_white_move = !pos.white_to_move;
        match mov.piece {
            Piece::Pawn => {
                if unmaking_white_move {
                    self.positional.white_pawns_per_file[mov.from.file() as usize] += 1;
                    self.pst.white_pawns += pst(&PAWN_PST, true, mov.from);
                } else {
                    self.positional.black_pawns_per_file[mov.from.file() as usize] += 1;
                    self.pst.black_pawns += pst(&PAWN_PST, false, mov.from);
                }
            }
            Piece::Knight => {
                if unmaking_white_move {
                    self.pst.white_knights += pst(&KNIGHT_PST, true, mov.from);
                    self.pst.white_knights -= pst(&KNIGHT_PST, true, mov.to);
                } else {
                    self.pst.black_knights += pst(&KNIGHT_PST, false, mov.from);
                    self.pst.black_knights -= pst(&KNIGHT_PST, false, mov.to);
                }
            }
            _ => {}
        }

        match mov.captured {
            Some(Piece::Pawn) => {
                if unmaking_white_move {
                    self.positional.black_pawns_per_file[mov.to.file() as usize] += 1;
                    self.material.black_pawns += 1;
                    self.pst.black_pawns += pst(&PAWN_PST, false, mov.to);
                } else {
                    self.positional.white_pawns_per_file[mov.to.file() as usize] += 1;
                    self.material.white_pawns += 1;
                    self.pst.white_pawns += pst(&PAWN_PST, true, mov.to);
                }
            }
            Some(Piece::Knight) => {
                if unmaking_white_move {
                    self.material.black_knights += 1;
                    self.pst.black_knights += pst(&KNIGHT_PST, false, mov.to);
                } else {
                    self.material.white_knights += 1;
                    self.pst.white_knights += pst(&KNIGHT_PST, true, mov.to);
                }
            }
            Some(Piece::Bishop) => {
                if unmaking_white_move {
                    self.material.black_bishops += 1;
                } else {
                    self.material.white_bishops += 1;
                }
            }
            Some(Piece::Rook) => {
                if unmaking_white_move {
                    self.material.black_rooks += 1;
                } else {
                    self.material.white_rooks += 1;
                }
            }
            Some(Piece::Queen) => {
                if unmaking_white_move {
                    self.material.black_queens += 1;
                } else {
                    self.material.white_queens += 1;
                }
            }
            _ => {}
        }

        if mov.piece == Piece::Pawn {
            match mov.promoted {
                None => {
                    if unmaking_white_move {
                        self.positional.white_pawns_per_file[mov.to.file() as usize] -= 1;
                        self.pst.white_pawns -= pst(&PAWN_PST, true, mov.to);
                    } else {
                        self.positional.black_pawns_per_file[mov.to.file() as usize] -= 1;
                        self.pst.black_pawns -= pst(&PAWN_PST, false, mov.to);
                    }
                }
                Some(Piece::Knight) => {
                    if unmaking_white_move {
                        self.material.white_pawns += 1;
                        self.material.white_knights -= 1;
                        self.pst.white_knights -= pst(&KNIGHT_PST, true, mov.to);
                    } else {
                        self.material.black_pawns += 1;
                        self.material.black_knights -= 1;
                        self.pst.black_knights -= pst(&KNIGHT_PST, true, mov.to);
                    }
                }
                Some(Piece::Bishop) => {
                    if unmaking_white_move {
                        self.material.white_pawns += 1;
                        self.material.white_bishops -= 1;
                    } else {
                        self.material.black_pawns += 1;
                        self.material.black_bishops -= 1;
                    }
                }
                Some(Piece::Rook) => {
                    if unmaking_white_move {
                        self.material.white_pawns += 1;
                        self.material.white_rooks -= 1;
                    } else {
                        self.material.black_pawns += 1;
                        self.material.black_rooks -= 1;
                    }
                }
                Some(Piece::Queen) => {
                    if unmaking_white_move {
                        self.material.white_pawns += 1;
                        self.material.white_queens -= 1;
                    } else {
                        self.material.black_pawns += 1;
                        self.material.black_queens -= 1;
                    }
                }
                _ => {}
            }
        }
    }
}

impl From<Position> for Eval {
    fn from(pos: Position) -> Eval {
        Eval {
            material: Material::from(pos),
            pst: PST::from(pos),
            positional: Positional::from(pos),
        }
    }
}

impl Material {
    pub fn is_draw(&self) -> bool {
        if self.white_pawns > 0 || self.white_rooks > 0 || self.white_queens > 0 {
            return false;
        }

        if self.black_pawns > 0 || self.black_rooks > 0 || self.black_queens > 0 {
            return false;
        }

        if self.white_bishops == 0 && self.white_knights == 0 {
            if self.black_bishops == 0 && self.black_knights < 3 {
                return true;
            }

            if self.black_bishops > 0 && self.black_bishops + self.black_knights > 1 {
                return false;
            }

            return true;
        }

        if self.black_bishops == 0 && self.black_knights == 0 {
            if self.white_bishops == 0 && self.white_knights < 3 {
                return true;
            }

            if self.white_bishops > 0 && self.white_bishops + self.white_knights > 1 {
                return false;
            }

            return true;
        }

        false
    }

    pub fn score(&self) -> Score {
        let mut white_material = 0;
        white_material += PAWN_SCORE * self.white_pawns as Score;
        white_material += KNIGHT_SCORE * self.white_knights as Score;
        white_material += BISHOP_SCORE * self.white_bishops as Score;
        white_material += ROOK_SCORE * self.white_rooks as Score;
        white_material += QUEEN_SCORE * self.white_queens as Score;

        let mut black_material = 0;
        black_material += PAWN_SCORE * self.black_pawns as Score;
        black_material += KNIGHT_SCORE * self.black_knights as Score;
        black_material += BISHOP_SCORE * self.black_bishops as Score;
        black_material += ROOK_SCORE * self.black_rooks as Score;
        black_material += QUEEN_SCORE * self.black_queens as Score;

        // encourage trading pieces if ahead in material or pawns if behind in material
        if white_material > black_material + 50 {
            white_material += 4 * self.white_pawns as Score;
            black_material -= 4 * self.black_pawns as Score;
        } else if black_material > white_material + 50 {
            black_material += 4 * self.black_pawns as Score;
            white_material -= 4 * self.white_pawns as Score;
        }

        white_material - black_material
    }

    pub fn non_pawn_material(&self) -> Score {
        let mut white_material = 0;
        white_material += 3 * self.white_knights as Score;
        white_material += 3 * self.white_bishops as Score;
        white_material += 5 * self.white_rooks as Score;
        white_material += 9 * self.white_queens as Score;

        let mut black_material = 0;
        black_material += 3 * self.black_knights as Score;
        black_material += 3 * self.black_bishops as Score;
        black_material += 5 * self.black_rooks as Score;
        black_material += 9 * self.black_queens as Score;

        white_material + black_material
    }
}

impl From<Position> for Material {
    fn from(pos: Position) -> Material {
        Material {
            white_pawns: (pos.white_pieces & pos.pawns).popcount() as usize,
            white_knights: (pos.white_pieces & pos.knights).popcount() as usize,
            white_bishops: (pos.white_pieces & pos.bishops).popcount() as usize,
            white_rooks: (pos.white_pieces & pos.rooks).popcount() as usize,
            white_queens: (pos.white_pieces & pos.queens).popcount() as usize,
            black_pawns: (pos.black_pieces & pos.pawns).popcount() as usize,
            black_knights: (pos.black_pieces & pos.knights).popcount() as usize,
            black_bishops: (pos.black_pieces & pos.bishops).popcount() as usize,
            black_rooks: (pos.black_pieces & pos.rooks).popcount() as usize,
            black_queens: (pos.black_pieces & pos.queens).popcount() as usize,
        }
    }
}

impl PST {
    fn score(&self) -> Score {
        self.white_pawns - self.black_pawns
    }
}

impl From<Position> for PST {
    fn from(pos: Position) -> PST {
        PST {
            white_pawns: (pos.white_pieces & pos.pawns)
                .squares()
                .map(|sq| pst(&PAWN_PST, true, sq))
                .sum(),
            black_pawns: (pos.black_pieces & pos.pawns)
                .squares()
                .map(|sq| pst(&PAWN_PST, false, sq))
                .sum(),
            white_knights: (pos.white_pieces & pos.knights)
                .squares()
                .map(|sq| pst(&KNIGHT_PST, true, sq))
                .sum(),
            black_knights: (pos.black_pieces & pos.knights)
                .squares()
                .map(|sq| pst(&KNIGHT_PST, false, sq))
                .sum(),
        }
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
pub const PAWN_PST: [Score; 64] = [
     24,  28,  35,  50,  50,  35,  28,  24,
     16,  23,  27,  34,  34,  27,  23,  16,
      5,   7,  11,  20,  20,  11,   7,   5,
     -7,  -4,  -2,  11,  11,  -2,  -4,  -7,
    -13, -11,  -3,   2,   2,  -3, -11, -13,
    -17, -14, -14,  -6,  -6, -14, -14, -17,
    -21, -20, -18, -15, -15, -18, -20, -21,
      0,   0,   0,   0,   0,   0,   0,   0,
];

pub fn pst(pst: &[Score; 64], from_white_perspective: bool, sq: Square) -> Score {
    if from_white_perspective {
        pst[sq.0 as usize ^ 0b11_1000]
    } else {
        pst[sq.0 as usize]
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
pub const KNIGHT_PST: [Score; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
];

impl Positional {
    fn score(&self, _pos: Position) -> Score {
        let mut score = 0;
        let penalty = [0, 0, 25, 60, 90, 140, 200, 270];

        for &num_pawns in &self.white_pawns_per_file {
            score -= penalty[num_pawns];
        }

        for &num_pawns in &self.black_pawns_per_file {
            score += penalty[num_pawns];
        }

        score
    }

    pub fn king_safety(&self, pos: Position) -> (Score, Score) {
        let (wmg, weg) = self.king_safety_for_side(pos, true);
        let (bmg, beg) = self.king_safety_for_side(pos, false);
        (wmg - bmg, weg - beg)
    }

    fn king_safety_for_side(&self, pos: Position, white: bool) -> (Score, Score) {
        let us = if white {
            pos.white_pieces
        } else {
            pos.black_pieces
        };
        let them = !us;

        #[cfg_attr(rustfmt, rustfmt_skip)]
        const CENTER_DISTANCE: [Score; 64] = [
            3, 3, 3, 3, 3, 3, 3, 3,
            3, 2, 2, 2, 2, 2, 2, 3,
            3, 2, 1, 1, 1, 1, 2, 3,
            3, 2, 1, 0, 0, 1, 2, 3,
            3, 2, 1, 0, 0, 1, 2, 3,
            3, 2, 1, 1, 1, 1, 2, 3,
            3, 2, 2, 2, 2, 2, 2, 3,
            3, 3, 3, 3, 3, 3, 3, 3,
        ];

        let mut index = 0;

        let king = pos.kings & us;
        let king_sq = king.squares().nth(0).unwrap();
        let file = king_sq.file();
        let king_file = FILES[file as usize];
        let adjacent_files = king.left(1) | king | king.right(1);
        let front = adjacent_files.forward(white, 1);
        let distant_front = adjacent_files.forward(white, 2);

        index += (3 - (front & pos.pawns & us).popcount()) * 2;
        index += 3 - (distant_front & pos.pawns & us).popcount();
        index += (front & pos.pawns & them).popcount();
        index += (distant_front & pos.pawns & them).popcount();

        // is king on open file
        if (king_file & pos.pawns).is_empty() {
            index += 2;
        }

        // is king on half-open file
        if (king_file & pos.pawns).popcount() == 1 {
            index += 1;
        }

        // on same file as opposing rook
        if !(king_file & pos.rooks & them).is_empty() {
            index += 1;
        }

        let mg_penalty = (index * index) as Score;
        let eg_penalty = CENTER_DISTANCE[king_sq.0 as usize];
        (-mg_penalty, -5 * eg_penalty)
    }
}

impl From<Position> for Positional {
    fn from(pos: Position) -> Positional {
        let mut white_pawns_per_file = [0; 8];
        let mut black_pawns_per_file = [0; 8];

        for pawn in (pos.pawns & pos.white_pieces).squares() {
            white_pawns_per_file[pawn.file() as usize] += 1;
        }

        for pawn in (pos.pawns & pos.black_pieces).squares() {
            black_pawns_per_file[pawn.file() as usize] += 1;
        }

        Positional {
            white_pawns_per_file,
            black_pawns_per_file,
        }
    }
}

const KNIGHT_MOBILITY: [Score; 9] = [-200, -90, 0, 40, 70, 90, 100, 105, 108];
const KNIGHT_MOBILITY_AVG: Score = 25;

const BISHOP_MOBILITY: [Score; 14] = [
    -200, -90, 0, 40, 70, 90, 100, 105, 108, 110, 112, 114, 116, 118,
];
const BISHOP_MOBILITY_AVG: Score = 57;

const ROOK_MOBILITY: [Score; 15] = [
    -200, -90, 0, 40, 70, 90, 100, 105, 108, 110, 112, 114, 116, 118, 120,
];
const ROOK_MOBILITY_AVG: Score = 61;
