use std::mem;

use crate::bitboard::Square;

pub struct SquareMap<T> {
    data: [T; 64],
}

impl<T> SquareMap<T> {
    pub fn iter_mut(&mut self) -> std::slice::IterMut<T> {
        self.data.iter_mut()
    }

    pub const fn from_array(data: [T; 64]) -> SquareMap<T> {
        SquareMap { data }
    }
}

impl<T: Copy> Copy for SquareMap<T> {}

impl<T: Clone> Clone for SquareMap<T> {
    fn clone(&self) -> Self {
        SquareMap {
            data: self.data.clone(),
        }
    }
}

impl<T> rand::AsByteSliceMut for SquareMap<T>
where
    [T]: rand::AsByteSliceMut,
{
    fn as_byte_slice_mut(&mut self) -> &mut [u8] {
        self.data.as_byte_slice_mut()
    }

    fn to_le(&mut self) {
        self.data.to_le()
    }
}

impl<T: Copy + Default> Default for SquareMap<T> {
    fn default() -> Self {
        SquareMap {
            data: [Default::default(); 64],
        }
    }
}

impl<T> From<[T; 64]> for SquareMap<T> {
    fn from(data: [T; 64]) -> SquareMap<T> {
        SquareMap { data }
    }
}

impl<T> std::ops::Index<Square> for SquareMap<T> {
    type Output = T;

    fn index(&self, sq: Square) -> &Self::Output {
        unsafe {
            let i = mem::transmute::<Square, u8>(sq);
            self.data.get_unchecked(i as usize)
        }
    }
}

impl<T> std::ops::IndexMut<Square> for SquareMap<T> {
    fn index_mut(&mut self, sq: Square) -> &mut Self::Output {
        unsafe {
            let i = mem::transmute::<Square, u8>(sq);
            self.data.get_unchecked_mut(i as usize)
        }
    }
}
