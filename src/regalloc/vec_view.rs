use std::ops::{Deref, DerefMut};

#[derive(Debug, PartialEq, Eq)]
pub struct VecView<'a, T> {
    start: usize,
    end: usize,
    inner: &'a mut Vec<T>,
}

impl<'a, T> VecView<'a, T> {
    pub fn new(inner: &'a mut Vec<T>, start: usize, end: usize) -> Self {
        VecView { start, end, inner }
    }
    pub fn push(&mut self, value: T) {
        self.inner.insert(self.end, value);
        self.end += 1;
    }
    pub fn insert(&mut self, index: usize, value: T) {
        self.inner.insert(self.start + index, value);
        self.end += 1;
    }
}

impl<'a, T> Deref for VecView<'a, T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.inner[self.start..self.end]
    }
}
impl<'a, T> DerefMut for VecView<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner[self.start..self.end]
    }
}
