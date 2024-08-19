use std::collections::VecDeque;

use anyhow::{Context, Result};

pub struct MultiPeek<T: Iterator> {
    buffer: VecDeque<Option<<T as Iterator>::Item>>,
    iter: T,
    pub idx: usize,
}
impl<T: Iterator + Sized> MultiPeek<T> {
    pub fn new<U: IntoIterator<IntoIter = T, Item = T::Item>>(val: U) -> Self {
        let iter = val.into_iter();
        Self {
            iter,
            buffer: VecDeque::with_capacity(5),
            idx: 0,
        }
    }
    pub fn new_with_hint<U: IntoIterator<IntoIter = T, Item = T::Item>>(
        val: U,
        size_hint: usize,
    ) -> Self {
        Self {
            iter: val.into_iter(),
            buffer: VecDeque::with_capacity(size_hint),
            idx: 0,
        }
    }
    pub fn multipeek(&mut self) -> &Option<T::Item> {
        let value = self.iter.next();

        self.buffer.push_back(value);
        self.buffer.back().unwrap()
    }
    pub fn multipeek_res(&mut self) -> Result<&T::Item> {
        self.multipeek().as_ref().context("unexpected EOF, ")
    }
    pub fn catch_up(&mut self) {
        self.buffer.clear();
    }
    pub fn peek_next(&mut self) -> &Option<T::Item> {
        if self.buffer.is_empty() {
            let value = self.iter.next();
            self.buffer.push_back(value);
        }
        self.buffer.front().unwrap()
    }
    pub fn peek_next_res(&mut self) -> Result<&T::Item> {
        self.peek_next().as_ref().context("unexpected EOF")
    }
    pub fn inner(&self) -> &T {
        &self.iter
    }
    pub fn discard(&mut self) -> Result<()> {
        let _ = self.next_res()?;
        Ok(())
    }
    pub fn next_res(&mut self) -> Result<T::Item> {
        self.next().context("unexpected EOF")
    }
}

impl<T: Iterator> Iterator for MultiPeek<T> {
    type Item = <T as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.idx += 1;
        if let Some(next) = self.buffer.pop_front() {
            return next;
        }
        self.iter.next()
    }
}

pub trait IteratorExt: Iterator {
    fn multipeek(self) -> MultiPeek<Self>
    where
        Self: Sized,
    {
        MultiPeek::new(self)
    }
}

impl<T: Iterator + Sized> IteratorExt for T {}
