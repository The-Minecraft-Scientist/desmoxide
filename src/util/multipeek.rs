use std::cell::RefCell;
pub struct MultiPeek<T: Iterator> {
    buffer: Vec<Option<<T as Iterator>::Item>>,
    iter: T,
}
impl<T: Iterator + Sized> MultiPeek<T> {
    pub fn new<U: IntoIterator<IntoIter = T, Item = T::Item>>(val: U) -> Self {
        let iter = val.into_iter();
        Self {
            iter,
            buffer: Vec::with_capacity(5),
        }
    }
    pub fn new_with_hint<U: IntoIterator<IntoIter = T, Item = T::Item>>(
        val: U,
        size_hint: usize,
    ) -> Self {
        Self {
            iter: val.into_iter(),
            buffer: Vec::with_capacity(size_hint),
        }
    }
    pub fn peek(&mut self) -> &Option<T::Item> {
        let value = self.iter.next();
        self.buffer.push(value);
        self.buffer.last().unwrap()
    }
}
impl<T: Iterator> Iterator for MultiPeek<T> {
    type Item = <T as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.buffer.pop() {
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
