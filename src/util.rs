use logos::{Lexer, Logos, Source};

pub mod macros;
pub mod multipeek;
pub mod thin_str;

pub struct LexIter<'a, T: Logos<'a>> {
    lexer: Lexer<'a, T>,
}
impl<'a, T: Logos<'a>> LexIter<'a, T> {
    pub fn new(lexer: Lexer<'a, T>) -> Self {
        Self { lexer }
    }
}
impl<'a, T> Iterator for LexIter<'a, T>
where
    T: Logos<'a>,
{
    type Item = (<<T as Logos<'a>>::Source as Source>::Slice<'a>, T);

    fn next(&mut self) -> Option<Self::Item> {
        let n = self.lexer.next()?.ok()?;
        Some((self.lexer.slice(), n))
    }
}
pub trait Discard: Sized {
    /// Discards a value inline by consuming it
    fn discard(self) {}
}
impl<T> Discard for T {}
