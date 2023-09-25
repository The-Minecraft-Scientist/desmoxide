use logos::{Lexer, Logos, Source};

pub mod multipeek;

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
    type Item = (&'a <<T as Logos<'a>>::Source as Source>::Slice, T);

    fn next(&mut self) -> Option<Self::Item> {
        let n = self.lexer.next()?.map_err(|e| e).ok()?;
        Some((self.lexer.slice(), n))
    }
}
