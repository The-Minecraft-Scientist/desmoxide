use super::lexer::{DesmosVal, Opcode};
#[derive(Debug, Clone)]
pub enum Node<'a> {
    ValueNode(&'a DesmosVal),
    OperatorNode(Box<Node<'a>>, Opcode, Box<Node<'a>>),
}



// Extend tuples
pub trait TupleExt<'a, T> {
    fn x(&'a self) -> &'a T;
    fn y(&'a self) -> &'a T;
}
impl<'a, T> TupleExt<'a, T> for (T, T) {
    fn x(&'a self) -> &'a T {
        &self.0
    }
    fn y(&'a self) -> &'a T {
        &self.1
    }
}
