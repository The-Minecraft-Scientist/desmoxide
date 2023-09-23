use egg::Id;

pub mod parser;
#[derive(Debug, Clone, Copy)]
pub enum ExpressionAST {
    // a + b
    Add([Id; 2]),
    // a - b
    Sub([Id; 2]),
    // a * b
    Mul([Id; 2]),
    // a/b
    Div([Id; 2]),
    // a^b
    Pow([Id; 2]),
}
