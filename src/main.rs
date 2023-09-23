use desmoparse::{ast::parser::Parser, lexer::Token};

fn main() {
    let str = "y=2\\left(2x^{2}+1\\right)3-\\frac{2}{6} + {x=2: 1, 10}";
    dbg!(Parser::new(vec![str.to_owned()]).expression_ast(0));
}
