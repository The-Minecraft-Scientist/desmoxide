use crate::parser::ParsingScope;

mod parser;
mod render;

fn main() {
    let in_str = include_str!("test.json");
    let scope = ParsingScope::from_str(in_str).unwrap();
    dbg!(scope.build_ast(&1).unwrap());
}
