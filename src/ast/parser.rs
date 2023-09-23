use std::collections::HashMap;

use logos::Logos;

use crate::{lexer::Token, util::multipeek::IteratorExt};

use super::ExpressionAST;

pub struct Parser {
    pub storage: Vec<String>,
    pub lexed: HashMap<u32, Vec<Token>>,
    paused_stack: Vec<PausedParse>,
}
struct PausedParse;
impl Parser {
    pub fn new(lines: Vec<String>) -> Self {
        let mut linemap = HashMap::with_capacity(lines.len());
        let mut u = 0u32;
        for (k, v) in lines.iter().map(|a| {
            let rt = (u, a);
            u += 1;
            rt
        }) {
            linemap.insert(
                k,
                Token::lexer(v)
                    .map_while(|a| a.ok())
                    .collect::<Vec<Token>>(),
            );
        }
        Self {
            lexed: linemap,
            paused_stack: Vec::with_capacity(lines.len() / 2),
            storage: lines,
        }
    }
    pub fn expression_ast(&self, expr: u32) -> Option<(ExpressionAST, Vec<ExpressionAST>)> {
        let line = self.lexed.get(&expr)?;
        let mut peek = line.iter().multipeek();
        for _ in 0..10 {
            dbg!(&peek.peek());
        }
        None
    }
}
