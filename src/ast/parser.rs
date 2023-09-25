use logos::{Lexer, Logos};
macro_rules! bad_token {
    ($s:expr, $t:expr) => {{
        anyhow::bail!("bad token {:?} at {:?}", $t, $s.char_indices())
    }};
    ($s:expr, $t:expr, $msg:expr) => {{
        anyhow::bail!("bad token {:?} at {:?}: {}", $t, $s.char_indices(), $msg)
    }};
}
macro_rules! assert_next_token_eq {
    ($l:expr, $e:expr) => {
        assert_eq!($l.next().context("unexpected EOF")?.1, $e);
    };
}
use crate::{
    ast::{Opcode, Value},
    lexer::Token,
    util::{multipeek::MultiPeek, LexIter},
};
use anyhow::{Context, Result};

use super::ASTNode;
use anyhow::bail;
pub struct Parser {
    pub storage: Vec<String>,
}
impl Parser {
    pub fn new(lines: Vec<String>) -> Self {
        Self { storage: lines }
    }
    pub fn line_lexer<'a>(&'a self, line: usize) -> Lexer<'a, Token> {
        Token::lexer(&(self.storage[line]))
    }
    pub fn expression_ast<'a>(&'a self, expr: usize) -> Result<ASTNode<'a>> {
        let mut lexer = MultiPeek::new(LexIter::new(self.line_lexer(expr)));
        let (ident, Token::Ident) = lexer.next().context("Unexpected EOF")? else {
            return None.context(" first token not an indentifier");
        };
        let (_, Token::Eq) = lexer.next().context("unexpected EOF")? else {
            return None.context("second token not \"=\"");
        };
        self.recursive_parse_expr(&mut lexer, 0)
    }
    pub fn recursive_parse_expr<'a>(
        &self,
        lexer: &mut MultiPeek<LexIter<'a, Token>>,
        min_binding_power: u8,
    ) -> Result<ASTNode<'a>> {
        println!("called");
        //get LHS token
        let mut next = lexer.next().context("unexpected EOF")?;
        dbg!(next);
        //Handle implicit multiplication (xyz = x * y * z)
        let mut lhs = match next {
            //Identifier
            (s, Token::Ident) => ASTNode::Val(Value::Ident(s)),
            //Static value
            (_, Token::IntegerLit(i)) => ASTNode::Val(Value::Constant(i.into())),
            (_, Token::FloatLit(f)) => ASTNode::Val(Value::Constant(f.into())),
            //Prefix negation
            (_, Token::Minus) => ASTNode::Op(
                Opcode::Neg,
                vec![self.recursive_parse_expr(lexer, *Opcode::Neg.prefix_bp()?)?],
            ),
            //Handle unabiguous parenthesized statements (e.g. 1 + (2 + 3))
            (_, t) if t.begins_scope().is_some() => {
                let ret = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, t.begins_scope().unwrap());
                ret
            }
            (s, t) => bad_token!(s, t, "getting lhs"),
        };

        //Loop across this level
        loop {
            let Some(a) = *lexer.peek_next() else { break };
            let op = match a {
                (_, Token::Plus) => Opcode::Add,
                (_, Token::Minus) => Opcode::Sub,
                (_, Token::Mul) => Opcode::Mul,
                (_, Token::Pow) => Opcode::Pow,
                (_, Token::LBracket) => Opcode::Index,
                (_, t) if t.ends_scope() => {
                    break;
                }
                (_, t) if t.is_value() => {
                    lexer.push(("*", Token::Mul));
                    Opcode::Mul
                }
                (_, Token::LParen) => Opcode::Parens,
                (s, t) => bad_token!(s, t, "getting opcode"),
            };
            if let Ok(bp) = op.postfix_bp() {
                if *bp < min_binding_power {
                    break;
                }
                lexer.next();
                match (op, &lhs) {
                    (Opcode::Parens, &ASTNode::Val(Value::Ident(_))) => {
                        let rhs = self.recursive_parse_expr(lexer, 0)?;
                        assert_next_token_eq!(lexer, Token::RParen);
                        lhs = ASTNode::Op(Opcode::Parens, vec![lhs, rhs]);
                    }
                    (Opcode::Parens, &ASTNode::Val(Value::Constant(_))) => {
                        let rhs = self.recursive_parse_expr(lexer, 0)?;
                        assert_next_token_eq!(lexer, Token::RParen);
                        lhs = ASTNode::Op(Opcode::Mul, vec![lhs, rhs]);
                    }
                    (Opcode::Index, &ASTNode::Val(Value::Ident(_))) => {
                        let rhs = self.recursive_parse_expr(lexer, 0)?;
                        assert_next_token_eq!(lexer, Token::RBracket);
                        lhs = ASTNode::Op(Opcode::Parens, vec![lhs]);
                    }

                    t => bail!("bad opcode: {:?}", t),
                }
                continue;
            }
            if let Ok(bp) = op.infix_bp() {
                if bp.left < min_binding_power {
                    break;
                }
                let _ = lexer.next();
                let rhs = self.recursive_parse_expr(lexer, bp.right)?;

                lhs = ASTNode::Op(op, vec![lhs, rhs]);
                continue;
            }
            break;
        }
        Ok(lhs)
    }

    fn djikstra_parse_expr<'a>(
        &self,
        lexer: &mut MultiPeek<LexIter<'a, Token>>,
    ) -> Result<ASTNode<'a>> {
        let mut opcode_stack: Vec<Opcode> = Vec::with_capacity(64);
        let mut val_stack: Vec<Value> = Vec::with_capacity(64);
        loop {
            match lexer.next().context("unexpected EOF")? {
                (s, Token::Ident) => val_stack.push(Value::Ident(s)),

                (s, t) => bad_token!(s, t),
            }
        }
        todo!()
    }
}
