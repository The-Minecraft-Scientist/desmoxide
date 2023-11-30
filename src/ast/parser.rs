use std::collections::HashMap;

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
    ($l:expr, $e:expr) => {{
        let __token = $l.next().context("unexpected EOF")?;
        if __token.1 != $e {
            anyhow::bail!(
                "Next token {:?} did not match expected token {:?}",
                __token.1,
                $e
            );
        }
    }};
}
use crate::{
    ast::{Opcode, Value},
    lexer::Token,
    util::{multipeek::MultiPeek, LexIter},
};
use anyhow::{Context, Result};

use super::{expression::ExpressionMeta, ASTNode, ASTNodeType, Ident};
use anyhow::bail;

pub struct Parser<'a> {
    pub storage: Vec<String>,
    pub meta: HashMap<u32, ExpressionMeta<'a>>,
}
impl<'a> Parser<'a> {
    pub fn new(lines: Vec<String>) -> Self {
        Self {
            meta: HashMap::with_capacity(lines.len()),
            storage: lines,
        }
    }
    pub fn line_lexer(&'a self, line: usize) -> Lexer<'a, Token> {
        Token::lexer(&(self.storage[line]))
    }
    pub fn expression_ast(&'a self, expr: usize) -> Result<ASTNode<'a>> {
        let mut lexer = MultiPeek::new(LexIter::new(self.line_lexer(expr)));
        let (ident, Token::Ident) = lexer.next().context("Unexpected EOF")? else {
            return bail!(" first token not an indentifier");
        };
        let (_, Token::Eq) = lexer.next().context("unexpected EOF")? else {
            bail!("second token not \"=\"");
        };
        self.recursive_parse_expr(&mut lexer, 0)
    }
    pub fn recursive_parse_expr(
        &'a self,
        lexer: &mut MultiPeek<LexIter<'a, Token>>,
        min_binding_power: u8,
    ) -> Result<ASTNode<'a>> {
        //get LHS token
        let mut next = lexer.next().context("unexpected EOF")?;
        //Handle implicit multiplication (xyz = x * y * z)
        let mut lhs = match next {
            //Identifier
            (s, Token::Ident) => ASTNodeType::Val(s.into()).into(),
            //Static value
            (_, Token::IntegerLit(i)) => ASTNodeType::Val(i.into()).into(),
            (_, Token::FloatLit(f)) => ASTNodeType::Val(f.into()).into(),
            //Prefix negation
            (_, Token::Minus) => {
                ASTNodeType::Neg(self.recursive_parse_expr(lexer, *Opcode::Neg.prefix_bp()?)?)
                    .into()
            }
            //Handle unambiguous parenthesized statements (e.g. 1 + (2 + 3))
            (_, Token::LGroup) => {
                let ret = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RGroup);
                ret
            }
            (_, Token::LParen) => {
                let ret = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RParen);
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
                (_, Token::Comma) => Opcode::Comma,
                (_, t) if t.is_value() => {
                    //This is super jank but we unconditionally pop the lexer every iteration so...
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
}
