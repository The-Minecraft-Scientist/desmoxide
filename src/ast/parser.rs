use std::collections::HashMap;

use logos::{Lexer, Logos};
use serde_json::de;
use thin_vec::{thin_vec, ThinVec};

use crate::{
    assert_next_token_eq,
    ast::{Opcode, Value},
    bad_token,
    lexer::Token,
    util::{multipeek::MultiPeek, LexIter},
};
use anyhow::{Context, Result};

use super::{expression::ExpressionMeta, ASTNode, ASTNodeType, Ident, ListCompInfo};
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
        let (st, Token::Eq) = lexer.next().context("unexpected EOF")? else {
            bail!("second token not \"=\"");
        };
        self.recursive_parse_expr(&mut lexer, 0)
    }
    fn handle_list(
        parser: &'a Self,
        lexer: &mut MultiPeek<LexIter<'a, Token>>,
    ) -> Result<ASTNode<'a>> {
        let next_token = lexer.peek_next().context("unexpected EOF")?;
        dbg!(next_token);
        //Empty list
        if next_token.1 == Token::RBracket {
            let _ = lexer.next().context("unexpected EOF")?;
            //empty list
            return Ok(ASTNodeType::NodeList(ThinVec::new()).into());
        }
        let first_scope = parser.recursive_parse_expr(lexer, 0)?;
        let next_token = lexer.peek_next().context("unexpected EOF")?;
        //Range list ([0...3])
        if let ASTNodeType::RangeList(_, _) = &*first_scope {
            assert_next_token_eq!(lexer, Token::RBracket);
            return Ok(first_scope);
        }
        Ok(match next_token.1 {
            //List comprehension list
            Token::For => {
                let _ = lexer.next().context("unexpected EOF")?;
                //LHS is the expression to be run
                let mut v: ThinVec<(Ident<'a>, ASTNode<'a>)> = ThinVec::with_capacity(2);
                loop {
                    let (id, Token::Ident) = lexer.peek_next().context("unexpected EOF")? else {
                        break;
                    };
                    let _ = lexer.next().context("unexpected EOF")?;
                    assert_next_token_eq!(lexer, Token::Eq);
                    v.push((id.into(), parser.recursive_parse_expr(lexer, 0)?));
                    if lexer.peek_next().context("unexpected EOF")?.1 != Token::Comma {
                        break;
                    } else {
                        let _ = lexer.next().context("unexpected EOF")?;
                    }
                }
                ASTNodeType::ListCompList(first_scope, ListCompInfo { vars: v }).into()
            }
            //Multi element list
            Token::Comma => {
                let mut v = ThinVec::with_capacity(10);
                loop {
                    match lexer.next().context("unexpected EOF")? {
                        //We reached the last item
                        (_, Token::RBracket) => break,
                        // Normal, continue to the next item
                        (_, Token::Comma) => {}
                        (s, t) => {
                            bad_token!(s, t, "parsing list")
                        }
                    }
                    v.push(parser.recursive_parse_expr(lexer, 0)?)
                }
                ASTNodeType::NodeList(v).into()
            }
            //Single element list
            Token::RBracket => ASTNodeType::NodeList(thin_vec![first_scope]).into(),
            t => {
                bad_token!(next_token.0, t, "parsing list")
            }
        })
    }
    pub fn recursive_parse_expr(
        &'a self,
        lexer: &mut MultiPeek<LexIter<'a, Token>>,
        min_binding_power: u8,
    ) -> Result<ASTNode<'a>> {
        //get LHS token
        let mut next = lexer.next().context("unexpected EOF")?;
        //Handle implicit multiplication (xyz = x * y * z)
        let mut lhs = match next.1 {
            //Identifier
            Token::Ident => ASTNodeType::Val(next.0.into()).into(),
            //Static value
            Token::IntegerLit(i) => ASTNodeType::Val(i.into()).into(),
            Token::FloatLit(f) => ASTNodeType::Val(f.into()).into(),
            //Prefix negation
            Token::Minus => {
                ASTNodeType::Neg(self.recursive_parse_expr(lexer, *Opcode::Neg.prefix_bp()?)?)
                    .into()
            }
            // Handle unambiguous parenthesized or grouped statements (e.g. 1 + (2 + 3))
            // Not sure if "{" needs to have this behavior since it typically only appears inside of
            // latex commands, which all have custom logic
            Token::LGroup => {
                let ret = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RGroup);
                ret
            }
            Token::LParen => {
                let ret = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RParen);
                ret
            }
            Token::Frac => {
                //Handle latex frac command (\frac{numerator}{denominator})
                assert_next_token_eq!(lexer, Token::LGroup);
                let numerator = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RGroup);
                assert_next_token_eq!(lexer, Token::LGroup);
                let denominator = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RGroup);
                ASTNodeType::Div(numerator, denominator).into()
            }
            Token::LBracket => Self::handle_list(self, lexer)?,
            t if t.is_trig() => {
                assert_next_token_eq!(lexer, Token::LParen);
                let inner = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RParen);
                ASTNode::new_trig(t, inner)?
            }
            t => bad_token!(next.0, t, "getting lhs"),
        };

        //Loop across this level
        loop {
            let Some(a) = *lexer.peek_next() else { break };
            let op = match a.1 {
                Token::Plus => Opcode::Add,
                Token::Minus => Opcode::Sub,
                Token::Mul => Opcode::Mul,
                Token::Pow => Opcode::Pow,
                Token::LBracket => Opcode::Index,
                Token::LParen => Opcode::Parens,
                Token::Dot => {
                    let n = lexer.next().context("unexpected EOF")?;
                    lhs.can_be_list();
                    continue;
                }
                Token::Range => {
                    let _ = lexer.next().context("unexpected EOF")?;
                    //Immediately return
                    return Ok(
                        ASTNodeType::RangeList(lhs, self.recursive_parse_expr(lexer, 0)?).into(),
                    );
                }
                //TODO: fix this jankery
                t if t.is_value() => {
                    //This is super jank but we unconditionally pop the lexer every iteration so...
                    lexer.push(("*", Token::Mul));
                    Opcode::Mul
                }
                t if t.ends_parse() => {
                    break;
                }
                t => bad_token!(a.0, t, "getting opcode"),
            };
            if let Ok(bp) = op.postfix_bp() {
                if *bp < min_binding_power {
                    break;
                }
                lexer.next();
                match (op, &*lhs) {
                    (Opcode::Parens, ASTNodeType::Val(Value::Ident(ref s))) => {
                        let rhs = self.recursive_parse_expr(lexer, 0)?;
                        assert_next_token_eq!(lexer, Token::RParen);
                        lhs = ASTNodeType::Parens(s.clone(), rhs).into();
                    }
                    (
                        Opcode::Parens,
                        ASTNodeType::Val(Value::ConstantF64(_) | Value::ConstantI64(_)),
                    ) => {
                        let rhs = self.recursive_parse_expr(lexer, 0)?;
                        assert_next_token_eq!(lexer, Token::RParen);
                        lhs = ASTNodeType::Mul(lhs, rhs).into();
                    }
                    (Opcode::Index, node) if node.can_be_list() => {
                        let rhs = self.recursive_parse_expr(lexer, 0)?;
                        assert_next_token_eq!(lexer, Token::RBracket);
                        lhs = ASTNodeType::Index(lhs, rhs).into();
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

                lhs = ASTNode::new_opcode_2arg(op, lhs, rhs);
                continue;
            }
            break;
        }
        Ok(lhs)
    }
}
