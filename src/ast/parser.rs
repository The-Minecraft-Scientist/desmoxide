use std::{cell::RefCell, collections::HashMap};

use logos::{Lexer, Logos};
use thin_vec::{thin_vec, ThinVec};

use anyhow::{bail, Context, Result};

use super::{
    expression::{EquationType, ExpressionMeta, ExpressionType},
    ASTNode, ASTNodeType,
    DotAccess::*,
    Ident, ListCompInfo,
};
use crate::{
    assert_next_token_eq,
    ast::{List, Opcode, Value},
    bad_token,
    lexer::Token,
    util::{multipeek::MultiPeek, LexIter},
};

pub struct Parser<'a> {
    pub storage: &'a HashMap<u32, &'a str>,
    pub meta: RefCell<HashMap<u32, ExpressionMeta<'a>>>,
}
impl<'a> Parser<'a> {
    pub fn new(lines: &'a HashMap<u32, &'a str>) -> Self {
        Self {
            meta: RefCell::new(HashMap::with_capacity(lines.len())),
            storage: lines,
        }
    }
    pub fn line_lexer(&self, line: u32) -> Result<Lexer<'a, Token>> {
        Ok(Token::lexer(
            *self
                .storage
                .get(&line)
                .context(format!("line with ID {} not found!", line))?,
        ))
    }
    fn scan_expression_type(
        &self,
        idx: u32,
        lexer: &mut MultiPeek<LexIter<'a, Token>>,
    ) -> Result<()> {
        let first = *lexer.multipeek_res()?;
        let t = match first.1 {
            Token::Ident => match lexer.multipeek_res()?.1 {
                //Function definition
                Token::LParen => {
                    let mut argv: ThinVec<Ident<'a>> = ThinVec::with_capacity(3);
                    loop {
                        match lexer.multipeek_res()? {
                            (s, Token::Ident) => argv.push((*s).into()),
                            (_, Token::Comma) => {}
                            (_, Token::RParen) => {
                                lexer.catch_up();
                                assert_next_token_eq!(lexer, Token::Eq);
                                break Some(ExpressionType::Fn {
                                    name: first.0.into(),
                                    params: argv,
                                });
                            }
                            (_, _) => break None,
                        };
                    }
                }
                //[Ident]=[stuff]
                Token::Eq => Some(ExpressionType::Var(first.0.into())),
                _ => None,
            },
            _ => None,
        };
        let mut meta = ExpressionMeta::INVALID;
        if let Some(typ) = t {
            meta.expression_type = Some(typ);
        } else {
            //Default case, equation
            let lhs = self.parse_from_lexer(lexer)?;
            let next = lexer.next_res()?;
            let s = match next.1 {
                Token::Gt => EquationType::InEq(Opcode::Gt),
                Token::Ge => EquationType::InEq(Opcode::Ge),
                Token::Lt => EquationType::InEq(Opcode::Lt),
                Token::Le => EquationType::InEq(Opcode::Le),
                Token::Eq => EquationType::Implicit,
                t => {
                    bad_token!(next.0, t, "determining expression type")
                }
            };
            meta.cached_lhs_ast = Some(lhs);
            meta.expression_type = Some(ExpressionType::Eq {
                eq_type: EquationType::Implicit,
            });
            //Might as well cache this work
        }

        self.meta.borrow_mut().insert(idx, meta);
        Ok(())
    }
    //pub fn compile_line(&'a self, lex: &mut MultiPeek<LexIter<'a, Token>>) {}
    pub fn parse_all(&self) -> Result<()> {
        Ok(())
    }
    pub fn expression_ast(&self, id: u32) -> Result<ASTNode<'a>> {
        self.parse_from_lexer(&mut MultiPeek::new(LexIter::new(self.line_lexer(id)?)))
    }
    pub fn parse_from_lexer(
        &self,
        lexer: &mut MultiPeek<LexIter<'a, Token>>,
    ) -> Result<ASTNode<'a>> {
        let (_ident, Token::Ident) = lexer.next_res()? else {
            bail!("first token not an indentifier");
        };
        let (_st, Token::Eq) = lexer.next_res()? else {
            bail!("second token not \"=\"");
        };
        self.recursive_parse_expr(lexer, 0)
    }
    fn parse_list_body(&self, lexer: &mut MultiPeek<LexIter<'a, Token>>) -> Result<List<'a>> {
        let next_token = lexer.peek_next().context("unexpected EOF")?;
        //Empty list
        if next_token.1 == Token::RBracket {
            lexer.discard()?;
            //empty list
            return Ok(List::List(ThinVec::new()));
        }
        let first_scope = self.recursive_parse_expr(lexer, 0)?;
        let next_token = lexer.peek_next().context("unexpected EOF")?;
        //Range list ([0...3])
        if let ASTNodeType::List(l) = &*first_scope {
            assert_next_token_eq!(lexer, Token::RBracket);
            return Ok(l.clone());
        }
        Ok(match next_token.1 {
            Token::Comma => {
                let mut vars = ThinVec::with_capacity(10);
                vars.push(first_scope);
                loop {
                    match lexer.next_res()? {
                        //We reached the last item
                        (_, Token::RBracket) => break,
                        // Normal, continue to the next item
                        (_, Token::Comma) => {}
                        (s, t) => {
                            bad_token!(s, t, "parsing list")
                        }
                    }
                    vars.push(self.recursive_parse_expr(lexer, 0)?)
                }
                List::List(vars)
            }
            //Single element list
            Token::RBracket => List::List(thin_vec![first_scope]),
            t => {
                bad_token!(next_token.0, t, "parsing list")
            }
        })
    }
    ///Special case handling for min, max, count, total and join
    fn parse_autojoin_args(&self, lexer: &mut MultiPeek<LexIter<'a, Token>>) -> Result<List<'a>> {
        assert_next_token_eq!(lexer, Token::LParen);
        self.parse_fn_args(lexer, ThinVec::with_capacity(20))
            .map(|s| List::List(s))
    }

    fn parse_fn_args(
        &self,
        lexer: &mut MultiPeek<LexIter<'a, Token>>,
        mut vars: ThinVec<ASTNode<'a>>,
    ) -> Result<ThinVec<ASTNode<'a>>> {
        let next_token = lexer.peek_next().context("unexpected EOF")?;
        //Empty list
        if next_token.1 == Token::RParen {
            lexer.discard()?;
            //empty list
            return Ok(vars);
        }
        let first_scope = self.recursive_parse_expr(lexer, 0)?;
        let next_token = lexer.peek_next().context("unexpected EOF")?;
        //Range list ([0...3])
        if let ASTNodeType::List(_l) = &*first_scope {
            if next_token.1 == Token::RParen {
                vars.push(first_scope);
                return Ok(vars);
            }
        }
        Ok(match next_token.1 {
            Token::Comma => {
                vars.push(first_scope);
                loop {
                    match lexer.next_res()? {
                        //We reached the last item
                        (_, Token::RParen) => break,
                        // Normal, continue to the next item
                        (_, Token::Comma) => {}
                        (s, t) => {
                            bad_token!(s, t, "parsing list")
                        }
                    }
                    vars.push(self.recursive_parse_expr(lexer, 0)?)
                }
                vars
            }
            //Single element list
            Token::RParen => {
                vars.push(first_scope);
                vars
            }
            t => {
                bad_token!(next_token.0, t, "parsing list")
            }
        })
    }
    fn recursive_parse_expr(
        &self,
        lexer: &mut MultiPeek<LexIter<'a, Token>>,
        min_binding_power: u8,
    ) -> Result<ASTNode<'a>> {
        //get LHS token
        let next = lexer.next_res()?;
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
            // TODO: piecewise functions
            Token::LGroup => {
                let ret = self.recursive_parse_expr(lexer, 0)?;
                {}
                ret
            }
            Token::LParen => {
                let ret = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RParen);
                ret
            }
            Token::Frac => {
                dbg!("called");
                //Handle latex frac command (\frac{numerator}{denominator})
                assert_next_token_eq!(lexer, Token::LGroup);
                let numerator = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RGroup);
                assert_next_token_eq!(lexer, Token::LGroup);
                let denominator = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RGroup);
                ASTNodeType::Div(numerator, denominator).into()
            }
            Token::Sqrt => {
                match lexer.next_res()? {
                    //handle nthroot
                    (_, Token::LBracket) => {
                        let n = self.recursive_parse_expr(lexer, 0)?;
                        assert_next_token_eq!(lexer, Token::RBracket);
                        assert_next_token_eq!(lexer, Token::LGroup);
                        let base = self.recursive_parse_expr(lexer, 0)?;
                        assert_next_token_eq!(lexer, Token::RGroup);
                        ASTNodeType::NthRoot(n, base).into()
                    }
                    //handle pure sqrt
                    (_, Token::LGroup) => {
                        let base = self.recursive_parse_expr(lexer, 0)?;
                        assert_next_token_eq!(lexer, Token::RGroup);
                        ASTNodeType::NthRoot(ASTNodeType::Val(Value::ConstantI64(2)).into(), base)
                            .into()
                    }
                    (s, t) => bad_token!(s, t, "in sqrt expression"),
                }
            }
            Token::LBracket => ASTNodeType::List(Self::parse_list_body(self, lexer)?).into(),
            t if t.is_trig() => {
                assert_next_token_eq!(lexer, Token::LParen);
                let inner = self.recursive_parse_expr(lexer, 0)?;
                assert_next_token_eq!(lexer, Token::RParen);
                ASTNode::new_simple_with_node(t, inner)?
            }
            t if t.should_autojoin_args() => {
                let arg = Self::parse_autojoin_args(self, lexer)?;
                ASTNode::new_autojoin_fn(t, arg)?
            }
            t => bad_token!(next.0, t, "getting lhs"),
        };
        dbg!(&lhs);

        //Loop across this level
        loop {
            dbg!("in loop");
            let Some(a) = *lexer.peek_next() else { break };
            let op = match a.1 {
                Token::Plus => Opcode::Add,
                Token::Minus => Opcode::Sub,
                Token::Mul => Opcode::Mul,
                Token::Pow => Opcode::Pow,
                Token::LBracket => Opcode::Index,
                Token::LParen => Opcode::Parens,
                Token::Dot => {
                    lexer.discard()?;
                    let id = lexer.next_res()?;
                    if let Token::Ident = id.1 {
                        lexer.discard()?;
                        if !lhs.can_be_point() {
                            bail!("cannot get coordinate of a number")
                        }
                        let a = match id.0 {
                            "x" => DotAccessX,
                            "y" => DotAccessY,
                            "z" => DotAccessZ,
                            s => {
                                bail!("unexpected ident \"{}\" in dot accessor!", s)
                            }
                        };
                        lhs = ASTNodeType::DotAccess(lhs, a).into();
                        continue;
                    }
                    if !id.1.suffix_call_allowed() {
                        bail!("cannot call builtin {:?} as a suffix", id.1)
                    }
                    // Handle list function calls in suffix position (e.g. [1,2,3].sort()   )
                    if id.1.should_autojoin_args() && lhs.can_be_list() {
                        let mut vars = ThinVec::with_capacity(10);
                        vars.push(lhs);
                        lhs = ASTNode::new_autojoin_fn(
                            id.1,
                            List::List(self.parse_fn_args(lexer, vars)?),
                        )?;
                        continue;
                    }

                    todo!("Not all dot functions yet implemented")
                }
                Token::Range => {
                    lexer.discard()?;
                    //Immediately return
                    return Ok(ASTNodeType::List(List::Range(
                        lhs,
                        self.recursive_parse_expr(lexer, 0)?,
                    ))
                    .into());
                }
                //List comp
                Token::For => {
                    lexer.discard()?;
                    //LHS is the expression to be run
                    let mut vars: ThinVec<(Ident<'a>, ASTNode<'a>)> = ThinVec::with_capacity(2);
                    loop {
                        let (id, Token::Ident) = lexer.peek_next().context("unexpected EOF")?
                        else {
                            break;
                        };
                        lexer.discard()?;
                        assert_next_token_eq!(lexer, Token::Eq);
                        vars.push((id.into(), self.recursive_parse_expr(lexer, 0)?));
                        if lexer.peek_next().context("unexpected EOF")?.1 != Token::Comma {
                            break;
                        } else {
                            lexer.discard()?;
                        }
                    }
                    lhs = ASTNodeType::List(List::ListComp(lhs, ListCompInfo { vars })).into();
                    continue;
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
                        let tok = lexer.next_res()?;
                        match tok.1 {
                            Token::RParen => {
                                lhs = ASTNodeType::Parens(s.clone(), rhs).into();
                            }
                            Token::Comma => {
                                let mut v = ThinVec::with_capacity(10);
                                v.push(rhs);
                                let v = self.parse_fn_args(lexer, v)?;
                                lhs = ASTNodeType::FunctionCall(s.clone(), v).into();
                            }
                            t => {
                                bad_token!(tok.0, t, "while parsing function call arguments")
                            }
                        }
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
