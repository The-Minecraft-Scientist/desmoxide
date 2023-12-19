use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
};

use logos::{Lexer, Logos};
use thin_vec::{thin_vec, ThinVec};

use anyhow::{bail, Context, Result};

use super::{
    expression::{EquationType, ExpressionMeta, ExpressionType},
    parse_manager::ParseManager,
    ASTNode, ASTNodeRef, Comparison,
    CoordinateAccess::*,
    Ident, ListCompInfo,
};
use crate::{
    assert_token_matches,
    ast::{List, Opcode, PiecewiseEntry, Value},
    bad_token,
    lexer::Token,
    util::{multipeek::MultiPeek, LexIter},
};
#[derive(Debug)]
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
    pub fn line_lexer(&self, line: u32) -> Result<MultiPeek<LexIter<'a, Token>>> {
        Ok(MultiPeek::new(LexIter::new(Token::lexer(
            *self
                .storage
                .get(&line)
                .context(format!("line with ID {} not found!", line))?,
        ))))
    }
    pub fn scan_expression_type(&self, idx: u32) -> Result<MultiPeek<LexIter<'a, Token>>> {
        let mut lexer = self.line_lexer(idx)?;
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
                                if lexer.multipeek().is_none() {
                                    break None;
                                }
                                lexer.catch_up();
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
                Token::Eq => {
                    lexer.catch_up();
                    Some(ExpressionType::Var(first.0.into()))
                }
                _ => None,
            },
            _ => None,
        };
        let mut meta = ExpressionMeta::INVALID;
        if let Some(typ) = t {
            meta.expression_type = Some(typ);
        } else {
            //Default case, equation
            let mut pm = ParseManager::new(lexer);
            pm.parse()?;

            let (ast, mut lex) = pm.split();
            lexer = lex;
            let next = lexer.next_res();
            if let Ok(n) = next {
                let s = match n.1 {
                    Token::Gt => EquationType::InEq(Comparison::Greater),
                    Token::Ge => EquationType::InEq(Comparison::GreaterEq),
                    Token::Lt => EquationType::InEq(Comparison::Less),
                    Token::Le => EquationType::InEq(Comparison::LessEq),
                    Token::Eq => EquationType::Implicit,
                    t => {
                        bad_token!(n.0, t, "determining expression type")
                    }
                };
                meta.cached_lhs_ast = Some(ast);
                meta.expression_type = Some(ExpressionType::Eq { eq_type: s });
            } else {
                meta.cached_lhs_ast = Some(ast);
                meta.expression_type = Some(ExpressionType::Eq {
                    eq_type: EquationType::Explicit,
                });
            };
        }
        self.meta.borrow_mut().insert(idx, meta);
        Ok(lexer)
    }
    pub fn update_all(&self) -> Result<()> {
        let start = std::time::Instant::now();
        let mut ctr = 0;
        let mut problems = Vec::with_capacity(50);
        for k in self.storage.keys() {
            let p = self
                .parse_expr(*k)
                .context(format!("failed parsing line {}", k));
            if !p.is_err() {
                ctr += 1;
            } else {
                problems.push(p.unwrap_err());
            }
        }
        let end = std::time::Instant::now();
        println!(
            "successfully parsed {} out of {} expressions in {} microseconds",
            ctr,
            self.storage.len(),
            end.checked_duration_since(start).unwrap().as_micros()
        );
        println!("problematic expressions: \n {:?}", problems);
        Ok(())
    }
    pub fn parse_expr(&self, idx: u32) -> Result<()> {
        let mut lex = self.line_lexer(idx)?;
        lex = self.scan_expression_type(idx)?;
        let mut rhs = None;
        if let Some(_) = lex.peek_next() {
            let mut pm = ParseManager::new(lex);
            pm.parse()?;
            rhs = Some(pm.ast);
        }
        {
            match self.meta.borrow_mut().entry(idx) {
                Entry::Occupied(mut v) => v.get_mut().cached_rhs_ast = rhs,
                Entry::Vacant(a) => {
                    let mut v = ExpressionMeta::INVALID;
                    v.cached_rhs_ast = rhs;
                    a.insert(v);
                }
            }
        }

        Ok(())
    }
}
