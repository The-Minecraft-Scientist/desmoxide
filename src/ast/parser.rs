use std::collections::{hash_map::Entry, HashMap};

use logos::Logos;
use thin_vec::ThinVec;

use anyhow::{bail, Context, Error, Result};

use super::{
    expression::{EquationType, ExpressionMeta, ExpressionType},
    parse_manager::ParseManager,
    Comparison, Ident, AST,
};
use crate::{
    bad_token,
    compile::{frontend::IRSegment, ir::IRType},
    lexer::Token,
    util::{multipeek::MultiPeek, LexIter},
};
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FnId(pub u32, pub IRType);

#[derive(Debug)]
pub struct Expressions<'a> {
    pub storage: &'a HashMap<u32, &'a str>,
    pub meta: HashMap<u32, ExpressionMeta<'a>>,
    ident_lookup: HashMap<&'a str, u32>,
    fn_lookup: HashMap<&'a str, u32>,
}
impl<'a> Expressions<'a> {
    pub fn new(lines: &'a HashMap<u32, &'a str>) -> Self {
        Self {
            meta: HashMap::with_capacity(lines.len()),
            storage: lines,
            ident_lookup: HashMap::with_capacity(lines.len() / 2),
            fn_lookup: HashMap::with_capacity(lines.len() / 2),
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
    pub fn ident_ast(&self, i: &str) -> Result<&AST<'a>> {
        let idx = self.ident_lookup.get(i).context("could not find Ident")?;
        Ok(self
            .meta
            .get(idx)
            .context("could not get expression")?
            .cached_rhs_ast
            .as_ref()
            .context("Ident does not have valid LHS AST")?)
    }
    pub fn fn_ident_ast(&self, idx: u32) -> Result<(&ThinVec<Ident<'a>>, &AST<'a>)> {
        let meta = self.meta.get(&idx).context("could not get expression")?;
        if let ExpressionType::Fn { ref params, .. } = meta
            .expression_type
            .as_ref()
            .context("Expression was not processed")?
        {
            return Ok((
                params,
                meta.cached_rhs_ast
                    .as_ref()
                    .context("tried to get expression AST but it was't parsed yet")?,
            ));
        }
        bail!("Tried to get function AST of a non-function Ident")
    }
    pub fn fn_ident_id(&self, i: &str) -> Result<u32> {
        Ok(*self.fn_lookup.get(i).context("could not find Ident")?)
    }
    pub(crate) fn cache_compiled_fn(&self, idx: u32, t: IRType, ir: IRSegment) -> Result<()> {
        self.meta
            .get(&idx)
            .context("could not get metadata of line")?
            .compiled_versions
            .borrow_mut()
            .get_or_insert(HashMap::with_capacity(1))
            .insert(t, ir);
        Ok(())
    }
    pub fn scan_expression_type(&mut self, idx: u32) -> Result<MultiPeek<LexIter<'a, Token>>> {
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
                    self.ident_lookup.insert(first.0, idx);
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

            let (ast, lex) = pm.split();
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
        self.meta.insert(idx, meta);
        Ok(lexer)
    }
    pub fn bench_test(&mut self) -> Result<Vec<Error>> {
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
        Ok(problems)
    }
    pub fn parse_expr(&mut self, idx: u32) -> Result<()> {
        let mut lex = self.scan_expression_type(idx)?;
        let mut rhs = None;
        if let Some(_) = lex.peek_next() {
            let mut pm = ParseManager::new(lex);
            pm.parse()?;
            rhs = Some(pm.ast);
        }
        {
            match self.meta.entry(idx) {
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
