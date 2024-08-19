use std::{
    borrow::{Borrow, BorrowMut},
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

use compact_str::CompactString;
use logos::Logos;
use shrinkwraprs::Shrinkwrap;
use thin_vec::ThinVec;

use anyhow::{bail, Context, Error, Result};

use crate::lang::{
    ast::{Comparison, Ident, AST},
    compiler::{
        frontend::Frontend,
        ir::{IRSegment, IRType},
        value::IRValue,
    },
    expression_provider::{ExpressionId, ExpressionProvider},
    lexer::Token,
    parser::Parser,
};
use crate::{
    bad_token,
    util::{multipeek::MultiPeek, LexIter},
};
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FnId {
    pub idx: u32,
    pub t: IRType,
}
#[derive(Debug)]
pub struct Expressions {
    pub storage: HashMap<ExpressionId, String>,
    pub max_id: u32,
    pub meta: HashMap<ExpressionId, ExpressionMeta>,
    pub ident_lookup: HashMap<Ident, ExpressionId>,
    pub errors: HashMap<ExpressionId, String>,
}

#[derive(Debug)]
pub struct CompiledEquations {
    pub compiled_equations:
        HashMap<ExpressionId, Result<(Option<IRSegment>, Option<IRSegment>, EquationType)>>,
    pub fn_cache: HashMap<(u32, Vec<IRType>), Arc<IRSegment>>,
}

impl Expressions {
    pub fn new(lines: HashMap<ExpressionId, String>) -> Self {
        let len = lines.len();
        Self {
            errors: HashMap::new(),
            meta: HashMap::with_capacity(len),
            max_id: 0,
            storage: lines,
            ident_lookup: HashMap::with_capacity(len / 2),
        }
    }

    pub fn set_equation(&mut self, id: ExpressionId, eq: String) {
        self.storage.insert(id, eq);
    }

    pub fn add_equation(&mut self, eq: String) {
        self.storage.insert(ExpressionId(self.max_id), eq);
        self.max_id += 1;
    }

    pub fn line_lexer(&self, line: ExpressionId) -> Result<MultiPeek<LexIter<'_, Token>>> {
        Ok(MultiPeek::new(LexIter::new(Token::lexer(
            self.storage
                .borrow()
                .get(&line)
                .context(format!("line with ID {} not found!", *line))?,
        ))))
    }

    pub fn compile_all(&self) -> CompiledEquations {
        let mut frontend = Frontend::new(self);
        CompiledEquations {
            compiled_equations: self
                .meta
                .iter()
                .map(|(k, meta)| {
                    (
                        k.clone(),
                        (|| {
                            Ok(match meta.expression_type {
                                Some(ExpressionType::Eq { eq_type }) => {
                                    let lhs = meta
                                        .latest_lhs_ast
                                        .as_ref()
                                        .map(|ast| frontend.compile_expr(&ast))
                                        .transpose()?;

                                    let rhs = meta
                                        .latest_lhs_ast
                                        .as_ref()
                                        .map(|ast| frontend.compile_expr(&ast))
                                        .transpose()?;

                                    Some((lhs, rhs, eq_type))
                                }
                                Some(ExpressionType::Fn { .. }) => None,
                                Some(ExpressionType::Var(..)) => None,
                                None => None,
                            })
                        })(),
                    )
                })
                .filter_map(|(k, eq)| match eq.transpose() {
                    Some(eq) => Some((k, eq)),
                    None => None,
                })
                .collect(),
            fn_cache: frontend.fn_cache,
        }
    }

    pub fn compile_expr(
        &self,
        eqs: &mut CompiledEquations,
        idx: ExpressionId,
    ) -> Result<Option<(Option<IRSegment>, Option<IRSegment>, EquationType)>> {
        let meta = self.meta.get(&idx).context("failed to get line")?;
        // TODO: REMOVE CLONE
        let mut frontend = Frontend {
            ctx: self,
            fn_cache: eqs.fn_cache.clone(),
        };
        let compiled = match meta.expression_type {
            Some(ExpressionType::Eq { eq_type }) => {
                let lhs = meta
                    .latest_lhs_ast
                    .as_ref()
                    .map(|ast| frontend.compile_expr(&ast))
                    .transpose()?;

                let rhs = meta
                    .latest_lhs_ast
                    .as_ref()
                    .map(|ast| frontend.compile_expr(&ast))
                    .transpose()?;

                Some((lhs, rhs, eq_type))
            }
            Some(ExpressionType::Fn { .. }) => None,
            Some(ExpressionType::Var(..)) => None,
            None => None,
        };

        Ok(compiled)
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
    pub fn scan_expression_type(
        &mut self,
        idx: ExpressionId,
    ) -> Result<(MultiPeek<LexIter<'_, Token>>, Option<Ident>, ExpressionMeta)> {
        let mut lexer = self.line_lexer(idx)?;

        let first = *lexer.multipeek_res()?;

        let mut ident = None;
        let t = match first.1 {
            Token::Ident => match lexer.multipeek_res()?.1 {
                //Function definition
                Token::LParen => {
                    let mut argv: ThinVec<Ident> = ThinVec::with_capacity(3);
                    loop {
                        match lexer.borrow_mut().multipeek_res()? {
                            (s, Token::Ident) => argv.push((*s).into()),
                            (_, Token::Comma) => {}
                            (_, Token::RParen) => {
                                if lexer.borrow_mut().multipeek().is_none() {
                                    break None;
                                }
                                lexer.borrow_mut().catch_up();
                                //              self.ident_lookup.insert(first.0.into(), idx.into());
                                ident = Some(first.0.into());
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
                    lexer.borrow_mut().catch_up();
                    ident = Some(first.0.into());
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
            let mut pm = Parser::new(lexer);
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
                meta.latest_lhs_ast = Some(ast);
                meta.expression_type = Some(ExpressionType::Eq { eq_type: s });
            } else {
                meta.latest_lhs_ast = Some(ast);
                meta.expression_type = Some(ExpressionType::Eq {
                    eq_type: EquationType::Explicit,
                });
            };
        }

        Ok((lexer, ident, meta))
    }
    pub fn parse_all(&mut self) -> Result<()> {
        let keys = self.storage.keys().cloned().collect::<Vec<_>>();
        for i in keys {
            match self.parse_expr(i) {
                Ok((ident, meta)) => {
                    self.meta.insert(i, meta);
                    ident.map(|ident| self.ident_lookup.insert(ident, i));
                }
                Err(e) => {
                    self.errors.insert(i, e.to_string());
                }
            }
        }
        Ok(())
    }
    pub fn parse_expr(&mut self, idx: ExpressionId) -> Result<(Option<Ident>, ExpressionMeta)> {
        let (mut lex, ident, mut meta) = self.scan_expression_type(idx)?;
        let mut rhs = None;
        if let Some(_) = lex.peek_next() {
            let mut pm = Parser::new(lex);
            pm.parse()?;
            rhs = Some(pm.ast);
        }
        {
            meta.latest_rhs_ast = rhs;
        }

        Ok((ident, meta))
    }
}

use std::{cell::RefCell, fmt::Debug};

#[derive(Debug, Clone)]
pub struct ExpressionMeta {
    pub latest_rhs_ast: Option<AST>,
    pub latest_lhs_ast: Option<AST>,
    pub compiled_versions: RefCell<Option<HashMap<IRType, IRSegment>>>,
    pub expression_type: Option<ExpressionType>,
}
impl ExpressionMeta {
    pub fn has_rhs_ast(&self) -> bool {
        self.latest_rhs_ast.is_some()
    }
    pub fn has_lhs_ast(&self) -> bool {
        self.latest_lhs_ast.is_some()
    }
    pub fn has_type(&self) -> bool {
        self.expression_type.is_some()
    }
    pub fn invalidate(&mut self) {
        *self = Self::INVALID;
    }
    pub const INVALID: Self = Self {
        latest_lhs_ast: None,
        latest_rhs_ast: None,
        compiled_versions: RefCell::new(None),
        expression_type: None,
    };
}

#[derive(Clone)]
pub enum ExpressionType {
    Fn { name: Ident, params: ThinVec<Ident> },
    Var(Ident),
    Eq { eq_type: EquationType },
}
impl Debug for ExpressionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn { name, params } => {
                let mut s = format!("Fn {}(", name.as_str());
                for p in params {
                    s.push_str(p.as_str());
                    s.push(',');
                }
                let _ = s.pop();
                f.write_fmt(format_args!("{})", s))
            }
            Self::Var(arg0) => f.debug_tuple("Var").field(&arg0.as_str()).finish(),
            Self::Eq { eq_type } => f.debug_struct("Eq").field("eq_type", eq_type).finish(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EquationType {
    Implicit,
    Explicit,
    InEq(Comparison),
}

impl ExpressionProvider for Expressions {
    fn get_ident_id(
        &self,
        ident: &Ident,
    ) -> Result<crate::lang::expression_provider::ExpressionId> {
        self.ident_lookup
            .get(ident)
            .map(|a| *a)
            .context("ident not found by get_ident_id")
    }

    fn expression_type(
        &self,
        id: crate::lang::expression_provider::ExpressionId,
    ) -> Result<&ExpressionType> {
        Ok(self
            .meta
            .get(&id)
            .map(|a| {
                a.expression_type
                    .as_ref()
                    .context("expression had no expression type")
            })
            .context("expression did not exist!")??)
    }

    fn fn_ast(
        &self,
        id: crate::lang::expression_provider::ExpressionId,
    ) -> Result<(&ThinVec<Ident>, &AST)> {
        let meta = self.meta.get(&id).context("could not get expression")?;
        if let ExpressionType::Fn { ref params, .. } = meta
            .expression_type
            .as_ref()
            .context("Expression was not processed")?
        {
            return Ok((
                params,
                meta.latest_rhs_ast
                    .as_ref()
                    .context("tried to get expression AST but it was't parsed yet")?,
            ));
        }
        bail!("Tried to get function AST of a non-function Ident")
    }

    fn ident_ast(&self, id: crate::lang::expression_provider::ExpressionId) -> Result<&AST> {
        Ok(self
            .meta
            .get(&id)
            .context("could not get expression")?
            .latest_rhs_ast
            .as_ref()
            .context("Ident does not have valid LHS AST")?)
    }
}
