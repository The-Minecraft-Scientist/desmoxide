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
    lexer::Token,
    parser::Parser,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Shrinkwrap)]
pub struct ExpressionId(pub u32);
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
}

#[derive(Debug, Default)]
pub struct CompiledEquations {
    pub compiled_equations: HashMap<ExpressionId, CompiledEquation>,
    pub fn_cache: HashMap<(u32, Vec<IRType>), Arc<IRSegment>>,
}

impl Expressions {
    pub fn new(lines: HashMap<ExpressionId, String>) -> Self {
        let len = lines.len();
        Self {
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

    pub fn compile_all(&self, errors: &mut HashMap<ExpressionId, String>) -> CompiledEquations {
        let mut frontend = Frontend::new(&self.meta, &self.ident_lookup);
        CompiledEquations {
            compiled_equations: self
                .meta
                .iter()
                .map(|(k, meta)| {
                    (
                        k.clone(),
                        (|| {
                            let compiled = match meta {
                                ExpressionMeta::Eq(eq) => Some(match eq {
                                    Equation::Implicit { lhs } => CompiledEquation::Implicit {
                                        lhs: frontend.compile_expr(lhs)?,
                                    },
                                    Equation::Explicit { lhs, rhs, comp } => {
                                        CompiledEquation::Explicit {
                                            lhs: frontend.compile_expr(lhs)?,
                                            rhs: frontend.compile_expr(rhs)?,
                                            comp: *comp,
                                        }
                                    }
                                }),
                                ExpressionMeta::Fn { name, params, rhs } => {
                                    let lhs = frontend.compile_expr(rhs)?;
                                    if lhs.ret.t() == IRType::Number {
                                        Some(CompiledEquation::Implicit { lhs })
                                    } else {
                                        None
                                    }
                                }
                                ExpressionMeta::Var { ident, rhs } => None,
                            };
                            Ok::<_, anyhow::Error>(compiled)
                        })(),
                    )
                })
                .filter_map(|(k, eq)| match eq {
                    Ok(eq) => eq.map(|eq| (k, eq)),
                    Err(err) => {
                        errors.insert(k, err.to_string());
                        None
                    }
                })
                .collect(),
            fn_cache: frontend.fn_cache,
        }
    }

    pub fn compile_expr(
        &self,
        eqs: &mut CompiledEquations,
        idx: ExpressionId,
    ) -> Result<Option<CompiledEquation>> {
        let meta = self.meta.get(&idx).context("failed to get line")?;
        // TODO: REMOVE CLONE
        let mut frontend = Frontend {
            exprs: &self.meta,
            idents: &self.ident_lookup,
            fn_cache: eqs.fn_cache.clone(),
        };

        let compiled = match meta {
            ExpressionMeta::Eq(eq) => Some(match eq {
                Equation::Implicit { lhs } => CompiledEquation::Implicit {
                    lhs: frontend.compile_expr(lhs)?,
                },
                Equation::Explicit { lhs, rhs, comp } => CompiledEquation::Explicit {
                    lhs: frontend.compile_expr(lhs)?,
                    rhs: frontend.compile_expr(rhs)?,
                    comp: *comp,
                },
            }),
            ExpressionMeta::Fn { name, params, rhs } => {
                let lhs = frontend.compile_expr(rhs)?;
                if lhs.ret.t() == IRType::Number {
                    Some(CompiledEquation::Implicit { lhs })
                } else {
                    None
                }
            }
            ExpressionMeta::Var { ident, rhs } => None,
        };

        Ok(compiled)
    }

    pub fn parse_all(&mut self) -> HashMap<ExpressionId, String> {
        let keys = self.storage.keys().cloned().collect::<Vec<_>>();
        let mut errors = HashMap::new();
        for i in keys {
            match self.parse_expr(i) {
                Ok(meta) => {
                    match &meta {
                        ExpressionMeta::Var { ident, .. } => {
                            self.ident_lookup.insert(ident.clone(), i);
                        }
                        ExpressionMeta::Fn { name, params, rhs } => {
                            self.ident_lookup.insert(name.clone(), i);
                        }
                        _ => (),
                    }

                    self.meta.insert(i, meta);
                }
                Err(e) => {
                    errors.insert(i, e.to_string());
                }
            }
        }
        errors
    }

    pub fn parse_expr(&mut self, idx: ExpressionId) -> Result<ExpressionMeta> {
        let mut lexer = self.line_lexer(idx)?;
        let first = *lexer.multipeek_res()?;

        let expr = match first.1 {
            Token::Ident => match lexer.multipeek_res() {
                //Function definition
                Ok((_, Token::LParen)) => {
                    let mut argv: ThinVec<Ident> = ThinVec::with_capacity(3);
                    loop {
                        match lexer.borrow_mut().multipeek_res()? {
                            (s, Token::Ident) => argv.push((*s).into()),
                            (_, Token::Comma) => {}
                            (_, Token::RParen) => {
                                if lexer.borrow_mut().multipeek().is_none() {
                                    break ();
                                }
                                lexer.borrow_mut().catch_up();
                                let mut pm = Parser::new(lexer);
                                pm.parse()?;
                                let rhs = pm.ast;
                                return Ok(ExpressionMeta::Fn {
                                    name: first.0.into(),
                                    params: argv,
                                    rhs,
                                });
                            }
                            (_, _) => break (),
                        };
                    }
                }
                //[Ident]=[stuff]
                Ok((_, Token::Eq)) => {
                    lexer.borrow_mut().catch_up();
                    let mut pm = Parser::new(lexer);
                    pm.parse()?;
                    let rhs = pm.ast;

                    return Ok(ExpressionMeta::Var {
                        ident: first.0.into(),
                        rhs,
                    });
                }
                _ => (),
            },
            _ => (),
        };

        Ok(ExpressionMeta::Eq({
            //Default case, equation
            let mut pm = Parser::new(lexer);

            pm.parse()?;

            let (lhs, mut lex) = pm.split();

            match lex.next() {
                Some((n, t)) => {
                    let mut pm = Parser::new(lex);
                    pm.parse()?;
                    let rhs = pm.ast;

                    let comp = match t {
                        Token::Gt => Comparison::Greater,

                        Token::Ge => Comparison::Greater,

                        Token::Lt => Comparison::Greater,

                        Token::Le => Comparison::Greater,

                        Token::Eq => Comparison::Greater,

                        t => {
                            bad_token!(n, t, "determining expression type")
                        }
                    };

                    Equation::Explicit { lhs, rhs, comp }
                }
                None => Equation::Implicit { lhs },
            }
        }))
    }
}

use std::{cell::RefCell, fmt::Debug};

#[derive(Debug)]
pub enum CompiledEquation {
    Implicit {
        lhs: IRSegment,
    },
    Explicit {
        lhs: IRSegment,
        rhs: IRSegment,
        comp: Comparison,
    },
}

#[derive(Clone)]
pub enum ExpressionMeta {
    Fn {
        name: Ident,
        params: ThinVec<Ident>,
        rhs: AST,
    },
    Var {
        ident: Ident,
        rhs: AST,
    },
    Eq(Equation),
}

impl Debug for ExpressionMeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn { name, params, rhs } => {
                let mut s = format!("Fn {}(", name.as_str());
                for p in params {
                    s.push_str(p.as_str());
                    s.push(',');
                }
                let _ = s.pop();
                f.write_fmt(format_args!("{})", s))
            }
            Self::Var { ident, rhs } => f.debug_tuple("Var").field(&ident.as_str()).finish(),
            Self::Eq(eq) => f.debug_struct("Eq").field("eq_type", eq).finish(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Equation {
    Implicit {
        lhs: AST,
    },
    Explicit {
        lhs: AST,
        rhs: AST,
        comp: Comparison,
    },
}

#[derive(Clone)]
pub enum ExpressionType {
    Fn { name: Ident, params: ThinVec<Ident> },
    Var(Ident),
    Eq(EquationType),
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
            Self::Eq(eq_type) => f.debug_struct("Eq").field("eq_type", eq_type).finish(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EquationType {
    Implicit,
    Explicit,
    InEq(Comparison),
}
