use std::{
    cell::{RefCell, RefMut},
    ops::{Deref, DerefMut},
};

use super::{ASTNode, Opcode};
use anyhow::{Context, Result};

#[derive(Debug, Clone)]
pub struct ExpressionMeta<'a> {
    cached_ast: Option<ASTNode<'a>>,
    expression_type: Option<ExpressionType<'a>>,
}
impl<'a> ExpressionMeta<'a> {
    pub fn has_ast(&self) -> bool {
        self.cached_ast.is_some()
    }
    pub fn has_type(&self) -> bool {
        self.expression_type.is_some()
    }
    pub fn invalidate(&mut self) {
        *self = Self::INVALID;
    }
    pub const INVALID: Self = Self {
        cached_ast: None,
        expression_type: None,
    };
}

#[derive(Debug, Clone)]
pub enum ExpressionType<'a> {
    Fn {
        ident: &'a str,
        params: Vec<&'a str>,
    },
    Var {
        ident: &'a str,
    },
    Equation {
        eq_type: EquationType,
    },
}
#[derive(Debug, Clone, Copy)]
pub enum EquationType {
    Implicit,
    Explicit,
    InEq(Opcode),
}
