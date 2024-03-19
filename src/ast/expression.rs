use std::{cell::RefCell, collections::HashMap, fmt::Debug};
use thin_vec::ThinVec;

use crate::compile::{frontend::IRSegment, ir::IRType};

use super::{Comparison, Ident, AST};

#[derive(Debug, Clone)]
pub struct ExpressionMeta<'a> {
    pub cached_rhs_ast: Option<AST<'a>>,
    pub cached_lhs_ast: Option<AST<'a>>,
    pub compiled_versions: RefCell<Option<HashMap<IRType, IRSegment>>>,
    pub expression_type: Option<ExpressionType<'a>>,
}
impl<'a> ExpressionMeta<'a> {
    pub fn has_rhs_ast(&self) -> bool {
        self.cached_rhs_ast.is_some()
    }
    pub fn has_lhs_ast(&self) -> bool {
        self.cached_lhs_ast.is_some()
    }
    pub fn has_type(&self) -> bool {
        self.expression_type.is_some()
    }
    pub fn invalidate(&mut self) {
        *self = Self::INVALID;
    }
    pub const INVALID: Self = Self {
        cached_lhs_ast: None,
        cached_rhs_ast: None,
        compiled_versions: RefCell::new(None),
        expression_type: None,
    };
}

#[derive(Clone)]
pub enum ExpressionType<'a> {
    Fn {
        name: Ident<'a>,
        params: ThinVec<Ident<'a>>,
    },
    Var(Ident<'a>),
    Eq {
        eq_type: EquationType,
    },
}
impl<'a> Debug for ExpressionType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn { name, params } => {
                let mut s = format!("Fn {}(", name.0);
                for p in params {
                    s.push_str(&p.0);
                    s.push(',');
                }
                let _ = s.pop();
                f.write_fmt(format_args!("{})", s))
            }
            Self::Var(arg0) => f.debug_tuple("Var").field(&arg0.0).finish(),
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
