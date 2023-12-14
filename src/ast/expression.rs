use thin_vec::ThinVec;

use super::{parse_manager::AST, ASTNode, ASTNodeRef, Ident, Opcode};

#[derive(Debug, Clone)]
pub struct ExpressionMeta<'a> {
    pub cached_rhs_ast: Option<AST<'a>>,
    pub cached_lhs_ast: Option<AST<'a>>,
    pub ast: Vec<ASTNode<'a>>,
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
        expression_type: None,
        ast: Vec::new(),
    };
}

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, Copy)]
pub enum EquationType {
    Implicit,
    Explicit,
    InEq(Opcode),
}
