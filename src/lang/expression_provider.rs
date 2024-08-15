use std::fmt::Debug;

use crate::{
    graph::expressions::ExpressionType,
    lang::ast::{Ident, AST},
};
use anyhow::Result;
use shrinkwraprs::Shrinkwrap;
use thin_vec::ThinVec;

use super::ir::Id;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Shrinkwrap)]
pub struct ExpressionId(pub u32);
pub trait ExpressionProvider<'a>: Debug {
    fn get_ident_id(&self, ident: &str) -> Result<ExpressionId>;
    fn expression_type(&self, id: ExpressionId) -> Result<&ExpressionType<'a>>;
    fn fn_ast(&self, id: ExpressionId) -> Result<(&ThinVec<Ident<'a>>, &AST<'a>)>;
    fn ident_ast(&self, id: ExpressionId) -> Result<&AST<'a>>;
    fn fn_ast_by_name(&self, ident: &str) -> Result<(&ThinVec<Ident<'a>>, &AST<'a>)> {
        self.fn_ast(self.get_ident_id(ident)?)
    }
    fn ident_ast_by_name(&self, ident: &str) -> Result<&AST<'a>> {
        self.ident_ast(self.get_ident_id(ident)?)
    }
    fn expression_type_by_name(&self, ident: &str) -> Result<&ExpressionType<'a>> {
        self.expression_type(self.get_ident_id(ident)?)
    }
}
