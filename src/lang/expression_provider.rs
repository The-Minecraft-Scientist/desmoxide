use std::fmt::Debug;

use crate::{
    graph::expressions::ExpressionType,
    lang::ast::{Ident, AST},
};
use anyhow::Result;
use shrinkwraprs::Shrinkwrap;
use thin_vec::ThinVec;

use super::compiler::ir::Id;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Shrinkwrap)]
pub struct ExpressionId(pub u32);
pub trait ExpressionProvider: Debug {
    fn get_ident_id(&self, ident: &Ident) -> Result<ExpressionId>;
    fn expression_type(&self, id: ExpressionId) -> Result<&ExpressionType>;
    fn fn_ast(&self, id: ExpressionId) -> Result<(&ThinVec<Ident>, &AST)>;
    fn ident_ast(&self, id: ExpressionId) -> Result<&AST>;
    fn fn_ast_by_name(&self, ident: &Ident) -> Result<(&ThinVec<Ident>, &AST)> {
        self.fn_ast(self.get_ident_id(ident)?)
    }
    fn ident_ast_by_name(&self, ident: &Ident) -> Result<&AST> {
        self.ident_ast(self.get_ident_id(ident)?)
    }
    fn expression_type_by_name(&self, ident: &Ident) -> Result<&ExpressionType> {
        self.expression_type(self.get_ident_id(ident)?)
    }
}
