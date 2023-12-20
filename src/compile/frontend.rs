use std::{collections::HashMap, marker::PhantomData};

use super::ir::IRType;
use crate::ast::{parser::Expressions, ASTNode, ASTNodeId, Ident, Value, AST};
use anyhow::Result;

#[derive(Debug)]
pub struct Frontend<'borrow, 'source> {
    ctx: &'borrow Expressions<'source>,
    t: &'borrow mut HashMap<ASTNodeId, IRType>,
}
impl<'borrow, 'source> Frontend<'borrow, 'source> {
    fn rec_resolve_type(&mut self, node: ASTNodeId, expr: &'borrow AST<'source>) -> Result<IRType> {
        Ok(match expr.get_node(node)? {
            ASTNode::Val(v) => match v {
                Value::ConstantF64(_) | Value::ConstantI64(_) => IRType::Number,
                Value::Ident(s) => {
                    let a = self.ctx.ident_ast(&s)?;
                    self.rec_resolve_type(a.root.unwrap(), a)?
                }
            },
            ASTNode::Binary(lhs, rhs, op) => todo!(),
            ASTNode::Unary(v, op) => todo!(),
            ASTNode::Parens(i, _) => todo!(),
            ASTNode::FunctionCall(_, _) => todo!(),
            ASTNode::Index(_, _) => todo!(),
            ASTNode::List(_) => todo!(),
            ASTNode::ListFilt(_, _) => todo!(),
            ASTNode::Point(_, _) => todo!(),
            ASTNode::ListOp(_, _) => todo!(),
            ASTNode::CoordinateAccess(_, _) => todo!(),
            ASTNode::Comparison(_, _, _) => todo!(),
            ASTNode::Piecewise { default, entries } => todo!(),
        })
    }
}
