use std::{collections::HashMap, fmt::Debug, hash::Hash};

use super::ir::IRType;
use crate::{
    ast::{parser::Expressions, ASTNode, ASTNodeId, BinaryOp, Ident, List, Value, AST},
    compiler_error, permute,
};
use anyhow::{bail, Result};

#[derive(Debug)]
pub struct Frontend<'borrow, 'source> {
    ctx: &'borrow Expressions<'source>,
}
impl<'borrow, 'source> Frontend<'borrow, 'source> {
    fn rec_resolve_type(
        &mut self,
        node: ASTNodeId,
        expr: &'borrow AST<'source>,
        frame: &impl Frame,
    ) -> Result<IRType> {
        Ok(match expr.get_node(node)? {
            ASTNode::Val(v) => match v {
                Value::ConstantF64(_) | Value::ConstantI64(_) => IRType::Number,
                Value::Ident(s) => {
                    if let Some(t) = frame.map(s.as_str()) {
                        t
                    } else {
                        let a = self.ctx.ident_ast(&s)?;
                        self.rec_resolve_type(a.root.unwrap(), a, frame)?
                    }
                }
            },
            ASTNode::Binary(arg0, arg1, op) => {
                let arg0 = self.rec_resolve_type(*arg0, expr, frame)?;
                let arg1 = self.rec_resolve_type(*arg1, expr, frame)?;
                match (
                    arg0.downcast_list().unwrap_or(arg0),
                    arg1.downcast_list().unwrap_or(arg1),
                    op,
                ) {
                    (IRType::Number, IRType::Number, _) => IRType::Number,
                    permute!(IRType::Number, IRType::Vec2, BinaryOp::Mul | BinaryOp::Div) => {
                        IRType::Vec2
                    }
                    permute!(IRType::Number, IRType::Vec3, BinaryOp::Mul | BinaryOp::Div) => {
                        IRType::Vec3
                    }
                    (IRType::Vec2, IRType::Vec2, BinaryOp::Add | BinaryOp::Sub) => IRType::Vec2,
                    (IRType::Vec3, IRType::Vec3, BinaryOp::Add | BinaryOp::Sub) => IRType::Vec3,
                    //TODO: better compiler errors here
                    (a, b, c) => {
                        compiler_error!(
                            node,
                            "operation {:?} cannot be called on a {}{:?} and a {}{:?}",
                            c,
                            if arg0.downcast_list().is_some() {
                                "list of "
                            } else {
                                ""
                            },
                            a,
                            if arg1.downcast_list().is_some() {
                                "list of "
                            } else {
                                ""
                            },
                            b
                        )
                    }
                }
            }
            ASTNode::Unary(v, op) => {
                let t = self.rec_resolve_type(*v, expr, frame)?;
                if t != IRType::Number {
                    compiler_error!(v, "cannot call unary operation {:?} on a {:?}", op, t)
                }
                t
            }
            ASTNode::Parens(i, n) => {}
            ASTNode::FunctionCall(f, a) => todo!(),
            ASTNode::Index(l, v) => todo!(),
            ASTNode::List(l) => {
                match l {
                    List::List(v) => {
                        if let Some(f) = v.first() {
                            let t = self.rec_resolve_type(*f, expr, frame)?;
                            for entry in v {
                                if self.rec_resolve_type(node, expr, frame)? != t {
                                    compiler_error!(
                                        node,
                                        "List entries must all have the same type"
                                    )
                                }
                            }
                            t
                        } else {
                            //TODO: empty list should probably have its own type (presumably what the Desmos ListOfAny IR type is used for)
                            IRType::NumberList
                        }
                    }
                    List::ListComp(body, vars) => {
                        let mut m = HashMap::with_capacity(vars.vars.len());
                        for (id, n) in vars.vars.iter() {
                            m.insert(id.as_str(), self.rec_resolve_type(*n, expr, frame)?);
                        }
                        let new_frame = frame.clone().union(ArgsFrame { m });
                        self.rec_resolve_type(*body, expr, &new_frame)?
                    }
                    List::Range(_, _) => IRType::NumberList,
                }
            }
            ASTNode::Point(_, _) => todo!(),
            ASTNode::ListOp(_, _) => todo!(),
            ASTNode::CoordinateAccess(_, _) => todo!(),
            ASTNode::Comparison(_, _, _) => todo!(),
            ASTNode::Piecewise {
                default: _,
                entries: _,
            } => todo!(),
        })
    }
}
#[derive(Debug, Clone)]
struct ArgsFrame<'a> {
    m: HashMap<&'a str, IRType>,
}
impl<'a> Frame for ArgsFrame<'a> {
    fn map(&self, id: &str) -> Option<IRType> {
        self.m.get(id).map(|a| *a)
    }
}

#[derive(Debug, Clone)]
struct EmptyFrame {}
impl Frame for EmptyFrame {
    fn map(&self, id: &str) -> Option<IRType> {
        None
    }
}
pub trait Frame: Debug + Clone {
    fn map(&self, id: &str) -> Option<IRType>;
    fn union<T: Frame>(self, new: T) -> FrameUnion<Self, T> {
        FrameUnion { old: self, new }
    }
}
#[derive(Debug, Clone)]
pub struct FrameUnion<Old: Frame, New: Frame> {
    old: Old,
    new: New,
}
impl<T: Frame, U: Frame> Frame for FrameUnion<T, U> {
    fn map(&self, id: &str) -> Option<IRType> {
        let n = self.new.map(id);
        if n.is_some() {
            return n;
        }
        self.old.map(id)
    }
}
