use std::{collections::HashMap, fmt::Debug, hash::Hash};

use super::ir::{IRInstructionSeq, IROp, IRType, Id};
use crate::{
    ast::{
        parser::Expressions, ASTNode, ASTNodeId, BinaryOp, CoordinateAccess, Ident, List, Value,
        AST,
    },
    compiler_error, permute,
};
use anyhow::{bail, Result};

#[derive(Debug)]
pub struct Frontend<'borrow, 'source> {
    ctx: &'borrow Expressions<'source>,
    seq: IRInstructionSeq,
}
impl<'borrow, 'source> Frontend<'borrow, 'source> {
    fn rec_build_ir(
        &mut self,
        node: ASTNodeId,
        expr: &'borrow AST<'source>,
        frame: &impl Frame,
    ) -> Result<Id> {
        let res = match expr.get_node(node)? {
            ASTNode::Val(v) => match v {
                Value::ConstantI64(v) => self.seq.place(IROp::IConst(*v)),
                Value::ConstantF64(v) => self.seq.place(IROp::Const(*v)),
                Value::Ident(s) => {
                    if let Some(t) = frame.map(s.as_str()) {
                        t
                    } else {
                        let a = self.ctx.ident_ast(&s)?;
                        self.rec_build_ir(a.root.unwrap(), a, frame)?
                    }
                }
            },
            ASTNode::Binary(arg0, arg1, op) => {
                let arg0 = self.rec_build_ir(*arg0, expr, frame)?;
                let arg1 = self.rec_build_ir(*arg1, expr, frame)?;
                match (arg0.t.downcast_list(), arg1.t.downcast_list()) {
                    // List [op] Value
                    (Some(t), None) => {
                        let 
                        self.seq.push(IROp::BeginBroadcast { end_index: (), write_to: () })
                    }
                    // Value [op] Value
                    (None, None) => {
                        self.build_simple_binary_fn(node, arg0, arg1, *op)?
                    }
                }

            }
            ASTNode::Unary(v, op) => {
                let t = self.rec_build_ir(*v, expr, frame)?;
                if t.t != IRType::Number {
                    compiler_error!(v, "cannot call unary operation {:?} on a {:?}", op, t)
                }
                t
            }
            ASTNode::Parens(i, n) => {
                todo!()
            }
            ASTNode::FunctionCall(f, a) => todo!(),
            ASTNode::Index(l, v) => todo!(),
            ASTNode::List(l) => {
                /*match l {
                    List::List(v) => {
                        if let Some(f) = v.first() {
                            let t = self.rec_build_ir(*f, expr, frame)?;
                            for entry in v {
                                if self.rec_build_ir(node, expr, frame)? != t {
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
                            m.insert(id.as_str(), self.rec_build_ir(*n, expr, frame)?);
                        }
                        let new_frame = frame.clone().union(ArgsFrame { m });
                        self.rec_build_ir(*body, expr, &new_frame)?
                    }
                    List::Range(_, _) => IRType::NumberList,
                }*/
                todo!()
            }
            ASTNode::Point(_, _) => todo!(),
            ASTNode::ListOp(_, _) => todo!(),
            ASTNode::CoordinateAccess(_, _) => todo!(),
            ASTNode::Comparison(_, _, _) => todo!(),
            ASTNode::Piecewise {
                default: _,
                entries: _,
            } => todo!(),
        };
        Ok(res)
    }
    fn build_simple_binary_fn(
        &mut self,
        node: ASTNodeId,
        arg0: Id,
        arg1: Id,
        op: BinaryOp,
    ) -> Result<Id> {
        Ok(match (arg0.t, arg1.t, op) {
            (IRType::Number, IRType::Number, _) => self.seq.place(IROp::Binary(arg0, arg1, op)),
            permute!(IRType::Number, IRType::Vec2, BinaryOp::Mul)
            | (IRType::Vec2, IRType::Number, BinaryOp::Div) => {
                let (x, y, number);
                if arg0.t == IRType::Vec2 {
                    (x, y) = self.seq.coordinates_of2d(arg0);
                    number = arg1;
                } else {
                    (x, y) = self.seq.coordinates_of2d(arg1);
                    number = arg0;
                }
                let xn = self.seq.place(IROp::Binary(x, number, op));
                let yn = self.seq.place(IROp::Binary(y, number, op));
                self.seq.place(IROp::Vec2(xn, yn))
            }
            permute!(IRType::Number, IRType::Vec3, BinaryOp::Mul | BinaryOp::Div) => {
                let (x, y, z, number);
                if arg0.t == IRType::Vec3 {
                    (x, y, z) = self.seq.coordinates_of3d(arg0);
                    number = arg1;
                } else {
                    (x, y, z) = self.seq.coordinates_of3d(arg1);
                    number = arg0;
                }
                let xn = self.seq.place(IROp::Binary(x, number, op));
                let yn = self.seq.place(IROp::Binary(y, number, op));
                let zn = self.seq.place(IROp::Binary(z, number, op));
                self.seq.place(IROp::Vec2(xn, yn))
            }
            //TODO: better compiler errors here
            (a, b, c) => {
                compiler_error!(
                    node,
                    "operation {:?} cannot be called on a {}{:?} and a {}{:?}",
                    c,
                    if arg0.t.downcast_list().is_some() {
                        "list of "
                    } else {
                        ""
                    },
                    a,
                    if arg1.t.downcast_list().is_some() {
                        "list of "
                    } else {
                        ""
                    },
                    b
                )
            }
        })
    }
}

#[derive(Debug, Clone)]
struct ArgsFrame<'a> {
    m: HashMap<&'a str, Id>,
}
impl<'a> Frame for ArgsFrame<'a> {
    fn map(&self, id: &str) -> Option<Id> {
        self.m.get(id).map(|a| *a)
    }
}

#[derive(Debug, Clone)]
struct EmptyFrame {}
impl Frame for EmptyFrame {
    fn map(&self, id: &str) -> Option<Id> {
        None
    }
}
pub trait Frame: Debug + Clone {
    fn map(&self, id: &str) -> Option<Id>;
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
    fn map(&self, id: &str) -> Option<Id> {
        let n = self.new.map(id);
        if n.is_some() {
            return n;
        }
        self.old.map(id)
    }
}
