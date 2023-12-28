use std::{collections::HashMap, fmt::Debug, hash::Hash};

use super::ir::{BroadcastArg, IRInstructionSeq, IROp, IRType, Id};
use crate::{
    ast::{
        parser::Expressions, ASTNode, ASTNodeId, BinaryOp, CoordinateAccess, Ident, List, Opcode,
        UnaryOp, Value, AST,
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
                    (Some(t), None) => self.build_broadcast_binary(node, arg0, t, arg1, op)?,
                    // Value [op] List
                    (None, Some(t)) => self.build_broadcast_binary(node, arg1, t, arg0, op)?,
                    // List [op] List
                    (Some(t0), Some(t1)) => {
                        if t1 != t0 {
                            compiler_error!(node, "cannot perform operation {:?} on a list of {:?} and a list of {:?}", op, t0, t1)
                        }
                        let t = t0;
                        //Generate broadcast header
                        let len0 = self.seq.place(IROp::ListLength(arg0));
                        let len1 = self.seq.place(IROp::ListLength(arg1));
                        let len = self.seq.place(IROp::Binary(len0, len1, BinaryOp::Min));
                        let begin = self.seq.place(IROp::BeginBroadcast {
                            inner_type: t,
                            end_index: len,
                        });
                        let arg0_bcd = BroadcastArg { t, id: 0 };
                        let arg1_bcd = BroadcastArg { t, id: 1 };
                        self.seq.push(IROp::SetBroadcastArg(arg0, arg0_bcd));
                        self.seq.push(IROp::SetBroadcastArg(arg1, arg1_bcd));
                        //Generate broadcast body
                        let arg0_inner = self.seq.place(IROp::LoadBroadcastArg(arg0_bcd));
                        let arg1_inner = self.seq.place(IROp::LoadBroadcastArg(arg1_bcd));
                        let ret = self.build_simple_binary_fn(node, arg0_inner, arg1_inner, *op)?;
                        self.seq.push(IROp::EndBroadcast { begin, ret });
                        begin
                    }
                    // Value [op] Value
                    (None, None) => self.build_simple_binary_fn(node, arg0, arg1, *op)?,
                }
            }
            ASTNode::Unary(v, op) => {
                //TODO: come up with a cleaner way to deal with broadcast codegen
                let inner = self.rec_build_ir(*v, expr, frame)?;
                if let Some(lt) = inner.t.downcast_list() {
                    let len = self.seq.place(IROp::ListLength(inner));
                    let begin = self.seq.place(IROp::BeginBroadcast {
                        inner_type: lt,
                        end_index: len,
                    });
                    let arg = BroadcastArg { t: lt, id: 0 };
                    self.seq.push(IROp::SetBroadcastArg(inner, arg));
                    let vali = self.seq.place(IROp::LoadBroadcastArg(arg));
                    let v = self.build_simple_unary_fn(node, vali, *op)?;
                    self.seq.place(IROp::EndBroadcast {
                        begin: begin,
                        ret: v,
                    })
                } else {
                    self.build_simple_unary_fn(node, inner, *op)?
                }
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
    fn build_simple_unary_fn(&mut self, node: ASTNodeId, val: Id, op: UnaryOp) -> Result<Id> {
        Ok(match (val.t, op) {
            //Special case for unary negation. All other unary operations are disallowed on points
            (IRType::Vec2, UnaryOp::Neg) => {
                let x = self
                    .seq
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessX));
                let y = self
                    .seq
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessY));
                let xn = self.seq.place(IROp::Unary(x, UnaryOp::Neg));
                let yn = self.seq.place(IROp::Unary(y, UnaryOp::Neg));
                self.seq.place(IROp::Vec2(xn, yn))
            }
            (IRType::Vec3, UnaryOp::Neg) => {
                let x = self
                    .seq
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessX));
                let y = self
                    .seq
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessY));
                let z = self
                    .seq
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessZ));
                let xn = self.seq.place(IROp::Unary(x, UnaryOp::Neg));
                let yn = self.seq.place(IROp::Unary(y, UnaryOp::Neg));
                let zn = self.seq.place(IROp::Unary(z, UnaryOp::Neg));
                self.seq.place(IROp::Vec3(xn, yn, zn))
            }
            //all unary operations are allowed on numbers
            (IRType::Number, op) => self.seq.place(IROp::Unary(val, op)),
            (t, op) => compiler_error!(node, "cannot perform function {:?} on a {:?}", op, t),
        })
    }
    fn build_broadcast_binary(
        &mut self,
        node: ASTNodeId,
        list_arg: Id,
        list_arg_t: IRType,
        value_arg: Id,
        op: &BinaryOp,
    ) -> Result<Id> {
        let arg0 = list_arg;
        let t = list_arg_t;
        let arg1 = value_arg;
        //Generate broadcast header
        let len = self.seq.place(IROp::ListLength(arg0));
        let list = self.seq.place(t.list_of(len)?);
        let begin = self.seq.place(IROp::BeginBroadcast {
            inner_type: t,
            end_index: len,
        });
        let arg0_bcd = BroadcastArg { t, id: 0 };
        let arg1_bcd = BroadcastArg { t: arg1.t, id: 1 };
        self.seq.push(IROp::SetBroadcastArg(arg0, arg0_bcd));
        self.seq.push(IROp::SetBroadcastArg(arg1, arg1_bcd));
        //Generate broadcast body
        let arg0_inner = self.seq.place(IROp::LoadBroadcastArg(arg0_bcd));
        let arg1_inner = self.seq.place(IROp::LoadBroadcastArg(arg1_bcd));
        let ret = self.build_simple_binary_fn(node, arg0_inner, arg1_inner, *op)?;
        self.seq.push(IROp::EndBroadcast { begin, ret });
        Ok(begin)
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
