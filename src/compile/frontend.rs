use std::{collections::HashMap, fmt::Debug, hash::Hash};

use super::ir::{ArgId, BroadcastArg, EndIndex, IRInstructionSeq, IROp, IRType, Id};
use crate::{
    ast::{
        parser::{Expressions, FnId},
        ASTNode, ASTNodeId, BinaryOp, CoordinateAccess, Ident, List, Opcode, UnaryOp, Value, AST,
    },
    compile::ir::BinaryListOp,
    compiler_error, permute,
};
use anyhow::{bail, Context, Result};
use thin_vec::ThinVec;

#[derive(Debug)]
pub struct Frontend<'borrow, 'source> {
    ctx: &'borrow Expressions<'source>,
}
/// Contains a standalone executable IR sequence along with metadata about its arguments and their types
#[derive(Debug, Clone)]
pub struct IRSegment {
    args: Vec<IRType>,
    instructions: IRInstructionSeq,
    ret: Option<Id>,
}
impl IRSegment {
    pub fn new(args: Vec<IRType>) -> Self {
        Self {
            args,
            instructions: IRInstructionSeq::new(),
            ret: None,
        }
    }
}
impl<'borrow, 'source> Frontend<'borrow, 'source> {
    fn compile_expr(&mut self, expr: &'borrow AST<'source>) -> Result<IRSegment> {
        let mut frame = Frame::empty();
        let mut segment = IRSegment::new(vec![IRType::Number, IRType::Number]);
        let x = segment.instructions.place(IROp::LoadArg(ArgId(Id {
            idx: 0,
            t: IRType::Number,
        })));
        let y = segment.instructions.place(IROp::LoadArg(ArgId(Id {
            idx: 1,
            t: IRType::Number,
        })));
        let mut s = Scope::with_capacity(2);
        s.insert("x", x);
        s.insert("y", y);
        frame.push_scope(s);
        let ret_id = self.rec_build_ir(
            &mut segment,
            expr.root
                .context("Tried to compile an AST that was not successfully parsed")?,
            expr,
            &frame,
        )?;
        segment.ret = Some(ret_id);
        Ok(segment)
    }
    fn compile_fn(
        &mut self,
        arg_types: &Vec<IRType>,
        args: &ThinVec<Ident<'source>>,
        expr: &'borrow AST<'source>,
    ) -> Result<IRSegment> {
        let mut frame = Frame::empty();
        let mut scope = Scope::with_capacity(args.len());
        let mut segment = IRSegment::new(arg_types.clone());
        for (index, ident) in args.iter().enumerate() {
            let arg = segment.instructions.place(IROp::LoadArg(ArgId(Id {
                idx: index as u32,
                t: arg_types[index],
            })));
            scope.insert(ident.as_str(), arg);
        }
        frame.push_scope(scope);
        let ret_id = self.rec_build_ir(
            &mut segment,
            expr.root
                .context("Tried to compile an AST that was not parsed")?,
            expr,
            &frame,
        )?;
        segment.ret = Some(ret_id);
        Ok(segment)
    }
    pub fn compile_and_cache_fn(&mut self, idx: u32, args: Vec<IRType>) -> Result<FnId> {
        let (argnames, ast) = self.ctx.fn_ident_ast(idx)?;
        let segment = self.compile_fn(&args, &argnames, ast)?;
        let t = segment.ret.unwrap().t;
        self.ctx.cache_compiled_fn(idx, t, segment)?;
        Ok(FnId(idx, t))
    }
    fn rec_build_ir(
        &mut self,
        segment: &mut IRSegment,
        node: ASTNodeId,
        expr: &'borrow AST<'source>,
        frame: &Frame,
    ) -> Result<Id> {
        let res = match expr.get_node(node)? {
            ASTNode::Val(v) => match v {
                Value::ConstantI64(v) => segment.instructions.place(IROp::IConst(*v)),
                Value::ConstantF64(v) => segment.instructions.place(IROp::Const(*v)),
                Value::Ident(s) => {
                    if let Some(t) = frame.map_ident(s.as_str()) {
                        t
                    } else {
                        let a = self.ctx.ident_ast(&s)?;
                        self.rec_build_ir(segment, a.root.unwrap(), a, frame)?
                    }
                }
            },
            ASTNode::Binary(arg0, arg1, op) => {
                let arg0 = self.rec_build_ir(segment, *arg0, expr, frame)?;
                let arg1 = self.rec_build_ir(segment, *arg1, expr, frame)?;
                match (arg0.t.downcast_list(), arg1.t.downcast_list()) {
                    // List [op] Value
                    (Some(t), None) => {
                        self.build_broadcast_binary(segment, node, arg0, t, arg1, op)?
                    }
                    // Value [op] List
                    (None, Some(t)) => {
                        self.build_broadcast_binary(segment, node, arg1, t, arg0, op)?
                    }
                    // List [op] List
                    (Some(t0), Some(t1)) => {
                        if t1 != t0 {
                            compiler_error!(node, "cannot perform operation {:?} on a list of {:?} and a list of {:?}", op, t0, t1)
                        }
                        let t = t0;
                        //Generate broadcast header
                        let len0 = segment.instructions.place(IROp::ListLength(arg0));
                        let len1 = segment.instructions.place(IROp::ListLength(arg1));
                        let len =
                            segment
                                .instructions
                                .place(IROp::Binary(len0, len1, BinaryOp::Min));
                        let begin = segment.instructions.place(IROp::BeginBroadcast {
                            inner_type: t,
                            end_index: super::ir::EndIndex::Val(len),
                        });
                        let arg0_bcd = BroadcastArg { t, id: 0 };
                        let arg1_bcd = BroadcastArg { t, id: 1 };
                        segment
                            .instructions
                            .push(IROp::SetBroadcastArg(arg0, arg0_bcd));
                        segment
                            .instructions
                            .push(IROp::SetBroadcastArg(arg1, arg1_bcd));
                        //Generate broadcast body
                        let arg0_inner =
                            segment.instructions.place(IROp::LoadBroadcastArg(arg0_bcd));
                        let arg1_inner =
                            segment.instructions.place(IROp::LoadBroadcastArg(arg1_bcd));
                        let ret = self
                            .build_simple_binary_fn(segment, node, arg0_inner, arg1_inner, *op)?;
                        segment.instructions.push(IROp::EndBroadcast { begin, ret });
                        begin
                    }
                    // Value [op] Value
                    (None, None) => self.build_simple_binary_fn(segment, node, arg0, arg1, *op)?,
                }
            }
            ASTNode::Unary(v, op) => {
                //TODO: come up with a cleaner way to deal with broadcast codegen
                let inner = self.rec_build_ir(segment, *v, expr, frame)?;
                if let Some(lt) = inner.t.downcast_list() {
                    let len = segment.instructions.place(IROp::ListLength(inner));
                    let begin = segment.instructions.place(IROp::BeginBroadcast {
                        inner_type: lt,
                        end_index: EndIndex::Val(len),
                    });
                    let arg = BroadcastArg { t: lt, id: 0 };
                    segment.instructions.push(IROp::SetBroadcastArg(inner, arg));
                    let vali = segment.instructions.place(IROp::LoadBroadcastArg(arg));
                    let v = self.build_simple_unary_fn(segment, node, vali, *op)?;
                    segment
                        .instructions
                        .place(IROp::EndBroadcast { begin, ret: v })
                } else {
                    self.build_simple_unary_fn(segment, node, inner, *op)?
                }
            }
            ASTNode::Parens(ident, n) => {
                let rhs = self.rec_build_ir(segment, *n, expr, frame)?;
                // ident could be an Ident or Fn expression. We handle the Ident case first

                //locally defined value
                if let Some(id) = frame.map_ident(ident.as_str()) {
                    return Ok(segment
                        .instructions
                        .place(IROp::Binary(id, rhs, BinaryOp::Mul)));
                }
                //wackscope
                if let Ok(a) = self.ctx.ident_ast(ident.as_str()) {
                    let lhs = self.rec_build_ir(segment, a.root.unwrap(), a, frame)?;
                    return Ok(segment
                        .instructions
                        .place(IROp::Binary(lhs, rhs, BinaryOp::Mul)));
                }
                //function call
                if let Ok(idx) = self.ctx.fn_ident_id(ident.as_str()) {
                    let t = self.compile_and_cache_fn(idx, vec![rhs.t])?;
                    let id = segment.instructions.place(IROp::FnCall(t));
                    segment.instructions.push(IROp::FnArg(rhs));
                    return Ok(id);
                }
                compiler_error!(
                    node,
                    "\"{}\" does not reference any value or function",
                    ident.as_str()
                )
            }
            ASTNode::FunctionCall(f, a) => {
                let mut args = Vec::with_capacity(a.len());
                for arg in a {
                    args.push(self.rec_build_ir(segment, *arg, expr, frame)?);
                }
                let fn_id = self.compile_and_cache_fn(
                    self.ctx.fn_ident_id(f.as_str())?,
                    args.iter().map(|id| id.t).collect::<Vec<_>>(),
                )?;
                let id = segment.instructions.place(IROp::FnCall(fn_id));
                let _ = segment
                    .instructions
                    .place_block(&args.iter().map(|a| IROp::FnArg(*a)).collect::<Vec<_>>());
                id
            }
            ASTNode::Index(list, index) => {
                let l = self.rec_build_ir(segment, *list, expr, frame)?;
                match expr.get_node(*index)? {
                    //List filter
                    ASTNode::Comparison(rhs, comp, lhs) => {
                        //TODO: assertions for all of this

                        let begin = segment.instructions.place(IROp::BeginBroadcast {
                            inner_type: l
                                .t
                                .downcast_list()
                                .context("expected a list, got a value")?,
                            end_index: EndIndex::Full,
                        });
                        let arg = BroadcastArg { t: l.t, id: 0 };
                        segment.instructions.push(IROp::SetBroadcastArg(l, arg));
                        let val = segment.instructions.place(IROp::LoadBroadcastArg(arg));
                        let comp_lhs = self.rec_build_ir(segment, *lhs, expr, frame)?;
                        let comp_rhs = self.rec_build_ir(segment, *rhs, expr, frame)?;
                        let comp = segment.instructions.place(IROp::Comparison {
                            lhs: comp_lhs,
                            comp: *comp,
                            rhs: comp_rhs,
                        });
                        let nop = segment.instructions.place(IROp::Nop);
                        let ret = segment
                            .instructions
                            .place(IROp::BeginPiecewise { comp, res: val });
                        segment
                            .instructions
                            .push(IROp::EndPiecewise { default: nop });
                        segment.instructions.push(IROp::EndBroadcast { begin, ret });
                        begin
                    }
                    //normal indexing
                    a => {
                        let idx_val = self.rec_build_ir(segment, *index, expr, frame)?;
                        segment.instructions.place(IROp::BinaryListOp(
                            l,
                            idx_val,
                            BinaryListOp::IndexRead,
                        ))
                    }
                }
            }
            ASTNode::List(l) => todo!(),
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
        segment: &mut IRSegment,
        node: ASTNodeId,
        arg0: Id,
        arg1: Id,
        op: BinaryOp,
    ) -> Result<Id> {
        Ok(match (arg0.t, arg1.t, op) {
            (IRType::Number, IRType::Number, _) => {
                segment.instructions.place(IROp::Binary(arg0, arg1, op))
            }
            permute!(IRType::Number, IRType::Vec2, BinaryOp::Mul)
            | (IRType::Vec2, IRType::Number, BinaryOp::Div) => {
                let (x, y, number);
                if arg0.t == IRType::Vec2 {
                    (x, y) = segment.instructions.coordinates_of2d(arg0);
                    number = arg1;
                } else {
                    (x, y) = segment.instructions.coordinates_of2d(arg1);
                    number = arg0;
                }
                let xn = segment.instructions.place(IROp::Binary(x, number, op));
                let yn = segment.instructions.place(IROp::Binary(y, number, op));
                segment.instructions.place(IROp::Vec2(xn, yn))
            }
            permute!(IRType::Number, IRType::Vec3, BinaryOp::Mul | BinaryOp::Div) => {
                let (x, y, z, number);
                if arg0.t == IRType::Vec3 {
                    (x, y, z) = segment.instructions.coordinates_of3d(arg0);
                    number = arg1;
                } else {
                    (x, y, z) = segment.instructions.coordinates_of3d(arg1);
                    number = arg0;
                }
                let xn = segment.instructions.place(IROp::Binary(x, number, op));
                let yn = segment.instructions.place(IROp::Binary(y, number, op));
                let zn = segment.instructions.place(IROp::Binary(z, number, op));
                segment.instructions.place(IROp::Vec2(xn, yn))
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
    fn build_simple_unary_fn(
        &mut self,
        segment: &mut IRSegment,
        node: ASTNodeId,
        val: Id,
        op: UnaryOp,
    ) -> Result<Id> {
        Ok(match (val.t, op) {
            //Special case for unary negation. All other unary operations are disallowed on points
            (IRType::Vec2, UnaryOp::Neg) => {
                let x = segment
                    .instructions
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessX));
                let y = segment
                    .instructions
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessY));
                let xn = segment.instructions.place(IROp::Unary(x, UnaryOp::Neg));
                let yn = segment.instructions.place(IROp::Unary(y, UnaryOp::Neg));
                segment.instructions.place(IROp::Vec2(xn, yn))
            }
            (IRType::Vec3, UnaryOp::Neg) => {
                let x = segment
                    .instructions
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessX));
                let y = segment
                    .instructions
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessY));
                let z = segment
                    .instructions
                    .place(IROp::CoordinateOf(val, CoordinateAccess::DotAccessZ));
                let xn = segment.instructions.place(IROp::Unary(x, UnaryOp::Neg));
                let yn = segment.instructions.place(IROp::Unary(y, UnaryOp::Neg));
                let zn = segment.instructions.place(IROp::Unary(z, UnaryOp::Neg));
                segment.instructions.place(IROp::Vec3(xn, yn, zn))
            }
            //all unary operations are allowed on numbers
            (IRType::Number, op) => segment.instructions.place(IROp::Unary(val, op)),
            (t, op) => compiler_error!(node, "cannot perform function {:?} on a {:?}", op, t),
        })
    }
    fn build_broadcast_binary(
        &mut self,
        segment: &mut IRSegment,
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
        let begin = segment.instructions.place(IROp::BeginBroadcast {
            inner_type: t,
            end_index: EndIndex::Full,
        });
        let arg0_bcd = BroadcastArg { t, id: 0 };
        let arg1_bcd = BroadcastArg { t: arg1.t, id: 1 };
        segment
            .instructions
            .push(IROp::SetBroadcastArg(arg0, arg0_bcd));
        segment
            .instructions
            .push(IROp::SetBroadcastArg(arg1, arg1_bcd));
        //Generate broadcast body
        let arg0_inner = segment.instructions.place(IROp::LoadBroadcastArg(arg0_bcd));
        let arg1_inner = segment.instructions.place(IROp::LoadBroadcastArg(arg1_bcd));
        let ret = self.build_simple_binary_fn(segment, node, arg0_inner, arg1_inner, *op)?;
        segment.instructions.push(IROp::EndBroadcast { begin, ret });
        Ok(begin)
    }
}

#[derive(Debug, Clone)]
pub struct Frame<'a> {
    scopes: Vec<Scope<'a>>,
}
impl<'a> Frame<'a> {
    pub fn empty() -> Self {
        Self {
            scopes: Vec::with_capacity(1),
        }
    }
    pub fn push_scope(&mut self, scope: Scope<'a>) {
        self.scopes.push(scope);
    }
    pub fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }
    pub fn map_ident(&self, ident: &'a str) -> Option<Id> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(ident) {
                return Some(id);
            }
        }
        None
    }
}
#[derive(Debug, Clone)]
pub struct Scope<'a> {
    map: HashMap<&'a str, Id>,
}
impl<'a> Scope<'a> {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            map: HashMap::with_capacity(cap),
        }
    }
    pub fn insert(&mut self, k: &'a str, v: Id) {
        self.map.insert(k, v);
    }
    pub fn get(&self, k: &'a str) -> Option<Id> {
        self.map.get(k).map(|a| *a)
    }
}
