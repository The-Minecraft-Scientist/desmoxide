use std::{collections::HashMap, fmt::Debug, sync::Arc};

use super::{
    super::ast::{
        ASTNode, ASTNodeId, BinaryOp, CoordinateAccess, Ident, List, ListOp, UnaryOp, Value, AST,
    },
    ir::{
        ArgId, BinaryListOp, BroadcastArg, EndIndex, FunctionId, IRInstructionSeq, IROp, IRSegment,
        IRType, Id,
    },
};
use crate::graph::expressions::{ExpressionId, ExpressionMeta};
use crate::{compiler_error, permute};

use anyhow::{bail, Context, Result};

use thin_vec::ThinVec;

#[derive(Debug)]
pub struct Frontend<'borrow> {
    pub exprs: &'borrow HashMap<ExpressionId, ExpressionMeta>,
    pub idents: &'borrow HashMap<Ident, ExpressionId>,
    pub fn_cache: HashMap<(u32, Vec<IRType>), Arc<IRSegment>>,
}

impl<'borrow> Frontend<'borrow> {
    pub fn new(
        exprs: &'borrow HashMap<ExpressionId, ExpressionMeta>,
        idents: &'borrow HashMap<Ident, ExpressionId>,
    ) -> Self {
        Self {
            exprs,
            idents,
            fn_cache: HashMap::with_capacity(10),
        }
    }
    pub fn compile_expr(&mut self, expr: &'borrow AST) -> Result<IRSegment> {
        let mut frame = Frame::empty();
        let mut segment = IRSegment::new(vec![IRType::Number, IRType::Number]);
        let x = segment.instructions.place(IROp::LoadArg(ArgId {
            idx: 0,
            t: IRType::Number,
        }));
        let y = segment.instructions.place(IROp::LoadArg(ArgId {
            idx: 1,
            t: IRType::Number,
        }));
        let mut s = Scope::with_capacity(2);
        s.insert("x", x);
        s.insert("y", y);
        frame.push_scope(s); // Don't need to pop the root scope
        let ret_id = self.rec_build_ir(
            &mut segment,
            expr.root
                .context("Tried to compile an AST that was not successfully parsed")?,
            expr,
            &mut frame,
        )?;
        segment.ret = ret_id;
        Ok(segment)
    }
    fn compile_fn(
        &mut self,
        arg_types: Vec<IRType>,
        args: &'borrow ThinVec<Ident>,
        expr: &'borrow AST,
    ) -> Result<IRSegment> {
        let mut frame = Frame::empty();
        let mut scope = Scope::with_capacity(args.len());
        let mut segment = IRSegment::new(arg_types.clone());
        for (index, ident) in args.iter().enumerate() {
            let arg = segment.instructions.place(IROp::LoadArg(ArgId {
                idx: index.try_into()?,
                t: arg_types[index],
            }));
            scope.insert(ident.as_str(), arg);
        }
        frame.push_scope(scope);
        let ret_id = self.rec_build_ir(
            &mut segment,
            expr.root
                .context("Tried to compile an AST that was not parsed")?,
            expr,
            &mut frame,
        )?;
        frame.pop_scope();
        segment.ret = ret_id;

        Ok(segment)
    }
    pub fn invalidate_cache(&mut self) {
        self.fn_cache.clear();
    }
    pub fn compile_and_cache_fn(
        &mut self,
        id: ExpressionId,
        arg_types: Vec<IRType>,
    ) -> Result<Arc<IRSegment>> {
        let k = (*id, arg_types);
        if let Some(compiled) = self.fn_cache.get(&k) {
            return Ok(Arc::clone(compiled));
        }
        if let ExpressionMeta::Fn {
            name,
            params: args,
            rhs: expr,
        } = self
            .exprs
            .get(&id)
            .context("getting function expression AST")?
        {
            let a = Arc::new(self.compile_fn(k.1.clone(), args, expr)?);
            self.fn_cache.insert((*id, k.1), Arc::clone(&a));
            Ok(a)
        } else {
            bail!("expected function at id: {:?}", id)
        }
    }
    pub fn direct_compile_fn(&mut self, i: &Ident) -> Result<IRSegment> {
        if let ExpressionMeta::Fn {
            name,
            params: args,
            rhs: expr,
        } = self
            .exprs
            .get(
                self.idents
                    .get(i)
                    .context(format!("could not find ident {:?}", i))?,
            )
            .context("getting function expression AST")?
        {
            let v = vec![IRType::Number; args.len()];

            self.compile_fn(v, args, expr)
        } else {
            bail!("expected function at id: {:?}", i)
        }
    }
    fn rec_build_ir(
        &mut self,
        segment: &mut IRSegment,
        node: ASTNodeId,
        expr: &'borrow AST,
        frame: &mut Frame<'borrow>,
    ) -> Result<Id> {
        let res = match expr.get_node(node)? {
            ASTNode::Val(v) => match v {
                Value::ConstantI64(v) => segment.instructions.place(IROp::IConst(*v)),
                Value::ConstantF64(v) => segment.instructions.place(IROp::Const(*v)),
                Value::Ident(s) => {
                    if let Some(t) = frame.map_ident(s.as_str()) {
                        t
                    } else {
                        if let ExpressionMeta::Var { rhs: expr, .. } = self
                            .exprs
                            .get(self.idents.get(s).context("getting ident id")?)
                            .context("getting function expression AST")?
                        {
                            let out =
                                self.rec_build_ir(segment, expr.root.unwrap(), expr, frame)?;
                            frame.insert_top(s.as_str(), out);
                            out
                        } else {
                            bail!("expected var at id: {:?}", s)
                        }
                    }
                }
            },
            ASTNode::Binary(arg0, arg1, op) => {
                let arg0 = self.rec_build_ir(segment, *arg0, expr, frame)?;
                let arg1 = self.rec_build_ir(segment, *arg1, expr, frame)?;
                match (arg0.t().downcast_list(), arg1.t().downcast_list()) {
                    // List [op] Value
                    (Some(t), None) => {
                        self.build_single_to_list_broadcast(segment, node, arg0, t, arg1, op)?
                    }
                    // Value [op] List
                    (None, Some(t)) => {
                        self.build_single_to_list_broadcast(segment, node, arg1, t, arg0, op)?
                    }
                    // List [op] List
                    (Some(t0), Some(t1)) => {
                        if t1 != t0 {
                            compiler_error!(node, "cannot perform operation {:?} on a list of {:?} and a list of {:?}", op, t0, t1)
                        }
                        let t = t0;
                        //Generate broadcast header
                        let len0 = segment
                            .instructions
                            .place(IROp::UnaryListOp(arg0, super::ir::UnaryListOp::Len));
                        let len1 = segment
                            .instructions
                            .place(IROp::UnaryListOp(arg1, super::ir::UnaryListOp::Len));
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
                let inner = self.rec_build_ir(segment, *v, expr, frame)?;
                if let Some(lt) = inner.t().downcast_list() {
                    let begin = segment.instructions.place(IROp::BeginBroadcast {
                        inner_type: lt,
                        end_index: EndIndex::Full,
                    });
                    let arg = BroadcastArg { t: lt, id: 0 };
                    segment.instructions.push(IROp::SetBroadcastArg(inner, arg));
                    let vali = segment.instructions.place(IROp::LoadBroadcastArg(arg));
                    let v = self.build_simple_unary_fn(segment, node, vali, *op)?;
                    segment
                        .instructions
                        .push(IROp::EndBroadcast { begin, ret: v });
                    begin
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

                let id = self
                    .idents
                    .get(ident)
                    .context("could not get id for ident")?;

                if let Some(t) = self.exprs.get(id) {
                    match t {
                        //wackscope
                        ExpressionMeta::Var { ident, rhs: ast } => {
                            let lhs = self.rec_build_ir(
                                segment,
                                ast.root.context("AST was not complete")?,
                                ast,
                                frame,
                            )?;
                            return Ok(segment.instructions.place(IROp::Binary(
                                lhs,
                                rhs,
                                BinaryOp::Mul,
                            )));
                        }
                        //function
                        ExpressionMeta::Fn {
                            name,
                            params,
                            rhs: expr,
                        } => {
                            let f = self.compile_and_cache_fn(*id, vec![rhs.t()])?;
                            let dep = segment.push_dependency(f, *id);
                            let id = segment.instructions.place(IROp::FnCall(dep));
                            let _ = segment.instructions.place(IROp::FnArg(rhs));
                        }
                        e => {
                            compiler_error!(
                                node,
                                "Got incorrect expression type {:?}, expected Var or Fn",
                                e
                            );
                        }
                    }
                    todo!()
                };
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
                let expr_id = self.idents.get(f).context("could not get expression id")?;
                let Some(ExpressionMeta::Fn { .. }) = self.exprs.get(expr_id) else {
                    compiler_error!(
                        node,
                        "identifier \"{}\" exists but is not a function!",
                        f.as_str()
                    )
                };
                let func = self.compile_and_cache_fn(
                    *expr_id,
                    args.iter().map(|id| id.t()).collect::<Vec<_>>(),
                )?;
                let id = segment.push_dependency(func, *expr_id);
                let id = segment.instructions.place(IROp::FnCall(id));
                let _ = segment
                    .instructions
                    .place_block(&args.iter().map(|a| IROp::FnArg(*a)).collect::<Vec<_>>());
                id
            }
            ASTNode::Index(list, index) => {
                let l = self.rec_build_ir(segment, *list, expr, frame)?;
                let inner_type = l
                    .t()
                    .downcast_list()
                    .context("expected a list, got a value")?;
                match expr.get_node(*index)? {
                    //List filter
                    ASTNode::Comparison(rhs, comp, lhs) => {
                        //TODO: assertions for all of thisshouldn't

                        let begin = segment.instructions.place(IROp::BeginBroadcast {
                            inner_type,
                            end_index: EndIndex::Full,
                        });
                        let arg = BroadcastArg { t: l.t(), id: 0 };
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
                    _a => {
                        //TODO: type assertions
                        let idx_val = self.rec_build_ir(segment, *index, expr, frame)?;
                        //Need to broadcast this index op
                        if idx_val.t() == IRType::NumberList {
                            let header = segment.instructions.place(IROp::BeginBroadcast {
                                inner_type,
                                end_index: EndIndex::Full,
                            });
                            let arg = BroadcastArg {
                                t: inner_type,
                                id: 0,
                            };
                            segment
                                .instructions
                                .push(IROp::SetBroadcastArg(idx_val, arg));
                            let idx = segment.instructions.place(IROp::LoadBroadcastArg(arg));
                            let indexed = segment.instructions.place(IROp::BinaryListOp(
                                l,
                                idx,
                                BinaryListOp::IndexRead,
                            ));
                            segment.instructions.push(IROp::EndBroadcast {
                                begin: header,
                                ret: indexed,
                            });
                            header
                        } else {
                            segment.instructions.place(IROp::BinaryListOp(
                                l,
                                idx_val,
                                BinaryListOp::IndexRead,
                            ))
                        }
                    }
                }
            }
            ASTNode::List(l) => match l {
                List::ListComp(_expr, _vals) => todo!(),
                List::Range(_begin, _end) => todo!(),
                List::List(exprs) => {
                    let mut it = exprs.iter();
                    let Some(head) = it.next() else {
                        compiler_error!(node, "desmoxide does not yet support empty list literals");
                    };
                    let head = self.rec_build_ir(segment, *head, expr, frame)?;
                    let i = it
                        .map(|id| self.rec_build_ir(segment, *id, expr, frame))
                        .collect::<Vec<_>>()
                        .into_iter();
                    let head = segment.instructions.place(IROp::ListLit(head));
                    for val in i {
                        segment.instructions.push(IROp::ListLit(val?));
                    }
                    head
                }
            },
            ASTNode::Point(x, y) => {
                let x = self.rec_build_ir(segment, *x, expr, frame)?;
                let y = self.rec_build_ir(segment, *y, expr, frame)?;
                segment.instructions.place(IROp::Vec2(x, y))
            }
            ASTNode::ListOp(v, op) => match op {
                ListOp::Min => {
                    let list = self.join_list_args(segment, node, v, expr, frame)?;
                    segment
                        .instructions
                        .place(IROp::UnaryListOp(list, super::ir::UnaryListOp::Min))
                }
                ListOp::Max => {
                    let list = self.join_list_args(segment, node, v, expr, frame)?;
                    segment
                        .instructions
                        .place(IROp::UnaryListOp(list, super::ir::UnaryListOp::Max))
                }
                ListOp::Count => todo!(),
                ListOp::Total => todo!(),
                ListOp::Join => todo!(),
                ListOp::Length => todo!(),
                ListOp::Unique => todo!(),
                ListOp::Sort => todo!(),
                ListOp::Shuffle => todo!(),
                //Welcome to hell
                ListOp::Random => {
                    let mut it = v.iter();
                    if let Some(first) = it.next() {
                        let first = self.rec_build_ir(segment, *first, expr, frame)?;
                        if let Some(_) = first.t().downcast_list() {
                            if let Some(second) = it.next() {
                                let second = self.rec_build_ir(segment, *second, expr, frame)?;
                                let seed;
                                if let Some(s) = it.next() {
                                    seed = Some(self.rec_build_ir(segment, *s, expr, frame)?);
                                } else {
                                    seed = None;
                                }
                                segment.instructions.place(IROp::Random(
                                    super::ir::RandomOp::Permute {
                                        list: first,
                                        count: Some(second),
                                        seed,
                                    },
                                ))
                            } else {
                                segment.instructions.place(IROp::Random(
                                    super::ir::RandomOp::Permute {
                                        list: first,
                                        count: None,
                                        seed: None,
                                    },
                                ))
                            }
                        } else {
                            if let Some(second) = it.next() {
                                let second = self.rec_build_ir(segment, *second, expr, frame)?;
                                let seed;
                                if let Some(sid) = it.next() {
                                    seed = Some(self.rec_build_ir(segment, *sid, expr, frame)?);
                                } else {
                                    seed = None;
                                }
                                segment.instructions.place(IROp::Random(
                                    super::ir::RandomOp::Count {
                                        count: second,
                                        seed,
                                    },
                                ))
                            } else {
                                segment.instructions.place(IROp::Random(
                                    super::ir::RandomOp::Count {
                                        count: first,
                                        seed: None,
                                    },
                                ))
                            }
                        }
                    } else {
                        segment
                            .instructions
                            .place(IROp::Random(super::ir::RandomOp::Single))
                    }
                }
            },
            ASTNode::CoordinateAccess(p, access) => {
                let p = self.rec_build_ir(segment, *p, expr, frame)?;
                segment.instructions.place(IROp::CoordinateOf(p, *access))
            }
            ASTNode::Comparison(lhs, comp, rhs) => {
                let lhs = self.rec_build_ir(segment, *lhs, expr, frame)?;
                let rhs = self.rec_build_ir(segment, *rhs, expr, frame)?;
                segment.instructions.place(IROp::Comparison {
                    lhs,
                    comp: *comp,
                    rhs,
                })
            }
            ASTNode::Piecewise { default, entries } => {
                if entries.len() == 0 {
                    compiler_error!(node, "Piecewises should have at least one condition")
                };
                let default = self.rec_build_ir(segment, *default, expr, frame)?;
                let mut v = Vec::with_capacity(entries.len());
                //codegen dependencies of this piecewise first
                for entry in entries {
                    let comp = self.rec_build_ir(segment, entry.comp, expr, frame)?;
                    let result = self.rec_build_ir(segment, entry.result, expr, frame)?;
                    v.push((comp, result));
                }
                let mut i = v.into_iter();
                let first = i.next().unwrap();
                let head = segment.instructions.place(IROp::BeginPiecewise {
                    comp: first.0,
                    res: first.1,
                });

                for (comp, res) in i {
                    segment
                        .instructions
                        .push(IROp::InnerPiecewise { comp, res })
                }
                segment.instructions.push(IROp::EndPiecewise { default });
                head
            }
        };
        Ok(res)
    }
    fn join_list_args(
        &mut self,
        segment: &mut IRSegment,
        _node: ASTNodeId,
        args: &ThinVec<ASTNodeId>,
        expr: &'borrow AST,
        frame: &mut Frame<'borrow>,
    ) -> Result<Id> {
        let mut iter = args.iter();
        let Some(initial) = iter.next() else {
            bail!("tried to join arguments for list function, got a zero-length list");
        };
        let mut prev = self.rec_build_ir(segment, *initial, expr, frame)?;
        for i in iter {
            let val = self.rec_build_ir(segment, *i, expr, frame)?;
            prev = segment
                .instructions
                .place(IROp::BinaryListOp(prev, val, BinaryListOp::Join));
        }
        Ok(prev)
    }
    fn build_simple_binary_fn(
        &mut self,
        segment: &mut IRSegment,
        node: ASTNodeId,
        arg0: Id,
        arg1: Id,
        op: BinaryOp,
    ) -> Result<Id> {
        Ok(match (arg0.t(), arg1.t(), op) {
            (IRType::Number, IRType::Number, _) => {
                segment.instructions.place(IROp::Binary(arg0, arg1, op))
            }
            permute!(IRType::Number, IRType::Vec2, BinaryOp::Mul)
            | (IRType::Vec2, IRType::Number, BinaryOp::Div) => {
                let (x, y, number);
                if arg0.t() == IRType::Vec2 {
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
                if arg0.t() == IRType::Vec3 {
                    (x, y, z) = segment.instructions.coordinates_of3d(arg0);
                    number = arg1;
                } else {
                    (x, y, z) = segment.instructions.coordinates_of3d(arg1);
                    number = arg0;
                }
                let xn = segment.instructions.place(IROp::Binary(x, number, op));
                let yn = segment.instructions.place(IROp::Binary(y, number, op));
                let _zn = segment.instructions.place(IROp::Binary(z, number, op));
                segment.instructions.place(IROp::Vec2(xn, yn))
            }
            //TODO: better compiler errors here
            (a, b, c) => {
                compiler_error!(
                    node,
                    "operation {:?} cannot be called on a {}{:?} and a {}{:?}",
                    c,
                    if arg0.t().downcast_list().is_some() {
                        "list of "
                    } else {
                        ""
                    },
                    a,
                    if arg1.t().downcast_list().is_some() {
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
        Ok(match (val.t(), op) {
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
    fn build_single_to_list_broadcast(
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
        let arg1_bcd = BroadcastArg { t: arg1.t(), id: 1 };
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
    pub fn insert_top(&mut self, k: &'a str, v: Id) {
        let s = match self.scopes.last_mut() {
            Some(s) => s,
            None => {
                self.scopes.push(Scope {
                    map: HashMap::with_capacity(1),
                });
                self.scopes.last_mut().unwrap()
            }
        };
        s.insert(k, v);
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
