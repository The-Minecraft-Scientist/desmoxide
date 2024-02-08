//#![allow(unused)]
pub mod expression;
pub mod parse_manager;
pub mod parser;
use crate::util::thin_str::ThinStr;
use anyhow::{bail, Context, Result};
//Re-export stuff from private scopes (used to keep enum name collisions down)
pub use ast_impl::*;
pub use bp::*;
use debug_tree::{AsTree, TreeBuilder, TreeConfig, TreeSymbols};
use strum::AsRefStr;

use std::{
    fmt::Debug,
    iter::{Enumerate, Map},
    num::NonZeroUsize,
    ops::{Deref, DerefMut, Index},
    slice::Iter,
};
use thin_vec::ThinVec;

#[derive(Debug, Clone, Copy)]
pub struct ASTNodeId(NonZeroUsize);

#[derive(Clone, Debug, strum::AsRefStr)]
//TODO: See about shrinking this
pub enum ASTNode<'a> {
    Val(Value<'a>),
    Binary(ASTNodeId, ASTNodeId, BinaryOp),
    // Unary operations
    Unary(ASTNodeId, UnaryOp),

    Parens(Ident<'a>, ASTNodeId), // Ambiguous case, either multiplication by juxtaposition or a function call
    FunctionCall(Ident<'a>, ThinVec<ASTNodeId>), // Function with its list of arguments
    Index(ASTNodeId, ASTNodeId),  // List indexing operations

    List(List<'a>), //List

    Point(ASTNodeId, ASTNodeId),

    //List builtins
    ListOp(ThinVec<ASTNodeId>, ListOp),
    CoordinateAccess(ASTNodeId, CoordinateAccess),
    //comparison operator
    Comparison(ASTNodeId, Comparison, ASTNodeId),

    Piecewise {
        default: ASTNodeId,
        entries: ThinVec<PiecewiseEntry>,
    },
}

mod ast_impl {

    use anyhow::{bail, Result};
    use thin_vec::ThinVec;

    use crate::lexer::Token;
    use crate::lexer::Token::*;

    use super::{ASTNode as AN, ASTNodeId, BinaryOp as B, ListOp, Opcode as OP, UnaryOp as U};

    impl<'a> AN<'a> {
        pub fn new_simple_with_node(token: Token, inner: ASTNodeId) -> Result<Self> {
            Ok(match token {
                //trig
                Sin => AN::Unary(inner, U::Sin),
                Cos => AN::Unary(inner, U::Cos),
                Tan => AN::Unary(inner, U::Tan),
                Csc => AN::Unary(inner, U::Csc),
                Sec => AN::Unary(inner, U::Sec),
                Cot => AN::Unary(inner, U::Cot),
                InvSin => AN::Unary(inner, U::InvSin),
                InvCos => AN::Unary(inner, U::InvCos),
                InvTan => AN::Unary(inner, U::InvTan),
                InvCsc => AN::Unary(inner, U::InvCsc),
                InvSec => AN::Unary(inner, U::InvSec),
                InvCot => AN::Unary(inner, U::InvCot),
                Ceil => AN::Unary(inner, U::Ceil),
                Floor => AN::Unary(inner, U::Floor),

                t => bail!("token {:?} is not a simple builtin", t),
            })
        }
        pub fn new_list_fn(token: Token, arg: ThinVec<ASTNodeId>) -> Result<Self> {
            Ok(match token {
                Min => AN::ListOp(arg, ListOp::Min),
                Max => AN::ListOp(arg, ListOp::Max),
                Count => AN::ListOp(arg, ListOp::Count),
                Total => AN::ListOp(arg, ListOp::Total),
                Join => AN::ListOp(arg, ListOp::Join),
                t => bail!("token {:?} does not autojoin its arguments", t),
            })
        }
        pub fn new_simple_binary(op: OP, arg0: ASTNodeId, arg1: ASTNodeId) -> Result<Self> {
            Ok(match op {
                OP::Mod => AN::Binary(arg0, arg1, B::Mod),
                OP::Add => AN::Binary(arg0, arg1, B::Add),
                OP::Sub => AN::Binary(arg0, arg1, B::Sub),
                OP::Mul => AN::Binary(arg0, arg1, B::Mul),
                OP::Div => AN::Binary(arg0, arg1, B::Div),
                OP::Pow => AN::Binary(arg0, arg1, B::Pow),
                t => bail!("{:?} not a binary operation", t),
            })
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::AsRefStr)]
pub enum UnaryOp {
    Sqrt,
    Neg,
    Sin,
    Cos,
    Tan,
    Csc,
    Sec,
    Cot,
    InvSin,
    InvCos,
    InvTan,
    InvCsc,
    InvSec,
    InvCot,
    Floor,
    Ceil,
    Gamma,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::AsRefStr)]
pub enum ListOp {
    Min,
    Max,
    Count,
    Total,
    Join,
    Length,
    Unique,
    Sort,
    Shuffle,
    Random,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::AsRefStr)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    NthRoot,
    Mod,
    Min,
    Max,
}
#[derive(Debug, Clone)]
pub enum List<'a> {
    ListComp(ASTNodeId, ListCompInfo<'a>), // List defined by a list comphrehension inner member stored in the child node
    Range(ASTNodeId, ASTNodeId),           // List defined by a range of values
    List(ThinVec<ASTNodeId>),              // List defined by a vector of AST nodes
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, AsRefStr)]
pub enum CoordinateAccess {
    DotAccessX,
    DotAccessY,
    DotAccessZ,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, strum::AsRefStr)]
pub enum Comparison {
    Eq,
    GreaterEq,
    Greater,
    LessEq,
    Less,
}

#[derive(Debug, Clone)]
pub struct ListCompInfo<'a> {
    pub vars: ThinVec<(Ident<'a>, ASTNodeId)>,
}
#[derive(Debug, Clone)]
pub struct PiecewiseEntry {
    pub comp: ASTNodeId,
    pub result: ASTNodeId,
}

#[derive(Clone)]
pub enum Value<'a> {
    Ident(Ident<'a>),
    ConstantI64(i64),
    ConstantF64(f64),
}
impl<'a> Debug for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Ident(a) => a.0.fmt(f),
            Value::ConstantF64(a) => a.fmt(f),
            Value::ConstantI64(a) => a.fmt(f),
        }
    }
}
#[derive(Clone, Debug)]
pub struct Ident<'a>(ThinStr<'a>);
impl<'a> Ident<'a> {
    pub fn as_str(&self) -> &'a str {
        self.0.as_str()
    }
}
impl<'a> Deref for Ident<'a> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl From<i64> for Value<'_> {
    fn from(value: i64) -> Self {
        Self::ConstantI64(value)
    }
}
impl From<f64> for Value<'_> {
    fn from(value: f64) -> Self {
        Self::ConstantF64(value)
    }
}
impl<'a> From<&'a str> for Value<'a> {
    fn from(value: &'a str) -> Self {
        Value::Ident(value.into())
    }
}
impl<'a> From<&'a str> for Ident<'a> {
    fn from(value: &'a str) -> Self {
        Ident(value.into())
    }
}
#[derive(Clone)]
pub struct AST<'source> {
    pub store: Vec<ASTNode<'source>>,
    pub root: Option<ASTNodeId>,
}
impl<'source> AST<'source> {
    pub fn new() -> Self {
        Self {
            store: Vec::with_capacity(10),
            root: None,
        }
    }
    pub fn place(&mut self, node: ASTNode<'source>) -> ASTNodeId {
        self.push(node);
        unsafe { ASTNodeId(NonZeroUsize::new_unchecked(self.len())) }
    }
    pub fn get_node(&self, idx: ASTNodeId) -> Result<&ASTNode<'source>> {
        self.get(idx.0.get() as usize - 1)
            .context("invalid AST node reference")
    }
    pub fn get_node_mut(&mut self, idx: ASTNodeId) -> Result<&mut ASTNode<'source>> {
        self.get_mut(idx.0.get() as usize - 1)
            .context("invalid AST node reference")
    }
    pub fn place_root(&mut self, root: ASTNode<'source>) {
        self.root = Some(self.place(root));
    }
    pub fn id_node_iter<'b>(
        &self,
    ) -> Map<
        Enumerate<Iter<'_, ASTNode<'source>>>,
        for<'a> fn((usize, &'a ASTNode<'source>)) -> (ASTNodeId, &'a ASTNode<'source>),
    > {
        self.iter().enumerate().map(Self::map_tuple)
    }
    fn map_tuple<'b>(t: (usize, &'b ASTNode<'source>)) -> (ASTNodeId, &'b ASTNode<'source>) {
        (
            ASTNodeId(unsafe { NonZeroUsize::new_unchecked(t.0 + 1) }),
            t.1,
        )
    }
    pub fn recursive_dbg(&self, builder: &mut TreeBuilder, nid: ASTNodeId) -> Result<()> {
        macro_rules! named_branch {
            ($bname:expr, $bctx:expr,$($child:expr),+) => {
                {let b = builder.add_branch(&format!("{}: {}", $bname, $bctx));
                $(
                self.recursive_dbg(builder, *$child)?;
                )*
                b}
            };
        }
        macro_rules! named_branch_list {
            ($bname:expr, $bctx:expr,$children:expr) => {{
                let b = builder.add_branch(&format!("{}: {}", $bname, $bctx));
                for child in $children {
                    self.recursive_dbg(builder, *child)?;
                }
                b
            }};
        }

        let n = self.get_node(nid)?;
        if let ASTNode::Val(v) = n {
            builder.add_leaf(&format!("{:?}", v));
            return Ok(());
        }
        let name = n.as_ref();
        let _s = match n {
            ASTNode::Binary(a, b, c) => named_branch!(name, c.as_ref(), a, b),
            ASTNode::Unary(a, c) => named_branch!(name, c.as_ref(), a),
            ASTNode::Parens(i, a) => named_branch!(name, i.0.as_str(), a),
            ASTNode::FunctionCall(i, r) => named_branch_list!(name, i.0.as_str(), r),
            ASTNode::Index(r, v) => named_branch!(name, "", r, v),
            ASTNode::List(l) => match l {
                List::List(v) => named_branch_list!(name, "", v),
                List::ListComp(a, info) => {
                    builder.add_branch("List Comprehension");
                    self.recursive_dbg(builder, *a)?;
                    named_branch_list!(name, "", info.vars.iter().map(|a| &a.1))
                }
                List::Range(a, b) => named_branch!("Range list", "", a, b),
            },
            ASTNode::Point(a, b) => named_branch!(name, "", a, b),
            ASTNode::ListOp(a, o) => named_branch_list!(name, o.as_ref(), a),
            ASTNode::CoordinateAccess(a, b) => named_branch!(name, b.as_ref(), a),
            ASTNode::Comparison(a, c, ba) => {
                let b = builder.add_branch("Comparison");
                self.recursive_dbg(builder, *a)?;
                builder.add_leaf(c.as_ref());
                self.recursive_dbg(builder, *ba)?;
                b
            }
            ASTNode::Piecewise {
                default: _,
                entries,
            } => {
                let b = builder.add_branch("if");
                for entry in entries {
                    let mut b1 = builder.add_branch("if");
                    self.recursive_dbg(builder, entry.comp)?;
                    let mut b0 = builder.add_branch("then");
                    self.recursive_dbg(builder, entry.result)?;
                    b0.release();
                    b1.release();
                }
                b
            }
            _t => {
                bail!("incorrect AST node")
            }
        };
        Ok(())
    }
}
impl<'source> Index<ASTNodeId> for AST<'source> {
    type Output = ASTNode<'source>;
    fn index(&self, index: ASTNodeId) -> &Self::Output {
        &self.store[index.0.get() as usize - 1]
    }
}
impl<'source> Debug for AST<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("\n");
        let mut builder = TreeBuilder::new();
        builder.set_config_override(TreeConfig::new().symbols(TreeSymbols::with_rounded()));
        self.recursive_dbg(&mut builder, self.root.unwrap())
            .unwrap();
        f.write_str(&builder.as_tree().string())?;
        Ok(())
    }
}
impl<'a> Deref for AST<'a> {
    type Target = Vec<ASTNode<'a>>;
    fn deref(&self) -> &Self::Target {
        &self.store
    }
}
impl<'a> DerefMut for AST<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.store
    }
}

//Opcode structure for simple in/pre/postfix operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    Add,
    Sub,
    Neg,
    Div,
    Mul,
    Pow,
    Mod,
    Index,
    Parens,
    Comma,
    CoordSel,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InfixBP {
    left: u8,
    right: u8,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PrefixBP(u8);
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PostfixBP(u8);
impl Deref for PrefixBP {
    type Target = u8;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Deref for PostfixBP {
    type Target = u8;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
mod bp {
    use anyhow::{bail, Result};

    use super::{InfixBP, Opcode, Opcode::*, PostfixBP, PrefixBP};
    impl Opcode {
        pub fn infix_bp(&self) -> Result<InfixBP> {
            match self {
                Add | Sub => Ok(InfixBP { left: 1, right: 2 }),
                Div | Mul => Ok(InfixBP { left: 3, right: 4 }),
                Pow => Ok(InfixBP { left: 5, right: 6 }),
                t => bail!("bad op: {:?}", t),
            }
        }
        pub fn prefix_bp(&self) -> Result<PrefixBP> {
            match self {
                Neg => Ok(PrefixBP(5)),
                t => bail!("bad op: {:?}", t),
            }
        }
        pub fn postfix_bp(&self) -> Result<PostfixBP> {
            match self {
                Index => Ok(PostfixBP(10)),
                Parens => Ok(PostfixBP(100)),
                t => bail!("bad op: {:?}", t),
            }
        }
    }
}
