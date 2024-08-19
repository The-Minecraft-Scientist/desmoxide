use anyhow::{bail, Context, Result};
//Re-export stuff from private scopes (used to keep enum name collisions down)

use compact_str::CompactString;
use debug_tree::{AsTree, TreeBuilder, TreeConfig, TreeSymbols};
use strum::AsRefStr;

use std::{
    fmt::Debug,
    iter::{Map, Zip},
    num::NonZeroU32,
    ops::{Deref, DerefMut, Index, RangeFrom},
    slice::Iter,
};
use thin_vec::ThinVec;

#[derive(Debug, Clone, Copy)]
pub struct ASTNodeId(NonZeroU32);

#[derive(Clone, Debug, strum::AsRefStr)]
pub enum ASTNode {
    Val(Value),
    Binary(ASTNodeId, ASTNodeId, BinaryOp),
    // Unary operations
    Unary(ASTNodeId, UnaryOp),

    Parens(Ident, ASTNodeId), // Ambiguous case, either multiplication by juxtaposition or a function call
    FunctionCall(Ident, ThinVec<ASTNodeId>), // Function with its list of arguments
    Index(ASTNodeId, ASTNodeId), // List indexing operations

    List(List), //List

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

    use crate::lang::lexer::Token;

    use crate::lang::lexer::Token::*;

    use super::{ASTNode as AN, ASTNodeId, BinaryOp as B, ListOp, Opcode as OP, UnaryOp as U};

    impl AN {
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
                OP::None => AN::Binary(arg0, arg1, B::Mul),
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
pub enum List {
    ListComp(ASTNodeId, ListCompInfo), // List defined by a list comphrehension inner member stored in the child node
    Range(ASTNodeId, ASTNodeId),       // List defined by a range of values
    List(ThinVec<ASTNodeId>),          // List defined by a vector of AST nodes
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
pub struct ListCompInfo {
    pub vars: ThinVec<(Ident, ASTNodeId)>,
}
#[derive(Debug, Clone)]
pub struct PiecewiseEntry {
    pub comp: ASTNodeId,
    pub result: ASTNodeId,
}

#[derive(Clone)]
pub enum Value {
    Ident(Ident),
    ConstantI64(i64),
    ConstantF64(f64),
}
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Ident(a) => a.0.fmt(f),
            Value::ConstantF64(a) => a.fmt(f),
            Value::ConstantI64(a) => a.fmt(f),
        }
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ident(CompactString);
impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
impl Deref for Ident {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::ConstantI64(value)
    }
}
impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::ConstantF64(value)
    }
}
impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Ident(value.into())
    }
}
impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Ident(value.into())
    }
}
#[derive(Clone)]
pub struct AST {
    pub store: Vec<ASTNode>,
    pub root: Option<ASTNodeId>,
}
impl AST {
    pub fn new() -> Self {
        Self {
            store: Vec::with_capacity(10),
            root: None,
        }
    }
    pub fn place(&mut self, node: ASTNode) -> ASTNodeId {
        self.push(node);
        unsafe { ASTNodeId(NonZeroU32::new_unchecked(self.len() as u32)) }
    }
    pub fn get_node(&self, idx: ASTNodeId) -> Result<&ASTNode> {
        self.get(idx.0.get() as usize - 1)
            .context("invalid AST node reference")
    }
    pub fn get_node_mut(&mut self, idx: ASTNodeId) -> Result<&mut ASTNode> {
        self.get_mut(idx.0.get() as usize - 1)
            .context("invalid AST node reference")
    }
    pub fn place_root(&mut self, root: ASTNode) {
        self.root = Some(self.place(root));
    }
    pub fn id_node_iter<'b>(
        &self,
    ) -> Map<
        Zip<Iter<'_, ASTNode>, RangeFrom<u32>>,
        for<'a> fn((&'a ASTNode, u32)) -> (ASTNodeId, &'a ASTNode),
    > {
        self.iter().zip(0u32..).map(Self::map_tuple)
    }
    fn map_tuple<'b>(t: (&'b ASTNode, u32)) -> (ASTNodeId, &'b ASTNode) {
        (
            ASTNodeId(unsafe { NonZeroU32::new_unchecked(t.1 + 1) }),
            t.0,
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
            ASTNode::Parens(i, a) => named_branch!(name, i.0, a),
            ASTNode::FunctionCall(i, r) => named_branch_list!(name, i.0, r),
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
impl Index<ASTNodeId> for AST {
    type Output = ASTNode;
    fn index(&self, index: ASTNodeId) -> &Self::Output {
        &self.store[index.0.get() as usize - 1]
    }
}
impl<'source> Debug for AST {
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
impl Deref for AST {
    type Target = Vec<ASTNode>;
    fn deref(&self) -> &Self::Target {
        &self.store
    }
}
impl<'a> DerefMut for AST {
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
    None,
    Pow,
    Mod,
    Index,
    Parens,
    Comma,
    CoordSel,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InfixBP {
    pub left: u8,
    pub right: u8,
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
                Div | Mul | None => Ok(InfixBP { left: 3, right: 4 }),
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
