#![allow(unused)]
pub mod expression;
pub mod parse_manager;
pub mod parser;
use crate::util::thin_str::ThinStr;
//Re-export stuff from private scopes (used to keep enum name collisions down)
pub use ast_impl::*;
pub use ast_node_impl::*;
pub use bp::*;

use std::{
    num::{NonZeroU64, NonZeroUsize},
    ops::{Deref, DerefMut},
};
use thin_vec::ThinVec;

#[derive(Debug, Clone, Copy)]
pub struct ASTNodeRef(NonZeroUsize);

mod ast_impl {
    use anyhow::{bail, Result};
    use thin_vec::ThinVec;

    use crate::lexer::Token;
    use crate::lexer::Token::*;

    use super::{ASTNode as AN, ASTNodeRef, BinaryOp as B, List, Opcode as OP, UnaryOp as U};

    impl<'a> AN<'a> {
        pub fn new_simple_with_node(token: Token, inner: ASTNodeRef) -> Result<Self> {
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
        pub fn new_autojoin_fn(token: Token, arg: ThinVec<ASTNodeRef>) -> Result<Self> {
            Ok(match token {
                Min => AN::Min(arg),
                Max => AN::Max(arg),
                Count => AN::Count(arg),
                Total => AN::Total(arg),
                Join => AN::Join(arg),
                t => bail!("token {:?} does not autojoin its arguments", t),
            })
        }
        pub fn new_simple_binary(tok: Token, arg0: ASTNodeRef, arg1: ASTNodeRef) -> Result<Self> {
            Ok(match tok {
                Mod => AN::Binary(arg0, arg1, B::Mod),
                Add => AN::Binary(arg0, arg1, B::Add),
                Sub => AN::Binary(arg0, arg1, B::Sub),
                Mul => AN::Binary(arg0, arg1, B::Mul),
                Div => AN::Binary(arg0, arg1, B::Div),
                Pow => AN::Binary(arg0, arg1, B::Pow),
                t => bail!("{:?} not a binary operation", t),
            })
        }
    }
}

#[derive(Clone, Debug)]
//TODO: this is 32 bytes for some reason. It should be 24
pub enum ASTNode<'a> {
    Val(Value<'a>),
    Binary(ASTNodeRef, ASTNodeRef, BinaryOp),
    // Unary operations
    Unary(ASTNodeRef, UnaryOp),

    Parens(Ident<'a>, ASTNodeRef), // Ambiguous case, either multiplication by juxtaposition or a function call
    FunctionCall(Ident<'a>, ThinVec<ASTNodeRef>), // Function with its list of arguments
    Index(ASTNodeRef, ASTNodeRef), // List indexing operations

    List(List<'a>), //List
    // Where b is a ref to a Comparison node
    ListFilt(ASTNodeRef, ASTNodeRef),

    Point(ASTNodeRef, ASTNodeRef),
    Comparison(ASTNodeRef, Comparison, ASTNodeRef),

    //List builtins
    Min(ThinVec<ASTNodeRef>),
    Max(ThinVec<ASTNodeRef>),
    Count(ThinVec<ASTNodeRef>),
    Total(ThinVec<ASTNodeRef>),
    Join(ThinVec<ASTNodeRef>),
    Length(ThinVec<ASTNodeRef>),

    // 2 argument sort is optional
    Sort(ASTNodeRef, Option<ASTNodeRef>),
    // Seed argument is optional
    Shuffle(ASTNodeRef, Option<ASTNodeRef>),
    Unique(ASTNodeRef),
    // Random can be called with 0, 1, or 2 arguments
    Random(Option<(ASTNodeRef, Option<ASTNodeRef>)>),
    CoordinateAccess(ASTNodeRef, CoordinateAccess),
    //comparison operator
    Comp(ASTNodeRef, Comparison, ASTNodeRef),

    // "number theory functions"
    /// a % b
    Mod(ASTNodeRef, ASTNodeRef),

    Piecewise {
        default: ASTNodeRef,
        entries: ThinVec<PiecewiseEntry>,
    },
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    NthRoot,
    Mod,
}
#[derive(Debug, Clone)]
pub enum List<'a> {
    ListComp(ASTNodeRef, ListCompInfo<'a>), // List defined by a list comphrehension inner member stored in the child node
    Range(ASTNodeRef, ASTNodeRef),          // List defined by a range of values
    List(ThinVec<ASTNodeRef>),              // List defined by a vector of AST nodes
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CoordinateAccess {
    DotAccessX,
    DotAccessY,
    DotAccessZ,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Comparison {
    Eq,
    Ge,
    Gt,
    Le,
    Lt,
}
mod ast_node_impl {

    use super::ASTNode;
    use super::ASTNode::*;

    impl<'a> ASTNode<'a> {
        pub fn can_be_list(&self) -> bool {
            matches!(
                self,
                List(_)
                    | Val(super::Value::Ident(_))
                    | Min(_)
                    | Max(_)
                    | Count(_)
                    | Total(_)
                    | Join(_)
                    | Length(_)
            )
        }
        pub fn can_be_point(&self) -> bool {
            matches!(self, Point(_, _) | Val(super::Value::Ident(_)))
        }
    }
}
#[derive(Debug, Clone)]
pub struct ListCompInfo<'a> {
    vars: ThinVec<(Ident<'a>, ASTNodeRef)>,
}
#[derive(Debug, Clone)]
pub struct PiecewiseEntry {
    lhs: ASTNodeRef,
    comp: Comparison,
    rhs: ASTNodeRef,
    result: ASTNodeRef,
}

#[derive(Clone, Debug)]
pub enum Value<'a> {
    Ident(Ident<'a>),
    ConstantI64(i64),
    ConstantF64(f64),
}
#[derive(Clone, Debug)]
pub struct Ident<'a>(ThinStr<'a>);

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
//Opcode structure for simple in/pre/postfix operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    Add,
    Sub,
    Neg,
    Div,
    Mul,
    Pow,
    Ge,
    Le,
    Gt,
    Lt,
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
