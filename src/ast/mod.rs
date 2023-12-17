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

    use super::{ASTNode as AN, ASTNodeRef, List, Opcode as OP};

    impl<'a> AN<'a> {
        pub fn new_simple_with_node(token: Token, inner: ASTNodeRef) -> Result<Self> {
            Ok(match token {
                //trig
                Sin => AN::Sin(inner),
                Cos => AN::Cos(inner),
                Tan => AN::Tan(inner),
                Csc => AN::Csc(inner),
                Sec => AN::Sec(inner),
                Cot => AN::Cot(inner),
                InvSin => AN::InvSin(inner),
                InvCos => AN::InvCos(inner),
                InvTan => AN::InvTan(inner),
                InvCsc => AN::InvCsc(inner),
                InvSec => AN::InvSec(inner),
                InvCot => AN::InvCot(inner),
                Ceil => AN::Ceil(inner),
                Floor => AN::Floor(inner),

                t => bail!("token {:?} is not a simple builtin", t),
            })
        }
        pub fn new_simple_2arg(token: Token, arg0: ASTNodeRef, arg1: ASTNodeRef) -> Result<Self> {
            Ok(match token {
                Token::Mod => AN::Mod(arg0, arg1),
                t => bail!("token {:?} is not a simple 2-argument builtin", t),
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
        pub fn new_opcode_2arg(op: OP, arg0: ASTNodeRef, arg1: ASTNodeRef) -> Self {
            match op {
                OP::Add => AN::Add(arg0, arg1),
                OP::Sub => AN::Sub(arg0, arg1),
                OP::Mul => AN::Mul(arg0, arg1),
                OP::Div => AN::Div(arg0, arg1),
                OP::Pow => AN::Pow(arg0, arg1),
                _ => panic!("not a 2 arg operation"),
            }
        }
    }
}

#[derive(Clone, Debug)]
//TODO: this is 32 bytes for some reason. It should be 24
pub enum ASTNode<'a> {
    Val(Value<'a>),
    Add(ASTNodeRef, ASTNodeRef),
    Sub(ASTNodeRef, ASTNodeRef),
    Mul(ASTNodeRef, ASTNodeRef),
    Div(ASTNodeRef, ASTNodeRef),
    Pow(ASTNodeRef, ASTNodeRef),
    NthRoot(ASTNodeRef, ASTNodeRef),
    // Unary operators
    Neg(ASTNodeRef),
    Sqrt(ASTNodeRef),

    Parens(Ident<'a>, ASTNodeRef), // Ambiguous case, either multiplication by juxtaposition or a function call
    FunctionCall(Ident<'a>, ThinVec<ASTNodeRef>), // Function with its list of arguments
    Index(ASTNodeRef, ASTNodeRef), // List indexing operations

    List(List<'a>), //List
    ListFilt(ASTNodeRef, ASTNodeRef, Comparison, ASTNodeRef),

    Point(ASTNodeRef, ASTNodeRef),

    //Trigonometric functions
    Sin(ASTNodeRef),
    Cos(ASTNodeRef),
    Tan(ASTNodeRef),
    Csc(ASTNodeRef),
    Sec(ASTNodeRef),
    Cot(ASTNodeRef),
    InvSin(ASTNodeRef),
    InvCos(ASTNodeRef),
    InvTan(ASTNodeRef),
    InvCsc(ASTNodeRef),
    InvSec(ASTNodeRef),
    InvCot(ASTNodeRef),

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
    Floor(ASTNodeRef),
    Ceil(ASTNodeRef),

    Piecewise {
        default: ASTNodeRef,
        entries: ThinVec<PiecewiseEntry>,
    },
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
