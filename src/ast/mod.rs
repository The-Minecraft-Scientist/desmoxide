#![allow(unused)]
pub mod expression;
pub mod parser;
use std::ops::{Deref, DerefMut};

pub use bp::*;
use thin_vec::ThinVec;
pub use trig::*;

use crate::{lexer::Token, util::thin_str::ThinStr};
#[derive(Debug, Clone)]
pub struct ASTNode<'a>(Box<ASTNodeType<'a>>);
impl<'a> ASTNode<'a> {
    pub fn new_opcode_2arg(op: Opcode, arg0: ASTNode<'a>, arg1: ASTNode<'a>) -> Self {
        match op {
            Opcode::Add => {
                //2 arguments
                ASTNodeType::Add(arg0, arg1).into()
            }
            Opcode::Sub => {
                //2 arguments
                ASTNodeType::Sub(arg0, arg1).into()
            }
            Opcode::Mul => {
                //2 arguments
                ASTNodeType::Mul(arg0, arg1).into()
            }
            Opcode::Div => {
                //2 arguments
                ASTNodeType::Div(arg0, arg1).into()
            }
            Opcode::Pow => {
                //2 arguments
                ASTNodeType::Pow(arg0, arg1).into()
            }
            _ => panic!("not a 2 arg operation"),
        }
    }
}
mod trig {
    use anyhow::{bail, Result};

    use crate::lexer::Token;
    use crate::lexer::Token::*;

    use super::{ASTNode, ASTNodeType as AN};

    impl<'a> ASTNode<'a> {
        pub fn new_trig(token: Token, inner: ASTNode<'a>) -> Result<Self> {
            Ok(match token {
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
                t => bail!("token {:?} is not a trig builtin", t),
            }
            .into())
        }
    }
}
impl<'a> Deref for ASTNode<'a> {
    type Target = ASTNodeType<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'a> DerefMut for ASTNode<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl<'a> From<ASTNodeType<'a>> for ASTNode<'a> {
    fn from(value: ASTNodeType<'a>) -> Self {
        ASTNode(Box::new(value))
    }
}
#[derive(Clone, Debug)]
pub enum ASTNodeType<'a> {
    Val(Value<'a>),
    Add(ASTNode<'a>, ASTNode<'a>),
    Sub(ASTNode<'a>, ASTNode<'a>),
    Mul(ASTNode<'a>, ASTNode<'a>),
    Div(ASTNode<'a>, ASTNode<'a>),
    Pow(ASTNode<'a>, ASTNode<'a>),
    // Unary operators
    Neg(ASTNode<'a>),
    Sqrt(ASTNode<'a>),

    Parens(Ident<'a>, ASTNode<'a>), // Ambiguous case, either multiplication by juxtaposition or a function call
    Index(ASTNode<'a>, ASTNode<'a>), // List indexing operations

    ListCompList(ASTNode<'a>, ListCompInfo<'a>), // List defined by a list comphrehension inner member stored in the child node
    NodeList(ThinVec<ASTNode<'a>>),              // List defined by a vector of AST nodes
    RangeList(ASTNode<'a>, ASTNode<'a>),         // List defined by a range of values

    Point(ASTNode<'a>, ASTNode<'a>),

    //Trigonometric functions
    Sin(ASTNode<'a>),
    Cos(ASTNode<'a>),
    Tan(ASTNode<'a>),
    Csc(ASTNode<'a>),
    Sec(ASTNode<'a>),
    Cot(ASTNode<'a>),
    InvSin(ASTNode<'a>),
    InvCos(ASTNode<'a>),
    InvTan(ASTNode<'a>),
    InvCsc(ASTNode<'a>),
    InvSec(ASTNode<'a>),
    InvCot(ASTNode<'a>),
    //List builtins
    Min(ThinVec<ASTNode<'a>>),
    Max(ThinVec<ASTNode<'a>>),
    Count(ThinVec<ASTNode<'a>>),
    Total(ThinVec<ASTNode<'a>>),
    Join(ThinVec<ASTNode<'a>>),
    Length(ThinVec<ASTNode<'a>>),
    // 2 argument sort is optional
    Sort(ASTNode<'a>, Option<ASTNode<'a>>),
    // Seed argument is optional
    Shuffle(ASTNode<'a>, Option<ASTNode<'a>>),
    Unique(ASTNode<'a>),
    // Random can be called with 0, 1, or 2 arguments
    Random(Option<(ASTNode<'a>, Option<ASTNode<'a>>)>),
    DotAccess(ASTNode<'a>, DotAccess),
}
#[derive(Clone, Copy, Debug)]
pub enum DotAccess {
    DotAccessX,
    DotAccessY,
    DotAccessZ,
}
impl<'a> ASTNodeType<'a> {
    pub fn can_be_list(&self) -> bool {
        match self {
            Self::ListCompList(_, _)
            | Self::NodeList(_)
            | Self::RangeList(_, _)
            | Self::Val(Value::Ident(_)) => true,
            _ => false,
        }
    }
    pub fn is_point(&self) -> bool {
        let Self::Point(_, _) = self else {
            return false;
        };
        return true;
    }
}
#[derive(Debug, Clone)]
pub struct ListCompInfo<'a> {
    vars: ThinVec<(Ident<'a>, ASTNode<'a>)>,
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
