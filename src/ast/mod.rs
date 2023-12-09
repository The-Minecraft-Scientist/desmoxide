//#![allow(unused)]
pub mod expression;
pub mod parser;

use crate::util::thin_str::ThinStr;
//Re-export stuff from private scopes (used to keep enum name collisions down)
pub use ast_impl::*;
pub use ast_node_impl::*;
pub use bp::*;

use std::ops::{Deref, DerefMut};
use thin_vec::ThinVec;

#[derive(Debug, Clone)]
pub struct ASTNode<'a>(Box<ASTNodeType<'a>>);

mod ast_impl {
    use anyhow::{bail, Result};

    use crate::lexer::Token;
    use crate::lexer::Token::*;

    use super::{ASTNode, ASTNodeType as AN, List, Opcode as OP};

    impl<'a> ASTNode<'a> {
        pub fn new_simple_with_node(token: Token, inner: ASTNode<'a>) -> Result<Self> {
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
        pub fn new_autojoin_fn(token: Token, arg: List<'a>) -> Result<Self> {
            Ok(match token {
                Min => AN::Min(arg),
                Max => AN::Max(arg),
                Count => AN::Count(arg),
                Total => AN::Total(arg),
                Join => AN::Join(arg),
                t => bail!("token {:?} does not autojoin its arguments", t),
            }
            .into())
        }
        pub fn new_opcode_2arg(op: OP, arg0: ASTNode<'a>, arg1: ASTNode<'a>) -> Self {
            match op {
                OP::Add => AN::Add(arg0, arg1).into(),
                OP::Sub => AN::Sub(arg0, arg1).into(),
                OP::Mul => AN::Mul(arg0, arg1).into(),
                OP::Div => AN::Div(arg0, arg1).into(),
                OP::Pow => AN::Pow(arg0, arg1).into(),
                _ => panic!("not a 2 arg operation"),
            }
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
    FunctionCall(Ident<'a>, ThinVec<ASTNode<'a>>), // Function with its list of arguments
    Index(ASTNode<'a>, ASTNode<'a>), // List indexing operations

    List(List<'a>), //List
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
    Min(List<'a>),
    Max(List<'a>),
    Count(List<'a>),
    Total(List<'a>),
    Join(List<'a>),
    Length(ASTNode<'a>),
    // 2 argument sort is optional
    Sort(List<'a>, Option<ASTNode<'a>>),
    // Seed argument is optional
    Shuffle(List<'a>, Option<ASTNode<'a>>),
    Unique(List<'a>),
    // Random can be called with 0, 1, or 2 arguments
    Random(Option<(ASTNode<'a>, Option<ASTNode<'a>>)>),
    DotAccess(ASTNode<'a>, DotAccess),
}
#[derive(Debug, Clone)]
pub enum List<'a> {
    ListComp(ASTNode<'a>, ListCompInfo<'a>), // List defined by a list comphrehension inner member stored in the child node
    Range(ASTNode<'a>, ASTNode<'a>),         // List defined by a vector of AST nodes
    List(ThinVec<ASTNode<'a>>),              // List defined by a range of values
}

#[derive(Clone, Copy, Debug)]
pub enum DotAccess {
    DotAccessX,
    DotAccessY,
    DotAccessZ,
}
mod ast_node_impl {

    use super::ASTNodeType;
    use super::ASTNodeType::*;

    impl<'a> ASTNodeType<'a> {
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
