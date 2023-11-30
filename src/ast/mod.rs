#![allow(unused)]
pub mod expression;
pub mod parser;
#[derive(Debug, Clone)]
pub struct ASTNode<'a>(Box<ASTNodeType<'a>>);
impl<'a> ASTNode<'a> {
    pub fn from_opcode_args<const N: usize>(op: Opcode, args: [ASTNode<'a>; N]) -> Self {
        match op {
            Opcode::Add => {
                //2 arguments
                const _: () = assert!(N == 2);
                ASTNodeType::Add(args[0], args[1]).into()
            }
            Opcode::Sub => {
                //2 arguments
                const _: () = assert!(N == 2);
                ASTNodeType::Sub(args[0], args[1]).into()
            }
            Opcode::Mul => {
                //2 arguments
                const _: () = assert!(N == 2);
                ASTNodeType::Mul(args[0], args[1]).into()
            }
            Opcode::Div => {
                //2 arguments
                const _: () = assert!(N == 2);
                ASTNodeType::Div(args[0], args[1]).into()
            }
            Opcode::Pow => {
                //2 arguments
                const _: () = assert!(N == 2);
                ASTNodeType::Pow(args[0], args[1]).into()
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
    Neg(ASTNode<'a>),
    Sqrt(ASTNode<'a>),

    Parens(Ident<'a>, ASTNode<'a>),
    Index(Ident<'a>, ASTNode<'a>),
    ListComp(ASTNode<'a>, Ident<'a>),
    //Op(Opcode, Vec<ASTNode<'a>>),
}
pub struct ListCompInfo<'a> {
    vars: Vec<(Ident<'a>, ASTNode<'a>)>,
}
pub enum ListLit<'a> {
    Simple(Vec<Value<'a>>),
    WithAST(Vec<ASTNode<'a>>),
}

#[derive(Clone, Debug)]
pub enum Value<'a> {
    Ident(Ident<'a>),
    ConstantI64(i64),
    ConstantF64(f64),
}
#[derive(Clone, Debug)]
#[repr(transparent)]
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
        Value::Ident(Ident(value.into()))
    }
}
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
    ListComp,
    Parens,
    Comma,
    CoordSel,
}
use std::ops::{Deref, DerefMut};

pub use bp::*;

use crate::util::thin_str::ThinStr;
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
