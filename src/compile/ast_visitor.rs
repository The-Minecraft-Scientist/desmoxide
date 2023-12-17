use thin_vec::{thin_vec, ThinVec};

use crate::ast::{parse_manager::AST, ASTNode, ASTNodeRef};

impl<'source> ASTNode<'source> {
    pub fn children(&self) -> Option<ThinVec<ASTNodeRef>> {
        match self {
            ASTNode::Val(_) => None,
            ASTNode::Add(a, b)
            | ASTNode::Sub(a, b)
            | ASTNode::Mul(a, b)
            | ASTNode::Div(a, b)
            | ASTNode::Pow(a, b)
            | ASTNode::NthRoot(a, b) => Some(thin_vec![*a, *b]),
            ASTNode::Neg(_) => todo!(),
            ASTNode::Sqrt(_) => todo!(),
            ASTNode::Parens(_, a) => todo!(),
            ASTNode::Index(_, a) => todo!(),
            ASTNode::FunctionCall(_, a) => todo!(),
            ASTNode::List(_) => todo!(),
            ASTNode::ListFilt(_, _, _, _) => todo!(),
            ASTNode::Point(_, _) => todo!(),
            ASTNode::Sin(_) => todo!(),
            ASTNode::Cos(_) => todo!(),
            ASTNode::Tan(_) => todo!(),
            ASTNode::Csc(_) => todo!(),
            ASTNode::Sec(_) => todo!(),
            ASTNode::Cot(_) => todo!(),
            ASTNode::InvSin(_) => todo!(),
            ASTNode::InvCos(_) => todo!(),
            ASTNode::InvTan(_) => todo!(),
            ASTNode::InvCsc(_) => todo!(),
            ASTNode::InvSec(_) => todo!(),
            ASTNode::InvCot(_) => todo!(),
            ASTNode::Min(_) => todo!(),
            ASTNode::Max(_) => todo!(),
            ASTNode::Count(_) => todo!(),
            ASTNode::Total(_) => todo!(),
            ASTNode::Join(_) => todo!(),
            ASTNode::Length(_) => todo!(),
            ASTNode::Sort(_, _) => todo!(),
            ASTNode::Shuffle(_, _) => todo!(),
            ASTNode::Unique(_) => todo!(),
            ASTNode::Random(_) => todo!(),
            ASTNode::CoordinateAccess(_, _) => todo!(),
            ASTNode::Comp(_, _, _) => todo!(),
            ASTNode::Mod(_, _) => todo!(),
            ASTNode::Floor(_) => todo!(),
            ASTNode::Ceil(_) => todo!(),
            ASTNode::Piecewise { default, entries } => todo!(),
        }
    }
}
pub struct ASTVisitor<'borrow, 'source, F, Extra = ()>
where
    for<'a> F: Fn(&'a mut Extra, &'a ASTNode<'source>),
{
    ast: &'borrow AST<'source>,
    func: F,
    extra: Extra,
}
impl<'borrow, 'source, F, Extra> ASTVisitor<'borrow, 'source, F, Extra>
where
    for<'a> F: Fn(&'a mut Extra, &'a ASTNode<'source>),
{
    pub fn new(ast: &'borrow AST<'source>, func: F, extra: Extra) -> Self {
        Self { ast, func, extra }
    }
}
