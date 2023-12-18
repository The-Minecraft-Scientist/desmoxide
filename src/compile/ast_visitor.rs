use thin_vec::{thin_vec, ThinVec};

use crate::ast::{parse_manager::AST, ASTNode, ASTNodeRef};

impl<'source> ASTNode<'source> {}
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
