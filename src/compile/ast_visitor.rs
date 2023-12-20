use anyhow::{Context, Result};

use crate::ast::{ASTNode, ASTNodeId, AST};

impl<'source> ASTNode<'source> {}
pub struct ASTVisitor<'borrow, 'source, F, Extra = ()> {
    to_visit: Vec<ASTNodeId>,
    ast: &'borrow AST<'source>,
    func: F,
    extra: Extra,
}
impl<'borrow, 'source, F, Extra> ASTVisitor<'borrow, 'source, F, Extra>
where
    for<'a> F: Fn(&'a mut Vec<ASTNodeId>, &'a mut Extra, &'a ASTNode<'source>),
{
    pub fn new(start_at: ASTNodeId, ast: &'borrow AST<'source>, func: F, extra: Extra) -> Self {
        Self {
            ast,
            func,
            extra,
            to_visit: vec![start_at],
        }
    }
    pub fn visit(&mut self) -> Result<()> {
        let Some(next) = self.to_visit.pop() else {
            return Ok(());
        };
        let next_node = self.ast.get_node(next).context("while visiting children")?;
        (self.func)(&mut self.to_visit, &mut self.extra, next_node);
        self.visit()?;
        return Ok(());
    }
}
