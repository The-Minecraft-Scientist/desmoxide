use super::{Opcode, Value};

#[derive(Debug, Clone)]
pub struct ExpressionMeta<'a> {
    expression_type: ExpressionType<'a>,
}
#[derive(Debug, Clone)]
pub enum ExpressionType<'a> {
    Fn {
        ident: &'a str,
        params: Vec<&'a str>,
    },
    Var {
        ident: &'a str,
    },
    Equation {
        eq_type: EquationType,
    },
}
#[derive(Debug, Clone, Copy)]
pub enum EquationType {
    Implicit,
    Explicit,
    InEq(Opcode),
}
