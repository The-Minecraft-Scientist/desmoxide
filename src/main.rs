use std::collections::HashMap;

use anyhow::Result;
use desmoparse::{
    ast::parser::Parser,
    interop::graph_state::{Expression, Graph, GraphMeta, GraphState},
};
use serde::Deserialize;
fn main() -> Result<()> {
    let s = include_str!("test.json");
    let state: Graph = serde_json::de::from_str::<Graph>(&s)?;
    let mut m = HashMap::with_capacity(50);
    for expr in state.exprs().into_iter().enumerate() {
        if let Expression::Expression {
            id,
            latex: Some(s),
            color: _,
            other: _,
        } = expr.1
        {
            m.insert(*id, s.as_str());
        }
    }
    let p = Parser::new(&m);
    dbg!(p.expression_ast(452));
    Ok(())
}
