use std::collections::HashMap;

use crate::{
    ast::parser::Expressions,
    interop::{Expression, Graph},
};

#[test]
fn test_parse_simplex_4d() {
    let simplex = include_str!("../tests/simplex.json");
    parse_graph_exprs(simplex);
}
pub fn parse_graph_exprs(g: &str) {
    let state: Graph = serde_json::de::from_str::<Graph>(g).unwrap();
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
    let p = Expressions::new(&m);
    let v = p.bench_test().unwrap();
    if v.len() > 0 {
        println!("test failed to parse the following expressions: {:?}", v);
        panic!();
    }
}
