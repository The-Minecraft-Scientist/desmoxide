use std::collections::HashMap;

use crate::{
    ast::parser::Expressions,
    compile::frontend::Frontend,
    interop::{Expression, Graph},
};

#[test]
fn test_parse_simplex_4d() {
    let simplex = include_str!("../tests/simplex.json");
    let graph = serde_json::de::from_str(include_str!("../tests/simplex.json")).unwrap();
    let exprs = get_exprs(&graph);
    parse_graph_exprs(&exprs);
}
#[test]
fn test_compile() {
    let simplex = include_str!("../tests/simplex.json");
    let graph = serde_json::de::from_str(include_str!("../tests/simplex.json")).unwrap();
    let exprs = get_exprs(&graph);
    let mut p = parse_graph_exprs(&exprs);
    let mut f = Frontend { ctx: &p };
    dbg!(f.compile_expr(p.ident_ast("i").unwrap()));
}
fn parse_graph_exprs<'a>(exprs: &'a HashMap<u32, &'a str>) -> Expressions<'a> {
    let mut p = Expressions::new(exprs);
    let v = p.bench_test().unwrap();
    if v.len() > 0 {
        println!("test failed to parse the following expressions: {:?}", v);
        panic!();
    }
    p
}
fn get_exprs<'a>(body: &'a Graph) -> HashMap<u32, &'a str> {
    let mut m = HashMap::with_capacity(50);
    for expr in body.exprs().into_iter().enumerate() {
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
    m
}
