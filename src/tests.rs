use std::{collections::HashMap, time::Instant};

use crate::{
    ast::parser::Expressions,
    compile::frontend::Frontend,
    interop::{Expression, Graph, GraphState},
};

#[test]
fn test_parse_simplex_4d() {
    let simplex = include_str!("../tests/simplex.json");
    let graph = serde_json::de::from_str(include_str!("../tests/simplex.json")).unwrap();
    let exprs = get_exprs(&graph);
    parse_graph_exprs(&exprs);
}
#[test]
fn test_compile_simplex() {
    test_compile_fn("tests/simplex.json", "s_{implex4D}");
}
#[test]
fn test_compile_listmul() {
    test_compile_fn("tests/listmul.json", "g");
}

fn test_compile_fn(path: &'static str, func: &'static str) {
    let graph = serde_json::de::from_str(&std::fs::read_to_string(path).unwrap()).unwrap();
    let exprs = get_exprs(&graph);
    let mut p = parse_graph_exprs(&exprs);
    let mut f = Frontend { ctx: &p };
    let t = Instant::now();
    let val = f.direct_compile_fn(func).unwrap();
    val.instructions.debug_print(val.ret.unwrap());
    println!(
        "done in {:?} microseconds",
        Instant::now()
            .checked_duration_since(t)
            .unwrap()
            .as_micros()
    );
}
fn parse_graph_exprs<'a>(exprs: &'a HashMap<u32, &'a str>) -> Expressions<'a> {
    let mut p = Expressions::new(exprs);
    let v = p.bench_test().unwrap();
    if v.len() > 0 {
        eprintln!("test failed to parse the following expressions: {:?}", v);
        panic!();
    }
    p
}
fn get_exprs<'a>(body: &'a GraphState) -> HashMap<u32, &'a str> {
    let mut m = HashMap::with_capacity(50);
    for expr in body.expressions.list.iter().enumerate() {
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
