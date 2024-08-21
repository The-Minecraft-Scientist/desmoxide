use crate::{
    graph::expressions::{ExpressionId, Expressions},
    interop::{Expression, GraphState},
    lang::{ast::Ident, compiler::frontend::Frontend},
};
use std::{collections::HashMap, time::Instant};

#[test]
fn test_parse_simplex_4d() {
    let _simplex = include_str!("../tests/simplex.json");
    let graph = serde_json::de::from_str(include_str!("../tests/simplex.json")).unwrap();
    let exprs = get_exprs(&graph);
    parse_graph_exprs(exprs);
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
    let p = parse_graph_exprs(exprs);
    let mut f = Frontend::new(&p.meta, &p.ident_lookup);
    let t = Instant::now();
    let val = f.direct_compile_fn(&Ident::from(func)).unwrap();
    let after = Instant::now();
    val.instructions.debug_print(val.ret);
    println!(
        "done in {:?} microseconds, compiled to {:?} instructions",
        after.checked_duration_since(t).unwrap().as_micros(),
        val.instructions.len()
    );
}
pub fn parse_graph_exprs(exprs: HashMap<ExpressionId, String>) -> Expressions {
    let mut p = Expressions::new(exprs);
    let errors = p.parse_all();
    for (_, err) in &errors {
        eprintln!("{}", err);
    }
    assert!(errors.is_empty());

    p
}
pub fn get_exprs(body: &GraphState) -> HashMap<ExpressionId, String> {
    let mut m = HashMap::with_capacity(50);
    for expr in body.expressions.list.iter().enumerate() {
        if let Expression::Expression {
            id,
            latex: Some(s),
            color: _,
            other: _,
        } = expr.1
        {
            m.insert(ExpressionId(*id), s.to_owned());
        }
    }
    m
}
