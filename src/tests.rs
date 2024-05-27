use crate::{
    graph::expressions::Expressions,
    interop::{Expression, GraphState},
    lang::compiler::{expression_provider::ExpressionId, frontend::Frontend},
    util::thin_str::ThinStr,
};
use std::{collections::HashMap, time::Instant};

#[test]
fn test_parse_simplex_4d() {
    let _simplex = include_str!("../tests/simplex.json");
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
#[test]
fn test_thin_str_soundness() {
    let teststr = String::from_utf8(vec![b'a'; ThinStr::MAX_SLICE_LEN + 1]).unwrap();
    dbg!(&teststr);
    let s = ThinStr::from_slice(&teststr);
    println!("{}", s);
}

fn test_compile_fn(path: &'static str, func: &'static str) {
    let graph = serde_json::de::from_str(&std::fs::read_to_string(path).unwrap()).unwrap();
    let exprs = get_exprs(&graph);
    let p = parse_graph_exprs(&exprs);
    let mut f = Frontend::new(&p);
    let t = Instant::now();
    let val = f.direct_compile_fn(func).unwrap();
    let after = Instant::now();
    val.instructions.debug_print(val.ret.unwrap());
    println!(
        "done in {:?} microseconds, compiled to {:?} instructions",
        after.checked_duration_since(t).unwrap().as_micros(),
        val.instructions.len()
    );
}
pub fn parse_graph_exprs<'a>(exprs: &'a HashMap<ExpressionId, &'a str>) -> Expressions<'a> {
    let mut p = Expressions::new(exprs);
    p.parse_all().unwrap();
    p
}
pub fn get_exprs<'a>(body: &'a GraphState) -> HashMap<ExpressionId, &'a str> {
    let mut m = HashMap::with_capacity(50);
    for expr in body.expressions.list.iter().enumerate() {
        if let Expression::Expression {
            id,
            latex: Some(s),
            color: _,
            other: _,
        } = expr.1
        {
            m.insert(ExpressionId(*id), s.as_str());
        }
    }
    m
}
