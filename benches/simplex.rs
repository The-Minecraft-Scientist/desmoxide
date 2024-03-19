use std::collections::HashMap;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use desmoxide::{
    ast::expression_manager::Expressions,
    compile::frontend::Frontend,
    interop::{Expression, GraphState},
};

fn benchmark_compile(c: &mut Criterion) {
    let graph =
        serde_json::de::from_str(&std::fs::read_to_string("tests/simplex.json").unwrap()).unwrap();
    let exprs = get_exprs(&graph);
    let p = parse_graph_exprs(&exprs);
    c.bench_function("simplex compile", |b| {
        b.iter_batched_ref(
            || Frontend { ctx: &p },
            |a| a.direct_compile_fn(black_box("s_{implex4D}")),
            criterion::BatchSize::SmallInput,
        )
    });
}
fn benchmark_parse(c: &mut Criterion) {
    let graph =
        serde_json::de::from_str(&std::fs::read_to_string("tests/simplex.json").unwrap()).unwrap();
    let exprs = get_exprs(&graph);
    c.bench_function("simplex parse", |b| {
        b.iter_with_large_drop(|| Expressions::new(&exprs).parse_all().unwrap())
    });
}

pub fn parse_graph_exprs<'a>(exprs: &'a HashMap<u32, &'a str>) -> Expressions<'a> {
    let mut p = Expressions::new(exprs);
    p.parse_all().unwrap();
    p
}
pub fn get_exprs<'a>(body: &'a GraphState) -> HashMap<u32, &'a str> {
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
criterion_group!(benches, benchmark_compile, benchmark_parse);
criterion_main!(benches);
