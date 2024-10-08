use std::collections::HashMap;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use desmoxide::{
    graph::expressions::{ExpressionId, Expressions},
    interop::{Expression, GraphState},
    lang::{ast::Ident, compiler::frontend::Frontend},
};

fn benchmark_compile(c: &mut Criterion) {
    let graph =
        serde_json::de::from_str(&std::fs::read_to_string("tests/simplex.json").unwrap()).unwrap();
    let exprs = get_exprs(&graph);
    let p = parse_graph_exprs(exprs);
    c.bench_function("simplex compile", |b| {
        b.iter_batched_ref(
            || Frontend::new(&p.meta, &p.ident_lookup),
            |a| a.direct_compile_fn(&Ident::from(black_box("s_{implex4D}"))),
            criterion::BatchSize::SmallInput,
        )
    });
}
fn benchmark_parse(c: &mut Criterion) {
    let graph =
        serde_json::de::from_str(&std::fs::read_to_string("tests/simplex.json").unwrap()).unwrap();
    let exprs = get_exprs(&graph);
    c.bench_function("simplex parse", |b| {
        b.iter_with_large_drop(|| {
            let errors = Expressions::new(exprs.clone()).parse_all();
            for (_, err) in &errors {
                eprintln!("{}", err);
            }
            assert!(errors.is_empty());
        })
    });
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
criterion_group!(benches, benchmark_compile, benchmark_parse);
criterion_main!(benches);
