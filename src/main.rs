use std::collections::HashMap;

use anyhow::Result;
use desmoparse::{
    ast::parser::Parser,
    interop::graph_state::{Graph, GraphMeta, GraphState},
};
use serde::Deserialize;
fn main() -> Result<()> {
    let s = include_str!("test.json");
    let state = serde_json::de::from_str::<Graph>(&s)?;
    dbg!(&state);
    Ok(())
}
