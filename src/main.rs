mod ast;

mod render;
fn main() {
    let in_str = include_str!("test.json");
    let graph = json::parse(in_str).unwrap()["state"].clone();
    dbg!(&graph["expressions"]["list"][0]["latex"].as_str().unwrap().replace("\\left", "l"));
}
