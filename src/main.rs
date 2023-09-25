use desmoparse::ast::parser::Parser;

fn main() {
    let str = r"\psi = \psi_{s} + \psi_{p}";
    let str1 = r"b = a(cb) + 12.2(2+b) + (l[i] + 2.5bc^{4+5})";
    let p = Parser::new(vec![str.to_owned(), str1.to_owned()]);
    //dbg!(p.line_lexer(1).collect::<Vec<_>>());
    let _ = dbg!(p.expression_ast(1));
}
