use desmoparse::ast::parser::Parser;

fn main() {
    let str = r"\psi = \psi_{s} + \psi_{p}";
    let str1 = r"b = a(cb) + 12.2(2+b) + (l[i] + 2.5bc^{4+5})";
    let str2 = r"L_{0}=[x+2\operatorname{for}i=[0...3]]";
    let p = Parser::new(vec![str.to_owned(), str1.to_owned(), str2.to_owned()]);
    dbg!(p.line_lexer(2).collect::<Vec<_>>());
    let _ = dbg!(p.expression_ast(2));
}
