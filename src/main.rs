use anyhow::Result;
use desmoparse::ast::parser::Parser;
fn main() -> Result<()> {
    let str = r"\psi = \psi_{s} + \psi_{p}";
    let str1 = r"b = a(cb) + 12.2(2+b) + (l[i] + 2.5bc^{4+5})";
    let str2 =
        r"L_{0}=[\frac{i+j+k}{2(ijk) + l[i+j]}\operatorname{for}i=[0...3],k=[0...3],j=[0...3]]";
    let str3 = r"L_{1}=\frac{1}{0}";
    let str4 = r"L_{2}=[i \operatorname{for} i = [1...3]].\operatorname{join}([1...3], 2, 4)";
    let p = Parser::new(vec![
        str.to_owned(),
        str1.to_owned(),
        str2.to_owned(),
        str3.to_owned(),
        str4.to_owned(),
    ]);
    //dbg!(p.line_lexer(2).collect::<Vec<_>>());
    dbg!(p.expression_ast(4)?);

    Ok(())
}
