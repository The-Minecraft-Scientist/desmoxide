use logos::Lexer;

#[derive(logos::Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+|\\left|\\right")]
pub enum Token {
    //Floating point literal (desmos compliant, no inline exponent... etc.)
    #[regex("[-+]?[0-9]*\\.[0-9]+", |lex| lex.slice().parse().ok())]
    FloatLit(f64),
    //Integer literal
    #[regex("[+-]?\\d*", |lex| i64::from_str_radix(lex.slice(), 10).ok())]
    IntegerLit(i64),
    #[token("[")]
    LBracket,
    #[token("]")]
    Rbracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LGroup,
    #[token("}")]
    RGroup,
    #[token("\\frac")]
    Frac,
    #[token(",")]
    Comma,
    #[token("\\div")]
    Div,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("\\cdot")]
    Mul,
    #[token("=")]
    Eq,
    #[token("^")]
    Pow,
    #[token(":")]
    Colon,
    //Unrecognized LaTeX command; Treated as an identifer.
    #[regex("\\[a-zA-Z]+", |lex| if lex.slice() == "\\theta" {SpecialIdent::Theta} else {SpecialIdent::Normal})]
    //Single letter
    #[regex("[a-zA-Z]", |lex| match lex.slice() {
        "x" => SpecialIdent::X,
        "y" => SpecialIdent::Y,
        "t" => SpecialIdent::T,
          _ => SpecialIdent::Normal
    })]
    //name with subscript:
    #[regex("[a-zA-Z]_\\{[a-zA-Z]+\\}", |_| SpecialIdent::Normal)]
    /// Variable identifier
    Ident(SpecialIdent),
}
#[derive(Debug, PartialEq)]
pub enum SpecialIdent {
    X,
    Y,
    Theta,
    T,
    Normal,
}
