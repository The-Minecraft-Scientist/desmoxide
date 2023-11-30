#[derive(logos::Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+|\\left|\\right")]
pub enum Token {
    //Floating point literal (desmos compliant, no inline exponent... etc.)
    #[regex("[0-9]*\\.[0-9]+", |lex| {lex.slice().parse().ok()})]
    FloatLit(f64),
    //Integer literal
    #[regex("\\d*", |lex| {i64::from_str_radix(lex.slice(), 10).ok()})]
    IntegerLit(i64),
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
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
    Plus,
    #[token("-")]
    Minus,
    #[token("\\cdot")]
    Mul,
    #[token("=")]
    Eq,
    #[token("\\ge")]
    Ge,
    #[token("\\le")]
    Le,
    #[token("\\gt")]
    Gt,
    #[token("\\lt")]
    Lt,
    #[token("^")]
    Pow,
    #[token(":")]
    Colon,
    #[token("...")]
    Dots,
    #[token(".")]
    Dot,
    #[token("\\operatorname{for}")]
    For,
    #[token("\\sqrt")]
    Sqrt,
    #[token("_", priority = 1)]
    Subscript,
    //Unrecognized LaTeX command; Treated as an identifer.
    #[regex(r"\\[a-zA-Z]+(_\{[a-zA-Z]+\})?")]
    //Single letter
    #[regex("[a-zA-Z]")]
    //name with subscript:
    #[regex(r"[a-zA-Z]_\{[a-zA-Z]+\}")]
    /// Variable identifier
    Ident,
}
impl Token {
    pub fn is_value(&self) -> bool {
        match self {
            Self::FloatLit(_) | Self::IntegerLit(_) | Self::Ident => true,
            _ => false,
        }
    }
    pub fn ends_scope(&self) -> bool {
        match self {
            Self::RGroup
            | Self::RParen
            | Self::Eq
            | Self::Gt
            | Self::Ge
            | Self::Le
            | Self::Lt
            | Self::Colon
            | Self::RBracket => true,
            _ => false,
        }
    }
    pub fn begins_scope(&self) -> Option<Token> {
        match self {
            Self::LParen => Some(Self::RParen),
            Self::LGroup => Some(Self::RGroup),
            Self::LBracket => Some(Self::LBracket),
            _ => None,
        }
    }
    pub fn is_comparison(&self) -> bool {
        match self {
            Self::Ge | Self::Gt | Self::Le | Self::Lt | Self::Eq => true,
            _ => false,
        }
    }
}
