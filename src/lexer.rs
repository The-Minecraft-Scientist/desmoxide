/// Lexer token
#[derive(logos::Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+|\\left|\\right")]
pub enum Token {
    // LITERALS --------------------------------------------
    /// Floating point literal
    #[regex("[0-9]*\\.[0-9]+", |lex| {lex.slice().parse().ok()})]
    FloatLit(f64),
    ///Integer literal
    #[regex("\\d*", |lex| {i64::from_str_radix(lex.slice(), 10).ok()})]
    IntegerLit(i64),

    // IDENTIFIERS -----------------------------------------
    //Unrecognized LaTeX command; Treated as an identifer.
    #[regex(r"\\[a-zA-Z]+(_\{[a-zA-Z]+\})?")]
    //Single letter
    #[regex("[a-zA-Z]")]
    //name with subscript:
    #[regex(r"[a-zA-Z]_\{[a-zA-Z0-9\.]+\}", priority = 0)]
    /// Variable identifier
    Ident,

    // SEPERATORS ------------------------------------------
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
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("_")]
    Subscript,

    // COMPARISON OPERATORS --------------------------------
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

    // OPERATORS -------------------------------------------
    #[token("\\frac")]
    Frac,
    #[token("\\div")]
    #[token("/")]
    Div,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("\\cdot")]
    #[token("*")]
    Mul,
    #[token("^")]
    Pow,
    #[token("...")]
    Range,
    #[token(".")]
    Dot,
    #[token("\\sqrt")]
    Sqrt,

    // BUILTINS --------------------------------------------
    #[token("\\min")]
    Min,
    #[token("\\max")]
    Max,
    #[token("\\operatorname{count}")]
    Count,
    #[token("\\operatorname{total}")]
    Total,
    #[token("\\operatorname{join}")]
    Join,
    #[token("\\operatorname{sort}")]
    Sort,
    #[token("\\operatorname{shuffle}")]
    Shuffle,
    #[token("\\operatorname{unique}")]
    Unique,
    #[token("\\operatorname{for}")]
    For,
    #[token("\\sin")]
    Sin,
    #[token("\\cos")]
    Cos,
    #[token("\\tan")]
    Tan,
    #[token("\\csc")]
    Csc,
    #[token("\\sec")]
    Sec,
    #[token("\\cot")]
    Cot,
    #[token("\\sin^{-1}")]
    InvSin,
    #[token("\\cos^{-1}")]
    InvCos,
    #[token("\\tan^{-1}")]
    InvTan,
    #[token("\\csc^{-1}")]
    InvCsc,
    #[token("\\sec^{-1}")]
    InvSec,
    #[token("\\cot^{-1}")]
    InvCot,
}
impl Token {
    pub fn is_value(&self) -> bool {
        match self {
            Self::FloatLit(_) | Self::IntegerLit(_) | Self::Ident => true,
            _ => false,
        }
    }
    pub fn ends_parse(&self) -> bool {
        match self {
            Self::RGroup
            | Self::For
            | Self::RParen
            | Self::Eq
            | Self::Gt
            | Self::Ge
            | Self::Le
            | Self::Lt
            | Self::Colon
            | Self::RBracket
            | Self::Comma => true,
            _ => false,
        }
    }
    pub fn begins_scope(&self) -> Option<Token> {
        match self {
            Self::LParen => Some(Self::RParen),
            Self::LGroup => Some(Self::RGroup),
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
