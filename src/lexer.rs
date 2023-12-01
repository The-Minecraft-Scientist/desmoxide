/// Lexer token
#[derive(logos::Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+|\\left|\\right")]
pub enum Token {
    // LITERALS --------------------------------------------
    /// Floating point literal
    #[regex("[0-9]*\\.[0-9]+", |lex| {lex.slice().parse().ok()})]
    FloatLit(f64),
    ///Integer literal
    #[regex(r"\d+", |lex| {lex.slice().parse().ok()})]
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
    #[regex(r"\++")]
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
    #[token("\\operatorname{random}")]
    Random,
    #[token("\\min")]
    Min,
    #[token("\\max")]
    Max,
    #[token("\\operatorname{count}")]
    Count,
    #[token("\\operatorname{total}")]
    Total,
    #[token("\\operatorname{length}")]
    Length,
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
pub use categories::*;
mod categories {
    use super::Token;
    use super::Token::*;
    impl Token {
        pub fn is_value(&self) -> bool {
            matches!(self, FloatLit(_) | IntegerLit(_) | Ident)
        }
        pub fn ends_parse(&self) -> bool {
            matches!(
                self,
                RGroup | For | RParen | Eq | Gt | Ge | Le | Lt | Colon | RBracket | Comma
            )
        }
        pub fn is_trig(&self) -> bool {
            matches!(
                self,
                Sin | Cos
                    | Tan
                    | Csc
                    | Sec
                    | Cot
                    | InvSin
                    | InvCos
                    | InvTan
                    | InvCsc
                    | InvSec
                    | InvCot
            )
        }
        pub fn suffix_call_allowed(&self) -> bool {
            //TODO: this should be exhaustive
            matches!(self, Min | Max | Count | Total | Join | Length)
        }
        pub fn should_autojoin_args(&self) -> bool {
            //TODO: this is probably not exhaustive either
            matches!(self, Min | Max | Count | Total | Join)
        }
        pub fn begins_scope(&self) -> Option<Token> {
            match self {
                LParen => Some(RParen),
                LGroup => Some(RGroup),
                _ => None,
            }
        }
        pub fn is_comparison(&self) -> bool {
            matches!(self, Ge | Gt | Le | Lt | Eq)
        }
    }
}
