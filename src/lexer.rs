pub use categories::*;
/// Lexer token
#[derive(logos::Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+|\\+left|\\+right")]
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
    #[regex(r"\\[a-zA-Z]+(_\{[a-zA-Z]+\})?", priority = 0)]
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
    #[token(r"=")]
    Eq,
    #[token(r"\ge")]
    Ge,
    #[token(r"\le")]
    Le,
    #[token(r"\gt")]
    Gt,
    #[token(r"\lt")]
    Lt,

    // OPERATORS -------------------------------------------
    #[token(r"\frac")]
    Frac,
    #[token(r"\div")]
    #[token("/")]
    Div,
    #[regex(r"\++")]
    Plus,
    #[token("-")]
    Minus,
    #[token(r"\cdot")]
    #[token("*")]
    Mul,
    #[token("^")]
    Pow,
    #[token("...")]
    Range,
    #[token(".")]
    Dot,
    #[token(r"\sqrt")]
    Sqrt,

    // BUILTINS --------------------------------------------
    #[token(r"\operatorname{random}")]
    Random,
    #[token(r"\min")]
    Min,
    #[token(r"\max")]
    Max,
    #[token(r"\operatorname{count}")]
    Count,
    #[token(r"\operatorname{total}")]
    Total,
    #[token(r"\operatorname{length}")]
    Length,
    #[token(r"\operatorname{join}")]
    Join,
    #[token(r"\operatorname{sort}")]
    Sort,
    #[token(r"\operatorname{shuffle}")]
    Shuffle,
    #[token(r"\operatorname{unique}")]
    Unique,
    #[token(r"\operatorname{for}")]
    For,
    #[token(r"\sin")]
    Sin,
    #[token(r"\cos")]
    Cos,
    #[token(r"\tan")]
    Tan,
    #[token(r"\csc")]
    Csc,
    #[token(r"\sec")]
    Sec,
    #[token(r"\cot")]
    Cot,
    #[token(r"\sin^{-1}")]
    InvSin,
    #[token(r"\cos^{-1}")]
    InvCos,
    #[token(r"\tan^{-1}")]
    InvTan,
    #[token(r"\csc^{-1}")]
    InvCsc,
    #[token(r"\sec^{-1}")]
    InvSec,
    #[token(r"\cot^{-1}")]
    InvCot,
}
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
