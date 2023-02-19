use std::{iter::Peekable, str::Chars};
use self::LexerToken::*;
use super::opcodes::*;
use std::str::FromStr;

#[derive(Copy, Debug, Clone)]
#[repr(u8)]
pub enum Assoc {
    LEFT,
    RIGHT,
}


#[derive(Copy, Debug, Clone)]
pub struct Opcode {
    pub bindingPower: u8,
    assoc: Assoc,
}

impl Opcode {
    pub const fn newr(power: u8) -> Self {
        Self { bindingPower: power, assoc: Assoc::RIGHT }
    }
    pub const fn newl(power: u8) -> Self {
        Self { bindingPower: power, assoc: Assoc::LEFT }
    }
}


/// floating point type used by Desmoclient; Could be changed to do multiprecision evaluation
pub type DesmosNum = f64;
pub type DesmosVec<T> = Vec<T>;


/// indirection to wrap all possible non-operator token values in one type
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum DesmosVal {
    ConstNum(DesmosNum),
    ConstPoint((DesmosNum,DesmosNum)),
    ConstNumList(DesmosVec<DesmosNum>),
    ConstPointList(DesmosVec<(DesmosNum,DesmosNum)>),
    DynNum(String),
    DynPoint(String),
    DynNumList(String),
    DynPointList(String),

}


pub struct TokenStream {
    tokens: DesmosVec<LexerToken>
}

impl<'a> TokenStream {
    fn new(of: String) -> Result<Self, LexError> {
        let mut prepped = of;
        prepped = prepped.replace_multi(vec!["\\left", "\\right"], "");
        let mut tokens = vec![];
        let mut chars: Peekable<Chars> = prepped.chars().into_iter().peekable();
        // We use a loop and explicitly call next() so we can advance (which requires &mut) the iterator within arms of the match statement
        loop {
            let Some(next) = chars.next() else {
                break
            };
            match next {
                '+' => {
                    tokens.push(Operator(ADD));
                }
                '-' => {
                    tokens.push(Operator(SUB));
                }
                //We hit a LaTeX codepoint
                '\\' => {
                    let Some(cmd) = chars.get_string_filter(|x| -> bool {x.is_ascii_alphabetic()}) else {
                        return Err(LexError::IncompleteLaTeXCommand);
                    };
                    // collect chars into an &str for convenient matching
                    match cmd.as_str() {
                        "frac" => {

                        }
                        "cdot" => {

                        }
                        any => {
                            return Err(LexError::UnrecognizedLaTexCommand(any.to_string()))
                        }
                    }
                }
                num if num.is_ascii_digit() || num == '.' => {
                    let nump = chars.get_string_filter(|x|->bool{x.is_ascii_digit() || x == &'.'}).unwrap_or("".to_string());
                    let val: DesmosNum = f64::from_str().unwrap();
                }
                any => {
                    return Err(LexError::UnrecognizedSymbol(any));
                }
            }
        }
        todo!()
    }
    fn consume(&mut self) -> Option<LexerToken> {
        self.tokens.pop()
    }
    fn peek(&'a self) -> Option<&'a LexerToken> {
        self.tokens.last()
    }
}


pub enum LexerToken {
    Value(DesmosVal),
    Operator(Opcode),
}


#[derive(Debug)]
pub enum LexError {
    UnrecognizedSymbol(char),
    UnrecognizedLaTexCommand(String),
    IncompleteLaTeXCommand,
    UnexpectedEOL,
}
pub trait StrExt<'a> {
    fn replace_multi(&'a self, filts: Vec<&'a str>, with: &'a str) -> String;
}
impl<'a> StrExt<'a> for str {
    fn replace_multi(&'a self, filts: Vec<&'a str>, with: &'a str) -> String {
        let mut strout = self.to_string();
        for filt in filts {
            strout = strout.replace(filt, with);
        }
        strout
    }

}
impl<'a> StrExt<'a> for String {
    fn replace_multi(&'a self, filts: Vec<&'a str>, with: &'a str) -> String {
        let mut strout = self.to_string();
        for filt in filts {
            strout = strout.replace(filt, with);
        }
        strout
    }
}

pub trait PeekableExt {
    fn get_string_filter(&mut self, filt: fn(&char) -> bool) -> Option<String>;
}
impl<'a> PeekableExt for Peekable<Chars<'a>> {
    fn get_string_filter(&mut self, filt: fn(&char) -> bool) -> Option<String> {
        let mut v = vec![];
        let Some(first) = self.peek() else {
            return None
        };
        if (filt)(first) {
            v.push(self.next().unwrap());
        }
        loop {
            let Some(next_ref) = self.peek() else {
                break
            };
            if (filt)(next_ref) {
                v.push(self.next().unwrap())
            } else {
                break
            }
            let c = self.peek();
        };
        Some(v.into_iter().collect::<String>())
    }
}
