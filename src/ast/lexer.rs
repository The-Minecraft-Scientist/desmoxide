use std::{iter::Peekable, str::Chars, num::ParseFloatError};
use self::LexerToken::*;
use super::opcodes::*;
use std::str::FromStr;

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Assoc {
    LEFT,
    RIGHT,
}


#[derive(Copy, Debug, Clone, PartialEq, Eq)]
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
    pub fn not_type_eq(&self) -> bool {
        self != &EQ
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
        // We use a loop and explicitly call next() so we can advance (which takes &mut) the iterator within arms of the match statement
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
                    let Some(cmd) = chars.get_until_false(|x| -> bool {x.is_ascii_alphabetic()}) else {
                        return Err(LexError::IncompleteLaTeXCommand);
                    };
                    // collect chars into an &str for convenient matching
                    match cmd.as_str() {
                        "frac" => {
                            tokens.push(LexerToken::Operator(DIV))
                        }
                        "cdot" => {
                            tokens.push(LexerToken::Operator(MUL))
                        }
                        "cos" => {
                            tokens.push(LexerToken::Operator(COS))
                        }
                        "sqrt" => {

                        }
                        "sin" => {
                            tokens.push(LexerToken::Operator(SIN))
                        }
                        any => {
                            return Err(LexError::UnrecognizedLaTexCommand(any.to_string()))
                        }
                    }
                }
                num if num.is_ascii_digit() || num == '.' => {
                    let nump = chars.get_until_false(|x|->bool{x.is_ascii_digit() || x == &'.'}).unwrap_or("".to_string());
                    let Ok(val) = f64::from_str(&nump) else {
                        return Err(LexError::FloatParseError)
                    };
                    tokens.push(LexerToken::Value(DesmosVal::ConstNum(val)));
                }
                '(' => {
                    tokens.push(LexerToken::Operator(LPARENS))
                }
                ')' => {
                    tokens.push(LexerToken::Operator(RPARENS))
                }
                //Start of an indexing OR list declaration.
                '[' => {
                    if let Some(&LexerToken::Value(_)) = tokens.last() {
                        // we are an index
                        tokens.push(LexerToken::Operator(INDEX));
                        // TODO - make this properly advance the iterator
                    } else {
                        // we are a list definition
                        let Ok(list) = parse_list(&mut chars) else {
                            return Err(LexError::ListParseError)
                        };
                        tokens.push(LexerToken::Value(list));
                    }
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

fn parse_list(to_parse: &mut Peekable<Chars> ) -> Result<DesmosVal, LexError> {
    let Some(next) = to_parse.next() else {
        return Err(LexError::ListParseError)
    };
    match next.clone() {
        '(' => {
            //list of points
            let Some(list_string) = 
            to_parse.get_until_false(|x| -> bool {x.is_ascii_digit() || x == &'(' || x == &')' || x == &'.' || x == &','}) 
            else {
                return Err(LexError::UnexpectedEOL)
            };
            let mut list_out = vec![];
            for point in list_string.split(")") {
                let spl: Vec<&str> = point.split(",").collect();
                let (Ok(x), Ok(y)) = (DesmosNum::from_str(&spl[0][1..]), DesmosNum::from_str(&spl[1])) else {
                    return Err(LexError::FloatParseError)
                };
                list_out.push((x, y));

            }
            Ok(DesmosVal::ConstPointList(list_out))
        }
        c if c.is_ascii_digit() || c == '.' => {
            let mut list_out = vec![];
            //list of numbers
            let Some(list_string) = 
            to_parse.get_until_false(|x| -> bool {x.is_ascii_digit() || x == &'.' || x == &','})
            else {
                return Err(LexError::UnexpectedEOL)
            };
            for number in list_string.split(",") {
                let Ok(num) = DesmosNum::from_str(number) else {
                    return Err(LexError::FloatParseError)
                };
                list_out.push(num);
            }
            Ok(DesmosVal::ConstNumList(list_out))
        }
        any => {
            Err(LexError::IncorrectSymbol(any))
        }
    }
}
#[derive(Debug)]
pub enum LexError {
    UnrecognizedSymbol(char),
    IncorrectSymbol(char),
    UnrecognizedLaTexCommand(String),
    IncompleteLaTeXCommand,
    UnexpectedEOL,
    FloatParseError,
    ListParseError,
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
    fn get_until_false(&mut self, filt: fn(&char) -> bool) -> Option<String>;
    fn get_next_num(&mut self) -> Option<DesmosVal>;
}
impl<'a> PeekableExt for Peekable<Chars<'a>> {
    fn get_until_false(&mut self, filt: fn(&char) -> bool) -> Option<String> {
        let mut v = vec![];
        let Some(first) = self.peek() else {
            return None
        };
        if (filt)(first) {
            v.push(self.next().unwrap());
        } else {
            return None
        }
        loop {
            let Some(next_ref) = self.peek() else {
                break
            };
            if (filt)(next_ref) {
                v.push(self.next().unwrap())
            } else {
                break
            };
        };
        Some(v.into_iter().collect::<String>())
        
    }
    fn get_next_num(&mut self) -> Option<DesmosVal> {
        None
    }
}
