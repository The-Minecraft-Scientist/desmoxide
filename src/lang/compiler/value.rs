use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Neg, Sub},
};

use num::{pow::Pow, rational::Ratio, Float, ToPrimitive};

use super::ir::IRType;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum IRValue {
    None,
    Bool(bool),
    Number(Number),
    Vec2(Number, Number),
    Vec3(Number, Number, Number),
    NumberList(Vec<Number>),
    Vec2List(Vec<(Number, Number)>),
    Vec3List(Vec<(Number, Number, Number)>),
}

impl IRValue {
    pub fn ir_type(&self) -> IRType {
        match self {
            Self::None => IRType::Never,
            Self::Bool(bool) => IRType::Bool,
            Self::Number(_) => IRType::Number,
            Self::Vec2(_, _) => IRType::Vec2,
            Self::Vec3(_, _, _) => IRType::Vec3,
            Self::NumberList(_) => IRType::NumberList,
            Self::Vec2List(_) => IRType::Vec2List,
            Self::Vec3List(_) => IRType::Vec3List,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Number {
    Fraction(Ratio<i64>),
    Double(f64),
    Undefined,
}

#[macro_export]
macro_rules! call_irrational {
    ($var:ident, $method:ident) => {
        match $var {
            &Number::Double($var) => $var.$method(),
            &Number::Fraction($var) => $var.to_f64().unwrap().$method(),
            &Number::Undefined => f64::NAN,
        }
    };
}

impl Number {
    pub fn ceil(&self) -> Number {
        match self {
            &Number::Double(n) => n.ceil().into(),
            &Number::Fraction(n) => n.ceil().into(),
            &Number::Undefined => Number::Undefined,
        }
    }

    pub fn floor(&self) -> Number {
        match self {
            &Number::Double(n) => n.floor().into(),
            &Number::Fraction(n) => n.floor().into(),
            &Number::Undefined => Number::Undefined,
        }
    }
}

impl Into<f64> for Number {
    fn into(self) -> f64 {
        match self {
            Number::Double(y) => y,
            Number::Fraction(y) => y.to_f64().unwrap(),
            Number::Undefined => f64::NAN,
        }
    }
}

impl Into<f32> for Number {
    fn into(self) -> f32 {
        match self {
            Number::Double(y) => y as f32,
            Number::Fraction(y) => y.to_f32().unwrap(),
            Number::Undefined => f32::NAN,
        }
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        if value.is_nan() {
            Number::Undefined
        } else if value.is_infinite() {
            Number::Fraction(Ratio::new(value.signum() as i64 * 1, 0))
        } else {
            Number::Double(value)
        }
    }
}

impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Self::Fraction(value.into())
    }
}

impl From<Ratio<i64>> for Number {
    fn from(value: Ratio<i64>) -> Self {
        Self::Fraction(value)
    }
}

macro_rules! impl_op {
    ($trait:ident, $method:ident) => {
        impl $trait for Number {
            type Output = Number;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Number::Double(lhs), Number::Double(rhs)) => {
                        let result = lhs.$method(rhs);
                        if result.is_nan() {
                            Number::Undefined
                        } else if result.is_infinite() {
                            Number::Fraction(Ratio::new(result.signum() as i64 * 1, 0))
                        } else {
                            result.into()
                        }
                    }

                    (Number::Fraction(lhs), Number::Fraction(rhs)) => {
                        if *rhs.denom() == 0 {
                            Number::Undefined
                        } else {
                            let result = lhs.$method(rhs);

                            result.into()
                        }
                    }

                    (Number::Fraction(lhs), Number::Double(rhs)) => {
                        let result = lhs.to_f64().unwrap().$method(rhs);
                        if result.is_nan() {
                            Number::Undefined
                        } else if result.is_infinite() {
                            Number::Fraction(Ratio::new(result.signum() as i64 * 1, 0))
                        } else {
                            result.into()
                        }
                    }

                    (Number::Double(lhs), Number::Fraction(rhs)) => {
                        let result = lhs.$method(rhs.to_f64().unwrap());
                        if result.is_nan() {
                            Number::Undefined
                        } else if result.is_infinite() {
                            Number::Fraction(Ratio::new(result.signum() as i64 * 1, 0))
                        } else {
                            result.into()
                        }
                    }

                    (Number::Undefined, _) => Number::Undefined,
                    (_, Number::Undefined) => Number::Undefined,
                }
            }
        }

        impl $trait for &Number {
            type Output = Number;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Number::Double(lhs), Number::Double(rhs)) => lhs.$method(rhs).into(),

                    (Number::Fraction(lhs), Number::Fraction(rhs)) => {
                        if *rhs.numer() == 0 {
                            Number::Undefined
                        } else {
                            let result = lhs.$method(rhs);

                            Number::Fraction(result)
                        }
                    }

                    (Number::Fraction(lhs), Number::Double(rhs)) => {
                        lhs.to_f64().unwrap().$method(rhs).into()
                    }

                    (Number::Double(lhs), Number::Fraction(rhs)) => {
                        lhs.$method(rhs.to_f64().unwrap()).into()
                    }

                    (Number::Undefined, _) => Number::Undefined,
                    (_, Number::Undefined) => Number::Undefined,
                }
            }
        }
    };
}

impl_op!(Add, add);
impl_op!(Sub, sub);
impl_op!(Mul, mul);
impl_op!(Div, div);

impl PartialOrd for Number {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self, rhs) {
            (Number::Double(lhs), Number::Double(rhs)) => lhs.partial_cmp(rhs),

            (Number::Fraction(lhs), Number::Fraction(rhs)) => lhs.partial_cmp(rhs),

            (Number::Fraction(lhs), Number::Double(rhs)) => lhs.to_f64().unwrap().partial_cmp(rhs),

            (Number::Double(lhs), Number::Fraction(rhs)) => lhs.partial_cmp(&rhs.to_f64().unwrap()),

            (Number::Undefined, _) => None,
            (_, Number::Undefined) => None,
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Number::Double(lhs), Number::Double(rhs)) => lhs.eq(rhs),
            (Number::Fraction(lhs), Number::Fraction(rhs)) => lhs.eq(rhs),
            (Number::Fraction(lhs), Number::Double(rhs)) => lhs.to_f64().unwrap().eq(rhs),
            (Number::Double(lhs), Number::Fraction(rhs)) => lhs.eq(&rhs.to_f64().unwrap()),
            (Number::Undefined, _) => false,
            (_, Number::Undefined) => false,
        }
    }
}

impl Neg for Number {
    type Output = Number;
    fn neg(self) -> Number {
        match self {
            Number::Double(n) => Number::Double(-n),
            Number::Fraction(n) => Number::Fraction(-n),
            Number::Undefined => Number::Undefined,
        }
    }
}

impl Pow<Number> for Number {
    type Output = Number;
    fn pow(self, rhs: Number) -> Self::Output {
        match (self, rhs) {
            (Number::Double(lhs), Number::Double(rhs)) => lhs.pow(rhs).into(),

            (Number::Fraction(lhs), Number::Fraction(rhs)) => {
                if let Some(rhs) = rhs.to_i64() {
                    let result = lhs.pow(rhs as i32);

                    Number::Fraction(result)
                } else {
                    lhs.to_f64().unwrap().pow(rhs.to_f64().unwrap()).into()
                }
            }

            (Number::Fraction(lhs), Number::Double(rhs)) => lhs.to_f64().unwrap().pow(rhs).into(),

            (Number::Double(lhs), Number::Fraction(rhs)) => {
                if let Some(rhs) = rhs.to_i64() {
                    lhs.pow(rhs as i32).into()
                } else {
                    lhs.to_f64().unwrap().pow(rhs.to_f64().unwrap()).into()
                }
            }

            (Number::Undefined, _) => Number::Undefined,
            (_, Number::Undefined) => Number::Undefined,
        }
    }
}

impl Pow<&Number> for &Number {
    type Output = Number;
    fn pow(self, rhs: &Number) -> Self::Output {
        match (self, rhs) {
            (Number::Double(lhs), Number::Double(rhs)) => lhs.pow(rhs).into(),

            (Number::Fraction(lhs), Number::Fraction(rhs)) => {
                if let Some(rhs) = rhs.to_i64() {
                    let result = lhs.pow(rhs as i32);

                    Number::Fraction(result)
                } else {
                    lhs.to_f64().unwrap().pow(rhs.to_f64().unwrap()).into()
                }
            }

            (Number::Fraction(lhs), Number::Double(rhs)) => lhs.to_f64().unwrap().pow(rhs).into(),

            (Number::Double(lhs), Number::Fraction(rhs)) => {
                if let Some(rhs) = rhs.to_i64() {
                    lhs.pow(rhs as i32).into()
                } else {
                    lhs.to_f64().unwrap().pow(rhs.to_f64().unwrap()).into()
                }
            }

            (Number::Undefined, _) => Number::Undefined,
            (_, Number::Undefined) => Number::Undefined,
        }
    }
}
