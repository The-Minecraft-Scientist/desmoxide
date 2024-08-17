use std::ops::{Add, Div, Mul, Sub};

use num::{rational::Ratio, ToPrimitive};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Number {
    Fraction(Ratio<i64>),
    Double(f64),
    Undefined,
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self::Double(value)
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
    ($trait:ident, $method:ident, $op:tt) => {
        impl std::ops::$trait for Number {
            type Output = Number;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Number::Double(lhs), Number::Double(rhs)) => {
                        let result = lhs.$method(rhs);
                        if result.is_nan() || result.is_infinite() {
                            Number::Undefined
                        } else {
                            Number::Double(result)
                        }
                    }

                    (Number::Fraction(lhs), Number::Fraction(rhs)) => {
                        let result = lhs $op rhs;

                        Number::Fraction(result)
                    }

                    (Number::Fraction(lhs), Number::Double(rhs)) => {
                        let result = lhs.to_f64().unwrap() $op rhs;
                        if result.is_nan() || result.is_infinite() {
                            Number::Undefined
                        } else {
                            Number::Double(result)
                        }
                    }

                    (Number::Double(lhs), Number::Fraction(rhs)) => {
                        let result = lhs $op rhs.to_f64().unwrap();
                        if result.is_nan() || result.is_infinite() {
                            Number::Undefined
                        } else {
                            Number::Double(result)
                        }
                    }

                    (Number::Undefined, _) => Number::Undefined,
                    (_, Number::Undefined) => Number::Undefined,
                }
            }
        }

        impl std::ops::$trait for &Number {
            type Output = Number;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Number::Double(lhs), Number::Double(rhs)) => {
                        let result = lhs $op rhs;
                        if result.is_nan() || result.is_infinite() {
                            Number::Undefined
                        } else {
                            Number::Double(result)
                        }
                    }

                    (Number::Fraction(lhs), Number::Fraction(rhs)) => {
                        let result = lhs $op rhs;

                        Number::Fraction(result)
                    }

                    (Number::Fraction(lhs), Number::Double(rhs)) => {
                        let result = lhs.to_f64().unwrap() $op rhs;
                        if result.is_nan() || result.is_infinite() {
                            Number::Undefined
                        } else {
                            Number::Double(result)
                        }
                    }

                    (Number::Double(lhs), Number::Fraction(rhs)) => {
                        let result = lhs $op rhs.to_f64().unwrap();
                        if result.is_nan() || result.is_infinite() {
                            Number::Undefined
                        } else {
                            Number::Double(result)
                        }
                    }

                    (Number::Undefined, _) => Number::Undefined,
                    (_, Number::Undefined) => Number::Undefined,
                }
            }
        }
    };
}

impl_op!(Add, add, +);
impl_op!(Sub, sub, -);
impl_op!(Mul, mul, *);
impl_op!(Div, div, /);
