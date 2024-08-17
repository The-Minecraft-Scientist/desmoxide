use std::ops::{Add, Div, Mul, Sub};

use num::{pow::Pow, rational::Ratio, ToPrimitive};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Number {
    Fraction(Ratio<i64>),
    Double(f64),
    Undefined,
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
                            Number::Double(result)
                        }
                    }

                    (Number::Fraction(lhs), Number::Fraction(rhs)) => {
                        let result = lhs.$method(rhs);

                        Number::Fraction(result)
                    }

                    (Number::Fraction(lhs), Number::Double(rhs)) => {
                        let result = lhs.to_f64().unwrap().$method(rhs);
                        if result.is_nan() {
                            Number::Undefined
                        } else if result.is_infinite() {
                            Number::Fraction(Ratio::new(result.signum() as i64 * 1, 0))
                        } else {
                            Number::Double(result)
                        }
                    }

                    (Number::Double(lhs), Number::Fraction(rhs)) => {
                        let result = lhs.$method(rhs.to_f64().unwrap());
                        if result.is_nan() {
                            Number::Undefined
                        } else if result.is_infinite() {
                            Number::Fraction(Ratio::new(result.signum() as i64 * 1, 0))
                        } else {
                            Number::Double(result)
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
                        let result = lhs.$method(rhs);

                        Number::Fraction(result)
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
