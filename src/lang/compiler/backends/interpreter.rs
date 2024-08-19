use core::fmt;
use std::{error::Error, f64::consts::PI, iter::Peekable, ops::Index};

use num::{pow::Pow, rational::Ratio, ToPrimitive};
use thiserror::Error;

use crate::{
    call_irrational,
    lang::{
        ast::{BinaryOp, Comparison, CoordinateAccess, UnaryOp},
        compiler::{
            ir::{FunctionId, IROp, IRSegment, IRType, Id},
            value::{IRValue, Number},
        },
    },
};

struct ValStorage {
    vals: Vec<IRValue>,
}

impl ValStorage {
    pub fn new() -> Self {
        Self {
            vals: Default::default(),
        }
    }

    pub fn push(&mut self, val: IRValue) {
        self.vals.push(val)
    }

    pub fn get(&self, id: Id) -> Result<&IRValue, EvalError> {
        self.vals
            .get(id.idx() as usize)
            .ok_or_else(|| EvalError::InstructionNotExecuted(id.idx()))
    }

    pub fn get_typechecked(&self, id: Id, expected: IRType) -> Result<&IRValue, EvalError> {
        let value = self.get(id)?;

        typecheck(value, expected)
    }
}

pub fn typecheck(value: &IRValue, expected: IRType) -> Result<&IRValue, EvalError> {
    match (value.ir_type(), value) {
        (expected, v) => Ok(v),
        (t, _) => Err(EvalError::TypeError(TypeError {
            expected: vec![expected],
            found: t,
        })),
    }
}

#[derive(Debug)]
pub struct TypeError {
    expected: Vec<IRType>,
    found: IRType,
}

impl Error for TypeError {}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeError, expected ")?;
        for (i, ty) in self.expected.iter().enumerate() {
            if i > 0 {
                write!(f, " or ")?; // Add a comma between types
            }
            write!(f, "{:?}", ty)?;
        }
        write!(f, " found {:?}", self.found)
    }
}

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Instruction {0} wasnt yet executed")]
    InstructionNotExecuted(u32),
    #[error("Bytecode doesnt not contain return instruction")]
    NoReturn,
    #[error("Missing value at instruction: {0}")]
    MissingVal(u32),
    #[error(transparent)]
    TypeError(TypeError),
    #[error("Missing function: {0:?}")]
    MissingFunction(FunctionId),
    #[error("Invalid coordinate access")]
    InvalidCoordinateAccses(IRType, CoordinateAccess),
}

pub fn eval(bytecode: &IRSegment, args: Vec<IRValue>) -> Result<IRValue, EvalError> {
    let mut vals = ValStorage::new();
    let mut iter = bytecode.instructions.iter().cloned().peekable();
    let mut id = 0;
    while let Some(op) = iter.next() {
        if let IROp::Ret(id) = op {
            return vals.get(id).cloned();
        } else if bytecode.ret.map(|i| i.idx()) == Some(id + 1) {
            return vals.get(bytecode.ret.unwrap()).cloned();
        }
        let val = execute_instruction(op, &bytecode, &mut iter, &mut vals, &args)?;
        vals.push(val);
        id += 1;
    }

    Err(EvalError::NoReturn)
}

fn execute_instruction(
    op: IROp,
    bytecode: &IRSegment,
    iter: &mut Peekable<impl Iterator<Item = IROp>>,
    vals: &mut ValStorage,
    args: &Vec<IRValue>,
) -> Result<IRValue, EvalError> {
    Ok(match op {
        IROp::Nop => IRValue::None,
        IROp::LoadArg(id) => args[id.idx as usize].clone(),
        IROp::Const(num) => IRValue::Number(num.into()),
        IROp::IConst(num) => IRValue::Number(num.into()),
        IROp::Vec2(x, y) => {
            let expected = IRType::Number;
            if let (IRValue::Number(x), IRValue::Number(y)) = (
                vals.get_typechecked(x, expected)?,
                vals.get_typechecked(y, expected)?,
            ) {
                let val = IRValue::Vec2(*x, *y);
                val
            } else {
                unreachable!("Should have been typechecked before, should not happen")
            }
        }
        IROp::Vec3(x, y, z) => {
            let expected = IRType::Number;
            if let (IRValue::Number(x), IRValue::Number(y), IRValue::Number(z)) = (
                vals.get_typechecked(x, expected)?,
                vals.get_typechecked(y, expected)?,
                vals.get_typechecked(z, expected)?,
            ) {
                let val = IRValue::Vec3(*x, *y, *z);
                val
            } else {
                unreachable!("Should have been typechecked before, should not happen")
            }
        }
        IROp::Binary(lhs, rhs, op) => match op {
            BinaryOp::Add => match (vals.get(lhs)?, vals.get(rhs)?) {
                (IRValue::Number(lhs), IRValue::Number(rhs)) => IRValue::Number(lhs + rhs),
                (IRValue::Vec2(x1, y1), IRValue::Vec2(x2, y2)) => IRValue::Vec2(x1 + x2, y1 + y2),
                (IRValue::Vec3(x1, y1, z1), IRValue::Vec3(x2, y2, z2)) => {
                    IRValue::Vec3(x1 + x2, y1 + y2, z1 + z2)
                }
                (v1, _) => {
                    return Err(EvalError::TypeError(TypeError {
                        expected: vec![IRType::Number, IRType::Vec2, IRType::Vec3],
                        found: v1.ir_type(),
                    }))
                }
                (_, v2) => {
                    return Err(EvalError::TypeError(TypeError {
                        expected: vec![IRType::Number, IRType::Vec2, IRType::Vec3],
                        found: v2.ir_type(),
                    }))
                }
            },
            BinaryOp::Sub => match (vals.get(lhs)?, vals.get(rhs)?) {
                (IRValue::Number(n1), IRValue::Number(n2)) => IRValue::Number(n1 - n2),
                (IRValue::Vec2(x1, y1), IRValue::Vec2(x2, y2)) => IRValue::Vec2(x1 - x2, y1 - y2),
                (IRValue::Vec3(x1, y1, z1), IRValue::Vec3(x2, y2, z2)) => {
                    IRValue::Vec3(x1 - x2, y1 - y2, z1 - z2)
                }
                (v1, _) => {
                    return Err(EvalError::TypeError(TypeError {
                        expected: vec![IRType::Number, IRType::Vec2, IRType::Vec3],
                        found: v1.ir_type(),
                    }))
                }
                (_, v2) => {
                    return Err(EvalError::TypeError(TypeError {
                        expected: vec![IRType::Number, IRType::Vec2, IRType::Vec3],
                        found: v2.ir_type(),
                    }))
                }
            },
            BinaryOp::Mul => match (vals.get(lhs)?, vals.get(rhs)?) {
                (IRValue::Number(lhs), IRValue::Number(rhs)) => IRValue::Number(lhs * rhs),
                (IRValue::Vec2(x1, y1), IRValue::Vec2(x2, y2)) => {
                    IRValue::Number(x1 * x2 + y1 * y2)
                }
                (IRValue::Vec3(x1, y1, z1), IRValue::Vec3(x2, y2, z2)) => {
                    IRValue::Number(x1 * x2 + y1 * y2 + z1 * z2)
                }
                (v1, _) => {
                    return Err(EvalError::TypeError(TypeError {
                        expected: vec![IRType::Number, IRType::Vec2, IRType::Vec3],
                        found: v1.ir_type(),
                    }))
                }
                (_, v2) => {
                    return Err(EvalError::TypeError(TypeError {
                        expected: vec![IRType::Number, IRType::Vec2, IRType::Vec3],
                        found: v2.ir_type(),
                    }))
                }
            },
            BinaryOp::Div => match (
                vals.get_typechecked(lhs, IRType::Number)?,
                vals.get_typechecked(rhs, IRType::Number)?,
            ) {
                (IRValue::Number(n1), IRValue::Number(n2)) => IRValue::Number(n1 / n2),
                _ => unreachable!("should have been typechecked before"),
            },
            BinaryOp::Pow => match (
                vals.get_typechecked(lhs, IRType::Number)?,
                vals.get_typechecked(rhs, IRType::Number)?,
            ) {
                (IRValue::Number(n1), IRValue::Number(n2)) => IRValue::Number(n1.pow(n2)),
                _ => unreachable!("Should have been typechecked before, should not happen"),
            },
            _ => todo!(),
        },
        IROp::Comparison { lhs, comp, rhs } => match (
            vals.get_typechecked(lhs, IRType::Number)?,
            vals.get_typechecked(rhs, IRType::Number)?,
        ) {
            (IRValue::Number(lhs), IRValue::Number(rhs)) => IRValue::Bool(match comp {
                Comparison::Eq => lhs == rhs,
                Comparison::GreaterEq => lhs >= rhs,
                Comparison::Greater => lhs > rhs,
                Comparison::LessEq => lhs <= rhs,
                Comparison::Less => lhs < rhs,
            }),
            _ => unreachable!("Should have been typechecked before, should not happen"),
        },
        IROp::Unary(id, op) => {
            if let IRValue::Number(val) = vals.get_typechecked(id, IRType::Number)? {
                IRValue::Number(match op {
                    UnaryOp::Neg => -*val,
                    UnaryOp::Sqrt => call_irrational!(val, sqrt).into(),

                    UnaryOp::Sin => call_irrational!(val, sin).into(),
                    UnaryOp::Cos => call_irrational!(val, cos).into(),
                    UnaryOp::Cot => (1.0 / call_irrational!(val, tan)).into(),
                    UnaryOp::Tan => call_irrational!(val, tan).into(),
                    UnaryOp::Sec => (1.0 / call_irrational!(val, sin)).into(),
                    UnaryOp::Csc => (1.0 / call_irrational!(val, cos)).into(),

                    UnaryOp::InvSin => call_irrational!(val, asin).into(),
                    UnaryOp::InvCos => call_irrational!(val, acos).into(),
                    UnaryOp::InvTan => call_irrational!(val, atan).into(),
                    UnaryOp::InvCot => (PI / 2.0 - call_irrational!(val, atan)).into(),
                    UnaryOp::InvCsc => {
                        let inv = &(&Number::Double(1.0) / val);
                        (call_irrational!(inv, asin)).into()
                    }
                    UnaryOp::InvSec => {
                        let inv = &(&Number::Double(1.0) / val);
                        (call_irrational!(inv, acos)).into()
                    }
                    UnaryOp::Ceil => val.ceil(),
                    UnaryOp::Floor => val.floor(),
                    UnaryOp::Gamma => todo!("Implement gamma function"),
                })
            } else {
                unreachable!("Should have been typechecked before, should not happen")
            }
        }
        IROp::FnCall(f_id) => {
            let mut args = Vec::new();
            while let Some(IROp::FnArg(id)) = iter.peek() {
                args.push(vals.get(*id)?.clone());
                vals.push(IRValue::None);
                iter.next();
            }
            if let Some(f) = bytecode.dependencies.get(&f_id) {
                eval(f, args)?
            } else {
                return Err(EvalError::MissingFunction(f_id));
            }
        }
        IROp::CoordinateOf(id, accses) => match vals.get(id)? {
            IRValue::Vec2(x, y) => match accses {
                CoordinateAccess::DotAccessX => IRValue::Number(*x),
                CoordinateAccess::DotAccessY => IRValue::Number(*y),
                CoordinateAccess::DotAccessZ => {
                    return Err(EvalError::InvalidCoordinateAccses(IRType::Vec2, accses))
                }
            },
            IRValue::Vec3(x, y, z) => match accses {
                CoordinateAccess::DotAccessX => IRValue::Number(*x),
                CoordinateAccess::DotAccessY => IRValue::Number(*y),
                CoordinateAccess::DotAccessZ => IRValue::Number(*z),
            },
            v => {
                return Err(EvalError::TypeError(TypeError {
                    expected: vec![IRType::Vec2, IRType::Vec3],
                    found: v.ir_type(),
                }))
            }
        },
        IROp::Ret(_) => unreachable!("Should have executed return before"),
        _ => todo!(),
    })
}

#[cfg(test)]
mod tests {

    mod eval_tests {
        use crate::lang::{
            ast::BinaryOp,
            compiler::{
                ir::{IRInstructionSeq, IROp, IRSegment, IRType, Id},
                value::IRValue,
            },
        };

        use super::super::eval;
        use crate::lang::compiler::ir::ArgId;
        use std::collections::HashMap;

        macro_rules! parameterized_tests{

            ($($name:ident: {ir: $ir:expr, args: $args:expr, expected: $expected:expr},)*) => {
             $(
                #[test]
                fn $name() {
                    let args :Vec<IRValue> = $args;
                    let segment = IRSegment{
                        args: args.iter().map(|arg| arg.ir_type()).collect(),
                        dependencies: HashMap::new(),
                        instructions: $ir.into(),
                        ret: None
                    };
                    let results = eval(&segment, args).unwrap();
                    assert_eq!(results, $expected);
                }
            )*
            }
        }

        parameterized_tests! {
            test_add: {ir: vec![IROp::Const(1.0),
                          IROp::Binary(Id::new(0, IRType::Number), Id::new(0, IRType::Number), BinaryOp::Add),
                          IROp::Ret(Id::new(1, IRType::Number))],
                          args: vec![],
                          expected: IRValue::Number(2.0.into())},

            test_args: {ir: vec![IROp::Const(1.0),
                          IROp::LoadArg(ArgId{
                              idx: 0,
                              t: IRType::Number
                          }),
                          IROp::Binary(Id::new(0, IRType::Number), Id::new(1, IRType::Number), BinaryOp::Add),
                          IROp::Ret(Id::new(2, IRType::Number))], args: vec![IRValue::Number(2.0.into())], expected: IRValue::Number(3.0.into())},
        }
    }
}
