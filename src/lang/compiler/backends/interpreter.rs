use core::fmt;
use std::{error::Error, ops::Index};

use thiserror::Error;

use crate::lang::{
    ast::BinaryOp,
    compiler::ir::{IROp, IRSegment, IRType, IRValue, Id},
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
}

pub fn eval(bytecode: &IRSegment, args: Vec<IRValue>) -> Result<IRValue, EvalError> {
    let mut vals = ValStorage::new();
    for op in bytecode.instructions.iter() {
        let val = match op {
            &IROp::Nop => IRValue::None,
            &IROp::LoadArg(id) => args[id.idx as usize].clone(),
            &IROp::Const(num) => IRValue::Number(num),
            &IROp::IConst(num) => IRValue::Number(num as f64),
            &IROp::Vec2(arg1, arg2) => {
                let expected = IRType::Number;
                if let (IRValue::Number(n1), IRValue::Number(n2)) = (
                    vals.get_typechecked(arg1, expected)?,
                    vals.get_typechecked(arg2, expected)?,
                ) {
                    let val = IRValue::Vec2(*n1, *n2);
                    val
                } else {
                    unreachable!("type check failed, this should not happen")
                }
            }
            &IROp::Vec3(arg1, arg2, arg3) => {
                let expected = IRType::Number;
                if let (IRValue::Number(n1), IRValue::Number(n2), IRValue::Number(n3)) = (
                    vals.get_typechecked(arg1, expected)?,
                    vals.get_typechecked(arg2, expected)?,
                    vals.get_typechecked(arg3, expected)?,
                ) {
                    let val = IRValue::Vec3(*n1, *n2, *n3);
                    val
                } else {
                    unreachable!("type check failed, this should not happen")
                }
            }
            &IROp::Ret(id) => return vals.get(id).cloned(),
            &IROp::Binary(arg1, arg2, op) => match op {
                BinaryOp::Add => match (vals.get(arg1)?, vals.get(arg2)?) {
                    (IRValue::Number(n1), IRValue::Number(n2)) => IRValue::Number(n1 + n2),
                    (IRValue::Vec2(x1, y1), IRValue::Vec2(x2, y2)) => {
                        IRValue::Vec2(x1 + x2, y1 + y2)
                    }
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
                _ => todo!(),
            },
            _ => todo!(),
        };
        vals.push(val)
    }

    Err(EvalError::NoReturn)
}

#[cfg(test)]
mod tests {

    mod eval_tests {
        use crate::lang::{
            ast::BinaryOp,
            compiler::ir::{IRInstructionSeq, IROp, IRSegment, IRType, IRValue, Id},
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
                          expected: IRValue::Number(2.0)},

            test_args: {ir: vec![IROp::Const(1.0),
                          IROp::LoadArg(ArgId{
                              idx: 0,
                              t: IRType::Number
                          }),
                          IROp::Binary(Id::new(0, IRType::Number), Id::new(1, IRType::Number), BinaryOp::Add),
                          IROp::Ret(Id::new(2, IRType::Number))], args: vec![IRValue::Number(1.0)], expected: IRValue::Number(2.0)},
        }
    }
}
