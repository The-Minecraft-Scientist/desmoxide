use std::ops::Index;

use thiserror::Error;

use crate::lang::compiler::ir::{IROp, IRSegment, IRType, IRValue, Id};

struct ValStorage {
    vals: Vec<Option<IRValue>>,
}

impl ValStorage {
    pub fn new() -> Self {
        Self {
            vals: Default::default(),
        }
    }

    pub fn push(&mut self, val: Option<IRValue>) {
        self.vals.push(val)
    }

    pub fn get(&self, id: Id) -> Result<&IRValue, EvalError> {
        self.vals
            .get(id.idx() as usize)
            .ok_or_else(|| EvalError::InstructionNotExecuted(id.idx()))?
            .as_ref()
            .ok_or_else(|| EvalError::MissingVal(id.idx()))
    }

    pub fn get_typechecked(&self, id: Id, expected: IRType) -> Result<&IRValue, EvalError> {
        let value = self.get(id)?;

        typecheck(value, expected)
    }
}

pub fn typecheck(value: &IRValue, expected: IRType) -> Result<&IRValue, EvalError> {
    match (value.ir_type(), value) {
        (expected, v) => Ok(v),
        (t, _) => Err(EvalError::TypeError(expected, t)),
    }
}

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Instruction {0} wasnt yet executed")]
    InstructionNotExecuted(u32),
    #[error("Missing value at instruction: {0}")]
    MissingVal(u32),
    #[error("TypeError, expected {0:?}, found {1:?}")]
    TypeError(IRType, IRType),
}

pub fn eval(bytecode: &IRSegment, args: Vec<IRValue>) -> Result<IRValue, EvalError> {
    let mut vals = ValStorage::new();
    for op in bytecode.instructions.iter() {
        let val = match op {
            &IROp::Nop => None,
            &IROp::LoadArg(id) => Some(args[id.idx as usize].clone()),
            &IROp::Const(num) => Some(IRValue::Number(num)),
            &IROp::IConst(num) => Some(IRValue::Number(num as f64)),
            &IROp::Vec2(arg1, arg2) => {
                let expected = IRType::Number;
                if let (IRValue::Number(n1), IRValue::Number(n2)) = (
                    vals.get_typechecked(arg1, expected)?,
                    vals.get_typechecked(arg2, expected)?,
                ) {
                    let val = IRValue::Vec2(*n1, *n2);
                    Some(val)
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
                    Some(val)
                } else {
                    unreachable!("type check failed, this should not happen")
                }
            }
            &IROp::Ret(id) => return vals.get(id).cloned(),
            _ => todo!(),
        };
        vals.push(val)
    }

    todo!()
}
