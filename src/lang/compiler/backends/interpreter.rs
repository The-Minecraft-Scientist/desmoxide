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
}

impl Index<Id> for ValStorage {
    type Output = Option<IRValue>;

    fn index(&self, id: Id) -> &Self::Output {
        &self.vals[id.idx() as usize]
    }
}

#[derive(Debug, Error)]
pub enum EvalError {
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
            &IROp::Vec2(arg1, arg2) => match (&vals[arg1], &vals[arg2]) {
                (&Some(IRValue::Number(n1)), &Some(IRValue::Number(n2))) => {
                    let val = IRValue::Vec2(n1, n2);
                    Some(val)
                }
                (Some(n1), _) => return Err(EvalError::TypeError(IRType::Number, n1.ir_type())),

                (_, Some(n2)) => return Err(EvalError::TypeError(IRType::Number, n2.ir_type())),
                (None, _) => {
                    return Err(EvalError::MissingVal(arg1.idx()));
                }
                (_, None) => {
                    return Err(EvalError::MissingVal(arg2.idx()));
                }
            },
            &IROp::Vec3(arg1, arg2, arg3) => match (&vals[arg1], &vals[arg2], &vals[arg3]) {
                (
                    &Some(IRValue::Number(n1)),
                    &Some(IRValue::Number(n2)),
                    &Some(IRValue::Number(n3)),
                ) => {
                    let val = IRValue::Vec2(n1, n2);
                    Some(val)
                }
                (Some(n1), _, _) => return Err(EvalError::TypeError(IRType::Number, n1.ir_type())),

                (_, Some(n2), _) => return Err(EvalError::TypeError(IRType::Number, n2.ir_type())),

                (_, _, Some(n3)) => return Err(EvalError::TypeError(IRType::Number, n3.ir_type())),
                (None, _, _) => {
                    return Err(EvalError::MissingVal(arg1.idx()));
                }
                (_, None, _) => {
                    return Err(EvalError::MissingVal(arg2.idx()));
                }
                (_, _, None) => {
                    return Err(EvalError::MissingVal(arg3.idx()));
                }
            },

            _ => todo!(),
        };
        vals.push(val)
    }

    todo!()
}
