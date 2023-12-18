use anyhow::{Context, Result};
use std::collections::BTreeMap;

use crate::ast::{Comparison, CoordinateAccess, UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IRType {
    Number,
    Vec2,
    Vec3,
    Never,
    Bool,
    NumberList,
    PointList,
}

/// Identifies a numeric argument to the relevant IRChunk by index in the argument list
#[derive(Debug, Clone, Copy)]
pub struct Id {
    idx: u32,
    t: IRType,
}

impl Id {
    pub fn new(idx: u32, t: IRType) -> Self {
        Self { idx, t }
    }
    pub fn with_idx(&self, idx: u32) -> Self {
        Self { t: self.t, idx }
    }
}
impl PartialEq for Id {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}
impl Eq for Id {}
impl PartialOrd for Id {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.idx.cmp(&other.idx))
    }
}
impl Ord for Id {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        //Always Some
        self.idx.cmp(&other.idx)
    }
}

/// Identifies an argument to the current broadcast scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BroadcastArg {
    Point(u8),
    Number(u8),
}
// typed indentifier that identifies an item of type and index in args
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArgId(Id);

/// ### Desmoxide IR format
/// This is mostly equivalent to the TAC-based IR format used by desmos (see https://github.com/DesModder/DesModder/blob/main/parsing/IR.ts).
/// #### types
///  * `Number`: floating point value of unspecified precision (the Constant IR operation currently uses f64, but this precision is not guaranteed)
///  * `Vec2/3`: two and three-dimensional vectors
///  * `Never`: instructions that Never yield a value of any kind
///  * `Bool`: comparison instructions that yield boolean type
///  * `List`: opaque list identifer
/// and special broadcasting instructions are used to iterate over complex types component-wise
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IROp {
    /// a + b
    Add(Id, Id),
    /// a - b
    Sub(Id, Id),
    /// a / b
    Div(Id, Id),
    /// a * b
    Mul(Id, Id),
    /// a^b
    Pow(Id, Id),

    Unary(Id, UnaryOp),
    /// 64-bit floating point constant
    Const(f64),
    /// load args\[a] to this reg
    LoadArg(ArgId),
    /// load args\[a]\[i] (if args\[a] is a list of number)
    CoordinateOf(Id, CoordinateAccess),
    /// 2d vector. This is of `Opaque` type
    Vec2(Id, Id),
    /// 3d vector. This is of `Opaque` type
    Vec3(Id, Id, Id),
    /// Begins a broadcast loop that executes its body over indices 0->end_index inclusive, and stores its output in b
    BeginBroadcast {
        end_index: u32,
        write_to: Id,
    },
    /// Only allowed directly following SetBroadcast or BeginBroadcast instructions. Sets the broadcast argument slot at b to the item a
    SetBroadcastArg(Id, BroadcastArg),
    EndBroadcast {
        /// ID of the corresponding BeginBroadcast register
        begin: u32,
        /// ID of the value to push to the output list
        ret: Id,
    },
    Comparison {
        lhs: Id,
        comp: Comparison,
        rhs: Id,
    },
    /// Piecewise consist of a BeginPiecewise, any number of InnerPiecewise s and an EndPiecewise
    BeginPiecewise {
        comp: Id,
        res: Id,
    },
    InnerPiecewise {
        comp: Id,
        res: Id,
    },
    EndPiecewise {
        default: Id,
    },
    /// Return the value stored
    Ret(Id),
}
impl IROp {
    pub fn type_of(&self) -> IRType {
        // this match statement should always be exhaustive to prevent new instructions from being made without assigning them a type
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct IRInstructionSeq {
    backing: BTreeMap<Id, IROp>,
}
impl IRInstructionSeq {
    pub fn push(&mut self, op: IROp) {
        let _ = self.place(op);
    }
    pub fn place(&mut self, op: IROp) -> Id {
        let mut nid = 0;
        if let Some(v) = self.backing.last_key_value() {
            nid = v.0.idx + 1;
        };
        let id = Id::new(nid, op.type_of());
        self.backing.insert(id, op);
        id
    }
    pub fn get(&self, id: &Id) -> Result<&IROp> {
        self.backing.get(id).context("Could not get IR opcode")
    }
    pub fn latest(&self) -> Result<&IROp> {
        self.backing
            .last_key_value()
            .map(|a| a.1)
            .context("called latest on empty InstructionSeq")
    }
}
