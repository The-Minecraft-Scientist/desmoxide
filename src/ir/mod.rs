use anyhow::{Context, Result};
use std::collections::BTreeMap;
use thin_vec::ThinVec;

use crate::ast::{Comparison, CoordinateAccess};

use self::types::FnType;

pub mod types;

/// Identifies a "register" (using LLVM terminology) by index in the IR of number type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RegId(u32);
impl Into<RegId> for u32 {
    fn into(self) -> RegId {
        RegId(self)
    }
}
/// Identifies a "register" (using LLVM terminology) by index in the IR of 2d point type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct OpaqueId(u32);
impl Into<OpaqueId> for u32 {
    fn into(self) -> OpaqueId {
        OpaqueId(self)
    }
}
/// Identifies an opaque list object (NOT a register)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ListId(u32);
impl Into<ListId> for u32 {
    fn into(self) -> ListId {
        ListId(self)
    }
}
/// Identifies a register of `None` type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NoneId(u32);
impl Into<NoneId> for u32 {
    fn into(self) -> NoneId {
        NoneId(self)
    }
}
/// Identifies a numeric argument to the relevant IRChunk by index in the argument list
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArgId(u32);
impl Into<ArgId> for u32 {
    fn into(self) -> ArgId {
        ArgId(self)
    }
}
/// Identifies a numeric argument to the relevant IRChunk by index in the argument list
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnyId(u32);
impl Into<AnyId> for u32 {
    fn into(self) -> AnyId {
        AnyId(self)
    }
}
impl Into<AnyId> for RegId {
    fn into(self) -> AnyId {
        AnyId(self.0)
    }
}
impl Into<AnyId> for OpaqueId {
    fn into(self) -> AnyId {
        AnyId(self.0)
    }
}

/// Identifies an argument to the current broadcast
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BroadcastArg(u8);
impl Into<BroadcastArg> for u8 {
    fn into(self) -> BroadcastArg {
        BroadcastArg(self)
    }
}
/// ### Desmoxide IR format
/// This is mostly equivalent to the TAC-based IR format used by desmos (see https://github.com/DesModder/DesModder/blob/main/parsing/IR.ts).
/// #### types
///  * `Number`: floating point value of unspecified precision (the Constant IR operation currently uses f64, but this precision is not guaranteed)
///  * `Opaque`: container type for a vector of numbrs. Members can be accessed via the [IROp::CoordinateOf] instruction
///  * `Any`: either a `Number` or an `Opaque`. Used very rarely, mostly for return statements
///  * `None`: does not yield a value. Used for control flow/context manipulation instructions
/// and special broadcasting instructions are used to iterate over complex types component-wise
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IROp {
    /// a + b
    Add(RegId, RegId),
    /// a - b
    Sub(RegId, RegId),
    /// a / b
    Div(RegId, RegId),
    /// a * b
    Mul(RegId, RegId),
    /// a^b
    Pow(RegId, RegId),
    /// -a
    Neg(RegId),
    /// 64-bit floating point constant
    Const(f64),
    /// load args\[a\] to this reg
    LoadArg(ArgId),
    /// load args\[a]\[i] (if args\[a] is a list of number)
    LoadListArg(ArgId, u32),
    CoordinateOf(OpaqueId, CoordinateAccess),
    /// 2d vector. This is of `Opaque` type
    Vec2(RegId, RegId),
    /// 3d vector. This is of `Opaque` type
    Vec3(RegId, RegId, RegId),
    /// Begins a broadcast loop that executes its body over indices 0->a inclusive, and stores its output in b
    BeginBroadcast {
        end_index: NoneId,
        write_to: ListId,
    },
    /// Only allowed directly following SetBroadcast* or BeginBroadcast instructions. Sets the broadcast argument slot at b to the list a
    SetBroadcastPointList(ListId, BroadcastArg),
    /// Only allowed directly following SetBroadcast* or BeginBroadcast instructions. Sets the broadcast argument slot at b to the number a
    SetBroadcastNum(RegId, BroadcastArg),
    EndBroadcast {
        /// ID of the corresponding BeginBroadcast register
        begin: u32,
        /// ID of the value to push to the output list
        ret: AnyId,
    },
    Piecewise {
        comp: Comparison,
        res: RegId,
    },
    EndPiecewise {
        default: RegId,
    },
    /// Return the value stored
    Ret(AnyId),

    Sin(RegId),
    Cos(RegId),
    Tan(RegId),
    Csc(RegId),
    Sec(RegId),
    Cot(RegId),
    InvSin(RegId),
    InvCos(RegId),
    InvTan(RegId),
    InvCsc(RegId),
    InvSec(RegId),
    InvCot(RegId),
}
#[derive(Debug, Clone)]
pub struct IRInstructionSeq {
    backing: BTreeMap<RegId, IROp>,
}
impl IRInstructionSeq {
    pub fn push(&mut self, op: IROp) {
        let next_id = RegId(self.backing.last_key_value().map(|a| a.0 .0).unwrap_or(0) + 1);
        self.backing.insert(next_id, op);
    }
    pub fn get(&self, id: &RegId) -> Result<&IROp> {
        self.backing.get(id).context("Could not get IR opcode")
    }
    pub fn latest(&self) -> Result<&IROp> {
        self.backing
            .last_key_value()
            .map(|a| a.1)
            .context("called latest on empty InstructionSeq")
    }
}
