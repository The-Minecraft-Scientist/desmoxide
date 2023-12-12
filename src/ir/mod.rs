use std::ops::Deref;

use self::types::FnType;

pub mod types;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct RegId(u32);
impl Into<RegId> for u32 {
    fn into(self) -> RegId {
        RegId(self)
    }
}
impl Deref for RegId {
    type Target = ValueId;
    fn deref(&self) -> &Self::Target {
        // casting between two repr transparent u32s is safe, does not violate invariants of either
        unsafe { std::mem::transmute(self) }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ValueId {
    inner: u32,
}
impl ValueId {
    pub const fn new_reg(local: u32) -> Self {
        Self { inner: local }
    }
    pub const fn new_arg(arg: u32) -> Self {
        Self {
            inner: arg & 0x80000000,
        }
    }
    pub const fn as_arg(&self) -> Option<u32> {
        if self.inner >> 31 == 1 {
            Some(self.inner & 0x7FFFFFFF)
        } else {
            None
        }
    }
    pub const fn as_reg(&self) -> Option<RegId> {
        if !self.inner >> 31 == 1 {
            Some(RegId(self.inner))
        } else {
            None
        }
    }
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IROp {
    Add(ValueId, ValueId),
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IRInstruction {
    id: RegId,
    op: IROp,
}
impl PartialOrd for IRInstruction {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

pub struct IRChunk {
    typ: FnType,
}

pub struct Funny1 {}

impl Deref for Funny1 {
    type Target = Funny2;
    fn deref(&self) -> &Self::Target {
        &Funny2 {}
    }
}
pub struct Funny2 {}
impl Deref for Funny2 {
    type Target = Funny1;
    fn deref(&self) -> &Self::Target {
        &Funny2 {}
    }
}
