use std::{collections::HashMap, num::NonZeroU32, sync::Arc};

use anyhow::{bail, Context, Result};
use debug_tree::TreeBuilder;

use shrinkwraprs::Shrinkwrap;
use strum::{AsRefStr, Display};

use super::{
    super::ast::{BinaryOp, Comparison, CoordinateAccess, UnaryOp},
    expression_provider::ExpressionId,
};
use crate::{graph::expressions::FnId, util::Discard};

/// Contains a standalone executable IR sequence along with metadata about its arguments and their types
#[derive(Debug, Clone)]
pub struct IRSegment {
    pub args: Vec<IRType>,
    pub dependencies: HashMap<FunctionId, Arc<Self>>,
    pub instructions: IRInstructionSeq,
    pub ret: Option<Id>,
}
impl IRSegment {
    pub fn new(args: Vec<IRType>) -> Self {
        Self {
            args,
            dependencies: HashMap::new(),
            instructions: IRInstructionSeq::new(),
            ret: None,
        }
    }
    pub fn push_dependency(&mut self, dep: Arc<Self>, expr: ExpressionId) -> FunctionId {
        // we have bigger problems if a function has > u32::MAX deps
        let id = FunctionId(Id::new(*expr, dep.ret.unwrap().t()));
        match self.dependencies.entry(id) {
            std::collections::hash_map::Entry::Occupied(o) => {
                return *o.key();
            }
            std::collections::hash_map::Entry::Vacant(v) => {
                v.insert(dep);
            }
        };
        id
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum IRType {
    Number,
    Vec2,
    Vec3,
    Never,
    Optional,
    Bool,
    NumberList,
    Vec2List,
    Vec3List,
}
impl IRType {
    // Is this type a valid input or output for an IRChunk
    pub fn is_value_type(&self) -> bool {
        matches!(
            self,
            Self::Number
                | Self::Vec2
                | Self::Vec3
                | Self::NumberList
                | Self::Vec2List
                | Self::Vec3List
        )
    }
    pub fn downcast_list(&self) -> Option<Self> {
        Some(match self {
            Self::NumberList => Self::Number,
            Self::Vec2 => Self::Vec2,
            Self::Vec3 => Self::Vec3,
            _ => return None,
        })
    }
    pub fn upcast_list(&self) -> Option<Self> {
        Some(match self {
            Self::Number => Self::NumberList,
            Self::Vec2 => Self::Vec2List,
            Self::Vec3 => Self::Vec3List,
            _ => return None,
        })
    }
}

/// Identifies a numeric argument to the relevant IRChunk by index in the argument list
#[derive(Debug, Clone, Copy, Hash)]
pub struct Id {
    inner: NonZeroU32,
}

impl Id {
    pub const INDEX_MASK: u32 = 0xFFFFFF;
    #[inline]
    pub fn new(mut idx: u32, t: IRType) -> Self {
        assert!(idx <= Self::INDEX_MASK - 1, "IR Opcode ID Overflowed");
        idx += 1;
        Self {
            //SAFETY: we have ensured idx is non-zero by ensuring it will not overflow and incrementing by one
            inner: unsafe { NonZeroU32::new_unchecked(idx | ((t as u8 as u32) << 24)) },
        }
    }
    #[inline(always)]
    pub fn idx(&self) -> u32 {
        (self.inner.get() & Self::INDEX_MASK) - 1
    }
    #[inline(always)]
    pub fn t(&self) -> IRType {
        //SAFETY: inner is private, and it's highest byte always contains a valid value of IRType
        unsafe { std::mem::transmute(self.inner.get().to_le_bytes()[3]) }
    }
}
impl PartialEq for Id {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}
impl Eq for Id {}

/// Identifies an argument to the current broadcast scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BroadcastArg {
    pub t: IRType,
    pub id: u8,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, AsRefStr)]
pub enum UnaryListOp {
    //Ret num
    Min,
    Max,
    Total,
    Len,

    //Ret list
    Unique,
    Sort,
    Shuffle,
}
impl UnaryListOp {
    pub fn ty(&self) -> IRType {
        match self {
            Self::Max | Self::Min | Self::Total | Self::Len => IRType::Number,
            Self::Unique | Self::Shuffle | Self::Sort => IRType::NumberList,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, AsRefStr)]
pub enum BinaryListOp {
    //Ret list
    Join,
    //idk there are probably more lol
    IndexRead,
    IndexWrite,
    Push,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RandomOp {
    Single,
    //optional seed
    Count {
        count: Id,
        seed: Option<Id>,
    },
    Permute {
        list: Id,
        count: Option<Id>,
        seed: Option<Id>,
    },
}
impl RandomOp {
    pub fn output_type(&self) -> IRType {
        match self {
            RandomOp::Single => IRType::Number,
            RandomOp::Count { .. } => IRType::NumberList,
            RandomOp::Permute { list, .. } => list.t().downcast_list().unwrap(),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EndIndex {
    Val(Id),
    Full,
}
#[derive(Debug, Clone, Copy, Shrinkwrap, PartialEq, Eq, Hash)]
pub struct FunctionId(pub(super) Id);
// typed indentifier that identifies an item of type and index in args
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArgId {
    pub idx: u8,
    pub t: IRType,
}
//TODO: investigate using this representation in egg/egglog
/// ### Desmoxide IR format
/// This is mostly equivalent to the TAC-based IR format used by desmos (see https://github.com/DesModder/DesModder/blob/main/parsing/IR.ts).
/// #### types
///  * `Number`: floating point value of unspecified precision (the Constant IR operation currently uses f64, but this precision is not guaranteed)
///  * `Vec2/3`: two and three-dimensional vectors
///  * `Never`: instructions that Never yield a value of any kind
///  * `Bool`: comparison instructions that yield boolean type
///  * `List`: opaque list identifer
///  * `Optional`: Either Some(value) or None. Not a value type, added specifically for internal implementations of various list operations
/// and special broadcasting instructions are used to iterate over complex types component-wise
#[derive(Copy, Clone, Debug, PartialEq, AsRefStr, Display)]
pub enum IROp {
    ///No-op, used to generate a valid Never value to point to without actually invoking a control flow instruction
    Nop,
    //Binary operation between two arguments
    Binary(Id, Id, BinaryOp),
    Unary(Id, UnaryOp),
    UnaryListOp(Id, UnaryListOp),
    BinaryListOp(Id, Id, BinaryListOp),
    /// 64-bit floating point constant
    Const(f64),
    /// 64-bit integer constant
    IConst(i64),
    /// load args\[a] to this reg
    LoadArg(ArgId),
    /// list literal. It is valid to refer to a subset of this list by referring to a ListLit that is not the beginning of a. All items in the list must be of the same type
    ListLit(Id),
    /// Range list generator literal
    RangeList {
        begin: Id,
        stride: Id,
        end: Id,
    },
    /// load args\[a]\[i] (if args\[a] is a list of number)
    CoordinateOf(Id, CoordinateAccess),
    /// 2d vector.
    Vec2(Id, Id),
    /// 3d vector.
    Vec3(Id, Id, Id),
    Random(RandomOp),
    /// Begins a broadcast loop that executes its body over indices 0->end_index inclusive, and stores its output in b
    BeginBroadcast {
        inner_type: IRType,
        end_index: EndIndex,
    },
    /// Only allowed directly following SetBroadcast or BeginBroadcast instructions. Sets the broadcast argument slot at b to the item a
    SetBroadcastArg(Id, BroadcastArg),
    LoadBroadcastArg(BroadcastArg),
    EndBroadcast {
        /// ID of the corresponding BeginBroadcast register
        begin: Id,
        /// ID of the value to push to the output list
        ret: Id,
    },
    Comparison {
        lhs: Id,
        comp: Comparison,
        rhs: Id,
    },
    /// Piecewise consist of a BeginPiecewise, any number of InnerPiecewise and an EndPiecewise
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
    /// Call a function
    FnCall(FunctionId),
    /// Define an argument to be passed into a function call
    FnArg(Id),
    /// Return the value stored in this register
    Ret(Id),
}
impl IROp {
    pub fn type_of(&self) -> IRType {
        // this match statement should always be exhaustive to prevent new instructions from being made without assigning them a type
        match self {
            //Number type
            IROp::Binary(..)
            | IROp::Unary(..)
            | IROp::Const(..)
            | IROp::IConst(..)
            | IROp::CoordinateOf(..) => IRType::Number,
            //Passthrough types
            IROp::LoadBroadcastArg(BroadcastArg { t, .. }) | IROp::LoadArg(ArgId { t, .. }) => *t,

            IROp::FnCall(FunctionId(id)) | IROp::BeginPiecewise { res: id, .. } | IROp::Ret(id) => {
                id.t()
            }
            //Opaque types
            IROp::Vec2(..) => IRType::Vec2,
            IROp::Vec3(..) => IRType::Vec3,
            IROp::ListLit(id) => id
                .t()
                .upcast_list()
                .expect("cannot make a list of non-number/vec types"),
            IROp::BeginBroadcast { inner_type: t, .. } => t
                .upcast_list()
                .expect("cannot make a list of non-number/vec types"),
            IROp::RangeList { .. } => IRType::NumberList,
            //Never types
            IROp::SetBroadcastArg(..)
            | IROp::EndBroadcast { .. }
            | IROp::FnArg(..)
            | IROp::InnerPiecewise { .. }
            | IROp::EndPiecewise { .. }
            | IROp::Nop => IRType::Never,
            //comparison
            IROp::Comparison { .. } => IRType::Bool,
            IROp::UnaryListOp(_l, op) => op.ty(),
            IROp::Random(op) => op.output_type(),
            IROp::BinaryListOp(lhs, _rhs, op) => {
                match op {
                    BinaryListOp::Join => lhs.t(),
                    //TODO: deal with this. This invariant always needs to hold, need to implement IR verifier/specify all possible invariants required to have valid IR
                    BinaryListOp::IndexRead => lhs.t().downcast_list().unwrap(),
                    BinaryListOp::IndexWrite => IRType::Never,
                    BinaryListOp::Push => IRType::Never,
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct IRInstructionSeq {
    backing: Vec<IROp>,
}
impl IRInstructionSeq {
    pub fn new() -> Self {
        Self {
            backing: Vec::new(),
        }
    }
    pub fn len(&self) -> usize {
        self.backing.len()
    }
    pub fn push(&mut self, op: IROp) {
        let _ = self.place(op);
    }
    pub fn place(&mut self, op: IROp) -> Id {
        let id = Id::new(self.backing.len() as u32, op.type_of());
        self.backing.push(op);
        id
    }
    pub fn coordinates_of2d(&mut self, point: Id) -> (Id, Id) {
        (
            self.place(IROp::CoordinateOf(point, CoordinateAccess::DotAccessX)),
            self.place(IROp::CoordinateOf(point, CoordinateAccess::DotAccessY)),
        )
    }
    pub fn coordinates_of3d(&mut self, point: Id) -> (Id, Id, Id) {
        (
            self.place(IROp::CoordinateOf(point, CoordinateAccess::DotAccessX)),
            self.place(IROp::CoordinateOf(point, CoordinateAccess::DotAccessY)),
            self.place(IROp::CoordinateOf(point, CoordinateAccess::DotAccessZ)),
        )
    }
    pub fn place_block(&mut self, ops: &[IROp]) -> Option<Id> {
        if ops.len() == 0 {
            None
        } else {
            let first = self.place(ops[0]);
            for i in &ops[1..] {
                self.push(*i);
            }
            Some(first)
        }
    }
    pub fn get(&self, id: &Id) -> Result<&IROp> {
        self.backing
            .get(id.idx() as usize)
            .context("Could not get IR opcode")
    }
    pub fn latest(&self) -> Result<&IROp> {
        self.backing
            .last()
            .context("called latest on empty InstructionSeq")
    }
    pub fn recursive_dbg(&self, builder: &mut TreeBuilder, node: Id) -> Result<()> {
        macro_rules! named_branch {
            ($bname:expr, $bctx:expr,$($child:expr),+) => {
                {let b = builder.add_branch(&format!("{}: {}", $bname, $bctx));
                $(
                self.recursive_dbg(builder, *$child)?;
                )*
                b}
            };
            ($bname:expr, $bctx:expr) => {
                builder.add_branch(&format!("{}: {}", $bname, $bctx))
            }
        }
        macro_rules! named_branch_list {
            ($bname:expr, $bctx:expr,$children:expr) => {{
                let b = builder.add_branch(&format!("{}: {}", $bname, $bctx));
                for child in $children {
                    self.recursive_dbg(builder, *child)?;
                }
                b
            }};
        }
        let n = self.get(&node)?;
        let _name = n.as_ref();
        let _branch = match n {
            IROp::Nop => named_branch!(n, ""),
            IROp::Binary(lhs, rhs, op) => named_branch!(n, op.as_ref(), lhs, rhs),
            IROp::Unary(val, op) => named_branch!(n, op.as_ref(), val),
            IROp::UnaryListOp(l, op) => named_branch!(n, op.as_ref(), l),
            IROp::BinaryListOp(a, b, op) => named_branch!(n, op.as_ref(), a, b),
            IROp::Const(val) => {
                builder.add_leaf(&format!("{:?}", val));
                return Ok(());
            }
            IROp::IConst(val) => {
                builder.add_leaf(&format!("{:?}", val));
                return Ok(());
            }
            IROp::LoadArg(arg) => {
                builder.add_leaf(&format!("LoadArg {:?}", arg.idx));
                return Ok(());
            }
            IROp::ListLit(_val) => {
                let mut it = self.backing[(node.idx() as usize)..].iter();
                let _b = builder.add_branch("List Literal");
                loop {
                    if let Some(IROp::ListLit(val)) = it.next() {
                        self.recursive_dbg(builder, *val)?;
                    } else {
                        break;
                    }
                }
                return Ok(());
            }
            IROp::RangeList {
                begin,
                stride: _,
                end: _,
            } => {
                let b = builder.add_branch("Range List");
                let mut b1 = builder.add_branch("from");
                self.recursive_dbg(builder, *begin)?;
                b1.release();
                let mut b2 = builder.add_branch("to");
                self.recursive_dbg(builder, *begin)?;
                b2.release();
                let mut b3 = builder.add_branch("with stride");
                b3.release();
                b
            }
            IROp::CoordinateOf(val, access) => named_branch!(n, access.as_ref(), val),
            IROp::Vec2(a, b) => named_branch!(n, "", a, b),
            IROp::Vec3(a, b, c) => named_branch!(n, "", a, b, c),
            IROp::BeginBroadcast {
                inner_type: _,
                end_index: _,
            } => {
                let outer = builder.add_branch("Broadcast");
                let mut args = builder.add_branch("with args:");
                let mut it = self.backing[node.idx() as usize..].iter();
                it.next().discard();
                loop {
                    if let Some(IROp::SetBroadcastArg(val, _id)) = it.next() {
                        self.recursive_dbg(builder, *val)?;
                    } else {
                        break;
                    }
                }
                args.release();
                let end = it
                    .find(|a| matches!(a, IROp::EndBroadcast { begin, .. } if *begin == node))
                    .unwrap();
                let IROp::EndBroadcast { begin: _, ret } = end else {
                    panic!("unreachable");
                };
                self.recursive_dbg(builder, *ret)?;
                outer
            }
            IROp::Random(r) => match r {
                RandomOp::Count { count, .. } => {
                    named_branch!("Random Count", "", count)
                }
                RandomOp::Permute { list, count, .. } => {
                    let b = builder.add_branch("Random Permute");
                    if let Some(c) = count {
                        self.recursive_dbg(builder, *c)?;
                    } else {
                        builder.add_branch("single output");
                    }
                    self.recursive_dbg(builder, *list)?;
                    b
                }
                RandomOp::Single => {
                    builder.add_leaf("Random number");
                    return Ok(());
                }
            },
            IROp::LoadBroadcastArg(arg) => {
                builder.add_leaf(&format!("{:?}", arg));
                return Ok(());
            }
            IROp::Comparison { lhs, comp, rhs } => named_branch!(n, comp.as_ref(), lhs, rhs),
            //This must address Inner/End Piecewises as well
            IROp::BeginPiecewise { comp, res } => {
                let mut it = self.backing[node.idx() as usize..].iter();
                it.next().discard();
                let b = builder.add_branch("Piecewise");
                let mut b0 = builder.add_branch("");
                let mut b1 = builder.add_branch("if");
                self.recursive_dbg(builder, *comp)?;
                b1.release();
                let mut b2 = builder.add_branch("then");
                self.recursive_dbg(builder, *res)?;
                b2.release();
                b0.release();
                let last = loop {
                    let next = it.next();
                    let Some(IROp::InnerPiecewise { comp, res }) = next else {
                        break next;
                    };
                    let mut b0 = builder.add_branch("");
                    let mut b1 = builder.add_branch("if");
                    self.recursive_dbg(builder, *comp)?;
                    b1.release();
                    let mut b2 = builder.add_branch("then");
                    self.recursive_dbg(builder, *res)?;
                    b2.release();
                    b0.release();
                };
                //TODO: EndPiecewise needs to track corresponding BeginPiecewise
                let Some(IROp::EndPiecewise { default }) = last else {
                    bail!("Piecewise without an EndPiecewise!!");
                };
                let mut b5 = builder.add_branch("else");
                self.recursive_dbg(builder, *default)?;
                b5.release();
                b
            }
            IROp::FnCall(f) => {
                let b = builder.add_branch(&format!("Function call (id {})", f.idx()));
                let mut it = self.backing[node.idx() as usize..].iter();
                it.next().discard();
                loop {
                    if let Some(IROp::FnArg(val)) = it.next() {
                        self.recursive_dbg(builder, *val)?;
                    } else {
                        break;
                    }
                }
                b
            }
            IROp::Ret(id) => named_branch!(n, "", id),
            _ => unreachable!(),
        };
        Ok(())
    }
    pub fn debug_print(&self, id: Id) -> Result<()> {
        let mut builder = TreeBuilder::new();
        self.recursive_dbg(&mut builder, id)?;
        builder.print();
        Ok(())
    }
}
