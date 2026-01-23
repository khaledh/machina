//! SSA + explicit-memory IR data model.
//!
//! Defines the core SSA entities (functions, blocks, values, instructions) and
//! the type table used by the formatter and early SSA lowering.

use std::fmt;

use crate::resolve::DefId;
use crate::ssa::{IrTypeId, IrTypeKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

impl ValueId {
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

impl BlockId {
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

impl LocalId {
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueDef {
    pub id: ValueId,
    pub ty: IrTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockParam {
    pub value: ValueDef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub id: BlockId,
    pub params: Vec<BlockParam>,
    pub insts: Vec<Instruction>,
    pub term: Terminator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub def_id: DefId,
    pub name: String,
    pub sig: FunctionSig,
    pub locals: Vec<Local>,
    pub blocks: Vec<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSig {
    pub params: Vec<IrTypeId>,
    pub ret: IrTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Local {
    pub id: LocalId,
    pub ty: IrTypeId,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    Unit,
    Bool(bool),
    Int { value: i128, signed: bool, bits: u8 },
    GlobalAddr { id: GlobalId },
    FuncAddr { def: DefId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CastKind {
    IntTrunc,
    IntExtend { signed: bool },
    PtrToInt,
    IntToPtr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Callee {
    Direct(DefId),
    Value(ValueId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    pub result: Option<ValueDef>,
    pub kind: InstKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstKind {
    Const {
        value: ConstValue,
    },
    BinOp {
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
    },
    UnOp {
        op: UnOp,
        value: ValueId,
    },
    Cmp {
        op: CmpOp,
        lhs: ValueId,
        rhs: ValueId,
    },
    Cast {
        kind: CastKind,
        value: ValueId,
        ty: IrTypeId,
    },
    AddrOfLocal {
        local: LocalId,
    },
    FieldAddr {
        base: ValueId,
        index: usize,
    },
    IndexAddr {
        base: ValueId,
        index: ValueId,
    },
    Load {
        ptr: ValueId,
    },
    Store {
        ptr: ValueId,
        value: ValueId,
    },
    MemCopy {
        dst: ValueId,
        src: ValueId,
        len: ValueId,
    },
    MemSet {
        dst: ValueId,
        byte: ValueId,
        len: ValueId,
    },
    Call {
        callee: Callee,
        args: Vec<ValueId>,
    },
    Drop {
        ptr: ValueId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchCase {
    pub value: ConstValue,
    pub target: BlockId,
    pub args: Vec<ValueId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Br {
        target: BlockId,
        args: Vec<ValueId>,
    },
    CondBr {
        cond: ValueId,
        then_bb: BlockId,
        then_args: Vec<ValueId>,
        else_bb: BlockId,
        else_args: Vec<ValueId>,
    },
    Switch {
        value: ValueId,
        cases: Vec<SwitchCase>,
        default: BlockId,
        default_args: Vec<ValueId>,
    },
    Return {
        value: Option<ValueId>,
    },
    Unreachable,
}

impl fmt::Display for IrTypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrTypeKind::Unit => write!(f, "()"),
            IrTypeKind::Bool => write!(f, "bool"),
            IrTypeKind::Int { signed, bits } => {
                let prefix = if *signed { "i" } else { "u" };
                write!(f, "{}{}", prefix, bits)
            }
            IrTypeKind::Ptr { elem } => write!(f, "ptr<{:?}>", elem),
            IrTypeKind::Array { elem, dims } => write!(f, "array<{:?}; {:?}>", elem, dims),
            IrTypeKind::Tuple { fields } => write!(f, "tuple<{:?}>", fields),
            IrTypeKind::Struct { fields } => write!(f, "struct<{:?}>", fields),
            IrTypeKind::Blob { size, align } => write!(f, "blob<{}, align={}>", size, align),
            IrTypeKind::Fn { params, ret } => write!(f, "fn({:?}) -> {:?}", params, ret),
        }
    }
}
