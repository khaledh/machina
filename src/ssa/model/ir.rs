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
pub struct GlobalData {
    pub id: GlobalId,
    pub bytes: Vec<u8>,
    pub align: u32,
}

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
    PtrToInt,
    IntToPtr,
    PtrToPtr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeFn {
    Trap,
    Print,
    StringFromBytes,
    FmtInit,
    FmtAppendBytes,
    FmtAppendU64,
    FmtAppendI64,
    FmtFinish,
    U64ToDec,
    I64ToDec,
    U64ToBin,
    U64ToOct,
    U64ToHex,
    MemSet,
    MemCopy,
    StringEnsure,
    StringDrop,
    StringAppendBytes,
    DynArrayEnsure,
    DynArrayAppendElem,
    Alloc,
    Realloc,
    Free,
    SetAllocTrace,
}

impl RuntimeFn {
    pub fn name(&self) -> &'static str {
        match self {
            RuntimeFn::Trap => "__rt_trap",
            RuntimeFn::Print => "__rt_print",
            RuntimeFn::StringFromBytes => "__rt_string_from_bytes",
            RuntimeFn::FmtInit => "__rt_fmt_init",
            RuntimeFn::FmtAppendBytes => "__rt_fmt_append_bytes",
            RuntimeFn::FmtAppendU64 => "__rt_fmt_append_u64",
            RuntimeFn::FmtAppendI64 => "__rt_fmt_append_i64",
            RuntimeFn::FmtFinish => "__rt_fmt_finish",
            RuntimeFn::U64ToDec => "__rt_u64_to_dec",
            RuntimeFn::I64ToDec => "__rt_i64_to_dec",
            RuntimeFn::U64ToBin => "__rt_u64_to_bin",
            RuntimeFn::U64ToOct => "__rt_u64_to_oct",
            RuntimeFn::U64ToHex => "__rt_u64_to_hex",
            RuntimeFn::MemSet => "__rt_memset",
            RuntimeFn::MemCopy => "__rt_memcpy",
            RuntimeFn::StringEnsure => "__rt_string_ensure",
            RuntimeFn::StringDrop => "__rt_string_drop",
            RuntimeFn::StringAppendBytes => "__rt_string_append_bytes",
            RuntimeFn::DynArrayEnsure => "__rt_dyn_array_ensure",
            RuntimeFn::DynArrayAppendElem => "__rt_dyn_array_append_elem",
            RuntimeFn::Alloc => "__rt_alloc",
            RuntimeFn::Realloc => "__rt_realloc",
            RuntimeFn::Free => "__rt_free",
            RuntimeFn::SetAllocTrace => "__rt_set_alloc_trace",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Callee {
    Direct(DefId),
    Value(ValueId),
    Runtime(RuntimeFn),
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
    IntTrunc {
        value: ValueId,
        ty: IrTypeId,
    },
    IntExtend {
        value: ValueId,
        ty: IrTypeId,
        signed: bool,
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

/// Visits each SSA value used by an instruction.
pub fn for_each_inst_use(kind: &InstKind, mut f: impl FnMut(ValueId)) {
    match kind {
        InstKind::Const { .. } | InstKind::AddrOfLocal { .. } => {}
        InstKind::BinOp { lhs, rhs, .. } | InstKind::Cmp { lhs, rhs, .. } => {
            f(*lhs);
            f(*rhs);
        }
        InstKind::UnOp { value, .. }
        | InstKind::IntTrunc { value, .. }
        | InstKind::IntExtend { value, .. }
        | InstKind::Cast { value, .. }
        | InstKind::FieldAddr { base: value, .. }
        | InstKind::Load { ptr: value } => f(*value),
        InstKind::IndexAddr { base, index } => {
            f(*base);
            f(*index);
        }
        InstKind::Store { ptr, value } => {
            f(*ptr);
            f(*value);
        }
        InstKind::MemCopy { dst, src, len } => {
            f(*dst);
            f(*src);
            f(*len);
        }
        InstKind::MemSet { dst, byte, len } => {
            f(*dst);
            f(*byte);
            f(*len);
        }
        InstKind::Call { callee, args } => {
            if let Callee::Value(value) = callee {
                f(*value);
            }
            for arg in args {
                f(*arg);
            }
        }
        InstKind::Drop { ptr } => f(*ptr),
    }
}

/// Replaces all uses of `from` with `to` in an instruction.
pub fn replace_value_in_inst(kind: &mut InstKind, from: ValueId, to: ValueId) {
    let replace = |value: &mut ValueId| {
        if *value == from {
            *value = to;
        }
    };

    match kind {
        InstKind::BinOp { lhs, rhs, .. } | InstKind::Cmp { lhs, rhs, .. } => {
            replace(lhs);
            replace(rhs);
        }
        InstKind::UnOp { value, .. }
        | InstKind::IntTrunc { value, .. }
        | InstKind::IntExtend { value, .. }
        | InstKind::Cast { value, .. }
        | InstKind::FieldAddr { base: value, .. }
        | InstKind::Load { ptr: value } => replace(value),
        InstKind::IndexAddr { base, index } => {
            replace(base);
            replace(index);
        }
        InstKind::Store { ptr, value } => {
            replace(ptr);
            replace(value);
        }
        InstKind::MemCopy { dst, src, len } => {
            replace(dst);
            replace(src);
            replace(len);
        }
        InstKind::MemSet { dst, byte, len } => {
            replace(dst);
            replace(byte);
            replace(len);
        }
        InstKind::Call { callee, args } => {
            if let Callee::Value(value) = callee {
                replace(value);
            }
            for arg in args {
                replace(arg);
            }
        }
        InstKind::Drop { ptr } => replace(ptr),
        InstKind::Const { .. } | InstKind::AddrOfLocal { .. } => {}
    }
}

/// Replaces all uses of `from` with `to` in a terminator.
pub fn replace_value_in_term(term: &mut Terminator, from: ValueId, to: ValueId) {
    let replace = |value: &mut ValueId| {
        if *value == from {
            *value = to;
        }
    };

    match term {
        Terminator::Br { args, .. } => {
            for value in args {
                replace(value);
            }
        }
        Terminator::CondBr {
            cond,
            then_args,
            else_args,
            ..
        } => {
            replace(cond);
            for value in then_args {
                replace(value);
            }
            for value in else_args {
                replace(value);
            }
        }
        Terminator::Switch {
            value,
            cases,
            default_args,
            ..
        } => {
            replace(value);
            for case in cases {
                for arg in &mut case.args {
                    replace(arg);
                }
            }
            for value in default_args {
                replace(value);
            }
        }
        Terminator::Return { value } => {
            if let Some(value) = value {
                replace(value);
            }
        }
        Terminator::Unreachable => {}
    }
}

/// Replaces `from` with `to` throughout a function, optionally skipping one instruction.
pub fn replace_value_in_func(
    func: &mut Function,
    from: ValueId,
    to: ValueId,
    ignore: Option<(usize, usize)>,
) {
    for (block_idx, block) in func.blocks.iter_mut().enumerate() {
        for (inst_idx, inst) in block.insts.iter_mut().enumerate() {
            if Some((block_idx, inst_idx)) == ignore {
                continue;
            }
            replace_value_in_inst(&mut inst.kind, from, to);
        }
        replace_value_in_term(&mut block.term, from, to);
    }
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
