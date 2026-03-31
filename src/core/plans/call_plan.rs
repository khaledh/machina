//! Semantic call plans consumed by lowering to avoid re-deriving call intent.

use std::collections::HashMap;

use crate::core::ast::NodeId;
use crate::core::ast::ParamMode;
use crate::core::resolve::DefId;

#[derive(Clone, Debug)]
pub enum IntrinsicCall {
    StringLen,
    StringLines,
    StringSplit,
    TypeOf,
    ViewAt,
    ViewSliceAt,
    ViewArrayAt,
    PtrAt,
    PtrRead,
    PtrWrite,
    AddressOffset,
    AddressAlignDown,
    AddressAlignUp,
    AddressIsAligned,
    AddressIsSome,
    AddressIsNone,
    AddressUnwrap,
    MachinePayloadPack,
    DynArrayAppend,
    SetInsert,
    SetContains,
    SetRemove,
    SetClear,
    MapInsert,
    MapContainsKey,
    MapGet,
    MapRemove,
    MapClear,
}

#[derive(Clone, Debug)]
pub enum RuntimeCall {
    Print,
    U64ToDec,
    MemSet,
    StringFromBytes,
    StringAppendBytes,
    MapIterInit,
    MapIterIsDone,
    MapIterLoadBytes,
    MapIterLoadStringKey,
    MapIterAdvance,
}

#[derive(Clone, Debug)]
pub enum CallTarget {
    Direct(DefId),
    Indirect,
    Intrinsic(IntrinsicCall),
    Runtime(RuntimeCall),
}

#[derive(Clone, Debug)]
pub enum CallInput {
    Receiver,
    Arg(usize),
}

#[derive(Clone, Debug)]
pub enum ArgLowering {
    Direct(CallInput),
    PtrLen { input: CallInput, len_bits: u8 },
}

/// Pre-computed lowering plan for a call expression.
/// The input order is receiver (if any) followed by args; drop_mask and input_modes align to that order.
#[derive(Clone, Debug)]
pub struct CallPlan {
    pub target: CallTarget,
    pub args: Vec<ArgLowering>,
    pub drop_mask: Vec<bool>,
    pub input_modes: Vec<ParamMode>,
    pub has_receiver: bool,
}

pub type CallPlanMap = HashMap<NodeId, CallPlan>;
