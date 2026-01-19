//! Semantic call plans consumed by lowering to avoid re-deriving call intent.

use crate::resolve::DefId;

#[derive(Clone, Debug)]
pub enum IntrinsicCall {
    StringAppendBytes,
}

#[derive(Clone, Debug)]
pub enum CallTarget {
    Direct(DefId),
    Indirect,
    Intrinsic(IntrinsicCall),
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
/// The input order is receiver (if any) followed by args; drop_mask aligns to that order.
#[derive(Clone, Debug)]
pub struct CallPlan {
    pub target: CallTarget,
    pub args: Vec<ArgLowering>,
    pub drop_mask: Vec<bool>,
    pub has_receiver: bool,
}
