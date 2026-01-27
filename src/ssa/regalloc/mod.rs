//! SSA register allocation scaffolding.
//!
//! This module mirrors the legacy regalloc interfaces but is specialized for
//! SSA `ValueId` allocations. We keep it minimal for now and will expand it
//! as interval construction and allocation land.

use std::collections::HashMap;

use crate::ssa::model::ir::ValueId;

pub use crate::regalloc::stack::StackSlotId;
pub use crate::regalloc::target::{PhysReg, TargetSpec};

pub mod intervals;

/// Location assigned to an SSA value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Reg(PhysReg),
    Stack(StackSlotId),
}

/// Allocation map for a single SSA function.
pub type ValueAllocMap = HashMap<ValueId, Location>;

/// Result of SSA register allocation for one function.
#[derive(Debug, Clone)]
pub struct AllocationResult {
    pub alloc_map: ValueAllocMap,
    pub frame_size: u32,
    pub stack_slot_count: u32,
}
