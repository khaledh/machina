//! SSA register allocation scaffolding.
//!
//! This module mirrors the legacy regalloc interfaces but is specialized for
//! SSA `ValueId` allocations. We keep it minimal for now and will expand it
//! as interval construction and allocation land.

use std::collections::HashMap;

use crate::ssa::model::ir::ValueId;

pub use crate::regalloc::stack::StackSlotId;
pub use crate::regalloc::target::{PhysReg, TargetSpec};

pub mod alloc;
pub mod intervals;
pub mod moves;

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
    pub edge_moves: Vec<moves::EdgeMove>,
    pub call_moves: Vec<moves::CallMove>,
}

/// Run SSA register allocation for a single function.
pub fn regalloc(
    func: &crate::ssa::model::ir::Function,
    types: &mut crate::ssa::IrTypeCache,
    live_map: &crate::ssa::analysis::liveness::LiveMap,
    target: &dyn TargetSpec,
) -> AllocationResult {
    let analysis = intervals::analyze(func, live_map);
    let mut result = alloc::LinearScan::new(&analysis, types, target).alloc();
    let moves = moves::build_move_plan(func, &result.alloc_map, target);
    let mut moves = moves;
    moves.resolve_parallel_moves(target.scratch_regs());
    result.edge_moves = moves.edge_moves;
    result.call_moves = moves.call_moves;
    result
}

#[cfg(test)]
#[path = "../../tests/ssa/regalloc/t_alloc.rs"]
mod tests;

#[cfg(test)]
#[path = "../../tests/ssa/regalloc/t_moves.rs"]
mod tests_moves;
