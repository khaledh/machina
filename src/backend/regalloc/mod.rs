//! SSA register allocation scaffolding.
//!
//! This module mirrors the legacy regalloc interfaces but is specialized for
//! SSA `ValueId` allocations. We keep it minimal for now and will expand it
//! as interval construction and allocation land.

use std::collections::HashMap;

use crate::ir::ir::ValueId;

pub mod arm64;
pub mod stack;
pub mod target;

pub use stack::StackSlotId;
pub use target::{PhysReg, TargetSpec};

pub mod alloc;
pub mod constraints;
pub mod intervals;
pub mod moves;

/// Location assigned to an SSA value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Reg(PhysReg),
    Stack(StackSlotId),
    /// Address of a stack slot (used for sret setup moves).
    StackAddr(StackSlotId),
    /// Byte offset into a stack slot (used for partial aggregate moves).
    StackOffset(StackSlotId, u32),
    /// Incoming stack argument at the given byte offset from SP (post-prologue).
    IncomingArg(u32),
    /// Outgoing stack argument at the given byte offset from SP (post-prologue).
    OutgoingArg(u32),
}

/// Allocation map for a single SSA function.
pub type ValueAllocMap = HashMap<ValueId, Location>;

/// Result of SSA register allocation for one function.
#[derive(Debug, Clone)]
pub struct AllocationResult {
    pub alloc_map: ValueAllocMap,
    pub frame_size: u32,
    pub stack_slot_count: u32,
    /// Callee-saved registers that must be preserved by the prologue/epilogue.
    pub used_callee_saved: Vec<PhysReg>,
    /// Entry moves for register-valued params.
    pub entry_moves: Vec<moves::MoveOp>,
    /// Aggregate parameter copies emitted at function entry.
    pub param_copies: Vec<moves::ParamCopy>,
    pub edge_moves: Vec<moves::EdgeMove>,
    pub call_moves: Vec<moves::CallMove>,
}

/// Run SSA register allocation for a single function.
pub fn regalloc(
    func: &crate::ir::ir::Function,
    types: &mut crate::backend::IrTypeCache,
    live_map: &crate::backend::analysis::liveness::LiveMap,
    target: &dyn TargetSpec,
) -> AllocationResult {
    // Build live intervals and allocate registers/stack slots.
    let analysis = intervals::analyze(func, live_map);
    let constraints = constraints::build(&analysis, types, target);
    let mut result = alloc::LinearScan::new(&analysis, types, target, &constraints).alloc();

    let param_reg_count = constraints.param_reg_count;

    // Reserve space for stack-passed call arguments.
    let mut max_stack_bytes = 0u32;
    for block in &func.blocks {
        for inst in &block.insts {
            if let crate::ir::ir::InstKind::Call { args, .. } = &inst.kind {
                let stack_bytes = moves::outgoing_stack_bytes_for_call(
                    args,
                    &analysis.value_types,
                    types,
                    target,
                    param_reg_count,
                );
                max_stack_bytes = max_stack_bytes.max(stack_bytes);
            }
        }
    }
    let outgoing_arg_size = max_stack_bytes;
    result.frame_size = result.frame_size.saturating_add(outgoing_arg_size);

    // Map stack-passed incoming params to their SP-relative slots.
    for (value, offset) in constraints.incoming_args {
        result
            .alloc_map
            .insert(value, Location::IncomingArg(offset));
    }

    // Compute move plans for edges and calls.
    let moves = moves::build_move_plan(func, &result.alloc_map, types, target, param_reg_count);
    let mut moves = moves;
    moves.resolve_parallel_moves(target.scratch_regs());
    result.entry_moves = moves.entry_moves;
    result.param_copies = moves.param_copies;
    result.edge_moves = moves.edge_moves;
    result.call_moves = moves.call_moves;

    // Record callee-saved registers that need prologue/epilogue saves.
    let mut used = Vec::new();
    for loc in result.alloc_map.values() {
        if let Location::Reg(reg) = loc {
            if target.callee_saved().iter().any(|saved| *saved == *reg) {
                if !used.iter().any(|r| r == reg) {
                    used.push(*reg);
                }
            }
        }
    }
    used.sort_by_key(|reg| reg.0);
    result.used_callee_saved = used;
    result
}

#[cfg(test)]
#[path = "../../tests/backend/regalloc/t_alloc.rs"]
mod tests;

#[cfg(test)]
#[path = "../../tests/backend/regalloc/t_moves.rs"]
mod tests_moves;

#[cfg(test)]
#[path = "../../tests/backend/regalloc/t_constraints.rs"]
mod tests_constraints;
