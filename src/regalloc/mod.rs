pub mod alloc;
pub mod constraints;
pub mod liveness;
pub mod moves;
pub mod pos;

use std::collections::HashMap;

use crate::context::{LoweredMcirContext, RegAllocatedContext};
use crate::mcir::types::LocalId;
use regs::Arm64Reg;
use stack::StackSlotId;

use self::alloc::RegAlloc;
use self::constraints::analyze_constraints;
use self::moves::FnMoveList;

#[derive(Debug, Clone)]
pub enum MappedLocal {
    Reg(Arm64Reg),
    Stack(StackSlotId),
    StackAddr(StackSlotId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalClass {
    Reg,
    StackAddr,
}

pub type LocalAllocMap = HashMap<LocalId, MappedLocal>;

#[derive(Debug)]
pub struct AllocationResult {
    pub alloc_map: LocalAllocMap,
    pub moves: FnMoveList,
    pub frame_size: u32,
    pub used_callee_saved: Vec<Arm64Reg>,
    pub stack_slot_count: u32,
}

/// Run register allocation for a lowered MCIR context.
pub fn regalloc(ctx: LoweredMcirContext) -> RegAllocatedContext {
    let mut alloc_results = Vec::new();
    for body in &ctx.func_bodies {
        let constraints = analyze_constraints(body);
        let alloc_result = RegAlloc::new(body, &constraints).alloc();
        alloc_results.push(alloc_result);
    }
    ctx.with_alloc_results(alloc_results)
}

impl AllocationResult {
    /// Format the allocation map for human-readable output.
    pub fn format_alloc_map(&self, func_name: &str) -> String {
        let mut out = String::new();
        out.push_str(&format!("Reg Alloc Map ({}):\n", func_name));
        out.push_str("--------------------------------\n");
        let mut locals: Vec<_> = self.alloc_map.iter().collect();
        locals.sort_by_key(|(id, _)| id.0);
        for (id, mapped) in locals {
            match mapped {
                MappedLocal::Reg(reg) => {
                    out.push_str(&format!("%t{} -> {}\n", id.0, reg));
                }
                MappedLocal::Stack(slot) => {
                    out.push_str(&format!("%t{} -> stack[{}]\n", id.0, slot.0));
                }
                MappedLocal::StackAddr(slot) => {
                    out.push_str(&format!("%t{} -> stack_addr[{}]\n", id.0, slot.0));
                }
            }
        }
        out.push_str("--------------------------------\n");
        out
    }
}

pub mod regs;
pub mod stack;
#[cfg(test)]
#[path = "../tests/t_regalloc.rs"]
mod tests;
