pub mod alloc;
pub mod constraints;
pub mod liveness;
pub mod moves;
pub mod pos;
pub mod target;

use std::collections::HashMap;

use crate::context::{LivenessContext, RegAllocatedContext};
use crate::mcir::types::LocalId;
use crate::regalloc::target::PhysReg;
use stack::StackSlotId;

use self::alloc::RegAlloc;
use self::constraints::analyze_constraints;
use self::moves::FnMoveList;

#[derive(Debug, Clone)]
pub enum MappedLocal {
    Reg(PhysReg),
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
    pub used_callee_saved: Vec<PhysReg>,
    pub stack_slot_count: u32,
}

/// Run register allocation for a lowered MCIR context.
pub fn regalloc(ctx: LivenessContext, target: &dyn target::TargetSpec) -> RegAllocatedContext {
    let LivenessContext {
        func_bodies,
        live_maps,
        globals,
        ..
    } = ctx;

    let mut alloc_results = Vec::new();

    for (body, live_map) in func_bodies.iter().zip(live_maps.iter()) {
        let constraints = analyze_constraints(body, target);
        let alloc_result = RegAlloc::new(body, &constraints, target, live_map).alloc();
        alloc_results.push(alloc_result);
    }

    RegAllocatedContext {
        func_bodies,
        globals,
        alloc_results,
        symbols: ctx.symbols,
    }
}

impl AllocationResult {
    /// Format the allocation map for human-readable output.
    pub fn format_alloc_map(&self, func_name: &str, target: &dyn target::TargetSpec) -> String {
        let mut out = String::new();
        out.push_str(&format!("Reg Alloc Map ({}):\n", func_name));
        out.push_str("--------------------------------\n");
        let mut locals: Vec<_> = self.alloc_map.iter().collect();
        locals.sort_by_key(|(id, _)| id.0);
        for (id, mapped) in locals {
            match mapped {
                MappedLocal::Reg(reg) => {
                    out.push_str(&format!("%t{} -> {}\n", id.0, target.reg_name(*reg)));
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

pub mod stack;
#[cfg(test)]
#[path = "../tests/t_regalloc.rs"]
mod tests;
