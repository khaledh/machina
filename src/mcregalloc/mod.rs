pub mod alloc;
pub mod constraints;
pub mod liveness;
pub mod moves;
pub mod pos;

use std::collections::HashMap;

use crate::mcir::types::LocalId;
use crate::regalloc::regs::Arm64Reg;
use crate::regalloc::stack::StackSlotId;

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

#[cfg(test)]
#[path = "tests/t_regalloc.rs"]
mod tests;
