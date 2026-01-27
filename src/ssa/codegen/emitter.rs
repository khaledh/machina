//! Target-agnostic SSA codegen emitter traits.

use crate::ssa::model::ir::{BinOp, Instruction, Terminator};
use crate::ssa::regalloc::Location;
use crate::ssa::regalloc::moves::MoveOp;

/// Minimal instruction selection interface for SSA codegen.
pub trait CodegenEmitter {
    /// Begin emitting a function symbol and prologue.
    fn begin_function(&mut self, _name: &str, _frame_size: u32) {}

    /// Begin emitting a new block label.
    fn begin_block(&mut self, label: &str);

    /// Emit a move sequence.
    fn emit_moves(&mut self, moves: &[MoveOp]);

    /// Emit a lowered instruction.
    fn emit_inst(&mut self, inst: &Instruction, locs: &LocationResolver);

    /// Emit a terminator.
    fn emit_terminator(&mut self, term: &Terminator, locs: &LocationResolver);

    /// Finish emitting a function.
    fn end_function(&mut self) {}
}

/// Resolves SSA values to allocated locations.
pub struct LocationResolver<'a> {
    pub map: &'a crate::ssa::regalloc::ValueAllocMap,
}

impl<'a> LocationResolver<'a> {
    pub fn value(&self, id: crate::ssa::model::ir::ValueId) -> Location {
        *self
            .map
            .get(&id)
            .unwrap_or_else(|| panic!("ssa codegen: missing alloc for {:?}", id))
    }
}

/// Minimal binop lowering helper for emitters.
pub fn binop_mnemonic(op: BinOp) -> &'static str {
    match op {
        BinOp::Add => "add",
        BinOp::Sub => "sub",
        BinOp::Mul => "mul",
        BinOp::Div => "sdiv",
        BinOp::Mod => "smod",
        BinOp::And => "and",
        BinOp::Or => "orr",
        BinOp::Xor => "eor",
        BinOp::Shl => "lsl",
        BinOp::Shr => "lsr",
    }
}
