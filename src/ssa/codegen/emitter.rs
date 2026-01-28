//! Target-agnostic SSA codegen emitter traits.

use std::collections::HashMap;

use crate::regalloc::target::PhysReg;
use crate::ssa::IrTypeCache;
use crate::ssa::IrTypeId;
use crate::ssa::model::ir::{BinOp, Instruction, LocalId, Terminator, ValueId};
use crate::ssa::model::layout::IrLayout;
use crate::ssa::regalloc::Location;
use crate::ssa::regalloc::moves::MoveOp;

/// Minimal instruction selection interface for SSA codegen.
pub trait CodegenEmitter {
    /// Begin emitting a function symbol and prologue.
    fn begin_function(&mut self, _name: &str, _frame_size: u32, _callee_saved: &[PhysReg]) {}

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
    pub value_types: &'a HashMap<ValueId, IrTypeId>,
    pub local_offsets: &'a HashMap<LocalId, u32>,
    pub types: &'a IrTypeCache,
    pub layouts: &'a HashMap<IrTypeId, IrLayout>,
}

impl<'a> LocationResolver<'a> {
    pub fn value(&self, id: crate::ssa::model::ir::ValueId) -> Location {
        *self
            .map
            .get(&id)
            .unwrap_or_else(|| panic!("ssa codegen: missing alloc for {:?}", id))
    }

    pub fn value_ty(&self, id: ValueId) -> IrTypeId {
        *self
            .value_types
            .get(&id)
            .unwrap_or_else(|| panic!("ssa codegen: missing type for {:?}", id))
    }

    pub fn local_offset(&self, id: LocalId) -> u32 {
        *self
            .local_offsets
            .get(&id)
            .unwrap_or_else(|| panic!("ssa codegen: missing local offset for {:?}", id))
    }

    pub fn layout(&self, id: IrTypeId) -> &IrLayout {
        self.layouts
            .get(&id)
            .unwrap_or_else(|| panic!("ssa codegen: missing layout for {:?}", id))
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
