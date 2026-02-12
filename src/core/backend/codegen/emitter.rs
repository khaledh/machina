//! Target-agnostic SSA codegen emitter traits.

use std::collections::HashMap;

use crate::core::backend::codegen::graph::CodegenBlockId;
use crate::core::backend::regalloc::moves::{MoveOp, ParamCopy};
use crate::core::backend::regalloc::target::PhysReg;
use crate::core::backend::regalloc::{Location, ValueAllocMap};
use crate::core::ir::layout::IrLayout;
use crate::core::ir::{BinOp, ConstValue, GlobalData, Instruction, LocalId, Terminator, ValueId};
use crate::core::ir::{IrTypeCache, IrTypeId};
use crate::core::resolve::DefId;

/// Minimal instruction selection interface for SSA codegen.
pub trait CodegenEmitter {
    /// Begin emitting a function symbol and prologue.
    fn begin_function(&mut self, _name: &str, _frame_size: u32, _callee_saved: &[PhysReg]) {}

    /// Emit aggregate parameter copies required at function entry.
    fn emit_param_copies(&mut self, _copies: &[ParamCopy]) {}

    /// Begin emitting a new block label.
    fn begin_block(&mut self, label: &str);

    /// Emit a global data item.
    fn emit_global(&mut self, _global: &GlobalData) {}

    /// Emit a move sequence.
    fn emit_moves(&mut self, moves: &[MoveOp]);

    /// Emit a lowered instruction.
    fn emit_inst(&mut self, inst: &Instruction, locs: &LocationResolver);

    /// Emit an unconditional branch to a resolved label.
    fn emit_branch(&mut self, target: CodegenBlockId);

    /// Emit a conditional branch to resolved labels.
    fn emit_cond_branch(
        &mut self,
        cond: ValueId,
        then_target: CodegenBlockId,
        else_target: CodegenBlockId,
        locs: &LocationResolver,
    );

    /// Emit a switch terminator with resolved labels.
    fn emit_switch(
        &mut self,
        value: ValueId,
        cases: &[(ConstValue, CodegenBlockId)],
        default_target: CodegenBlockId,
        locs: &LocationResolver,
    );

    /// Emit a terminator.
    fn emit_terminator(&mut self, term: &Terminator, locs: &LocationResolver);

    /// Finish emitting a function.
    fn end_function(&mut self) {}
}

/// Resolves SSA values to allocated locations.
pub struct LocationResolver<'a> {
    pub map: &'a ValueAllocMap,
    pub value_types: &'a HashMap<ValueId, IrTypeId>,
    pub local_offsets: &'a HashMap<LocalId, u32>,
    pub types: &'a IrTypeCache,
    pub layouts: &'a HashMap<IrTypeId, IrLayout>,
    pub def_names: &'a HashMap<DefId, String>,
    pub field_addr_folds: &'a HashMap<ValueId, (ValueId, u32)>,
    pub index_addr_folds: &'a HashMap<ValueId, (ValueId, u32)>,
    pub const_zero_values: &'a std::collections::HashSet<ValueId>,
    pub const_zero_skips: &'a std::collections::HashSet<ValueId>,
}

impl<'a> LocationResolver<'a> {
    pub fn value(&self, id: ValueId) -> Location {
        *self
            .map
            .get(&id)
            .unwrap_or_else(|| panic!("backend codegen: missing alloc for {:?}", id))
    }

    pub fn value_ty(&self, id: ValueId) -> IrTypeId {
        *self
            .value_types
            .get(&id)
            .unwrap_or_else(|| panic!("backend codegen: missing type for {:?}", id))
    }

    pub fn local_offset(&self, id: LocalId) -> u32 {
        *self
            .local_offsets
            .get(&id)
            .unwrap_or_else(|| panic!("backend codegen: missing local offset for {:?}", id))
    }

    pub fn layout(&self, id: IrTypeId) -> &IrLayout {
        self.layouts
            .get(&id)
            .unwrap_or_else(|| panic!("backend codegen: missing layout for {:?}", id))
    }

    pub fn def_name(&self, id: DefId) -> Option<&str> {
        self.def_names.get(&id).map(|name| name.as_str())
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
