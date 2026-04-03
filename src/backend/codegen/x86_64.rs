//! Minimal x86-64 SSA codegen emitter.

use std::fmt::Write;

use crate::backend::codegen::emitter::{CodegenEmitter, LocationResolver};
use crate::backend::codegen::graph::CodegenBlockId;
use crate::backend::regalloc::moves::{MoveOp, ParamCopy};
use crate::backend::regalloc::target::PhysReg;
use crate::backend::regalloc::{Location, StackSlotId};
use crate::ir::IrTypeId;
use crate::ir::{CmpOp, ConstValue, GlobalData, Instruction, Terminator, ValueId};

mod frame;
mod globals;
mod insts;
mod load_store;
mod moves;
mod terminators;
use self::frame::FrameLayout;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AsmSection {
    None,
    Text,
    Data,
}

pub struct X86_64Emitter {
    output: String,
    layout: FrameLayout,
    callee_saved: Vec<PhysReg>,
    section: AsmSection,
    block_prefix: String,
}

impl Default for X86_64Emitter {
    fn default() -> Self {
        Self::new()
    }
}

impl X86_64Emitter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            layout: FrameLayout {
                aligned_size: 0,
                pushed_size: 0,
                frame_size: 0,
            },
            callee_saved: Vec::new(),
            section: AsmSection::None,
            block_prefix: String::new(),
        }
    }

    pub fn finish(self) -> String {
        self.output
    }

    fn emit_line(&mut self, line: &str) {
        let _ = writeln!(self.output, "  {}", line);
    }

    fn reg_name(reg: PhysReg) -> &'static str {
        const REG_NAMES: [&str; 16] = [
            "rax", "rcx", "rdx", "rbx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13",
            "r14", "r15", "rbp", "rsp",
        ];
        REG_NAMES
            .get(reg.0 as usize)
            .copied()
            .unwrap_or_else(|| panic!("backend codegen: unsupported phys reg r{}", reg.0))
    }

    fn reg(bits: u32, reg: PhysReg) -> &'static str {
        match (Self::reg_name(reg), bits) {
            ("rax", 8) => "%al",
            ("rax", 16) => "%ax",
            ("rax", 32) => "%eax",
            ("rax", 64) => "%rax",
            ("rcx", 8) => "%cl",
            ("rcx", 16) => "%cx",
            ("rcx", 32) => "%ecx",
            ("rcx", 64) => "%rcx",
            ("rdx", 8) => "%dl",
            ("rdx", 16) => "%dx",
            ("rdx", 32) => "%edx",
            ("rdx", 64) => "%rdx",
            ("rbx", 8) => "%bl",
            ("rbx", 16) => "%bx",
            ("rbx", 32) => "%ebx",
            ("rbx", 64) => "%rbx",
            ("rsi", 8) => "%sil",
            ("rsi", 16) => "%si",
            ("rsi", 32) => "%esi",
            ("rsi", 64) => "%rsi",
            ("rdi", 8) => "%dil",
            ("rdi", 16) => "%di",
            ("rdi", 32) => "%edi",
            ("rdi", 64) => "%rdi",
            ("r8", 8) => "%r8b",
            ("r8", 16) => "%r8w",
            ("r8", 32) => "%r8d",
            ("r8", 64) => "%r8",
            ("r9", 8) => "%r9b",
            ("r9", 16) => "%r9w",
            ("r9", 32) => "%r9d",
            ("r9", 64) => "%r9",
            ("r10", 8) => "%r10b",
            ("r10", 16) => "%r10w",
            ("r10", 32) => "%r10d",
            ("r10", 64) => "%r10",
            ("r11", 8) => "%r11b",
            ("r11", 16) => "%r11w",
            ("r11", 32) => "%r11d",
            ("r11", 64) => "%r11",
            ("r12", 8) => "%r12b",
            ("r12", 16) => "%r12w",
            ("r12", 32) => "%r12d",
            ("r12", 64) => "%r12",
            ("r13", 8) => "%r13b",
            ("r13", 16) => "%r13w",
            ("r13", 32) => "%r13d",
            ("r13", 64) => "%r13",
            ("r14", 8) => "%r14b",
            ("r14", 16) => "%r14w",
            ("r14", 32) => "%r14d",
            ("r14", 64) => "%r14",
            ("r15", 8) => "%r15b",
            ("r15", 16) => "%r15w",
            ("r15", 32) => "%r15d",
            ("r15", 64) => "%r15",
            ("rbp", 8) => "%bpl",
            ("rbp", 16) => "%bp",
            ("rbp", 32) => "%ebp",
            ("rbp", 64) => "%rbp",
            ("rsp", 8) => "%spl",
            ("rsp", 16) => "%sp",
            ("rsp", 32) => "%esp",
            ("rsp", 64) => "%rsp",
            _ => panic!(
                "backend codegen: unsupported register width {} for {:?}",
                bits, reg
            ),
        }
    }

    fn mem(base: &str, offset: u32) -> String {
        if offset == 0 {
            format!("(%{})", base)
        } else {
            format!("{}(%{})", offset, base)
        }
    }

    fn mem_indexed(base: &str, index: &str, scale: u32) -> String {
        format!("(%{}, %{}, {})", base, index, scale)
    }

    fn codegen_label(&self, id: CodegenBlockId) -> String {
        match id {
            CodegenBlockId::Ssa(id) => format!("{}_bb{}", self.block_prefix, id.0),
            CodegenBlockId::Move(id) => format!("{}_mb{}", self.block_prefix, id.0),
        }
    }

    fn cmp_condition(op: CmpOp) -> &'static str {
        match op {
            CmpOp::Eq => "e",
            CmpOp::Ne => "ne",
            CmpOp::Lt => "l",
            CmpOp::Le => "le",
            CmpOp::Gt => "g",
            CmpOp::Ge => "ge",
        }
    }

    fn scalar_size(locs: &LocationResolver, ty: IrTypeId) -> u32 {
        locs.types.scalar_size_for_layout(ty, locs.layout(ty))
    }

    fn reg_for_type(locs: &LocationResolver, ty: IrTypeId, reg: PhysReg) -> &'static str {
        let size = Self::scalar_size(locs, ty);
        let bits = if size == 0 {
            64
        } else if size <= 4 {
            32
        } else {
            64
        };
        Self::reg(bits, reg)
    }

    fn stack_offset(&self, slot: StackSlotId) -> u32 {
        self.layout.slot_offset(slot)
    }

    fn saved_reg_offset(&self, reg: PhysReg) -> Option<u32> {
        let idx = self.callee_saved.iter().position(|saved| *saved == reg)?;
        let rev_idx = self.callee_saved.len().saturating_sub(idx + 1) as u32;
        Some(self.layout.aligned_size.saturating_add(rev_idx * 8))
    }

    fn emit_mov_imm(&mut self, dst: &str, value: i128, bits: u32) {
        match bits {
            0..=32 => self.emit_line(&format!("movl ${}, {}", value as i64, dst)),
            _ => self.emit_line(&format!("movabsq ${}, {}", value as i64, dst)),
        }
    }
}

impl CodegenEmitter for X86_64Emitter {
    fn emit_global(&mut self, global: &GlobalData) {
        self.emit_global_impl(global);
    }

    fn begin_function(&mut self, name: &str, frame_size: u32, callee_saved: &[PhysReg]) {
        self.begin_function_impl(name, frame_size, callee_saved);
    }

    fn emit_param_copies(&mut self, copies: &[ParamCopy]) {
        for copy in copies {
            if copy.size == 0 {
                continue;
            }

            let src_reg = match copy.src {
                Location::Reg(reg) => Self::reg(64, reg).to_string(),
                Location::IncomingArg(offset) => {
                    let offset = self.layout.incoming_offset(offset);
                    self.emit_line(&format!("movq {}, %r11", Self::mem("rsp", offset)));
                    "%r11".to_string()
                }
                other => {
                    panic!("backend codegen: unsupported param copy source {:?}", other);
                }
            };

            let dst_offset = self.stack_offset(copy.dst);
            self.copy_ptr_to_stack(&src_reg, dst_offset, copy.size);
        }
    }

    fn begin_block(&mut self, label: &str) {
        let _ = writeln!(self.output, "{}:", label);
    }

    fn emit_moves(&mut self, moves: &[MoveOp]) {
        self.emit_move_ops(moves);
    }

    fn emit_inst(&mut self, inst: &Instruction, locs: &LocationResolver) {
        self.emit_inst_impl(inst, locs);
    }

    fn emit_branch(&mut self, target: CodegenBlockId) {
        self.emit_branch_impl(target);
    }

    fn emit_cond_branch(
        &mut self,
        cond: ValueId,
        then_target: CodegenBlockId,
        else_target: CodegenBlockId,
        locs: &LocationResolver,
    ) {
        self.emit_cond_branch_impl(cond, then_target, else_target, locs);
    }

    fn emit_switch(
        &mut self,
        value: ValueId,
        cases: &[(ConstValue, CodegenBlockId)],
        default_target: CodegenBlockId,
        locs: &LocationResolver,
    ) {
        self.emit_switch_impl(value, cases, default_target, locs);
    }

    fn emit_terminator(&mut self, term: &Terminator, locs: &LocationResolver) {
        self.emit_terminator_impl(term, locs);
    }
}

trait ConstValueExt {
    fn as_int(&self) -> i128;
}

impl ConstValueExt for ConstValue {
    fn as_int(&self) -> i128 {
        match self {
            ConstValue::Int { value, .. } => *value,
            ConstValue::Bool(value) => i128::from(*value),
            ConstValue::Unit => 0,
            ConstValue::FuncAddr { .. } => 0,
            ConstValue::GlobalAddr { .. } => 0,
        }
    }
}

fn needs_sret(locs: &LocationResolver, ty: IrTypeId) -> bool {
    locs.types.needs_sret_for_layout(ty, locs.layout(ty))
}
