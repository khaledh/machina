//! Minimal ARM64 SSA codegen emitter.

use std::fmt::Write;

use crate::regalloc::target::PhysReg;
use crate::ssa::IrTypeKind;
use crate::ssa::codegen::emitter::{CodegenEmitter, LocationResolver};
use crate::ssa::codegen::graph::CodegenBlockId;
use crate::ssa::model::ir::{CmpOp, ConstValue, GlobalData, Instruction, Terminator, ValueId};
use crate::ssa::regalloc::Location;
use crate::ssa::regalloc::moves::{MoveOp, ParamCopy};

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

/// Simple string-based ARM64 emitter for early SSA codegen.
pub struct Arm64Emitter {
    output: String,
    layout: FrameLayout,
    /// Callee-saved registers to save/restore in the prologue/epilogue.
    callee_saved: Vec<PhysReg>,
    section: AsmSection,
    block_prefix: String,
}

impl Arm64Emitter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            layout: FrameLayout {
                aligned_size: 0,
                saved_size: 0,
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

    fn reg_name(reg: PhysReg) -> &'static str {
        const REG_NAMES: [&str; 31] = [
            "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13",
            "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25",
            "x26", "x27", "x28", "x29", "x30",
        ];
        REG_NAMES
            .get(reg.0 as usize)
            .copied()
            .unwrap_or_else(|| panic!("ssa codegen: unsupported phys reg x{}", reg.0))
    }

    fn emit_line(&mut self, line: &str) {
        let _ = writeln!(self.output, "  {}", line);
    }

    fn emit_mov_imm(&mut self, dst: &str, value: i128, bits: u8) {
        if bits == 0 {
            self.emit_line(&format!("mov {}, #0", dst));
            return;
        }

        let reg = if bits <= 32 {
            Self::w_reg(dst)
        } else {
            dst.to_string()
        };

        let mask = if bits >= 64 {
            u128::MAX
        } else {
            (1u128 << bits) - 1
        };
        let imm = (value as i128 as u128) & mask;

        if imm == 0 {
            self.emit_line(&format!("mov {}, #0", reg));
            return;
        }

        let max_shift = match bits {
            0..=16 => 0,
            17..=32 => 16,
            33..=48 => 32,
            _ => 48,
        };
        let mut emitted = false;
        for shift in (0..=max_shift).step_by(16) {
            let chunk = (imm >> shift) & 0xffff;
            if chunk == 0 {
                continue;
            }
            if !emitted {
                if shift == 0 {
                    self.emit_line(&format!("mov {}, #{}", reg, chunk));
                } else {
                    self.emit_line(&format!("movz {}, #{}, lsl #{}", reg, chunk, shift));
                }
                emitted = true;
            } else if shift == 0 {
                self.emit_line(&format!("movk {}, #{}", reg, chunk));
            } else {
                self.emit_line(&format!("movk {}, #{}, lsl #{}", reg, chunk, shift));
            }
        }
    }

    fn emit_sp_offset_addr(&mut self, dst: &str, offset: u32) {
        if offset <= 4095 {
            self.emit_line(&format!("add {}, sp, #{}", dst, offset));
            return;
        }

        // Materialize large offsets into a register, then add to SP.
        let mut remaining = offset;
        let mut shift = 0;
        self.emit_line(&format!("mov {}, #0", dst));
        while remaining > 0 {
            let chunk = (remaining & 0xffff) as u16;
            if chunk != 0 {
                self.emit_line(&format!("movk {}, #{}, lsl #{}", dst, chunk, shift));
            }
            remaining >>= 16;
            shift += 16;
        }
        self.emit_line(&format!("add {}, sp, {}", dst, dst));
    }

    fn emit_stp_sp(&mut self, reg0: PhysReg, reg1: PhysReg, offset: u32) {
        if offset <= 504 && offset % 8 == 0 {
            self.emit_line(&format!(
                "stp {}, {}, [sp, #{}]",
                Self::reg_name(reg0),
                Self::reg_name(reg1),
                offset
            ));
            return;
        }

        self.emit_sp_offset_addr("x9", offset);
        self.emit_line(&format!(
            "stp {}, {}, [x9]",
            Self::reg_name(reg0),
            Self::reg_name(reg1)
        ));
    }

    fn emit_ldp_sp(&mut self, reg0: PhysReg, reg1: PhysReg, offset: u32) {
        if offset <= 504 && offset % 8 == 0 {
            self.emit_line(&format!(
                "ldp {}, {}, [sp, #{}]",
                Self::reg_name(reg0),
                Self::reg_name(reg1),
                offset
            ));
            return;
        }

        self.emit_sp_offset_addr("x9", offset);
        self.emit_line(&format!(
            "ldp {}, {}, [x9]",
            Self::reg_name(reg0),
            Self::reg_name(reg1)
        ));
    }

    fn emit_str_sp(&mut self, reg: PhysReg, offset: u32) {
        if offset <= 4095 {
            self.emit_line(&format!("str {}, [sp, #{}]", Self::reg_name(reg), offset));
            return;
        }

        self.emit_sp_offset_addr("x9", offset);
        self.emit_line(&format!("str {}, [x9]", Self::reg_name(reg)));
    }

    fn emit_ldr_sp(&mut self, reg: PhysReg, offset: u32) {
        if offset <= 4095 {
            self.emit_line(&format!("ldr {}, [sp, #{}]", Self::reg_name(reg), offset));
            return;
        }

        self.emit_sp_offset_addr("x9", offset);
        self.emit_line(&format!("ldr {}, [x9]", Self::reg_name(reg)));
    }

    fn codegen_label(&self, id: CodegenBlockId) -> String {
        match id {
            CodegenBlockId::Ssa(id) => format!("{}_bb{}", self.block_prefix, id.0),
            CodegenBlockId::Move(id) => format!("{}_mb{}", self.block_prefix, id.0),
        }
    }

    fn cmp_condition(op: CmpOp) -> &'static str {
        match op {
            CmpOp::Eq => "eq",
            CmpOp::Ne => "ne",
            CmpOp::Lt => "lt",
            CmpOp::Le => "le",
            CmpOp::Gt => "gt",
            CmpOp::Ge => "ge",
        }
    }

    fn scalar_size(locs: &LocationResolver, ty: crate::ssa::IrTypeId) -> u32 {
        match locs.types.kind(ty) {
            IrTypeKind::Unit => 0,
            IrTypeKind::Bool => 1,
            IrTypeKind::Int { bits, .. } => (*bits as u32) / 8,
            IrTypeKind::Ptr { .. } | IrTypeKind::Fn { .. } => 8,
            _ => locs.layout(ty).size() as u32,
        }
    }

    fn reg_for_type(locs: &LocationResolver, ty: crate::ssa::IrTypeId, reg: PhysReg) -> String {
        let name = Self::reg_name(reg).to_string();
        let size = Self::scalar_size(locs, ty);
        if size > 0 && size <= 4 {
            Self::w_reg(&name)
        } else {
            name
        }
    }

    fn stack_offset(&self, slot: crate::ssa::regalloc::StackSlotId) -> u32 {
        // Stack slots live below the callee-saved area at the top of the frame.
        self.layout.slot_offset(slot)
    }

    fn saved_reg_offset(&self, reg: PhysReg) -> Option<u32> {
        let idx = self.callee_saved.iter().position(|saved| *saved == reg)?;
        let base = self.layout.saved_base();
        let pair = (idx / 2) as u32;
        let lane = (idx % 2) as u32;
        Some(base + pair * 16 + lane * 8)
    }

    fn w_reg(reg: &str) -> String {
        reg.strip_prefix('x')
            .map(|suffix| format!("w{}", suffix))
            .unwrap_or_else(|| reg.to_string())
    }

    fn x_reg(reg: &str) -> String {
        reg.strip_prefix('w')
            .map(|suffix| format!("x{}", suffix))
            .unwrap_or_else(|| reg.to_string())
    }
}

impl CodegenEmitter for Arm64Emitter {
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
                Location::Reg(reg) => Self::reg_name(reg).to_string(),
                Location::IncomingArg(offset) => {
                    let offset = self.layout.incoming_offset(offset);
                    self.emit_ldr_sp(PhysReg(15), offset);
                    "x15".to_string()
                }
                other => {
                    panic!("ssa codegen: unsupported param copy source {:?}", other);
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

impl ConstValueExt for crate::ssa::model::ir::ConstValue {
    fn as_int(&self) -> i128 {
        match self {
            crate::ssa::model::ir::ConstValue::Int { value, .. } => *value,
            crate::ssa::model::ir::ConstValue::Bool(value) => {
                if *value {
                    1
                } else {
                    0
                }
            }
            crate::ssa::model::ir::ConstValue::Unit => 0,
            crate::ssa::model::ir::ConstValue::FuncAddr { .. } => 0,
            crate::ssa::model::ir::ConstValue::GlobalAddr { .. } => 0,
        }
    }
}

fn needs_sret(locs: &LocationResolver, ty: crate::ssa::IrTypeId) -> bool {
    match locs.types.kind(ty) {
        IrTypeKind::Unit | IrTypeKind::Bool | IrTypeKind::Int { .. } | IrTypeKind::Ptr { .. } => {
            false
        }
        _ => locs.layout(ty).size() as u32 > 16,
    }
}
