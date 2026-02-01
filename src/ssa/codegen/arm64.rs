//! Minimal ARM64 SSA codegen emitter.

use std::fmt::Write;

use crate::regalloc::target::PhysReg;
use crate::ssa::IrTypeKind;
use crate::ssa::RuntimeFn;
use crate::ssa::codegen::emitter::{CodegenEmitter, LocationResolver, binop_mnemonic};
use crate::ssa::codegen::graph::CodegenBlockId;
use crate::ssa::model::ir::{
    BinOp, Callee, CastKind, CmpOp, ConstValue, GlobalData, InstKind, Instruction, Terminator,
    UnOp, ValueId,
};
use crate::ssa::regalloc::moves::MoveOp;
use crate::ssa::regalloc::{Location, StackSlotId};
use crate::targets::arm64::regs::INDIRECT_CALL_REG;

#[derive(Debug, Clone, Copy)]
struct FrameLayout {
    aligned_size: u32,
    saved_size: u32,
    frame_size: u32,
}

impl FrameLayout {
    fn new(frame_size: u32, saved_size: u32) -> Self {
        let total = frame_size.saturating_add(saved_size);
        let aligned_size = Self::align_frame(total);
        Self {
            aligned_size,
            saved_size,
            frame_size,
        }
    }

    fn align_frame(size: u32) -> u32 {
        (size + 15) & !15
    }

    fn slot_offset(self, slot: crate::ssa::regalloc::StackSlotId) -> u32 {
        if self.aligned_size == 0 {
            return slot.offset_bytes();
        }
        let slot_base = self.aligned_size.saturating_sub(self.saved_size);
        slot_base.saturating_sub(slot.offset_bytes())
    }

    fn saved_base(self) -> u32 {
        self.aligned_size.saturating_sub(self.saved_size)
    }

    fn outgoing_base(self) -> u32 {
        self.aligned_size
            .saturating_sub(self.frame_size.saturating_add(self.saved_size))
    }

    fn outgoing_offset(self, offset: u32) -> u32 {
        self.outgoing_base().saturating_add(offset)
    }

    fn incoming_offset(self, offset: u32) -> u32 {
        self.aligned_size.saturating_add(offset)
    }
}

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

    fn ensure_text(&mut self) {
        if self.section != AsmSection::Text {
            if !self.output.is_empty() {
                let _ = writeln!(self.output);
            }
            let _ = writeln!(self.output, ".text");
            self.section = AsmSection::Text;
        }
    }

    fn ensure_data(&mut self) {
        if self.section != AsmSection::Data {
            if !self.output.is_empty() {
                let _ = writeln!(self.output);
            }
            let _ = writeln!(self.output, ".data");
            self.section = AsmSection::Data;
        }
    }

    fn codegen_label(&self, id: CodegenBlockId) -> String {
        match id {
            CodegenBlockId::Ssa(id) => format!("{}_bb{}", self.block_prefix, id.0),
            CodegenBlockId::Move(id) => format!("{}_mb{}", self.block_prefix, id.0),
        }
    }

    fn global_label(id: crate::ssa::model::ir::GlobalId) -> String {
        format!("_g{}", id.0)
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

    fn load_value_typed(
        &mut self,
        locs: &LocationResolver,
        loc: Location,
        ty: crate::ssa::IrTypeId,
        scratch: &str,
    ) -> String {
        let size = Self::scalar_size(locs, ty);
        match loc {
            Location::Reg(reg) => Self::reg_for_type(locs, ty, reg),
            Location::Stack(slot) => {
                let offset = self.stack_offset(slot);
                self.emit_load_sized(scratch, offset, size);
                if size > 0 && size <= 4 {
                    Self::w_reg(scratch)
                } else {
                    scratch.to_string()
                }
            }
            Location::IncomingArg(offset) => {
                let offset = self.layout.incoming_offset(offset);
                self.emit_load_sized(scratch, offset, size);
                if size > 0 && size <= 4 {
                    Self::w_reg(scratch)
                } else {
                    scratch.to_string()
                }
            }
            Location::OutgoingArg(offset) => {
                let offset = self.layout.outgoing_offset(offset);
                self.emit_load_sized(scratch, offset, size);
                if size > 0 && size <= 4 {
                    Self::w_reg(scratch)
                } else {
                    scratch.to_string()
                }
            }
            Location::StackAddr(slot) => {
                let offset = self.stack_offset(slot);
                self.emit_add_imm(scratch, "sp", offset);
                scratch.to_string()
            }
        }
    }

    fn emit_load_sized(&mut self, scratch: &str, offset: u32, size: u32) {
        match size {
            0 => {}
            1 => {
                let w = Self::w_reg(scratch);
                self.emit_line(&format!("ldrb {}, [sp, #{}]", w, offset));
            }
            2 => {
                let w = Self::w_reg(scratch);
                self.emit_line(&format!("ldrh {}, [sp, #{}]", w, offset));
            }
            4 => {
                let w = Self::w_reg(scratch);
                self.emit_line(&format!("ldr {}, [sp, #{}]", w, offset));
            }
            8 => {
                self.emit_line(&format!("ldr {}, [sp, #{}]", scratch, offset));
            }
            other => {
                panic!("ssa codegen: unsupported scalar load size {other}");
            }
        }
    }

    fn emit_add_imm(&mut self, dst: &str, base: &str, offset: u32) {
        if offset == 0 {
            if dst != base {
                self.emit_line(&format!("mov {}, {}", dst, base));
            }
            return;
        }
        self.emit_line(&format!("add {}, {}, #{}", dst, base, offset));
    }

    fn emit_load_ptr_sized(&mut self, dst_reg: &str, addr_reg: &str, size: u32) {
        match size {
            0 => {}
            1 => {
                let w = Self::w_reg(dst_reg);
                self.emit_line(&format!("ldrb {}, [{}]", w, addr_reg));
            }
            2 => {
                let w = Self::w_reg(dst_reg);
                self.emit_line(&format!("ldrh {}, [{}]", w, addr_reg));
            }
            4 => {
                let w = Self::w_reg(dst_reg);
                self.emit_line(&format!("ldr {}, [{}]", w, addr_reg));
            }
            8 => {
                self.emit_line(&format!("ldr {}, [{}]", dst_reg, addr_reg));
            }
            other => {
                panic!("ssa codegen: unsupported scalar load size {other}");
            }
        }
    }

    fn emit_store_ptr_sized(&mut self, src_reg: &str, addr_reg: &str, size: u32) {
        match size {
            0 => {}
            1 => {
                let w = Self::w_reg(src_reg);
                self.emit_line(&format!("strb {}, [{}]", w, addr_reg));
            }
            2 => {
                let w = Self::w_reg(src_reg);
                self.emit_line(&format!("strh {}, [{}]", w, addr_reg));
            }
            4 => {
                let w = Self::w_reg(src_reg);
                self.emit_line(&format!("str {}, [{}]", w, addr_reg));
            }
            8 => {
                self.emit_line(&format!("str {}, [{}]", src_reg, addr_reg));
            }
            other => {
                panic!("ssa codegen: unsupported scalar store size {other}");
            }
        }
    }

    fn emit_store_ptr_sized_offset(
        &mut self,
        src_reg: &str,
        base_reg: &str,
        offset: u32,
        size: u32,
    ) {
        if offset == 0 {
            self.emit_store_ptr_sized(src_reg, base_reg, size);
            return;
        }

        let scaled = offset % size == 0 && (offset / size) <= 4095;
        if scaled {
            match size {
                1 => {
                    let w = Self::w_reg(src_reg);
                    self.emit_line(&format!("strb {}, [{}, #{}]", w, base_reg, offset));
                }
                2 => {
                    let w = Self::w_reg(src_reg);
                    self.emit_line(&format!("strh {}, [{}, #{}]", w, base_reg, offset));
                }
                4 => {
                    let w = Self::w_reg(src_reg);
                    self.emit_line(&format!("str {}, [{}, #{}]", w, base_reg, offset));
                }
                8 => {
                    self.emit_line(&format!("str {}, [{}, #{}]", src_reg, base_reg, offset));
                }
                other => {
                    panic!("ssa codegen: unsupported scalar store size {other}");
                }
            }
            return;
        }

        let scratch = if base_reg == "x15" { "x14" } else { "x15" };
        self.emit_add_imm(scratch, base_reg, offset);
        self.emit_store_ptr_sized(src_reg, scratch, size);
    }

    fn value_dst_typed(
        &mut self,
        locs: &LocationResolver,
        dst: Location,
        scratch: &str,
        context: &str,
        ty: crate::ssa::IrTypeId,
    ) -> (String, Option<StackSlotId>) {
        match dst {
            Location::Reg(reg) => (Self::reg_for_type(locs, ty, reg), None),
            Location::Stack(slot) => {
                let size = Self::scalar_size(locs, ty);
                if size > 0 && size <= 4 {
                    (Self::w_reg(scratch), Some(slot))
                } else {
                    (scratch.to_string(), Some(slot))
                }
            }
            other => {
                panic!("ssa codegen: invalid {} dst {:?}", context, other);
            }
        }
    }

    fn store_if_needed_typed(
        &mut self,
        locs: &LocationResolver,
        slot: Option<StackSlotId>,
        reg: &str,
        ty: crate::ssa::IrTypeId,
    ) {
        if let Some(slot) = slot {
            let size = Self::scalar_size(locs, ty);
            let offset = self.stack_offset(slot);
            match size {
                0 => {}
                1 => {
                    let w = Self::w_reg(reg);
                    self.emit_line(&format!("strb {}, [sp, #{}]", w, offset));
                }
                2 => {
                    let w = Self::w_reg(reg);
                    self.emit_line(&format!("strh {}, [sp, #{}]", w, offset));
                }
                4 => {
                    let w = Self::w_reg(reg);
                    self.emit_line(&format!("str {}, [sp, #{}]", w, offset));
                }
                8 => {
                    self.emit_line(&format!("str {}, [sp, #{}]", reg, offset));
                }
                other => {
                    panic!("ssa codegen: unsupported scalar store size {other}");
                }
            }
        }
    }

    fn copy_ptr_to_stack(&mut self, src_reg: &str, dst_offset: u32, size: u32) {
        let mut offset = 0u32;

        while offset + 8 <= size {
            self.emit_line(&format!("ldr x14, [{}, #{}]", src_reg, offset));
            self.emit_line(&format!("str x14, [sp, #{}]", dst_offset + offset));
            offset += 8;
        }

        if offset + 4 <= size {
            self.emit_line(&format!("ldr w14, [{}, #{}]", src_reg, offset));
            self.emit_line(&format!("str w14, [sp, #{}]", dst_offset + offset));
            offset += 4;
        }

        if offset + 2 <= size {
            self.emit_line(&format!("ldrh w14, [{}, #{}]", src_reg, offset));
            self.emit_line(&format!("strh w14, [sp, #{}]", dst_offset + offset));
            offset += 2;
        }

        if offset < size {
            self.emit_line(&format!("ldrb w14, [{}, #{}]", src_reg, offset));
            self.emit_line(&format!("strb w14, [sp, #{}]", dst_offset + offset));
        }
    }

    fn copy_stack_to_ptr(&mut self, src_offset: u32, dst_reg: &str, size: u32) {
        let mut offset = 0u32;

        while offset + 8 <= size {
            self.emit_line(&format!("ldr x14, [sp, #{}]", src_offset + offset));
            self.emit_line(&format!("str x14, [{}, #{}]", dst_reg, offset));
            offset += 8;
        }

        if offset + 4 <= size {
            self.emit_line(&format!("ldr w14, [sp, #{}]", src_offset + offset));
            self.emit_line(&format!("str w14, [{}, #{}]", dst_reg, offset));
            offset += 4;
        }

        if offset + 2 <= size {
            self.emit_line(&format!("ldrh w14, [sp, #{}]", src_offset + offset));
            self.emit_line(&format!("strh w14, [{}, #{}]", dst_reg, offset));
            offset += 2;
        }

        if offset < size {
            self.emit_line(&format!("ldrb w14, [sp, #{}]", src_offset + offset));
            self.emit_line(&format!("strb w14, [{}, #{}]", dst_reg, offset));
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
        self.ensure_data();
        if !self.output.is_empty() {
            let _ = writeln!(self.output);
        }

        // Emit a stable label for the global payload.
        let label = Self::global_label(global.id);

        // Align the data so references can use natural alignment.
        let align = global.align.max(1);
        if align.is_power_of_two() {
            let align_log2 = align.trailing_zeros();
            self.emit_line(&format!(".p2align {}", align_log2));
        } else {
            self.emit_line(&format!(".balign {}", align));
        }

        let _ = writeln!(self.output, "{}:", label);

        // Emit ASCII strings when possible, otherwise fall back to raw bytes.
        if let Some(text) = format_bytes_as_ascii(&global.bytes) {
            if text.is_empty() {
                self.emit_line(".space 0");
            } else {
                self.emit_line(&format!(".ascii \"{}\"", text));
            }
            return;
        }

        if global.bytes.is_empty() {
            self.emit_line(".space 0");
            return;
        }

        let data = global
            .bytes
            .iter()
            .map(|b| b.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        self.emit_line(&format!(".byte {}", data));
    }

    fn begin_function(&mut self, name: &str, frame_size: u32, callee_saved: &[PhysReg]) {
        self.ensure_text();
        if !self.output.is_empty() {
            let _ = writeln!(self.output);
        }
        self.block_prefix = format!(".L{}", name);
        let _ = writeln!(self.output, ".globl {}", name);
        // Capture the callee-saved set so we can restore it in the epilogue.
        self.callee_saved = callee_saved.to_vec();
        if !self.callee_saved.iter().any(|reg| reg.0 == 30) {
            self.callee_saved.push(PhysReg(30));
        }
        self.callee_saved.sort_by_key(|reg| reg.0);
        let saved_size = (self.callee_saved.len() as u32) * 8;
        // Lay out the frame as: [callee-saved area][local/slot area], aligned to 16 bytes.
        self.layout = FrameLayout::new(frame_size, saved_size);

        let _ = writeln!(self.output, "{}:", name);

        // Reserve the full frame size up front.
        if self.layout.aligned_size > 0 {
            self.emit_line(&format!("sub sp, sp, #{}", self.layout.aligned_size));
        }

        if !self.callee_saved.is_empty() {
            // Save callee-saved registers at the top of the frame.
            let base = self.layout.saved_base();
            let callee_saved = self.callee_saved.clone();
            let pair_count = callee_saved.len() / 2;

            // Emit paired saves for contiguous register pairs.
            for pair in 0..pair_count {
                let idx = pair * 2;
                let reg0 = callee_saved[idx];
                let reg1 = callee_saved[idx + 1];
                let offset = base + (pair as u32) * 16;
                self.emit_stp_sp(reg0, reg1, offset);
            }

            // Handle a trailing register if the count is odd.
            if callee_saved.len() % 2 == 1 {
                let reg = callee_saved[pair_count * 2];
                let offset = base + (pair_count as u32) * 16;
                self.emit_str_sp(reg, offset);
            }
        }
    }

    fn emit_param_moves(
        &mut self,
        func: &crate::ssa::model::ir::Function,
        locs: &crate::ssa::codegen::emitter::LocationResolver,
    ) {
        const PARAM_REG_COUNT: usize = 8;
        let Some(entry) = func.blocks.first() else {
            return;
        };

        let mut moves = Vec::new();
        for (idx, param) in entry.params.iter().enumerate() {
            if idx >= PARAM_REG_COUNT {
                continue;
            }

            let ty = locs.value_ty(param.value.id);
            let dst = locs.value(param.value.id);
            let src = Location::Reg(PhysReg(idx as u8));

            if locs.types.is_reg_type(ty) {
                if src != dst {
                    let size = locs.layout(ty).size() as u32;
                    moves.push(MoveOp { src, dst, size });
                }
            }
        }

        if !moves.is_empty() {
            self.emit_moves(&moves);
        }

        for (idx, param) in entry.params.iter().enumerate() {
            let ty = locs.value_ty(param.value.id);
            if locs.types.is_reg_type(ty) {
                continue;
            }

            let Location::Stack(slot) = locs.value(param.value.id) else {
                panic!(
                    "ssa codegen: aggregate param must be stack-backed, got {:?}",
                    locs.value(param.value.id)
                );
            };

            let size = locs.layout(ty).size() as u32;
            if size == 0 {
                continue;
            }

            let src_reg = if idx < PARAM_REG_COUNT {
                Self::reg_name(PhysReg(idx as u8))
            } else {
                let offset = self
                    .layout
                    .incoming_offset((idx - PARAM_REG_COUNT) as u32 * 8);
                self.emit_ldr_sp(PhysReg(15), offset);
                "x15"
            };

            let dst_offset = self.stack_offset(slot);
            self.copy_ptr_to_stack(src_reg, dst_offset, size);
        }
    }

    fn begin_block(&mut self, label: &str) {
        let _ = writeln!(self.output, "{}:", label);
    }

    fn emit_moves(&mut self, moves: &[MoveOp]) {
        for mov in moves {
            if mov.size == 0 {
                continue;
            }
            match (mov.src, mov.dst) {
                (Location::Reg(src), Location::Reg(dst)) => {
                    if mov.size <= 4 {
                        let dst = Self::w_reg(Self::reg_name(dst));
                        let src = Self::w_reg(Self::reg_name(src));
                        self.emit_line(&format!("mov {}, {}", dst, src));
                    } else {
                        self.emit_line(&format!(
                            "mov {}, {}",
                            Self::reg_name(dst),
                            Self::reg_name(src)
                        ));
                    }
                }
                (Location::Reg(src), Location::Stack(dst)) => {
                    let offset = self.stack_offset(dst);
                    match mov.size {
                        1 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strb {}, [sp, #{}]", src, offset));
                        }
                        2 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strh {}, [sp, #{}]", src, offset));
                        }
                        4 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("str {}, [sp, #{}]", src, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "str {}, [sp, #{}]",
                                Self::reg_name(src),
                                offset
                            ));
                        }
                        other => {
                            panic!("ssa codegen: unsupported reg->stack move size {other}");
                        }
                    }
                }
                (Location::Stack(src), Location::Reg(dst)) => {
                    let offset = self.stack_offset(src);
                    match mov.size {
                        1 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrb {}, [sp, #{}]", dst, offset));
                        }
                        2 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrh {}, [sp, #{}]", dst, offset));
                        }
                        4 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldr {}, [sp, #{}]", dst, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "ldr {}, [sp, #{}]",
                                Self::reg_name(dst),
                                offset
                            ));
                        }
                        other => {
                            panic!("ssa codegen: unsupported stack->reg move size {other}");
                        }
                    }
                }
                (Location::Stack(src), Location::Stack(dst)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.stack_offset(dst);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            self.emit_line(&format!("add x9, sp, #{}", src_offset));
                            self.copy_ptr_to_stack("x9", dst_offset, other);
                        }
                    }
                }
                (Location::Reg(src), Location::OutgoingArg(offset)) => {
                    let offset = self.layout.outgoing_offset(offset);
                    match mov.size {
                        1 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strb {}, [sp, #{}]", src, offset));
                        }
                        2 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strh {}, [sp, #{}]", src, offset));
                        }
                        4 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("str {}, [sp, #{}]", src, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "str {}, [sp, #{}]",
                                Self::reg_name(src),
                                offset
                            ));
                        }
                        other => {
                            panic!("ssa codegen: unsupported reg->outgoing move size {other}");
                        }
                    }
                }
                (Location::Stack(src), Location::OutgoingArg(offset)) => {
                    let offset = self.layout.outgoing_offset(offset);
                    let src_offset = self.stack_offset(src);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", offset));
                        }
                        other => {
                            panic!("ssa codegen: unsupported stack->outgoing move size {other}");
                        }
                    }
                }
                (Location::Reg(src), Location::IncomingArg(offset)) => {
                    let offset = self.layout.incoming_offset(offset);
                    match mov.size {
                        1 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strb {}, [sp, #{}]", src, offset));
                        }
                        2 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strh {}, [sp, #{}]", src, offset));
                        }
                        4 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("str {}, [sp, #{}]", src, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "str {}, [sp, #{}]",
                                Self::reg_name(src),
                                offset
                            ));
                        }
                        other => {
                            panic!("ssa codegen: unsupported reg->incoming move size {other}");
                        }
                    }
                }
                (Location::Stack(src), Location::IncomingArg(offset)) => {
                    let offset = self.layout.incoming_offset(offset);
                    let src_offset = self.stack_offset(src);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", offset));
                        }
                        other => {
                            panic!("ssa codegen: unsupported stack->incoming move size {other}");
                        }
                    }
                }
                (Location::StackAddr(src), Location::IncomingArg(offset)) => {
                    let src_offset = self.stack_offset(src);
                    let out_offset = self.layout.incoming_offset(offset);
                    self.emit_line(&format!("add x9, sp, #{}", src_offset));
                    self.emit_line(&format!("str x9, [sp, #{}]", out_offset));
                }
                (Location::IncomingArg(offset), Location::Reg(dst)) => {
                    let offset = self.layout.incoming_offset(offset);
                    match mov.size {
                        1 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrb {}, [sp, #{}]", dst, offset));
                        }
                        2 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrh {}, [sp, #{}]", dst, offset));
                        }
                        4 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldr {}, [sp, #{}]", dst, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "ldr {}, [sp, #{}]",
                                Self::reg_name(dst),
                                offset
                            ));
                        }
                        other => {
                            panic!("ssa codegen: unsupported incoming->reg move size {other}");
                        }
                    }
                }
                (Location::IncomingArg(offset), Location::Stack(dst)) => {
                    let offset = self.layout.incoming_offset(offset);
                    let dst_offset = self.stack_offset(dst);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            panic!("ssa codegen: unsupported incoming->stack move size {other}");
                        }
                    }
                }
                (Location::StackAddr(src), Location::Reg(dst)) => {
                    let offset = self.stack_offset(src);
                    self.emit_line(&format!("add {}, sp, #{}", Self::reg_name(dst), offset));
                }
                (Location::StackAddr(src), Location::Stack(dst)) => {
                    let offset = self.stack_offset(src);
                    self.emit_line(&format!("add x9, sp, #{}", offset));
                    self.emit_line(&format!("str x9, [sp, #{}]", self.stack_offset(dst)));
                }
                (Location::StackAddr(src), Location::OutgoingArg(offset)) => {
                    let src_offset = self.stack_offset(src);
                    let out_offset = self.layout.outgoing_offset(offset);
                    self.emit_line(&format!("add x9, sp, #{}", src_offset));
                    self.emit_line(&format!("str x9, [sp, #{}]", out_offset));
                }
                _ => {
                    panic!(
                        "ssa codegen: unsupported move {:?} -> {:?}",
                        mov.src, mov.dst
                    );
                }
            }
        }
    }

    fn emit_inst(&mut self, inst: &Instruction, locs: &LocationResolver) {
        match &inst.kind {
            InstKind::Const { value } => {
                // Materialize immediates into a register or stack slot.
                if let Some(result) = &inst.result {
                    if locs.const_zero_skips.contains(&result.id) {
                        return;
                    }
                    let dst = locs.value(result.id);
                    let dst_ty = locs.value_ty(result.id);
                    match value {
                        ConstValue::Int { .. } | ConstValue::Bool(_) | ConstValue::Unit => {
                            let (dst_reg, dst_slot) =
                                self.value_dst_typed(locs, dst, "x9", "const", dst_ty);
                            let size = Self::scalar_size(locs, dst_ty);
                            let bits = (size.saturating_mul(8)) as u8;
                            self.emit_mov_imm(&dst_reg, value.as_int(), bits);
                            self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                        }
                        ConstValue::FuncAddr { def } => {
                            let label = locs
                                .def_name(*def)
                                .map(|name| format!("_{}", name))
                                .unwrap_or_else(|| format!("_fn{}", def.0));
                            let (dst_reg, dst_slot) =
                                self.value_dst_typed(locs, dst, "x9", "const func", dst_ty);
                            self.emit_line(&format!("adrp {}, {}@PAGE", dst_reg, label));
                            self.emit_line(&format!(
                                "add {}, {}, {}@PAGEOFF",
                                dst_reg, dst_reg, label
                            ));
                            self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                        }
                        ConstValue::GlobalAddr { id } => {
                            // TODO: ensure SSA codegen emits global data labels.
                            let label = Self::global_label(*id);
                            let (dst_reg, dst_slot) =
                                self.value_dst_typed(locs, dst, "x9", "const global", dst_ty);
                            self.emit_line(&format!("adrp {}, {}@PAGE", dst_reg, label));
                            self.emit_line(&format!(
                                "add {}, {}, {}@PAGEOFF",
                                dst_reg, dst_reg, label
                            ));
                            self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                        }
                    }
                }
            }
            InstKind::BinOp { op, lhs, rhs } => {
                // Binary arithmetic/logical ops on integer registers.
                if let Some(result) = &inst.result {
                    let lhs_id = *lhs;
                    let rhs_id = *rhs;
                    let lhs = locs.value(lhs_id);
                    let rhs = locs.value(rhs_id);
                    let lhs_ty = locs.value_ty(lhs_id);
                    let rhs_ty = locs.value_ty(rhs_id);
                    let lhs_reg = self.load_value_typed(locs, lhs, lhs_ty, "x9");
                    let rhs_reg = self.load_value_typed(locs, rhs, rhs_ty, "x10");
                    let dst = locs.value(result.id);
                    let dst_ty = locs.value_ty(result.id);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, "x11", "binop", dst_ty);
                    match op {
                        BinOp::Mod => {
                            // ARM64 has no smod/umod: use div + msub.
                            let signed = match locs.types.kind(locs.value_ty(result.id)) {
                                IrTypeKind::Int { signed, .. } => *signed,
                                _ => true,
                            };
                            let narrow = dst_reg.starts_with('w');
                            let q_reg = if narrow { "w12" } else { "x12" };
                            let q_alt = if narrow { "w13" } else { "x13" };
                            let q_reg = if dst_reg != q_reg && lhs_reg != q_reg && rhs_reg != q_reg
                            {
                                q_reg
                            } else {
                                q_alt
                            };
                            let div_op = if signed { "sdiv" } else { "udiv" };
                            self.emit_line(&format!(
                                "{} {}, {}, {}",
                                div_op, q_reg, lhs_reg, rhs_reg
                            ));
                            // dst = lhs - q * rhs
                            self.emit_line(&format!(
                                "msub {}, {}, {}, {}",
                                dst_reg, q_reg, rhs_reg, lhs_reg
                            ));
                        }
                        _ => {
                            let op = binop_mnemonic(*op);
                            self.emit_line(&format!(
                                "{} {}, {}, {}",
                                op, dst_reg, lhs_reg, rhs_reg
                            ));
                        }
                    }
                    self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                }
            }
            InstKind::UnOp { op, value } => {
                // Unary ops over integer values.
                if let Some(result) = &inst.result {
                    let src_loc = locs.value(*value);
                    let src_ty = locs.value_ty(*value);
                    let src_reg = self.load_value_typed(locs, src_loc, src_ty, "x9");
                    let dst = locs.value(result.id);
                    let scratch = if src_reg == "x10" { "x11" } else { "x10" };
                    let dst_ty = locs.value_ty(result.id);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, scratch, "unop", dst_ty);

                    match op {
                        UnOp::Neg => {
                            self.emit_line(&format!("neg {}, {}", dst_reg, src_reg));
                        }
                        UnOp::BitNot => {
                            self.emit_line(&format!("mvn {}, {}", dst_reg, src_reg));
                        }
                        UnOp::Not => {
                            // Logical not: compare against zero and materialize a boolean.
                            self.emit_line(&format!("cmp {}, #0", src_reg));
                            self.emit_line(&format!("cset {}, eq", dst_reg));
                        }
                    }

                    self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                }
            }
            InstKind::Cmp { op, lhs, rhs } => {
                // Compare and produce a boolean via cset.
                if let Some(result) = &inst.result {
                    let lhs_id = *lhs;
                    let rhs_id = *rhs;
                    let lhs = locs.value(lhs_id);
                    let rhs = locs.value(rhs_id);
                    let lhs_ty = locs.value_ty(lhs_id);
                    let rhs_ty = locs.value_ty(rhs_id);
                    let lhs_reg = self.load_value_typed(locs, lhs, lhs_ty, "x9");
                    let rhs_reg = self.load_value_typed(
                        locs,
                        rhs,
                        rhs_ty,
                        if lhs_reg == "x9" || lhs_reg == "w9" {
                            "x10"
                        } else {
                            "x9"
                        },
                    );

                    self.emit_line(&format!("cmp {}, {}", lhs_reg, rhs_reg));
                    let cond = Self::cmp_condition(*op);
                    let dst = locs.value(result.id);
                    let dst_ty = locs.value_ty(result.id);
                    let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, "x9", "cmp", dst_ty);
                    self.emit_line(&format!("cset {}, {}", dst_reg, cond));
                    self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                }
            }
            InstKind::IntTrunc { value, ty } => {
                // Integer truncation via masking to the destination bit-width.
                if let Some(result) = &inst.result {
                    let src_loc = locs.value(*value);
                    let src_ty = locs.value_ty(*value);
                    let src_reg = self.load_value_typed(locs, src_loc, src_ty, "x9");
                    let dst = locs.value(result.id);
                    let scratch = if src_reg == "x10" { "x11" } else { "x10" };
                    let dst_ty = locs.value_ty(result.id);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, scratch, "trunc", dst_ty);
                    let src_reg = Self::x_reg(&src_reg);
                    let dst_reg = Self::x_reg(&dst_reg);

                    let dst_bits = match locs.types.kind(*ty) {
                        IrTypeKind::Int { bits, .. } => *bits as u32,
                        _ => 64,
                    };

                    match dst_bits {
                        64 => self.emit_line(&format!("mov {}, {}", dst_reg, src_reg)),
                        32 => self.emit_line(&format!(
                            "and {}, {}, #0x{:x}",
                            dst_reg, src_reg, 0xffff_ffffu64
                        )),
                        16 => self.emit_line(&format!(
                            "and {}, {}, #0x{:x}",
                            dst_reg, src_reg, 0xffffu64
                        )),
                        8 => self
                            .emit_line(&format!("and {}, {}, #0x{:x}", dst_reg, src_reg, 0xffu64)),
                        other => {
                            let mask = (1u64 << other).saturating_sub(1);
                            self.emit_line(&format!("and {}, {}, #0x{:x}", dst_reg, src_reg, mask));
                        }
                    }

                    self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                }
            }
            InstKind::IntExtend { value, ty, signed } => {
                // Integer extension to a wider destination type.
                if let Some(result) = &inst.result {
                    let src_loc = locs.value(*value);
                    let src_ty = locs.value_ty(*value);
                    let src_reg = self.load_value_typed(locs, src_loc, src_ty, "x9");
                    let dst = locs.value(result.id);
                    let scratch = if src_reg == "x10" { "x11" } else { "x10" };
                    let dst_ty = locs.value_ty(result.id);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, scratch, "extend", dst_ty);
                    let src_reg = Self::x_reg(&src_reg);
                    let dst_reg = Self::x_reg(&dst_reg);

                    let src_bits = match locs.types.kind(locs.value_ty(*value)) {
                        IrTypeKind::Int { bits, .. } => *bits as u32,
                        _ => 64,
                    };
                    let dst_bits = match locs.types.kind(*ty) {
                        IrTypeKind::Int { bits, .. } => *bits as u32,
                        _ => 64,
                    };

                    if dst_bits <= src_bits {
                        // Treat size-narrowing as a truncation mask.
                        let mask = if dst_bits >= 64 {
                            u64::MAX
                        } else {
                            (1u64 << dst_bits).saturating_sub(1)
                        };
                        self.emit_line(&format!("and {}, {}, #0x{:x}", dst_reg, src_reg, mask));
                    } else if *signed {
                        // Sign-extend by shifting up then arithmetic shifting down.
                        let shift = 64u32.saturating_sub(src_bits);
                        self.emit_line(&format!("lsl {}, {}, #{}", dst_reg, src_reg, shift));
                        self.emit_line(&format!("asr {}, {}, #{}", dst_reg, dst_reg, shift));
                    } else {
                        // Zero-extend by masking to the source bit-width.
                        let mask = if src_bits >= 64 {
                            u64::MAX
                        } else {
                            (1u64 << src_bits).saturating_sub(1)
                        };
                        self.emit_line(&format!("and {}, {}, #0x{:x}", dst_reg, src_reg, mask));
                    }

                    self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                }
            }
            InstKind::Cast { kind, value, ty: _ } => {
                // Pointer/integer bitcasts are modeled as moves.
                if let Some(result) = &inst.result {
                    let src_loc = locs.value(*value);
                    let src_ty = locs.value_ty(*value);
                    let src_reg = self.load_value_typed(locs, src_loc, src_ty, "x9");
                    let dst = locs.value(result.id);
                    let scratch = if src_reg == "x10" { "x11" } else { "x10" };
                    let dst_ty = locs.value_ty(result.id);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, scratch, "cast", dst_ty);

                    match kind {
                        CastKind::PtrToInt | CastKind::IntToPtr | CastKind::PtrToPtr => {
                            self.emit_line(&format!("mov {}, {}", dst_reg, src_reg));
                        }
                    }

                    self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                }
            }
            InstKind::Load { ptr } => {
                // Load through a pointer value into the destination.
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    let ty = result.ty;
                    let addr = if let Some((base, offset)) = locs.index_addr_folds.get(ptr).copied()
                    {
                        let base_loc = locs.value(base);
                        let base_ty = locs.value_ty(base);
                        let base_reg = self.load_value_typed(locs, base_loc, base_ty, "x15");
                        if offset == 0 {
                            base_reg
                        } else {
                            let scratch = if base_reg == "x15" { "x14" } else { "x15" };
                            self.emit_add_imm(scratch, &base_reg, offset);
                            scratch.to_string()
                        }
                    } else {
                        let src = locs.value(*ptr);
                        let src_ty = locs.value_ty(*ptr);
                        self.load_value_typed(locs, src, src_ty, "x15")
                    };
                    if locs.types.is_reg_type(ty) {
                        let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, "x9", "load", ty);
                        let size = locs.layout(ty).size() as u32;
                        self.emit_load_ptr_sized(&dst_reg, &addr, size);
                        self.store_if_needed_typed(locs, dst_slot, &dst_reg, ty);
                    } else {
                        let Location::Stack(slot) = dst else {
                            panic!("ssa codegen: aggregate load needs stack dst, got {:?}", dst);
                        };
                        let size = locs.layout(ty).size() as u32;
                        if size > 0 {
                            let dst_offset = self.stack_offset(slot);
                            self.copy_ptr_to_stack(&addr, dst_offset, size);
                        }
                    }
                }
            }
            InstKind::AddrOfLocal { local } => {
                // Compute the address of a stack local as a pointer value.
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    let dst_ty = locs.value_ty(result.id);
                    // Locals are laid out above spills; offsets are measured from the top.
                    let offset_from_top = locs.local_offset(*local);
                    // Convert the local's offset-from-top into an SP-relative address.
                    let sp_offset = self.layout.saved_base().saturating_sub(offset_from_top);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, "x9", "addr-of-local", dst_ty);
                    self.emit_add_imm(&dst_reg, "sp", sp_offset);
                    self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                }
            }
            InstKind::FieldAddr { base, index } => {
                // Compute base + field offset for a struct field address.
                if let Some(result) = &inst.result {
                    if locs.field_addr_folds.contains_key(&result.id) {
                        return;
                    }
                    let dst = locs.value(result.id);
                    let base_loc = locs.value(*base);
                    // Base is a pointer value; ensure we have it in a register.
                    let base_ty = locs.value_ty(*base);
                    let base_reg = self.load_value_typed(locs, base_loc, base_ty, "x9");
                    let base_ty = locs.value_ty(*base);
                    let elem_ty = match locs.types.kind(base_ty) {
                        IrTypeKind::Ptr { elem } => *elem,
                        other => {
                            panic!("ssa codegen: field addr base must be ptr, got {:?}", other);
                        }
                    };
                    let layout = locs.layout(elem_ty);
                    // Field offsets are precomputed in the pointee layout.
                    let offset = layout.field_offsets().get(*index).copied().unwrap_or(0) as u32;
                    // Add the field byte offset to the base address.
                    let dst_ty = locs.value_ty(result.id);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, "x9", "field-addr", dst_ty);
                    self.emit_add_imm(&dst_reg, &base_reg, offset);
                    self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                }
            }
            InstKind::IndexAddr { base, index } => {
                // Compute base + index * stride for array/slice indexing.
                if let Some(result) = &inst.result {
                    if locs.index_addr_folds.contains_key(&result.id) {
                        return;
                    }
                    let dst = locs.value(result.id);
                    let base_loc = locs.value(*base);
                    // Load base/index into registers for address arithmetic.
                    let base_ty = locs.value_ty(*base);
                    let base_reg = self.load_value_typed(locs, base_loc, base_ty, "x9");

                    let elem_ty = match locs.types.kind(result.ty) {
                        IrTypeKind::Ptr { elem } => *elem,
                        other => {
                            panic!("ssa codegen: index result must be ptr, got {:?}", other);
                        }
                    };
                    let stride = locs.layout(elem_ty).stride() as u32;

                    let dst_ty = locs.value_ty(result.id);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, "x11", "index-addr", dst_ty);

                    // Index 0: collapse to the base address without an add.
                    if locs.const_zero_values.contains(index) {
                        self.emit_line(&format!("mov {}, {}", dst_reg, base_reg));
                        self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                        return;
                    }

                    let index_loc = locs.value(*index);
                    let index_ty = locs.value_ty(*index);
                    let index_reg = self.load_value_typed(locs, index_loc, index_ty, "x10");

                    // Prefer scaled addressing when stride is a power of two.
                    if stride == 0 {
                        // Zero-sized elements collapse to the base address.
                        self.emit_line(&format!("mov {}, {}", dst_reg, base_reg));
                    } else if stride.is_power_of_two() {
                        let shift = stride.trailing_zeros();
                        if shift == 0 {
                            // Stride 1: plain add.
                            self.emit_line(&format!(
                                "add {}, {}, {}",
                                dst_reg, base_reg, index_reg
                            ));
                        } else {
                            // Stride 2^k: use scaled register offset.
                            self.emit_line(&format!(
                                "add {}, {}, {}, lsl #{}",
                                dst_reg, base_reg, index_reg, shift
                            ));
                        }
                    } else {
                        // Non power-of-two stride: multiply then add.
                        self.emit_mov_imm("x11", stride as i128, 64);
                        self.emit_line(&format!("mul x11, {}, x11", index_reg));
                        self.emit_line(&format!("add {}, {}, x11", dst_reg, base_reg));
                    }

                    // Spill the computed address if it doesn't live in a register.
                    self.store_if_needed_typed(locs, dst_slot, &dst_reg, dst_ty);
                }
            }
            InstKind::Store { ptr, value } => {
                // Store a value into a pointer destination.
                let ptr_id = *ptr;
                let value_id = *value;
                let ptr = locs.value(ptr_id);
                let value_loc = locs.value(value_id);
                let value_ty = locs.value_ty(value_id);
                let ptr_ty = locs.value_ty(ptr_id);
                let zero_store = locs.const_zero_values.contains(&value_id)
                    && matches!(
                        locs.types.kind(value_ty),
                        IrTypeKind::Bool | IrTypeKind::Int { .. }
                    );

                // Fast path: fold `field_addr` + `store` so we store directly at base+offset.
                if let Some((base, offset)) = locs.field_addr_folds.get(&ptr_id).copied() {
                    let base_loc = locs.value(base);
                    let base_ty = locs.value_ty(base);
                    let base_reg = self.load_value_typed(locs, base_loc, base_ty, "x15");

                    if locs.types.is_reg_type(value_ty) {
                        // Scalar stores can use scaled immediate offsets.
                        let size = locs.layout(value_ty).size() as u32;
                        if size > 0 {
                            let val = if zero_store {
                                if size <= 4 { "wzr" } else { "xzr" }.to_string()
                            } else {
                                self.load_value_typed(locs, value_loc, value_ty, "x10")
                            };
                            self.emit_store_ptr_sized_offset(&val, &base_reg, offset, size);
                        }
                    } else {
                        // Aggregate stores copy from a stack slot into the folded address.
                        let Location::Stack(slot) = value_loc else {
                            panic!(
                                "ssa codegen: aggregate store needs stack src, got {:?}",
                                value_loc
                            );
                        };
                        let size = locs.layout(value_ty).size() as u32;
                        if size > 0 {
                            if offset == 0 {
                                self.copy_stack_to_ptr(self.stack_offset(slot), &base_reg, size);
                            } else {
                                let scratch = if base_reg == "x15" { "x14" } else { "x15" };
                                self.emit_add_imm(scratch, &base_reg, offset);
                                self.copy_stack_to_ptr(self.stack_offset(slot), scratch, size);
                            }
                        }
                    }
                    return;
                }

                // Fast path: fold `index_addr` with constant-zero index to store directly at base.
                if let Some((base, offset)) = locs.index_addr_folds.get(&ptr_id).copied() {
                    let base_loc = locs.value(base);
                    let base_ty = locs.value_ty(base);
                    let base_reg = self.load_value_typed(locs, base_loc, base_ty, "x15");

                    if locs.types.is_reg_type(value_ty) {
                        let size = locs.layout(value_ty).size() as u32;
                        if size > 0 {
                            let val = if zero_store {
                                if size <= 4 { "wzr" } else { "xzr" }.to_string()
                            } else {
                                self.load_value_typed(locs, value_loc, value_ty, "x10")
                            };
                            self.emit_store_ptr_sized_offset(&val, &base_reg, offset, size);
                        }
                    } else {
                        let Location::Stack(slot) = value_loc else {
                            panic!(
                                "ssa codegen: aggregate store needs stack src, got {:?}",
                                value_loc
                            );
                        };
                        let size = locs.layout(value_ty).size() as u32;
                        if size > 0 {
                            if offset == 0 {
                                self.copy_stack_to_ptr(self.stack_offset(slot), &base_reg, size);
                            } else {
                                let scratch = if base_reg == "x15" { "x14" } else { "x15" };
                                self.emit_add_imm(scratch, &base_reg, offset);
                                self.copy_stack_to_ptr(self.stack_offset(slot), scratch, size);
                            }
                        }
                    }
                    return;
                }

                // General case: load the pointer value then store through it.
                let ptr = self.load_value_typed(locs, ptr, ptr_ty, "x15");
                if locs.types.is_reg_type(value_ty) {
                    // Scalar stores write the loaded register directly.
                    let size = locs.layout(value_ty).size() as u32;
                    if size > 0 {
                        let val = if zero_store {
                            if size <= 4 { "wzr" } else { "xzr" }.to_string()
                        } else {
                            self.load_value_typed(locs, value_loc, value_ty, "x10")
                        };
                        self.emit_store_ptr_sized(&val, &ptr, size);
                    }
                } else {
                    // Aggregate stores copy from a stack slot into the pointer target.
                    let Location::Stack(slot) = value_loc else {
                        panic!(
                            "ssa codegen: aggregate store needs stack src, got {:?}",
                            value_loc
                        );
                    };
                    let size = locs.layout(value_ty).size() as u32;
                    if size > 0 {
                        let src_offset = self.stack_offset(slot);
                        self.copy_stack_to_ptr(src_offset, &ptr, size);
                    }
                }
            }
            InstKind::MemCopy { dst, src, len } => {
                // Lower memcpy as a runtime call: __rt_memcpy(dst, src, len).
                let dst = self.load_value_typed(locs, locs.value(*dst), locs.value_ty(*dst), "x0");
                let src = self.load_value_typed(locs, locs.value(*src), locs.value_ty(*src), "x1");
                let len = self.load_value_typed(locs, locs.value(*len), locs.value_ty(*len), "x2");
                if dst != "x0" {
                    self.emit_line(&format!("mov x0, {}", dst));
                }
                if src != "x1" {
                    self.emit_line(&format!("mov x1, {}", src));
                }
                if len != "x2" {
                    self.emit_line(&format!("mov x2, {}", len));
                }
                self.emit_line(&format!("bl _{}", RuntimeFn::MemCopy.name()));
            }
            InstKind::MemSet { dst, byte, len } => {
                // Lower memset as a runtime call: __rt_memset(dst, len, value).
                let dst = self.load_value_typed(locs, locs.value(*dst), locs.value_ty(*dst), "x0");
                let len = self.load_value_typed(locs, locs.value(*len), locs.value_ty(*len), "x1");
                let byte =
                    self.load_value_typed(locs, locs.value(*byte), locs.value_ty(*byte), "x2");
                if dst != "x0" {
                    self.emit_line(&format!("mov x0, {}", dst));
                }
                if len != "x1" {
                    self.emit_line(&format!("mov x1, {}", len));
                }
                if byte != "x2" {
                    self.emit_line(&format!("mov x2, {}", byte));
                }
                self.emit_line(&format!("bl _{}", RuntimeFn::MemSet.name()));
            }
            InstKind::Call { callee, .. } => match callee {
                // Direct/runtime calls use named symbols; indirect calls use a register.
                Callee::Direct(def_id) => {
                    let name = locs
                        .def_name(*def_id)
                        .map(|name| format!("_{}", name))
                        .unwrap_or_else(|| format!("_fn{}", def_id.0));
                    self.emit_line(&format!("bl {}", name));
                }
                Callee::Runtime(rt) => {
                    self.emit_line(&format!("bl _{}", rt.name()));
                }
                Callee::Value(_) => {
                    let reg = Self::reg_name(INDIRECT_CALL_REG);
                    self.emit_line(&format!("blr {}", reg));
                }
            },
            InstKind::Drop { ptr } => {
                // Drop a pointer value; only string drops are supported in the emitter.
                let ptr_loc = locs.value(*ptr);
                let ptr_ty = locs.value_ty(*ptr);
                let ptr_reg = self.load_value_typed(locs, ptr_loc, ptr_ty, "x0");
                let elem_ty = match locs.types.kind(ptr_ty) {
                    IrTypeKind::Ptr { elem } => *elem,
                    other => {
                        panic!("ssa codegen: unsupported drop ptr {:?}", other);
                    }
                };
                let elem_name = locs.types.get(elem_ty).name.as_deref();
                if elem_name == Some("string") {
                    if ptr_reg != "x0" {
                        self.emit_line(&format!("mov x0, {}", ptr_reg));
                    }
                    self.emit_line(&format!("bl _{}", RuntimeFn::StringDrop.name()));
                } else {
                    panic!("ssa codegen: unsupported drop for {:?}", elem_name);
                }
            }
        }
    }

    fn emit_branch(&mut self, target: CodegenBlockId) {
        self.emit_line(&format!("b {}", self.codegen_label(target)));
    }

    fn emit_cond_branch(
        &mut self,
        cond: ValueId,
        then_target: CodegenBlockId,
        else_target: CodegenBlockId,
        locs: &LocationResolver,
    ) {
        // Conditional branch: jump to then_label if cond != 0.
        let cond_id = cond;
        let cond_loc = locs.value(cond_id);
        let cond_ty = locs.value_ty(cond_id);
        let cond = self.load_value_typed(locs, cond_loc, cond_ty, "x9");
        self.emit_line(&format!(
            "cbnz {}, {}",
            cond,
            self.codegen_label(then_target)
        ));
        self.emit_line(&format!("b {}", self.codegen_label(else_target)));
    }

    fn emit_switch(
        &mut self,
        value: ValueId,
        cases: &[(ConstValue, CodegenBlockId)],
        default_target: CodegenBlockId,
        locs: &LocationResolver,
    ) {
        // Switch lowered as a compare-and-branch chain.
        let value_loc = locs.value(value);
        let value_ty = locs.value_ty(value);
        let value_reg = self.load_value_typed(locs, value_loc, value_ty, "x9");
        let const_reg = if value_reg == "x9" || value_reg == "w9" {
            "x10"
        } else {
            "x9"
        };
        let const_reg = if value_reg.starts_with('w') {
            Self::w_reg(const_reg)
        } else {
            const_reg.to_string()
        };

        for (value, target) in cases {
            let size = Self::scalar_size(locs, value_ty);
            let bits = (size.saturating_mul(8)) as u8;
            self.emit_mov_imm(&const_reg, value.as_int(), bits);
            self.emit_line(&format!("cmp {}, {}", value_reg, const_reg));
            self.emit_line(&format!("b.eq {}", self.codegen_label(*target)));
        }

        self.emit_line(&format!("b {}", self.codegen_label(default_target)));
    }

    fn emit_terminator(&mut self, term: &Terminator, locs: &LocationResolver) {
        match term {
            Terminator::Br { target, .. } => {
                // Unconditional branch to the target block.
                self.emit_branch(CodegenBlockId::Ssa(*target));
            }
            Terminator::CondBr {
                cond,
                then_bb,
                else_bb,
                ..
            } => {
                self.emit_cond_branch(
                    *cond,
                    CodegenBlockId::Ssa(*then_bb),
                    CodegenBlockId::Ssa(*else_bb),
                    locs,
                );
            }
            Terminator::Switch {
                value,
                cases,
                default,
                ..
            } => {
                let case_labels: Vec<(ConstValue, CodegenBlockId)> = cases
                    .iter()
                    .map(|case| (case.value.clone(), CodegenBlockId::Ssa(case.target)))
                    .collect();
                self.emit_switch(*value, &case_labels, CodegenBlockId::Ssa(*default), locs);
            }
            Terminator::Return { value } => {
                // Materialize return value and tear down the stack frame.
                if let Some(value) = value {
                    let ty = locs.value_ty(*value);
                    if needs_sret(locs, ty) {
                        // Indirect return: store the value into the sret pointer (x8).
                        if let Some(offset) = self.saved_reg_offset(PhysReg(8)) {
                            self.emit_ldr_sp(PhysReg(8), offset);
                        }
                        let src_loc = locs.value(*value);
                        match src_loc {
                            Location::Reg(reg) => {
                                self.emit_line(&format!("str {}, [x8]", Self::reg_name(reg)));
                            }
                            Location::Stack(slot) => {
                                let offset = self.stack_offset(slot);
                                let size = locs.layout(ty).size() as u32;
                                self.emit_line("mov x0, x8");
                                self.emit_line(&format!("add x1, sp, #{}", offset));
                                self.emit_mov_imm("x2", size as i128, 64);
                                self.emit_line(&format!("bl _{}", RuntimeFn::MemCopy.name()));
                            }
                            _ => {
                                panic!("ssa codegen: unsupported sret source {:?}", src_loc);
                            }
                        }
                    } else {
                        let src = locs.value(*value);
                        let src = self.load_value_typed(locs, src, ty, "x9");
                        let size = Self::scalar_size(locs, ty);
                        if size > 0 && size <= 4 {
                            let dst = Self::w_reg("x0");
                            self.emit_line(&format!("mov {}, {}", dst, src));
                        } else {
                            self.emit_line(&format!("mov x0, {}", src));
                        }
                    }
                } else {
                    // Unit returns use x0=0 for a stable process exit code.
                    self.emit_line("mov x0, #0");
                }

                // Restore callee-saved registers before tearing down the frame.
                if self.layout.aligned_size > 0 {
                    if !self.callee_saved.is_empty() {
                        let base = self.layout.saved_base();
                        let callee_saved = self.callee_saved.clone();

                        // Restore callee-saved registers in reverse order.
                        let pair_count = callee_saved.len() / 2;

                        // Restore a trailing odd register first.
                        if callee_saved.len() % 2 == 1 {
                            let reg = callee_saved[pair_count * 2];
                            let offset = base + (pair_count as u32) * 16;
                            self.emit_ldr_sp(reg, offset);
                        }

                        // Restore paired registers in reverse pair order.
                        for pair in (0..pair_count).rev() {
                            let idx = pair * 2;
                            let reg0 = callee_saved[idx];
                            let reg1 = callee_saved[idx + 1];
                            let offset = base + (pair as u32) * 16;
                            self.emit_ldp_sp(reg0, reg1, offset);
                        }
                    }
                    self.emit_line(&format!("add sp, sp, #{}", self.layout.aligned_size));
                }

                self.emit_line("ret");
            }
            Terminator::Unreachable => {
                // Emit a trap for unreachable control flow.
                self.emit_line("brk #0");
            }
        }
    }
}

fn format_bytes_as_ascii(bytes: &[u8]) -> Option<String> {
    let text = std::str::from_utf8(bytes).ok()?;
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ' ' => out.push(' '),
            _ if ch.is_ascii_graphic() => out.push(ch),
            _ => return None,
        }
    }
    Some(out)
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
        _ => true,
    }
}
