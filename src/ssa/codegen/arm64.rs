//! Minimal ARM64 SSA codegen emitter.

use std::fmt::Write;

use crate::regalloc::target::PhysReg;
use crate::ssa::IrTypeKind;
use crate::ssa::codegen::emitter::{CodegenEmitter, LocationResolver, binop_mnemonic};
use crate::ssa::model::ir::{
    Callee, CastKind, CmpOp, ConstValue, GlobalData, InstKind, Instruction, Terminator, UnOp,
};
use crate::ssa::regalloc::moves::MoveOp;
use crate::ssa::regalloc::{Location, StackSlotId};

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
        }
    }

    pub fn finish(self) -> String {
        self.output
    }

    fn reg_name(reg: PhysReg) -> &'static str {
        match reg.0 {
            0 => "x0",
            1 => "x1",
            2 => "x2",
            3 => "x3",
            4 => "x4",
            5 => "x5",
            6 => "x6",
            7 => "x7",
            8 => "x8",
            9 => "x9",
            10 => "x10",
            11 => "x11",
            _ => "x9",
        }
    }

    fn emit_line(&mut self, line: &str) {
        let _ = writeln!(self.output, "  {}", line);
    }

    fn ensure_text(&mut self) {
        if self.section != AsmSection::Text {
            let _ = writeln!(self.output, ".text");
            self.section = AsmSection::Text;
        }
    }

    fn ensure_data(&mut self) {
        if self.section != AsmSection::Data {
            let _ = writeln!(self.output, ".data");
            self.section = AsmSection::Data;
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

    fn load_value(&mut self, loc: Location, scratch: &str) -> String {
        match loc {
            Location::Reg(reg) => Self::reg_name(reg).to_string(),
            Location::Stack(slot) => {
                let offset = self.stack_offset(slot);
                self.emit_line(&format!("ldr {}, [sp, #{}]", scratch, offset));
                scratch.to_string()
            }
            Location::IncomingArg(offset) => {
                let offset = self.layout.incoming_offset(offset);
                self.emit_line(&format!("ldr {}, [sp, #{}]", scratch, offset));
                scratch.to_string()
            }
            Location::OutgoingArg(offset) => {
                let offset = self.layout.outgoing_offset(offset);
                self.emit_line(&format!("ldr {}, [sp, #{}]", scratch, offset));
                scratch.to_string()
            }
            Location::StackAddr(slot) => {
                let offset = self.stack_offset(slot);
                self.emit_line(&format!("add {}, sp, #{}", scratch, offset));
                scratch.to_string()
            }
        }
    }

    fn value_dst(
        &mut self,
        dst: Location,
        scratch: &str,
        context: &str,
    ) -> (String, Option<StackSlotId>) {
        match dst {
            Location::Reg(reg) => (Self::reg_name(reg).to_string(), None),
            Location::Stack(slot) => (scratch.to_string(), Some(slot)),
            other => {
                panic!("ssa codegen: invalid {} dst {:?}", context, other);
            }
        }
    }

    fn store_if_needed(&mut self, slot: Option<StackSlotId>, reg: &str) {
        if let Some(slot) = slot {
            self.emit_line(&format!("str {}, [sp, #{}]", reg, self.stack_offset(slot)));
        }
    }

    fn stack_offset(&self, slot: crate::ssa::regalloc::StackSlotId) -> u32 {
        // Stack slots live below the callee-saved area at the top of the frame.
        self.layout.slot_offset(slot)
    }
}

impl CodegenEmitter for Arm64Emitter {
    fn emit_global(&mut self, global: &GlobalData) {
        self.ensure_data();

        // Emit a stable label for the global payload.
        let label = format!("g{}", global.id.0);

        // Align the data so references can use natural alignment.
        let align = global.align.max(1);
        if align.is_power_of_two() {
            let align_log2 = align.trailing_zeros();
            self.emit_line(&format!(".p2align {}", align_log2));
        } else {
            self.emit_line(&format!(".balign {}", align));
        }

        let _ = writeln!(self.output, "{}:", label);

        // Emit bytes verbatim; empty globals still reserve a byte.
        if global.bytes.is_empty() {
            self.emit_line(".byte 0");
        } else {
            let data = global
                .bytes
                .iter()
                .map(|b| b.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            self.emit_line(&format!(".byte {}", data));
        }
    }

    fn begin_function(&mut self, name: &str, frame_size: u32, callee_saved: &[PhysReg]) {
        self.ensure_text();
        // Capture the callee-saved set so we can restore it in the epilogue.
        self.callee_saved = callee_saved.to_vec();
        let saved_size = (callee_saved.len() as u32) * 8;
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
                self.emit_line(&format!(
                    "stp {}, {}, [sp, #{}]",
                    Self::reg_name(reg0),
                    Self::reg_name(reg1),
                    offset
                ));
            }

            // Handle a trailing register if the count is odd.
            if callee_saved.len() % 2 == 1 {
                let reg = callee_saved[pair_count * 2];
                let offset = base + (pair_count as u32) * 16;
                self.emit_line(&format!("str {}, [sp, #{}]", Self::reg_name(reg), offset));
            }
        }
    }

    fn begin_block(&mut self, label: &str) {
        let _ = writeln!(self.output, "{}:", label);
    }

    fn emit_moves(&mut self, moves: &[MoveOp]) {
        for mov in moves {
            match (mov.src, mov.dst) {
                (Location::Reg(src), Location::Reg(dst)) => {
                    self.emit_line(&format!(
                        "mov {}, {}",
                        Self::reg_name(dst),
                        Self::reg_name(src)
                    ));
                }
                (Location::Reg(src), Location::Stack(dst)) => {
                    self.emit_line(&format!(
                        "str {}, [sp, #{}]",
                        Self::reg_name(src),
                        self.stack_offset(dst)
                    ));
                }
                (Location::Stack(src), Location::Reg(dst)) => {
                    self.emit_line(&format!(
                        "ldr {}, [sp, #{}]",
                        Self::reg_name(dst),
                        self.stack_offset(src)
                    ));
                }
                (Location::Stack(src), Location::Stack(dst)) => {
                    self.emit_line(&format!("ldr x9, [sp, #{}]", self.stack_offset(src)));
                    self.emit_line(&format!("str x9, [sp, #{}]", self.stack_offset(dst)));
                }
                (Location::Reg(src), Location::OutgoingArg(offset)) => {
                    let offset = self.layout.outgoing_offset(offset);
                    self.emit_line(&format!("str {}, [sp, #{}]", Self::reg_name(src), offset));
                }
                (Location::Stack(src), Location::OutgoingArg(offset)) => {
                    let offset = self.layout.outgoing_offset(offset);
                    self.emit_line(&format!("ldr x9, [sp, #{}]", self.stack_offset(src)));
                    self.emit_line(&format!("str x9, [sp, #{}]", offset));
                }
                (Location::IncomingArg(offset), Location::Reg(dst)) => {
                    let offset = self.layout.incoming_offset(offset);
                    self.emit_line(&format!("ldr {}, [sp, #{}]", Self::reg_name(dst), offset));
                }
                (Location::IncomingArg(offset), Location::Stack(dst)) => {
                    let offset = self.layout.incoming_offset(offset);
                    self.emit_line(&format!("ldr x9, [sp, #{}]", offset));
                    self.emit_line(&format!("str x9, [sp, #{}]", self.stack_offset(dst)));
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
                    let dst = locs.value(result.id);
                    match value {
                        ConstValue::Int { .. } | ConstValue::Bool(_) | ConstValue::Unit => {
                            let (dst_reg, dst_slot) = self.value_dst(dst, "x9", "const");
                            self.emit_line(&format!("mov {}, #{}", dst_reg, value.as_int()));
                            self.store_if_needed(dst_slot, &dst_reg);
                        }
                        ConstValue::FuncAddr { def } => {
                            let label = format!("fn{}", def.0);
                            let (dst_reg, dst_slot) = self.value_dst(dst, "x9", "const func");
                            self.emit_line(&format!("adrp {}, {}", dst_reg, label));
                            self.emit_line(&format!(
                                "add {}, {}, :lo12:{}",
                                dst_reg, dst_reg, label
                            ));
                            self.store_if_needed(dst_slot, &dst_reg);
                        }
                        ConstValue::GlobalAddr { id } => {
                            // TODO: ensure SSA codegen emits global data labels.
                            let label = format!("g{}", id.0);
                            let (dst_reg, dst_slot) = self.value_dst(dst, "x9", "const global");
                            self.emit_line(&format!("adrp {}, {}", dst_reg, label));
                            self.emit_line(&format!(
                                "add {}, {}, :lo12:{}",
                                dst_reg, dst_reg, label
                            ));
                            self.store_if_needed(dst_slot, &dst_reg);
                        }
                    }
                }
            }
            InstKind::BinOp { op, lhs, rhs } => {
                // Binary arithmetic/logical ops on integer registers.
                if let Some(result) = &inst.result {
                    let lhs = locs.value(*lhs);
                    let rhs = locs.value(*rhs);
                    let lhs_reg = self.load_value(lhs, "x9");
                    let rhs_reg = self.load_value(rhs, "x10");
                    let dst = locs.value(result.id);
                    let (dst_reg, dst_slot) = self.value_dst(dst, "x11", "binop");
                    let op = binop_mnemonic(*op);
                    self.emit_line(&format!("{} {}, {}, {}", op, dst_reg, lhs_reg, rhs_reg));
                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::UnOp { op, value } => {
                // Unary ops over integer values.
                if let Some(result) = &inst.result {
                    let src_loc = locs.value(*value);
                    let src_reg = self.load_value(src_loc, "x9");
                    let dst = locs.value(result.id);
                    let scratch = if src_reg == "x10" { "x11" } else { "x10" };
                    let (dst_reg, dst_slot) = self.value_dst(dst, scratch, "unop");

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

                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::Cmp { op, lhs, rhs } => {
                // Compare and produce a boolean via cset.
                if let Some(result) = &inst.result {
                    let lhs = locs.value(*lhs);
                    let rhs = locs.value(*rhs);
                    let lhs_reg = self.load_value(lhs, "x9");
                    let rhs_reg = self.load_value(rhs, if lhs_reg == "x9" { "x10" } else { "x9" });

                    self.emit_line(&format!("cmp {}, {}", lhs_reg, rhs_reg));
                    let cond = Self::cmp_condition(*op);
                    let dst = locs.value(result.id);
                    let (dst_reg, dst_slot) = self.value_dst(dst, "x9", "cmp");
                    self.emit_line(&format!("cset {}, {}", dst_reg, cond));
                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::IntTrunc { value, ty } => {
                // Integer truncation via masking to the destination bit-width.
                if let Some(result) = &inst.result {
                    let src_loc = locs.value(*value);
                    let src_reg = self.load_value(src_loc, "x9");
                    let dst = locs.value(result.id);
                    let scratch = if src_reg == "x10" { "x11" } else { "x10" };
                    let (dst_reg, dst_slot) = self.value_dst(dst, scratch, "trunc");

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

                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::IntExtend { value, ty, signed } => {
                // Integer extension to a wider destination type.
                if let Some(result) = &inst.result {
                    let src_loc = locs.value(*value);
                    let src_reg = self.load_value(src_loc, "x9");
                    let dst = locs.value(result.id);
                    let scratch = if src_reg == "x10" { "x11" } else { "x10" };
                    let (dst_reg, dst_slot) = self.value_dst(dst, scratch, "extend");

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

                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::Cast { kind, value, ty: _ } => {
                // Pointer/integer bitcasts are modeled as moves.
                if let Some(result) = &inst.result {
                    let src_loc = locs.value(*value);
                    let src_reg = self.load_value(src_loc, "x9");
                    let dst = locs.value(result.id);
                    let scratch = if src_reg == "x10" { "x11" } else { "x10" };
                    let (dst_reg, dst_slot) = self.value_dst(dst, scratch, "cast");

                    match kind {
                        CastKind::PtrToInt | CastKind::IntToPtr => {
                            self.emit_line(&format!("mov {}, {}", dst_reg, src_reg));
                        }
                    }

                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::Load { ptr } => {
                // Load through a pointer value into the destination.
                let src = locs.value(*ptr);
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    let src = self.load_value(src, "x9");
                    let (dst_reg, dst_slot) = self.value_dst(dst, "x9", "load");
                    self.emit_line(&format!("ldr {}, [{}]", dst_reg, src));
                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::AddrOfLocal { local } => {
                // Compute the address of a stack local as a pointer value.
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    // Locals are laid out above spills; offsets are measured from the top.
                    let offset_from_top = locs.local_offset(*local);
                    // Convert the local's offset-from-top into an SP-relative address.
                    let sp_offset = self.layout.saved_base().saturating_sub(offset_from_top);
                    let (dst_reg, dst_slot) = self.value_dst(dst, "x9", "addr-of-local");
                    self.emit_line(&format!("add {}, sp, #{}", dst_reg, sp_offset));
                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::FieldAddr { base, index } => {
                // Compute base + field offset for a struct field address.
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    let base_loc = locs.value(*base);
                    // Base is a pointer value; ensure we have it in a register.
                    let base_reg = self.load_value(base_loc, "x9");
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
                    let (dst_reg, dst_slot) = self.value_dst(dst, "x9", "field-addr");
                    self.emit_line(&format!("add {}, {}, #{}", dst_reg, base_reg, offset));
                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::IndexAddr { base, index } => {
                // Compute base + index * stride for array/slice indexing.
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    let base_loc = locs.value(*base);
                    let index_loc = locs.value(*index);
                    // Load base/index into registers for address arithmetic.
                    let base_reg = self.load_value(base_loc, "x9");
                    let index_reg = self.load_value(index_loc, "x10");

                    let elem_ty = match locs.types.kind(result.ty) {
                        IrTypeKind::Ptr { elem } => *elem,
                        other => {
                            panic!("ssa codegen: index result must be ptr, got {:?}", other);
                        }
                    };
                    let stride = locs.layout(elem_ty).stride() as u32;

                    let (dst_reg, dst_slot) = self.value_dst(dst, "x11", "index-addr");

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
                        self.emit_line(&format!("mov x11, #{}", stride));
                        self.emit_line(&format!("mul x11, {}, x11", index_reg));
                        self.emit_line(&format!("add {}, {}, x11", dst_reg, base_reg));
                    }

                    // Spill the computed address if it doesn't live in a register.
                    self.store_if_needed(dst_slot, &dst_reg);
                }
            }
            InstKind::Store { ptr, value } => {
                // Store a value into a pointer destination.
                let ptr = locs.value(*ptr);
                let value = locs.value(*value);
                let ptr = self.load_value(ptr, "x9");
                let val = self.load_value(value, "x10");
                self.emit_line(&format!("str {}, [{}]", val, ptr));
            }
            InstKind::MemCopy { dst, src, len } => {
                // Lower memcpy as a runtime call: __rt_memcpy(dst, src, len).
                let dst = self.load_value(locs.value(*dst), "x0");
                let src = self.load_value(locs.value(*src), "x1");
                let len = self.load_value(locs.value(*len), "x2");
                if dst != "x0" {
                    self.emit_line(&format!("mov x0, {}", dst));
                }
                if src != "x1" {
                    self.emit_line(&format!("mov x1, {}", src));
                }
                if len != "x2" {
                    self.emit_line(&format!("mov x2, {}", len));
                }
                self.emit_line("bl __rt_memcpy");
            }
            InstKind::MemSet { dst, byte, len } => {
                // Lower memset as a runtime call: __rt_memset(dst, byte, len).
                let dst = self.load_value(locs.value(*dst), "x0");
                let byte = self.load_value(locs.value(*byte), "x1");
                let len = self.load_value(locs.value(*len), "x2");
                if dst != "x0" {
                    self.emit_line(&format!("mov x0, {}", dst));
                }
                if byte != "x1" {
                    self.emit_line(&format!("mov x1, {}", byte));
                }
                if len != "x2" {
                    self.emit_line(&format!("mov x2, {}", len));
                }
                self.emit_line("bl __rt_memset");
            }
            InstKind::Call { callee, .. } => match callee {
                // Direct/runtime calls use named symbols; indirect calls use a register.
                Callee::Direct(def_id) => {
                    self.emit_line(&format!("bl fn{}", def_id.0));
                }
                Callee::Runtime(rt) => {
                    self.emit_line(&format!("bl {}", rt.name()));
                }
                Callee::Value(value) => {
                    let callee = locs.value(*value);
                    let reg = self.load_value(callee, "x9");
                    self.emit_line(&format!("blr {}", reg));
                }
            },
            InstKind::Drop { ptr } => {
                // Drop a pointer value; only string drops are supported in the emitter.
                let ptr_loc = locs.value(*ptr);
                let ptr_reg = self.load_value(ptr_loc, "x0");
                let ptr_ty = locs.value_ty(*ptr);
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
                    self.emit_line("bl __rt_string_drop");
                } else {
                    panic!("ssa codegen: unsupported drop for {:?}", elem_name);
                }
            }
        }
    }

    fn emit_terminator(&mut self, term: &Terminator, locs: &LocationResolver) {
        match term {
            Terminator::Br { target, .. } => {
                // Unconditional branch to the target block.
                self.emit_line(&format!("b bb{}", target.0));
            }
            Terminator::CondBr {
                cond,
                then_bb,
                else_bb,
                ..
            } => {
                // Conditional branch: jump to then_bb if cond != 0.
                let cond = locs.value(*cond);
                let cond = self.load_value(cond, "x9");
                self.emit_line(&format!("cbnz {}, bb{}", cond, then_bb.0));
                self.emit_line(&format!("b bb{}", else_bb.0));
            }
            Terminator::Switch {
                value,
                cases,
                default,
                ..
            } => {
                // Switch lowered as a compare-and-branch chain.
                let value_loc = locs.value(*value);
                let value_reg = self.load_value(value_loc, "x9");
                let const_reg = if value_reg == "x9" { "x10" } else { "x9" };

                for case in cases {
                    self.emit_line(&format!("mov {}, #{}", const_reg, case.value.as_int()));
                    self.emit_line(&format!("cmp {}, {}", value_reg, const_reg));
                    self.emit_line(&format!("b.eq bb{}", case.target.0));
                }

                self.emit_line(&format!("b bb{}", default.0));
            }
            Terminator::Return { value } => {
                // Materialize return value and tear down the stack frame.
                if let Some(value) = value {
                    let ty = locs.value_ty(*value);
                    if needs_sret(locs, ty) {
                        // Indirect return: store the value into the sret pointer (x8).
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
                                self.emit_line(&format!("mov x2, #{}", size));
                                self.emit_line("bl __rt_memcpy");
                            }
                            _ => {
                                panic!("ssa codegen: unsupported sret source {:?}", src_loc);
                            }
                        }
                    } else {
                        let src = locs.value(*value);
                        let src = self.load_value(src, "x9");
                        self.emit_line(&format!("mov x0, {}", src));
                    }
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
                            self.emit_line(&format!(
                                "ldr {}, [sp, #{}]",
                                Self::reg_name(reg),
                                offset
                            ));
                        }

                        // Restore paired registers in reverse pair order.
                        for pair in (0..pair_count).rev() {
                            let idx = pair * 2;
                            let reg0 = callee_saved[idx];
                            let reg1 = callee_saved[idx + 1];
                            let offset = base + (pair as u32) * 16;
                            self.emit_line(&format!(
                                "ldp {}, {}, [sp, #{}]",
                                Self::reg_name(reg0),
                                Self::reg_name(reg1),
                                offset
                            ));
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
