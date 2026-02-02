use crate::backend::codegen::emitter::{LocationResolver, binop_mnemonic};
use crate::backend::regalloc::Location;
use crate::backend::regalloc::arm64::INDIRECT_CALL_REG;
use crate::ir::{
    BinOp, Callee, CastKind, ConstValue, InstKind, Instruction, IrTypeKind, RuntimeFn, UnOp,
};

use super::{Arm64Emitter, ConstValueExt};

impl Arm64Emitter {
    pub(super) fn emit_inst_impl(&mut self, inst: &Instruction, locs: &LocationResolver) {
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
                            panic!(
                                "backend codegen: aggregate load needs stack dst, got {:?}",
                                dst
                            );
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
                            panic!(
                                "backend codegen: field addr base must be ptr, got {:?}",
                                other
                            );
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
                            panic!("backend codegen: index result must be ptr, got {:?}", other);
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
                                "backend codegen: aggregate store needs stack src, got {:?}",
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
                                "backend codegen: aggregate store needs stack src, got {:?}",
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
                            "backend codegen: aggregate store needs stack src, got {:?}",
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
                        panic!("backend codegen: unsupported drop ptr {:?}", other);
                    }
                };
                let elem_name = locs.types.get(elem_ty).name.as_deref();
                if elem_name == Some("string") {
                    if ptr_reg != "x0" {
                        self.emit_line(&format!("mov x0, {}", ptr_reg));
                    }
                    self.emit_line(&format!("bl _{}", RuntimeFn::StringDrop.name()));
                } else {
                    panic!("backend codegen: unsupported drop for {:?}", elem_name);
                }
            }
        }
    }
}
