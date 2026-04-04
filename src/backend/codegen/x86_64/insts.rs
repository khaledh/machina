use crate::backend::regalloc::Location;
use crate::backend::regalloc::target::PhysReg;
use crate::backend::regalloc::x86_64::{INDIRECT_CALL_REG, X86_64Reg, phys};
use crate::ir::{
    BinOp, Callee, CastKind, ConstValue, InstKind, Instruction, IrTypeId, IrTypeKind, RuntimeFn,
    UnOp, ValueId,
};

use super::{ConstValueExt, X86_64Emitter};
use crate::backend::codegen::emitter::LocationResolver;

const RDX: PhysReg = phys(X86_64Reg::Rdx);
const RCX: PhysReg = phys(X86_64Reg::Rcx);
const RSI: PhysReg = phys(X86_64Reg::Rsi);
const RDI: PhysReg = phys(X86_64Reg::Rdi);
const R10: PhysReg = phys(X86_64Reg::R10);
const R11: PhysReg = phys(X86_64Reg::R11);

impl X86_64Emitter {
    pub(super) fn emit_inst_impl(&mut self, inst: &Instruction, locs: &LocationResolver) {
        match &inst.kind {
            InstKind::Const { value } => self.emit_const(inst, locs, value),
            InstKind::BinOp { op, lhs, rhs } => self.emit_binop(inst, locs, *op, *lhs, *rhs),
            InstKind::UnOp { op, value } => self.emit_unop(inst, locs, *op, *value),
            InstKind::Cmp { op, lhs, rhs } => self.emit_cmp(inst, locs, *op, *lhs, *rhs),
            InstKind::IntTrunc { value, ty } => self.emit_int_trunc(inst, locs, *value, *ty),
            InstKind::IntExtend { value, ty, signed } => {
                self.emit_int_extend(inst, locs, *value, *ty, *signed)
            }
            InstKind::Cast { kind, value, .. } => self.emit_cast(inst, locs, kind.clone(), *value),
            InstKind::Load { ptr } => self.emit_load(inst, locs, *ptr),
            InstKind::AddrOfLocal { local } => {
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    let dst_ty = locs.value_ty(result.id);
                    let offset_from_top = locs.local_offset(*local);
                    let sp_offset = self.layout.aligned_size.saturating_sub(offset_from_top);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, R10, "addr-of-local", dst_ty);
                    self.emit_lea_rsp_offset(dst_reg, sp_offset);
                    self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
                }
            }
            InstKind::FieldAddr { base, index } => self.emit_field_addr(inst, locs, *base, *index),
            InstKind::IndexAddr { base, index } => self.emit_index_addr(inst, locs, *base, *index),
            InstKind::Store { ptr, value } => self.emit_store(locs, *ptr, *value),
            InstKind::MemCopy { dst, src, len } => {
                let dst = self.load_value_typed(locs, locs.value(*dst), locs.value_ty(*dst), RDI);
                let src = self.load_value_typed(locs, locs.value(*src), locs.value_ty(*src), RSI);
                let len = self.load_value_typed(locs, locs.value(*len), locs.value_ty(*len), RDX);
                if dst != "%rdi" {
                    self.emit_move_bits(dst, "%rdi", 64);
                }
                if src != "%rsi" {
                    self.emit_move_bits(src, "%rsi", 64);
                }
                if len != "%rdx" && len != "%edx" {
                    self.emit_move_bits(len, "%rdx", 64);
                }
                self.emit_line(&format!(
                    "call {}",
                    self.mangle_symbol(RuntimeFn::MemCopy.name())
                ));
            }
            InstKind::MemSet { dst, byte, len } => {
                let dst = self.load_value_typed(locs, locs.value(*dst), locs.value_ty(*dst), RDI);
                let len = self.load_value_typed(locs, locs.value(*len), locs.value_ty(*len), RSI);
                let byte =
                    self.load_value_typed(locs, locs.value(*byte), locs.value_ty(*byte), RDX);
                if dst != "%rdi" {
                    self.emit_move_bits(dst, "%rdi", 64);
                }
                if len != "%rsi" && len != "%esi" {
                    self.emit_move_bits(len, "%rsi", 64);
                }
                if byte != "%rdx" && byte != "%edx" {
                    self.emit_move_bits(byte, "%rdx", 64);
                }
                self.emit_line(&format!(
                    "call {}",
                    self.mangle_symbol(RuntimeFn::MemSet.name())
                ));
            }
            InstKind::Call { callee, args } => match callee {
                Callee::Direct(def_id) => {
                    let name = locs
                        .def_name(*def_id)
                        .map(|name| self.mangle_symbol(name))
                        .unwrap_or_else(|| {
                            format!("{}fn{}", self.target.symbol_prefix(), def_id.0)
                        });
                    self.emit_line(&format!("call {}", name));
                }
                Callee::Runtime(rt) => match rt {
                    RuntimeFn::MemCopy if self.is_bare() => {
                        self.emit_bare_memcpy_call(locs, args);
                    }
                    RuntimeFn::Trap if self.is_bare() => {
                        // Bare targets lower traps directly in `unreachable`.
                    }
                    _ => self.emit_line(&format!("call {}", self.mangle_symbol(rt.name()))),
                },
                Callee::Value(_) => {
                    self.emit_line(&format!(
                        "call *{}",
                        X86_64Emitter::reg(64, INDIRECT_CALL_REG)
                    ));
                }
            },
            InstKind::Drop { ptr } => {
                let ptr_loc = locs.value(*ptr);
                let ptr_ty = locs.value_ty(*ptr);
                let ptr_reg = self.load_value_typed(locs, ptr_loc, ptr_ty, RDI);
                let elem_ty = match locs.types.kind(ptr_ty) {
                    IrTypeKind::Ptr { elem } => *elem,
                    other => panic!("backend codegen: unsupported drop ptr {:?}", other),
                };
                let elem_name = locs.types.get(elem_ty).name.as_deref();
                if elem_name == Some("string") {
                    if ptr_reg != "%rdi" {
                        self.emit_line(&format!("movq {}, %rdi", ptr_reg));
                    }
                    self.emit_line(&format!(
                        "call {}",
                        self.mangle_symbol(RuntimeFn::StringDrop.name())
                    ));
                } else {
                    panic!("backend codegen: unsupported drop for {:?}", elem_name);
                }
            }
        }
    }

    fn emit_bare_memcpy_call(&mut self, locs: &LocationResolver, args: &[ValueId]) {
        let [dst, src, len] = args else {
            panic!("backend codegen: runtime memcpy expects three arguments");
        };

        let dst = self.load_value_typed(locs, locs.value(*dst), locs.value_ty(*dst), RDI);
        let src = self.load_value_typed(locs, locs.value(*src), locs.value_ty(*src), RSI);
        let len = self.load_value_typed(locs, locs.value(*len), locs.value_ty(*len), RCX);

        if dst != "%rdi" {
            self.emit_move_bits(dst, "%rdi", 64);
        }
        if src != "%rsi" {
            self.emit_move_bits(src, "%rsi", 64);
        }
        if len != "%rcx" && len != "%ecx" {
            self.emit_move_bits(len, "%rcx", 64);
        }

        self.emit_line("cld");
        self.emit_line("rep movsb");
    }

    fn emit_const(&mut self, inst: &Instruction, locs: &LocationResolver, value: &ConstValue) {
        if let Some(result) = &inst.result {
            let dst = locs.value(result.id);
            let dst_ty = locs.value_ty(result.id);
            match value {
                ConstValue::Int { .. } | ConstValue::Bool(_) | ConstValue::Unit => {
                    let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R10, "const", dst_ty);
                    let size = X86_64Emitter::scalar_size(locs, dst_ty);
                    let bits = if size == 0 {
                        32
                    } else if size <= 4 {
                        32
                    } else {
                        64
                    };
                    self.emit_mov_imm(dst_reg, value.as_int(), bits);
                    self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
                }
                ConstValue::FuncAddr { def } => {
                    let label = locs
                        .def_name(*def)
                        .map(|name| self.mangle_symbol(name))
                        .unwrap_or_else(|| format!("{}fn{}", self.target.symbol_prefix(), def.0));
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, R10, "const func", dst_ty);
                    self.emit_line(&format!("leaq {}(%rip), {}", label, dst_reg));
                    self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
                }
                ConstValue::GlobalAddr { id } => {
                    let label = self.global_label(*id);
                    let (dst_reg, dst_slot) =
                        self.value_dst_typed(locs, dst, R10, "const global", dst_ty);
                    self.emit_line(&format!("leaq {}(%rip), {}", label, dst_reg));
                    self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
                }
            }
        }
    }

    fn emit_binop(
        &mut self,
        inst: &Instruction,
        locs: &LocationResolver,
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
    ) {
        if let Some(result) = &inst.result {
            let lhs_loc = locs.value(lhs);
            let rhs_loc = locs.value(rhs);
            let lhs_ty = locs.value_ty(lhs);
            let rhs_ty = locs.value_ty(rhs);
            let lhs_reg = self.load_value_typed(locs, lhs_loc, lhs_ty, R10);
            let rhs_reg = self.load_value_typed(locs, rhs_loc, rhs_ty, R11);
            let dst = locs.value(result.id);
            let dst_ty = locs.value_ty(result.id);
            let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R10, "binop", dst_ty);
            let size = X86_64Emitter::scalar_size(locs, dst_ty);
            let bits = if size <= 4 { 32 } else { 64 };

            match op {
                BinOp::Div | BinOp::Mod => {
                    if lhs_reg != "%rax" {
                        self.emit_line(&format!("movq {}, %rax", lhs_reg));
                    }
                    self.emit_line("cqto");
                    let divisor = if rhs_reg == "%rax" || rhs_reg == "%rdx" {
                        self.emit_move_bits(rhs_reg, "%r11", 64);
                        "%r11"
                    } else {
                        rhs_reg
                    };
                    self.emit_line(&format!("idivq {}", divisor));
                    let src = if matches!(op, BinOp::Div) {
                        "%rax"
                    } else {
                        "%rdx"
                    };
                    if dst_reg != src {
                        self.emit_line(&format!("movq {}, {}", src, dst_reg));
                    }
                }
                BinOp::Shl | BinOp::Shr => {
                    let work_reg = if dst_reg == "%ecx" || dst_reg == "%rcx" {
                        if bits == 32 { "%r11d" } else { "%r11" }
                    } else {
                        dst_reg
                    };
                    self.emit_move_bits(lhs_reg, work_reg, bits);
                    if rhs_reg != "%ecx" && rhs_reg != "%rcx" {
                        self.emit_move_bits(rhs_reg, "%ecx", 32);
                    }
                    let op = match op {
                        BinOp::Shl => {
                            if bits == 32 {
                                "shll"
                            } else {
                                "shlq"
                            }
                        }
                        BinOp::Shr => {
                            if bits == 32 {
                                "shrl"
                            } else {
                                "shrq"
                            }
                        }
                        _ => unreachable!(),
                    };
                    self.emit_line(&format!("{} %cl, {}", op, work_reg));
                    self.emit_move_bits(work_reg, dst_reg, bits);
                }
                _ => {
                    self.emit_move_bits(lhs_reg, dst_reg, bits);
                    let op = match op {
                        BinOp::Add => {
                            if bits == 32 {
                                "addl"
                            } else {
                                "addq"
                            }
                        }
                        BinOp::Sub => {
                            if bits == 32 {
                                "subl"
                            } else {
                                "subq"
                            }
                        }
                        BinOp::Mul => {
                            if bits == 32 {
                                "imull"
                            } else {
                                "imulq"
                            }
                        }
                        BinOp::And => {
                            if bits == 32 {
                                "andl"
                            } else {
                                "andq"
                            }
                        }
                        BinOp::Or => {
                            if bits == 32 {
                                "orl"
                            } else {
                                "orq"
                            }
                        }
                        BinOp::Xor => {
                            if bits == 32 {
                                "xorl"
                            } else {
                                "xorq"
                            }
                        }
                        _ => unreachable!(),
                    };
                    self.emit_line(&format!("{} {}, {}", op, rhs_reg, dst_reg));
                }
            }
            self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
        }
    }

    fn emit_unop(&mut self, inst: &Instruction, locs: &LocationResolver, op: UnOp, value: ValueId) {
        if let Some(result) = &inst.result {
            let src_loc = locs.value(value);
            let src_ty = locs.value_ty(value);
            let src_reg = self.load_value_typed(locs, src_loc, src_ty, R10);
            let dst = locs.value(result.id);
            let dst_ty = locs.value_ty(result.id);
            let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R11, "unop", dst_ty);
            let size = X86_64Emitter::scalar_size(locs, dst_ty);
            let bits = if size <= 4 { 32 } else { 64 };
            if dst_reg != src_reg {
                self.emit_line(&format!(
                    "mov{} {}, {}",
                    if bits == 32 { 'l' } else { 'q' },
                    src_reg,
                    dst_reg
                ));
            }
            match op {
                UnOp::Neg => self.emit_line(&format!(
                    "neg{} {}",
                    if bits == 32 { 'l' } else { 'q' },
                    dst_reg
                )),
                UnOp::BitNot => self.emit_line(&format!(
                    "not{} {}",
                    if bits == 32 { 'l' } else { 'q' },
                    dst_reg
                )),
                UnOp::Not => {
                    self.emit_line(&format!(
                        "test{} {}, {}",
                        if bits == 32 { 'l' } else { 'q' },
                        dst_reg,
                        dst_reg
                    ));
                    self.emit_line("sete %r11b");
                    self.emit_line(&format!("movzbl %r11b, {}", dst_reg));
                }
            }
            self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
        }
    }

    fn emit_cmp(
        &mut self,
        inst: &Instruction,
        locs: &LocationResolver,
        op: crate::ir::CmpOp,
        lhs: ValueId,
        rhs: ValueId,
    ) {
        if let Some(result) = &inst.result {
            let lhs_reg = self.load_value_typed(locs, locs.value(lhs), locs.value_ty(lhs), R10);
            let rhs_reg = self.load_value_typed(locs, locs.value(rhs), locs.value_ty(rhs), R11);
            let width = Self::bits_for_size(
                (locs.layout(locs.value_ty(lhs)).size() as u32)
                    .max(locs.layout(locs.value_ty(rhs)).size() as u32),
            );
            self.emit_line(&format!(
                "cmp{} {}, {}",
                Self::suffix_for_bits(width),
                Self::operand_as_bits(rhs_reg, width),
                Self::operand_as_bits(lhs_reg, width)
            ));
            self.emit_line(&format!("set{} %r11b", X86_64Emitter::cmp_condition(op)));
            let dst = locs.value(result.id);
            let dst_ty = locs.value_ty(result.id);
            let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R10, "cmp", dst_ty);
            self.emit_line(&format!("movzbl %r11b, {}", dst_reg));
            self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
        }
    }

    fn emit_int_trunc(
        &mut self,
        inst: &Instruction,
        locs: &LocationResolver,
        value: ValueId,
        ty: IrTypeId,
    ) {
        if let Some(result) = &inst.result {
            let src_reg = self.load_value_typed(locs, locs.value(value), locs.value_ty(value), R10);
            let dst = locs.value(result.id);
            let dst_ty = locs.value_ty(result.id);
            let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R11, "trunc", dst_ty);
            let dst_bits = match locs.types.kind(ty) {
                IrTypeKind::Int { bits, .. } => *bits as u32,
                _ => 64,
            };
            match dst_bits {
                64 => self.emit_move_bits(src_reg, dst_reg, 64),
                32 => self.emit_move_bits(src_reg, dst_reg, 32),
                16 => {
                    self.emit_move_bits(src_reg, dst_reg, 32);
                    self.emit_line(&format!("andl $0xffff, {}", dst_reg));
                }
                8 => {
                    self.emit_move_bits(src_reg, dst_reg, 32);
                    self.emit_line(&format!("andl $0xff, {}", dst_reg));
                }
                other => {
                    self.emit_move_bits(src_reg, dst_reg, 64);
                    let mask = (1u64 << other).saturating_sub(1);
                    self.emit_line(&format!("andq ${:#x}, {}", mask, dst_reg));
                }
            }
            self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
        }
    }

    fn emit_int_extend(
        &mut self,
        inst: &Instruction,
        locs: &LocationResolver,
        value: ValueId,
        ty: IrTypeId,
        signed: bool,
    ) {
        if let Some(result) = &inst.result {
            let src_ty = locs.value_ty(value);
            let src_bits = match locs.types.kind(src_ty) {
                IrTypeKind::Int { bits, .. } => *bits as u32,
                _ => 64,
            };
            let dst_bits = match locs.types.kind(ty) {
                IrTypeKind::Int { bits, .. } => *bits as u32,
                _ => 64,
            };
            let src_reg = self.load_value_typed(locs, locs.value(value), src_ty, R10);
            let dst = locs.value(result.id);
            let dst_ty = locs.value_ty(result.id);
            let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R11, "extend", dst_ty);
            if dst_bits <= src_bits {
                self.emit_int_trunc(inst, locs, value, ty);
                return;
            }
            match (src_bits, signed) {
                (8, true) => self.emit_line(&format!(
                    "movsbq {}, {}",
                    Self::operand_as_bits(src_reg, 8),
                    Self::operand_as_bits(dst_reg, 64)
                )),
                (16, true) => self.emit_line(&format!(
                    "movswq {}, {}",
                    Self::operand_as_bits(src_reg, 16),
                    Self::operand_as_bits(dst_reg, 64)
                )),
                (32, true) => self.emit_line(&format!(
                    "movslq {}, {}",
                    Self::operand_as_bits(src_reg, 32),
                    Self::operand_as_bits(dst_reg, 64)
                )),
                _ => self.emit_move_bits(src_reg, dst_reg, 64),
            }
            self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
        }
    }

    fn emit_cast(
        &mut self,
        inst: &Instruction,
        locs: &LocationResolver,
        _kind: CastKind,
        value: ValueId,
    ) {
        if let Some(result) = &inst.result {
            let src_reg = self.load_value_typed(locs, locs.value(value), locs.value_ty(value), R10);
            let dst = locs.value(result.id);
            let dst_ty = locs.value_ty(result.id);
            let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R11, "cast", dst_ty);
            let size = X86_64Emitter::scalar_size(locs, dst_ty);
            self.emit_move_bits(src_reg, dst_reg, Self::bits_for_size(size));
            self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
        }
    }

    fn emit_load(&mut self, inst: &Instruction, locs: &LocationResolver, ptr: ValueId) {
        if let Some(result) = &inst.result {
            let dst = locs.value(result.id);
            let ty = result.ty;
            let addr = if let Some((base, offset)) = locs
                .field_addr_folds
                .get(&ptr)
                .or_else(|| locs.index_addr_folds.get(&ptr))
                .copied()
            {
                let base_reg =
                    self.load_value_typed(locs, locs.value(base), locs.value_ty(base), R11);
                if offset == 0 {
                    base_reg.to_string()
                } else {
                    self.emit_line(&format!("leaq {}({}), %r11", offset, base_reg));
                    "%r11".to_string()
                }
            } else {
                self.load_value_typed(locs, locs.value(ptr), locs.value_ty(ptr), R11)
                    .to_string()
            };
            if locs.types.is_reg_type(ty) {
                let size = locs.layout(ty).size() as u32;
                let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R10, "load", ty);
                self.emit_load_ptr_sized(R10, &addr, size);
                let loaded = Self::operand_as_bits("%r10", Self::bits_for_size(size)).into_owned();
                if dst_reg != loaded {
                    self.emit_move_bits(&loaded, dst_reg, Self::bits_for_size(size));
                }
                self.store_if_needed_typed(locs, dst_slot, dst_reg, ty);
            } else {
                let Location::Stack(slot) = dst else {
                    panic!("backend codegen: aggregate load needs stack dst")
                };
                let size = locs.layout(ty).size() as u32;
                if size > 0 {
                    let dst_offset = self.stack_offset(slot);
                    self.copy_ptr_to_stack(&addr, dst_offset, size);
                }
            }
        }
    }

    fn emit_field_addr(
        &mut self,
        inst: &Instruction,
        locs: &LocationResolver,
        base: ValueId,
        index: usize,
    ) {
        if let Some(result) = &inst.result {
            if locs.field_addr_folds.contains_key(&result.id) {
                return;
            }
            let dst = locs.value(result.id);
            let base_reg = self.load_value_typed(locs, locs.value(base), locs.value_ty(base), R10);
            let base_ty = locs.value_ty(base);
            let elem_ty = match locs.types.kind(base_ty) {
                IrTypeKind::Ptr { elem } => *elem,
                other => panic!(
                    "backend codegen: field addr base must be ptr, got {:?}",
                    other
                ),
            };
            let layout = locs.layout(elem_ty);
            let offset = layout.field_offsets().get(index).copied().unwrap_or(0) as u32;
            let dst_ty = locs.value_ty(result.id);
            let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R11, "field-addr", dst_ty);
            if offset == 0 {
                self.emit_move_bits(base_reg, dst_reg, 64);
            } else {
                self.emit_line(&format!(
                    "leaq {}({}), {}",
                    offset,
                    Self::operand_as_bits(base_reg, 64),
                    Self::operand_as_bits(dst_reg, 64)
                ));
            }
            self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
        }
    }

    fn emit_index_addr(
        &mut self,
        inst: &Instruction,
        locs: &LocationResolver,
        base: ValueId,
        index: ValueId,
    ) {
        if let Some(result) = &inst.result {
            if locs.index_addr_folds.contains_key(&result.id) {
                return;
            }
            let dst = locs.value(result.id);
            let base_reg = self.load_value_typed(locs, locs.value(base), locs.value_ty(base), R10);
            let elem_ty = match locs.types.kind(result.ty) {
                IrTypeKind::Ptr { elem } => *elem,
                other => panic!("backend codegen: index result must be ptr, got {:?}", other),
            };
            let stride = locs.layout(elem_ty).stride() as u32;
            let dst_ty = locs.value_ty(result.id);
            let (dst_reg, dst_slot) = self.value_dst_typed(locs, dst, R11, "index-addr", dst_ty);
            if locs.const_zero_values.contains(&index) || stride == 0 {
                self.emit_move_bits(base_reg, dst_reg, 64);
                self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
                return;
            }
            let index_reg =
                self.load_value_typed(locs, locs.value(index), locs.value_ty(index), R11);
            match stride {
                1 | 2 | 4 | 8 => {
                    let idx64 = if index_reg.ends_with('d')
                        || index_reg.ends_with('w')
                        || index_reg.ends_with('b')
                    {
                        "%r11"
                    } else {
                        index_reg
                    };
                    if idx64 == "%r11" && index_reg != "%r11" {
                        self.emit_move_bits(index_reg, "%r11", 64);
                    }
                    self.emit_line(&format!(
                        "leaq {}, {}",
                        X86_64Emitter::mem_indexed(
                            Self::operand_as_bits(base_reg, 64).trim_start_matches('%'),
                            idx64.trim_start_matches('%'),
                            stride
                        ),
                        Self::operand_as_bits(dst_reg, 64)
                    ));
                }
                _ => {
                    if index_reg != "%r11" {
                        self.emit_move_bits(index_reg, "%r11", 64);
                    }
                    self.emit_line(&format!("imulq ${}, %r11", stride));
                    self.emit_line(&format!(
                        "leaq (%{}, %r11), {}",
                        Self::operand_as_bits(base_reg, 64).trim_start_matches('%'),
                        Self::operand_as_bits(dst_reg, 64)
                    ));
                }
            }
            self.store_if_needed_typed(locs, dst_slot, dst_reg, dst_ty);
        }
    }

    fn emit_store(&mut self, locs: &LocationResolver, ptr_id: ValueId, value_id: ValueId) {
        let ptr = locs.value(ptr_id);
        let value_loc = locs.value(value_id);
        let value_ty = locs.value_ty(value_id);
        let ptr_ty = locs.value_ty(ptr_id);
        let value_reg = match value_loc {
            Location::Reg(reg) => Some(reg),
            _ => None,
        };
        let ptr_scratch = if value_reg == Some(R11) { R10 } else { R11 };
        let value_scratch = if ptr_scratch == R10 { R11 } else { R10 };
        let zero_store = locs.const_zero_values.contains(&value_id)
            && matches!(
                locs.types.kind(value_ty),
                IrTypeKind::Bool | IrTypeKind::Int { .. }
            );

        if let Some((base, offset)) = locs
            .field_addr_folds
            .get(&ptr_id)
            .or_else(|| locs.index_addr_folds.get(&ptr_id))
            .copied()
        {
            let base_reg =
                self.load_value_typed(locs, locs.value(base), locs.value_ty(base), ptr_scratch);
            if locs.types.is_reg_type(value_ty) {
                let size = locs.layout(value_ty).size() as u32;
                if size > 0 {
                    let val = if zero_store {
                        if size <= 4 {
                            Self::reg(32, value_scratch)
                        } else {
                            Self::reg(64, value_scratch)
                        }
                        .to_string()
                    } else {
                        self.load_value_typed(locs, value_loc, value_ty, value_scratch)
                            .to_string()
                    };
                    if zero_store {
                        self.emit_line(&format!(
                            "{} {}, {}",
                            if size <= 4 { "xorl" } else { "xorq" },
                            if size <= 4 {
                                Self::reg(32, value_scratch)
                            } else {
                                Self::reg(64, value_scratch)
                            },
                            if size <= 4 {
                                Self::reg(32, value_scratch)
                            } else {
                                Self::reg(64, value_scratch)
                            }
                        ));
                    }
                    self.emit_store_ptr_sized_offset(&val, base_reg, offset, size);
                }
            } else {
                let Location::Stack(slot) = value_loc else {
                    panic!("backend codegen: aggregate store needs stack src")
                };
                let size = locs.layout(value_ty).size() as u32;
                if size > 0 {
                    if offset == 0 {
                        self.copy_stack_to_ptr(self.stack_offset(slot), base_reg, size);
                    } else {
                        self.emit_line(&format!("leaq {}({}), %r11", offset, base_reg));
                        self.copy_stack_to_ptr(self.stack_offset(slot), "%r11", size);
                    }
                }
            }
            return;
        }

        let ptr = self.load_value_typed(locs, ptr, ptr_ty, ptr_scratch);
        if locs.types.is_reg_type(value_ty) {
            let size = locs.layout(value_ty).size() as u32;
            if size > 0 {
                let val = if zero_store {
                    if size <= 4 {
                        Self::reg(32, value_scratch)
                    } else {
                        Self::reg(64, value_scratch)
                    }
                    .to_string()
                } else {
                    self.load_value_typed(locs, value_loc, value_ty, value_scratch)
                        .to_string()
                };
                if zero_store {
                    self.emit_line(&format!(
                        "{} {}, {}",
                        if size <= 4 { "xorl" } else { "xorq" },
                        if size <= 4 {
                            Self::reg(32, value_scratch)
                        } else {
                            Self::reg(64, value_scratch)
                        },
                        if size <= 4 {
                            Self::reg(32, value_scratch)
                        } else {
                            Self::reg(64, value_scratch)
                        }
                    ));
                }
                self.emit_store_ptr_sized(&val, ptr, size);
            }
        } else {
            let Location::Stack(slot) = value_loc else {
                panic!("backend codegen: aggregate store needs stack src")
            };
            let size = locs.layout(value_ty).size() as u32;
            if size > 0 {
                self.copy_stack_to_ptr(self.stack_offset(slot), ptr, size);
            }
        }
    }
}
