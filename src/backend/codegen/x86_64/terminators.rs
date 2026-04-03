use crate::backend::codegen::emitter::LocationResolver;
use crate::backend::codegen::graph::CodegenBlockId;
use crate::backend::regalloc::Location;
use crate::backend::regalloc::target::PhysReg;
use crate::backend::regalloc::x86_64::{X86_64Reg, phys};
use crate::ir::{ConstValue, RuntimeFn, Terminator, ValueId};

use super::{ConstValueExt, X86_64Emitter, needs_sret};

const RAX: PhysReg = phys(X86_64Reg::Rax);
const RDX: PhysReg = phys(X86_64Reg::Rdx);
const RDI: PhysReg = phys(X86_64Reg::Rdi);
const R10: PhysReg = phys(X86_64Reg::R10);

impl X86_64Emitter {
    pub(super) fn emit_branch_impl(&mut self, target: CodegenBlockId) {
        self.emit_line(&format!("jmp {}", self.codegen_label(target)));
    }

    pub(super) fn emit_cond_branch_impl(
        &mut self,
        cond: ValueId,
        then_target: CodegenBlockId,
        else_target: CodegenBlockId,
        locs: &LocationResolver,
    ) {
        let cond_loc = locs.value(cond);
        let cond_ty = locs.value_ty(cond);
        let cond_reg = self.load_value_typed(locs, cond_loc, cond_ty, R10);
        self.emit_line(&format!("testl {}, {}", cond_reg, cond_reg));
        self.emit_line(&format!("jne {}", self.codegen_label(then_target)));
        self.emit_line(&format!("jmp {}", self.codegen_label(else_target)));
    }

    pub(super) fn emit_switch_impl(
        &mut self,
        value: ValueId,
        cases: &[(ConstValue, CodegenBlockId)],
        default_target: CodegenBlockId,
        locs: &LocationResolver,
    ) {
        let value_loc = locs.value(value);
        let value_ty = locs.value_ty(value);
        let value_reg = self.load_value_typed(locs, value_loc, value_ty, R10);
        let width = X86_64Emitter::bits_for_size(locs.layout(value_ty).size() as u32);
        for (case, target) in cases {
            self.emit_mov_imm("%r11", case.as_int(), width);
            self.emit_line(&format!(
                "cmp{} {}, {}",
                X86_64Emitter::suffix_for_bits(width),
                X86_64Emitter::operand_as_bits("%r11", width),
                X86_64Emitter::operand_as_bits(value_reg, width)
            ));
            self.emit_line(&format!("je {}", self.codegen_label(*target)));
        }
        self.emit_line(&format!("jmp {}", self.codegen_label(default_target)));
    }

    pub(super) fn emit_terminator_impl(&mut self, term: &Terminator, locs: &LocationResolver) {
        match term {
            Terminator::Br { target, .. } => self.emit_branch_impl(CodegenBlockId::Ssa(*target)),
            Terminator::CondBr {
                cond,
                then_bb,
                else_bb,
                ..
            } => self.emit_cond_branch_impl(
                *cond,
                CodegenBlockId::Ssa(*then_bb),
                CodegenBlockId::Ssa(*else_bb),
                locs,
            ),
            Terminator::Switch {
                value,
                cases,
                default,
                ..
            } => {
                let case_labels = cases
                    .iter()
                    .map(|case| (case.value.clone(), CodegenBlockId::Ssa(case.target)))
                    .collect::<Vec<_>>();
                self.emit_switch_impl(*value, &case_labels, CodegenBlockId::Ssa(*default), locs);
            }
            Terminator::Return { value } => {
                if let Some(value) = value {
                    let ty = locs.value_ty(*value);
                    if needs_sret(locs, ty) {
                        let sret_reg = RDI;
                        let offset = self
                            .saved_reg_offset(sret_reg)
                            .unwrap_or_else(|| panic!("backend codegen: missing sret save slot"));
                        self.emit_line(&format!(
                            "movq {}, %rdi",
                            X86_64Emitter::mem("rsp", offset)
                        ));
                        let Location::Stack(slot) = locs.value(*value) else {
                            panic!("backend codegen: sret result must be stack-backed");
                        };
                        let size = locs.layout(ty).size() as u32;
                        let src_offset = self.stack_offset(slot);
                        self.copy_stack_to_ptr(src_offset, "%rdi", size);
                        self.emit_line("movq %rdi, %rax");
                    } else if locs.types.is_reg_type(ty) {
                        let src = locs.value(*value);
                        let src_reg = self.load_value_typed(locs, src, ty, R10);
                        let size = X86_64Emitter::scalar_size(locs, ty);
                        match size {
                            0 => self.emit_line("xorl %eax, %eax"),
                            1 | 2 | 4 => self.emit_move_bits(src_reg, "%eax", 32),
                            8 => self.emit_move_bits(src_reg, "%rax", 64),
                            _ => panic!("backend codegen: unsupported scalar return size {size}"),
                        }
                    } else {
                        let size = locs.layout(ty).size() as u32;
                        let Location::Stack(slot) = locs.value(*value) else {
                            panic!("backend codegen: aggregate return must be stack-backed");
                        };
                        let base = self.stack_offset(slot);
                        if size == 0 {
                            self.emit_line("xorl %eax, %eax");
                        } else {
                            self.emit_load_sized(RAX, "rsp", base, size.min(8));
                            if size > 8 {
                                self.emit_load_sized(
                                    RDX,
                                    "rsp",
                                    base.saturating_add(8),
                                    size.saturating_sub(8).min(8),
                                );
                            }
                        }
                    }
                } else {
                    self.emit_line("xorl %eax, %eax");
                }
                self.emit_epilogue();
                self.emit_line("retq");
            }
            Terminator::Unreachable => {
                self.emit_line(&format!(
                    "call {}",
                    self.mangle_symbol(RuntimeFn::Trap.name())
                ));
                self.emit_line("ud2");
            }
        }
    }
}
