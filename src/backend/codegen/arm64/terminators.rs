use crate::backend::RuntimeFn;
use crate::backend::codegen::emitter::LocationResolver;
use crate::backend::codegen::graph::CodegenBlockId;
use crate::backend::regalloc::Location;
use crate::backend::regalloc::target::PhysReg;
use crate::ir::ir::{ConstValue, Terminator, ValueId};

use super::{Arm64Emitter, ConstValueExt, needs_sret};

impl Arm64Emitter {
    pub(super) fn emit_branch_impl(&mut self, target: CodegenBlockId) {
        self.emit_line(&format!("b {}", self.codegen_label(target)));
    }

    pub(super) fn emit_cond_branch_impl(
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

    pub(super) fn emit_switch_impl(
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

    pub(super) fn emit_terminator_impl(&mut self, term: &Terminator, locs: &LocationResolver) {
        match term {
            Terminator::Br { target, .. } => {
                // Unconditional branch to the target block.
                self.emit_branch_impl(CodegenBlockId::Ssa(*target));
            }
            Terminator::CondBr {
                cond,
                then_bb,
                else_bb,
                ..
            } => {
                self.emit_cond_branch_impl(
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
                self.emit_switch_impl(*value, &case_labels, CodegenBlockId::Ssa(*default), locs);
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
                                panic!("backend codegen: unsupported sret source {:?}", src_loc);
                            }
                        }
                    } else if locs.types.is_reg_type(ty) {
                        let src = locs.value(*value);
                        let src = self.load_value_typed(locs, src, ty, "x9");
                        let size = Self::scalar_size(locs, ty);
                        if size > 0 && size <= 4 {
                            let dst = Self::w_reg("x0");
                            self.emit_line(&format!("mov {}, {}", dst, src));
                        } else {
                            self.emit_line(&format!("mov x0, {}", src));
                        }
                    } else {
                        let size = locs.layout(ty).size() as u32;
                        let Location::Stack(slot) = locs.value(*value) else {
                            panic!(
                                "backend codegen: aggregate return must be stack-backed, got {:?}",
                                locs.value(*value)
                            );
                        };
                        let base = self.stack_offset(slot);
                        if size == 0 {
                            self.emit_line("mov x0, #0");
                        } else {
                            let size0 = size.min(8);
                            self.emit_load_sized("x0", base, size0);
                            if size > 8 {
                                let size1 = size.saturating_sub(8).min(8);
                                self.emit_load_sized("x1", base.saturating_add(8), size1);
                            }
                        }
                    }
                } else {
                    // Unit returns use x0=0 for a stable process exit code.
                    self.emit_line("mov x0, #0");
                }

                // Restore callee-saved registers before tearing down the frame.
                self.emit_epilogue();

                self.emit_line("ret");
            }
            Terminator::Unreachable => {
                // Emit a trap for unreachable control flow.
                self.emit_line("brk #0");
            }
        }
    }
}
