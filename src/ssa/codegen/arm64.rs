//! Minimal ARM64 SSA codegen emitter.

use std::fmt::Write;

use crate::regalloc::target::PhysReg;
use crate::ssa::codegen::emitter::{CodegenEmitter, LocationResolver, binop_mnemonic};
use crate::ssa::model::ir::{InstKind, Instruction, Terminator};
use crate::ssa::regalloc::Location;
use crate::ssa::regalloc::moves::MoveOp;

/// Simple string-based ARM64 emitter for early SSA codegen.
pub struct Arm64Emitter {
    output: String,
}

impl Arm64Emitter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
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
            _ => "x9",
        }
    }

    fn fmt_loc(loc: Location) -> String {
        match loc {
            Location::Reg(reg) => Self::reg_name(reg).to_string(),
            Location::Stack(slot) => format!("[sp, #{}]", slot.offset_bytes()),
        }
    }

    fn emit_line(&mut self, line: &str) {
        let _ = writeln!(self.output, "  {}", line);
    }
}

impl CodegenEmitter for Arm64Emitter {
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
                        dst.offset_bytes()
                    ));
                }
                (Location::Stack(src), Location::Reg(dst)) => {
                    self.emit_line(&format!(
                        "ldr {}, [sp, #{}]",
                        Self::reg_name(dst),
                        src.offset_bytes()
                    ));
                }
                (Location::Stack(src), Location::Stack(dst)) => {
                    self.emit_line(&format!("ldr x9, [sp, #{}]", src.offset_bytes()));
                    self.emit_line(&format!("str x9, [sp, #{}]", dst.offset_bytes()));
                }
            }
        }
    }

    fn emit_inst(&mut self, inst: &Instruction, locs: &LocationResolver) {
        match &inst.kind {
            InstKind::Const { value } => {
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    match dst {
                        Location::Reg(reg) => {
                            self.emit_line(&format!(
                                "mov {}, #{}",
                                Self::reg_name(reg),
                                value.as_int()
                            ));
                        }
                        Location::Stack(slot) => {
                            self.emit_line(&format!("mov x9, #{}", value.as_int()));
                            self.emit_line(&format!("str x9, [sp, #{}]", slot.offset_bytes()));
                        }
                    }
                }
            }
            InstKind::BinOp { op, lhs, rhs } => {
                let lhs = locs.value(*lhs);
                let rhs = locs.value(*rhs);
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    let op = binop_mnemonic(*op);
                    match (lhs, rhs, dst) {
                        (Location::Reg(a), Location::Reg(b), Location::Reg(out)) => {
                            self.emit_line(&format!(
                                "{} {}, {}, {}",
                                op,
                                Self::reg_name(out),
                                Self::reg_name(a),
                                Self::reg_name(b)
                            ));
                        }
                        _ => {
                            self.emit_line("// binop fallback");
                        }
                    }
                }
            }
            InstKind::Load { ptr } => {
                let src = locs.value(*ptr);
                if let Some(result) = &inst.result {
                    let dst = locs.value(result.id);
                    if let (Location::Reg(src), Location::Reg(dst)) = (src, dst) {
                        self.emit_line(&format!(
                            "ldr {}, [{}]",
                            Self::reg_name(dst),
                            Self::reg_name(src)
                        ));
                    }
                }
            }
            InstKind::Store { ptr, value } => {
                let ptr = locs.value(*ptr);
                let value = locs.value(*value);
                if let (Location::Reg(ptr), Location::Reg(val)) = (ptr, value) {
                    self.emit_line(&format!(
                        "str {}, [{}]",
                        Self::reg_name(val),
                        Self::reg_name(ptr)
                    ));
                }
            }
            _ => {
                self.emit_line("// unsupported inst");
            }
        }
    }

    fn emit_terminator(&mut self, term: &Terminator, locs: &LocationResolver) {
        match term {
            Terminator::Br { target, .. } => {
                self.emit_line(&format!("b bb{}", target.0));
            }
            Terminator::CondBr {
                cond,
                then_bb,
                else_bb,
                ..
            } => {
                let cond = locs.value(*cond);
                if let Location::Reg(reg) = cond {
                    self.emit_line(&format!("cbnz {}, bb{}", Self::reg_name(reg), then_bb.0));
                    self.emit_line(&format!("b bb{}", else_bb.0));
                }
            }
            Terminator::Return { value } => {
                if let Some(value) = value {
                    let src = locs.value(*value);
                    if let Location::Reg(reg) = src {
                        self.emit_line(&format!("mov x0, {}", Self::reg_name(reg)));
                    }
                }
                self.emit_line("ret");
            }
            _ => self.emit_line("// unsupported terminator"),
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
