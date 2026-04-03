use std::fmt;

use crate::backend::regalloc::target::{PhysReg, TargetSpec};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
#[allow(dead_code)]
pub enum X86_64Reg {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Rbp,
    Rsp,
}

pub const fn phys(reg: X86_64Reg) -> PhysReg {
    PhysReg(reg as u8)
}

pub fn from_phys(reg: PhysReg) -> X86_64Reg {
    match reg.0 {
        0 => X86_64Reg::Rax,
        1 => X86_64Reg::Rcx,
        2 => X86_64Reg::Rdx,
        3 => X86_64Reg::Rbx,
        4 => X86_64Reg::Rsi,
        5 => X86_64Reg::Rdi,
        6 => X86_64Reg::R8,
        7 => X86_64Reg::R9,
        8 => X86_64Reg::R10,
        9 => X86_64Reg::R11,
        10 => X86_64Reg::R12,
        11 => X86_64Reg::R13,
        12 => X86_64Reg::R14,
        13 => X86_64Reg::R15,
        14 => X86_64Reg::Rbp,
        15 => X86_64Reg::Rsp,
        _ => panic!("Invalid x86-64 phys reg: {}", reg.0),
    }
}

pub struct X86_64Target;

impl Default for X86_64Target {
    fn default() -> Self {
        Self::new()
    }
}

impl X86_64Target {
    pub const fn new() -> Self {
        Self
    }
}

const ALLOCATABLE: [PhysReg; 14] = [
    phys(X86_64Reg::Rax),
    phys(X86_64Reg::Rcx),
    phys(X86_64Reg::Rdx),
    phys(X86_64Reg::Rbx),
    phys(X86_64Reg::Rsi),
    phys(X86_64Reg::Rdi),
    phys(X86_64Reg::R8),
    phys(X86_64Reg::R9),
    phys(X86_64Reg::R10),
    phys(X86_64Reg::R11),
    phys(X86_64Reg::R12),
    phys(X86_64Reg::R13),
    phys(X86_64Reg::R14),
    phys(X86_64Reg::R15),
];

const CALLER_SAVED: [PhysReg; 9] = [
    phys(X86_64Reg::Rax),
    phys(X86_64Reg::Rcx),
    phys(X86_64Reg::Rdx),
    phys(X86_64Reg::Rsi),
    phys(X86_64Reg::Rdi),
    phys(X86_64Reg::R8),
    phys(X86_64Reg::R9),
    phys(X86_64Reg::R10),
    phys(X86_64Reg::R11),
];

const CALLEE_SAVED: [PhysReg; 5] = [
    phys(X86_64Reg::Rbx),
    phys(X86_64Reg::R12),
    phys(X86_64Reg::R13),
    phys(X86_64Reg::R14),
    phys(X86_64Reg::R15),
];

const SCRATCH: [PhysReg; 2] = [phys(X86_64Reg::R10), phys(X86_64Reg::R11)];

pub const INDIRECT_CALL_REG: PhysReg = phys(X86_64Reg::R11);
pub const INDIRECT_RESULT_REG: PhysReg = phys(X86_64Reg::Rdi);

impl TargetSpec for X86_64Target {
    fn allocatable_regs(&self) -> &[PhysReg] {
        &ALLOCATABLE
    }

    fn caller_saved(&self) -> &[PhysReg] {
        &CALLER_SAVED
    }

    fn callee_saved(&self) -> &[PhysReg] {
        &CALLEE_SAVED
    }

    fn param_reg(&self, index: u32) -> Option<PhysReg> {
        match index {
            0 => Some(phys(X86_64Reg::Rdi)),
            1 => Some(phys(X86_64Reg::Rsi)),
            2 => Some(phys(X86_64Reg::Rdx)),
            3 => Some(phys(X86_64Reg::Rcx)),
            4 => Some(phys(X86_64Reg::R8)),
            5 => Some(phys(X86_64Reg::R9)),
            _ => None,
        }
    }

    fn result_reg(&self) -> PhysReg {
        phys(X86_64Reg::Rax)
    }

    fn indirect_result_reg(&self) -> Option<PhysReg> {
        Some(INDIRECT_RESULT_REG)
    }

    fn indirect_call_reg(&self) -> PhysReg {
        INDIRECT_CALL_REG
    }

    fn scratch_regs(&self) -> &[PhysReg] {
        &SCRATCH
    }

    fn reg_name(&self, reg: PhysReg) -> &'static str {
        match from_phys(reg) {
            X86_64Reg::Rax => "rax",
            X86_64Reg::Rcx => "rcx",
            X86_64Reg::Rdx => "rdx",
            X86_64Reg::Rbx => "rbx",
            X86_64Reg::Rsi => "rsi",
            X86_64Reg::Rdi => "rdi",
            X86_64Reg::R8 => "r8",
            X86_64Reg::R9 => "r9",
            X86_64Reg::R10 => "r10",
            X86_64Reg::R11 => "r11",
            X86_64Reg::R12 => "r12",
            X86_64Reg::R13 => "r13",
            X86_64Reg::R14 => "r14",
            X86_64Reg::R15 => "r15",
            X86_64Reg::Rbp => "rbp",
            X86_64Reg::Rsp => "rsp",
        }
    }
}

impl fmt::Display for X86_64Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            X86_64Reg::Rax => "rax",
            X86_64Reg::Rcx => "rcx",
            X86_64Reg::Rdx => "rdx",
            X86_64Reg::Rbx => "rbx",
            X86_64Reg::Rsi => "rsi",
            X86_64Reg::Rdi => "rdi",
            X86_64Reg::R8 => "r8",
            X86_64Reg::R9 => "r9",
            X86_64Reg::R10 => "r10",
            X86_64Reg::R11 => "r11",
            X86_64Reg::R12 => "r12",
            X86_64Reg::R13 => "r13",
            X86_64Reg::R14 => "r14",
            X86_64Reg::R15 => "r15",
            X86_64Reg::Rbp => "rbp",
            X86_64Reg::Rsp => "rsp",
        })
    }
}
