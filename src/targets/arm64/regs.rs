use std::fmt;

use crate::regalloc::target::{PhysReg, TargetSpec};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
#[allow(dead_code)]
pub enum Arm64Reg {
    // Params (Caller-saved)
    X0, // also function return register
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    // Indirect result (Caller-saved)
    // (Used to pass a pointer to memory allocated by the caller for returning large structs/aggregates by value)
    X8,
    // Available registers (Caller-saved)
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    // Intra-procedure call temp (Caller-saved)
    // Reserved as "scratch" registers for the linker or dynamic loader
    X16,
    X17,
    // Platform register (Reserved for platform-specific use by the OS)
    X18,
    // Local variables (Callee-saved; preserved across function calls)
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    // Frame Pointer (FP) - points to the base of the current stack frame
    X29,
    // Link Register (LR) - contains the return address for the current function
    X30,
    // Stack Pointer (SP) - points to the current top of the stack
    SP,
    // Zero Register (XZR) - always contains 0
    Xzr,
}

pub const fn phys(reg: Arm64Reg) -> PhysReg {
    PhysReg(reg as u8)
}

pub fn from_phys(reg: PhysReg) -> Arm64Reg {
    match reg.0 {
        0 => Arm64Reg::X0,
        1 => Arm64Reg::X1,
        2 => Arm64Reg::X2,
        3 => Arm64Reg::X3,
        4 => Arm64Reg::X4,
        5 => Arm64Reg::X5,
        6 => Arm64Reg::X6,
        7 => Arm64Reg::X7,
        8 => Arm64Reg::X8,
        9 => Arm64Reg::X9,
        10 => Arm64Reg::X10,
        11 => Arm64Reg::X11,
        12 => Arm64Reg::X12,
        13 => Arm64Reg::X13,
        14 => Arm64Reg::X14,
        15 => Arm64Reg::X15,
        16 => Arm64Reg::X16,
        17 => Arm64Reg::X17,
        18 => Arm64Reg::X18,
        19 => Arm64Reg::X19,
        20 => Arm64Reg::X20,
        21 => Arm64Reg::X21,
        22 => Arm64Reg::X22,
        23 => Arm64Reg::X23,
        24 => Arm64Reg::X24,
        25 => Arm64Reg::X25,
        26 => Arm64Reg::X26,
        27 => Arm64Reg::X27,
        28 => Arm64Reg::X28,
        29 => Arm64Reg::X29,
        30 => Arm64Reg::X30,
        31 => Arm64Reg::SP,
        32 => Arm64Reg::Xzr,
        _ => panic!("Invalid arm64 phys reg: {}", reg.0),
    }
}

/// Return the corresponding 32-bit W-register name for a given X-reg.
pub fn to_w_reg(reg: Arm64Reg) -> &'static str {
    match reg {
        Arm64Reg::X0 => "w0",
        Arm64Reg::X1 => "w1",
        Arm64Reg::X2 => "w2",
        Arm64Reg::X3 => "w3",
        Arm64Reg::X4 => "w4",
        Arm64Reg::X5 => "w5",
        Arm64Reg::X6 => "w6",
        Arm64Reg::X7 => "w7",
        Arm64Reg::X8 => "w8",
        Arm64Reg::X9 => "w9",
        Arm64Reg::X10 => "w10",
        Arm64Reg::X11 => "w11",
        Arm64Reg::X12 => "w12",
        Arm64Reg::X13 => "w13",
        Arm64Reg::X14 => "w14",
        Arm64Reg::X15 => "w15",
        Arm64Reg::X16 => "w16",
        Arm64Reg::X17 => "w17",
        Arm64Reg::X18 => "w18",
        Arm64Reg::X19 => "w19",
        Arm64Reg::X20 => "w20",
        Arm64Reg::X21 => "w21",
        Arm64Reg::X22 => "w22",
        Arm64Reg::X23 => "w23",
        Arm64Reg::X24 => "w24",
        Arm64Reg::X25 => "w25",
        Arm64Reg::X26 => "w26",
        Arm64Reg::X27 => "w27",
        Arm64Reg::X28 => "w28",
        Arm64Reg::X29 => "w29",
        Arm64Reg::X30 => "w30",
        Arm64Reg::SP => "wsp",
        Arm64Reg::Xzr => "wzr",
    }
}

pub struct Arm64Target;

impl Default for Arm64Target {
    fn default() -> Self {
        Self::new()
    }
}

impl Arm64Target {
    pub const fn new() -> Self {
        Self
    }
}

const ALLOCATABLE: [PhysReg; 24] = [
    phys(Arm64Reg::X0),
    phys(Arm64Reg::X1),
    phys(Arm64Reg::X2),
    phys(Arm64Reg::X3),
    phys(Arm64Reg::X4),
    phys(Arm64Reg::X5),
    phys(Arm64Reg::X6),
    phys(Arm64Reg::X7),
    phys(Arm64Reg::X8),
    phys(Arm64Reg::X9),
    phys(Arm64Reg::X10),
    phys(Arm64Reg::X11),
    phys(Arm64Reg::X12),
    phys(Arm64Reg::X13),
    phys(Arm64Reg::X19),
    phys(Arm64Reg::X20),
    phys(Arm64Reg::X21),
    phys(Arm64Reg::X22),
    phys(Arm64Reg::X23),
    phys(Arm64Reg::X24),
    phys(Arm64Reg::X25),
    phys(Arm64Reg::X26),
    phys(Arm64Reg::X27),
    phys(Arm64Reg::X28),
];

const CALLER_SAVED: [PhysReg; 16] = [
    phys(Arm64Reg::X0),
    phys(Arm64Reg::X1),
    phys(Arm64Reg::X2),
    phys(Arm64Reg::X3),
    phys(Arm64Reg::X4),
    phys(Arm64Reg::X5),
    phys(Arm64Reg::X6),
    phys(Arm64Reg::X7),
    phys(Arm64Reg::X8),
    phys(Arm64Reg::X9),
    phys(Arm64Reg::X10),
    phys(Arm64Reg::X11),
    phys(Arm64Reg::X12),
    phys(Arm64Reg::X13),
    phys(Arm64Reg::X14),
    phys(Arm64Reg::X15),
];

const CALLEE_SAVED: [PhysReg; 10] = [
    phys(Arm64Reg::X19),
    phys(Arm64Reg::X20),
    phys(Arm64Reg::X21),
    phys(Arm64Reg::X22),
    phys(Arm64Reg::X23),
    phys(Arm64Reg::X24),
    phys(Arm64Reg::X25),
    phys(Arm64Reg::X26),
    phys(Arm64Reg::X27),
    phys(Arm64Reg::X28),
];

// Registers reserved for codegen temps and ABI-specified scratch.
const SCRATCH: [PhysReg; 4] = [
    phys(Arm64Reg::X14),
    phys(Arm64Reg::X15),
    phys(Arm64Reg::X16),
    phys(Arm64Reg::X17),
];

pub const INDIRECT_CALL_REG: PhysReg = phys(Arm64Reg::X16);

impl TargetSpec for Arm64Target {
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
            0 => Some(phys(Arm64Reg::X0)),
            1 => Some(phys(Arm64Reg::X1)),
            2 => Some(phys(Arm64Reg::X2)),
            3 => Some(phys(Arm64Reg::X3)),
            4 => Some(phys(Arm64Reg::X4)),
            5 => Some(phys(Arm64Reg::X5)),
            6 => Some(phys(Arm64Reg::X6)),
            7 => Some(phys(Arm64Reg::X7)),
            _ => None,
        }
    }

    fn result_reg(&self) -> PhysReg {
        phys(Arm64Reg::X0)
    }

    fn indirect_result_reg(&self) -> Option<PhysReg> {
        Some(phys(Arm64Reg::X8))
    }

    fn indirect_call_reg(&self) -> PhysReg {
        INDIRECT_CALL_REG
    }

    fn scratch_regs(&self) -> &[PhysReg] {
        &SCRATCH
    }

    fn reg_name(&self, reg: PhysReg) -> &'static str {
        match from_phys(reg) {
            Arm64Reg::SP => "sp",
            Arm64Reg::Xzr => "xzr",
            other => {
                let n = other as u8;
                match n {
                    0..=30 => {
                        const NAMES: [&str; 31] = [
                            "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10",
                            "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20",
                            "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30",
                        ];
                        NAMES[n as usize]
                    }
                    _ => "x?",
                }
            }
        }
    }
}

impl fmt::Display for Arm64Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arm64Reg::SP => write!(f, "sp"),
            Arm64Reg::Xzr => write!(f, "xzr"),
            _ => write!(f, "x{}", *self as u8),
        }
    }
}
