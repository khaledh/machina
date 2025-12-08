use std::fmt;

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

pub const CALLER_SAVED_REGS: [Arm64Reg; 16] = [
    Arm64Reg::X0,
    Arm64Reg::X1,
    Arm64Reg::X2,
    Arm64Reg::X3,
    Arm64Reg::X4,
    Arm64Reg::X5,
    Arm64Reg::X6,
    Arm64Reg::X7,
    Arm64Reg::X8,
    Arm64Reg::X9,
    Arm64Reg::X10,
    Arm64Reg::X11,
    Arm64Reg::X12,
    Arm64Reg::X13,
    Arm64Reg::X14,
    Arm64Reg::X15,
];

pub const CALLEE_SAVED_REGS: [Arm64Reg; 10] = [
    Arm64Reg::X19,
    Arm64Reg::X20,
    Arm64Reg::X21,
    Arm64Reg::X22,
    Arm64Reg::X23,
    Arm64Reg::X24,
    Arm64Reg::X25,
    Arm64Reg::X26,
    Arm64Reg::X27,
    Arm64Reg::X28,
];

pub fn get_param_reg(index: u32) -> Arm64Reg {
    match index {
        0 => Arm64Reg::X0,
        1 => Arm64Reg::X1,
        2 => Arm64Reg::X2,
        3 => Arm64Reg::X3,
        4 => Arm64Reg::X4,
        5 => Arm64Reg::X5,
        6 => Arm64Reg::X6,
        7 => Arm64Reg::X7,
        _ => panic!("Invalid arm64 param index: {}", index),
    }
}

pub fn get_result_reg() -> Arm64Reg {
    Arm64Reg::X0
}

pub fn get_indirect_result_reg() -> Arm64Reg {
    Arm64Reg::X8
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
