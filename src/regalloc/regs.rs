use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
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
    // Reserved as "scratch" registers for the linker or dynamic loader)
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
    XZR,
}

impl fmt::Display for Arm64Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arm64Reg::SP => write!(f, "sp"),
            Arm64Reg::XZR => write!(f, "xzr"),
            _ => write!(f, "x{}", *self as u8),
        }
    }
}
