#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeFn {
    Trap,
    PrintStr,
    PrintLn,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeFnSig {
    pub name: &'static str,
    pub arg_count: u8,
    pub ret: Option<RuntimeRet>,
    pub effect: CallEffect,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeRet {
    None,   // void / noreturn
    Scalar, // returned in result reg
    Addr,   // pointer-like scalar (also in result reg), distinct for ABI docs
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallEffect {
    Pure,      // no observable side effects
    ReadOnly,  // reads memory, no writes
    ReadWrite, // reads + writes memory / IO
    NoReturn,  // does not return
}

impl RuntimeFn {
    pub fn sig(&self) -> RuntimeFnSig {
        match self {
            RuntimeFn::Trap => RuntimeFnSig {
                name: "__mc_trap",
                arg_count: 4,
                ret: None,
                effect: CallEffect::NoReturn,
            },
            RuntimeFn::PrintStr => RuntimeFnSig {
                name: "__mc_print_str",
                arg_count: 1,
                ret: None,
                effect: CallEffect::ReadOnly,
            },
            RuntimeFn::PrintLn => RuntimeFnSig {
                name: "__mc_print_ln",
                arg_count: 0,
                ret: None,
                effect: CallEffect::ReadOnly,
            },
        }
    }
}
