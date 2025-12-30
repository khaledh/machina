#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeFn {
    Trap,
    Print,
    U64ToDec,
    MemSet,
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
            RuntimeFn::Print => RuntimeFnSig {
                name: "__mc_print",
                arg_count: 2,
                ret: None,
                effect: CallEffect::ReadOnly,
            },
            RuntimeFn::U64ToDec => RuntimeFnSig {
                name: "__mc_u64_to_dec",
                arg_count: 2,
                ret: Some(RuntimeRet::Scalar),
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::MemSet => RuntimeFnSig {
                name: "__mc_memset",
                arg_count: 2,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
        }
    }
}
