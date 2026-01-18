#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeFn {
    Trap,
    Print,
    StringFromBytes,
    FmtInit,
    FmtAppendBytes,
    FmtAppendU64,
    FmtAppendI64,
    FmtFinish,
    U64ToDec,
    I64ToDec,
    U64ToBin,
    U64ToOct,
    U64ToHex,
    MemSet,
    MemCopy,
    StringEnsure,
    StringDrop,
    StringAppendBytes,
    DynArrayEnsure,
    DynArrayAppendElem,
    Alloc,
    Realloc,
    Free,
    SetAllocTrace,
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
            RuntimeFn::StringFromBytes => RuntimeFnSig {
                name: "__mc_string_from_bytes",
                arg_count: 2,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::FmtInit => RuntimeFnSig {
                name: "__mc_fmt_init",
                arg_count: 2,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::FmtAppendBytes => RuntimeFnSig {
                name: "__mc_fmt_append_bytes",
                arg_count: 3,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::FmtAppendU64 => RuntimeFnSig {
                name: "__mc_fmt_append_u64",
                arg_count: 2,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::FmtAppendI64 => RuntimeFnSig {
                name: "__mc_fmt_append_i64",
                arg_count: 2,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::FmtFinish => RuntimeFnSig {
                name: "__mc_fmt_finish",
                arg_count: 2,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::U64ToDec => RuntimeFnSig {
                name: "__mc_u64_to_dec",
                arg_count: 2,
                ret: Some(RuntimeRet::Scalar),
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::I64ToDec => RuntimeFnSig {
                name: "__mc_i64_to_dec",
                arg_count: 2,
                ret: Some(RuntimeRet::Scalar),
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::U64ToBin => RuntimeFnSig {
                name: "__mc_u64_to_bin",
                arg_count: 2,
                ret: Some(RuntimeRet::Scalar),
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::U64ToOct => RuntimeFnSig {
                name: "__mc_u64_to_oct",
                arg_count: 2,
                ret: Some(RuntimeRet::Scalar),
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::U64ToHex => RuntimeFnSig {
                name: "__mc_u64_to_hex",
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
            RuntimeFn::MemCopy => RuntimeFnSig {
                name: "__mc_memcpy",
                arg_count: 2,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::StringEnsure => RuntimeFnSig {
                name: "__mc_string_ensure",
                arg_count: 2,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::StringDrop => RuntimeFnSig {
                name: "__mc_string_drop",
                arg_count: 1,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::StringAppendBytes => RuntimeFnSig {
                name: "__mc_string_append_bytes",
                arg_count: 3,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::DynArrayEnsure => RuntimeFnSig {
                name: "__mc_dyn_array_ensure",
                arg_count: 4,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::DynArrayAppendElem => RuntimeFnSig {
                name: "__mc_dyn_array_append_elem",
                arg_count: 4,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::Alloc => RuntimeFnSig {
                name: "__mc_alloc",
                arg_count: 2,
                ret: Some(RuntimeRet::Addr),
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::Realloc => RuntimeFnSig {
                name: "__mc_realloc",
                arg_count: 3,
                ret: Some(RuntimeRet::Addr),
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::Free => RuntimeFnSig {
                name: "__mc_free",
                arg_count: 1,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
            RuntimeFn::SetAllocTrace => RuntimeFnSig {
                name: "__mc_set_alloc_trace",
                arg_count: 1,
                ret: None,
                effect: CallEffect::ReadWrite,
            },
        }
    }
}
