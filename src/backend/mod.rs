pub mod analysis;
pub mod codegen;
pub mod lower;
pub mod opt;
pub mod regalloc;
pub mod verify;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum TargetKind {
    #[default]
    Arm64,
    X86_64,
    X86_64Linux,
}

impl TargetKind {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Arm64 => "arm64",
            Self::X86_64 => "x86_64",
            Self::X86_64Linux => "x86_64_linux",
        }
    }

    pub fn config_key(self) -> &'static str {
        match self {
            Self::Arm64 => "arm64",
            Self::X86_64 => "x86-64",
            Self::X86_64Linux => "x86-64-linux",
        }
    }

    pub fn host() -> Self {
        match (cfg!(target_os = "macos"), cfg!(target_arch = "aarch64")) {
            (true, true) => Self::Arm64,
            (true, false) => Self::X86_64,
            (false, false) => Self::X86_64Linux,
            (false, true) => Self::Arm64,
        }
    }

    pub fn supports_object_backed_stdlib(self) -> bool {
        matches!(self, Self::Arm64 | Self::X86_64)
    }

    pub fn macos_cc_arch(self) -> Option<&'static str> {
        match self {
            Self::Arm64 => Some("arm64"),
            Self::X86_64 => Some("x86_64"),
            Self::X86_64Linux => None,
        }
    }

    pub fn symbol_prefix(self) -> &'static str {
        match self {
            Self::Arm64 | Self::X86_64 => "_",
            Self::X86_64Linux => "",
        }
    }

    pub fn mangle_symbol(self, symbol: &str) -> String {
        format!("{}{}", self.symbol_prefix(), symbol)
    }
}

pub use lower::*;
