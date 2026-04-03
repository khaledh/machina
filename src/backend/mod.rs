pub mod analysis;
pub mod codegen;
pub mod lower;
pub mod opt;
pub mod regalloc;
pub mod verify;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum TargetKind {
    #[default]
    Arm64Macos,
    X86_64Macos,
    X86_64Linux,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PlatformKind {
    Macos,
    Linux,
    None,
}

impl PlatformKind {
    pub fn is_hosted(self) -> bool {
        !matches!(self, Self::None)
    }
}

impl TargetKind {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Arm64Macos => "arm64_macos",
            Self::X86_64Macos => "x86_64_macos",
            Self::X86_64Linux => "x86_64_linux",
        }
    }

    pub fn config_key(self) -> &'static str {
        match self {
            Self::Arm64Macos => "arm64-macos",
            Self::X86_64Macos => "x86-64-macos",
            Self::X86_64Linux => "x86-64-linux",
        }
    }

    pub fn host() -> Self {
        match (cfg!(target_os = "macos"), cfg!(target_arch = "aarch64")) {
            (true, true) => Self::Arm64Macos,
            (true, false) => Self::X86_64Macos,
            (false, false) => Self::X86_64Linux,
            (false, true) => Self::Arm64Macos,
        }
    }

    pub fn supports_object_backed_stdlib(self) -> bool {
        matches!(self, Self::Arm64Macos | Self::X86_64Macos)
    }

    pub fn macos_cc_arch(self) -> Option<&'static str> {
        match self {
            Self::Arm64Macos => Some("arm64"),
            Self::X86_64Macos => Some("x86_64"),
            Self::X86_64Linux => None,
        }
    }

    pub fn symbol_prefix(self) -> &'static str {
        match self {
            Self::Arm64Macos | Self::X86_64Macos => "_",
            Self::X86_64Linux => "",
        }
    }

    pub fn mangle_symbol(self, symbol: &str) -> String {
        format!("{}{}", self.symbol_prefix(), symbol)
    }
}

pub use lower::*;
