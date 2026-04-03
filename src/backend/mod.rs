pub mod analysis;
pub mod codegen;
pub mod lower;
pub mod opt;
pub mod regalloc;
pub mod verify;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TargetKind {
    #[default]
    Arm64,
    X86_64,
}

impl TargetKind {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Arm64 => "arm64",
            Self::X86_64 => "x86_64",
        }
    }

    pub fn host() -> Self {
        if cfg!(target_arch = "aarch64") {
            Self::Arm64
        } else {
            Self::X86_64
        }
    }

    pub fn supports_object_backed_stdlib(self) -> bool {
        matches!(self, Self::Arm64 | Self::X86_64)
    }
}

pub use lower::*;
