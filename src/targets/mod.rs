use clap::ValueEnum;

pub mod arm64;

#[derive(Clone, Copy, Debug, ValueEnum)]
pub enum TargetKind {
    Arm64,
}

#[derive(Debug)]
pub enum CodegenError {
    Arm64(arm64::CodegenError),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::Arm64(err) => err.fmt(f),
        }
    }
}

impl std::error::Error for CodegenError {}

impl From<arm64::CodegenError> for CodegenError {
    fn from(err: arm64::CodegenError) -> Self {
        CodegenError::Arm64(err)
    }
}
