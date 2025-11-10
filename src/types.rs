use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Unit,
    UInt32,
    Bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "unit"),
            Type::UInt32 => write!(f, "u32"),
            Type::Bool => write!(f, "bool"),
        }
    }
}