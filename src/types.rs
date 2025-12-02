use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Unit,
    UInt64,
    Bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "unit"),
            Type::UInt64 => write!(f, "u64"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

impl Type {
    pub fn size_of(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::UInt64 => 8,
            Type::Bool => 1,
            Type::Unknown => panic!("Unknown type"),
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            Type::Unit => 1,
            Type::UInt64 => 8,
            Type::Bool => 1,
            Type::Unknown => panic!("Unknown type"),
        }
    }
}
