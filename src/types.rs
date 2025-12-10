use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Unit,
    UInt64,
    Bool,

    // Compound Types
    Array { elem_ty: Box<Type>, len: usize },
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "unit"),
            Type::UInt64 => write!(f, "u64"),
            Type::Bool => write!(f, "bool"),
            Type::Array { elem_ty, len } => write!(f, "{}[{}]", elem_ty, len),
        }
    }
}

impl Type {
    pub fn size_of(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::UInt64 => 8,
            Type::Bool => 1,
            Type::Array { elem_ty, len } => elem_ty.size_of() * len,
            Type::Unknown => panic!("Unknown type"),
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            Type::Unit => 1,
            Type::UInt64 => 8,
            Type::Bool => 1,
            Type::Array { elem_ty, .. } => elem_ty.align_of(),
            Type::Unknown => panic!("Unknown type"),
        }
    }

    pub fn is_compound(&self) -> bool {
        matches!(self, Type::Array { .. })
    }
}
