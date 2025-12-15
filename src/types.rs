use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Unit,
    UInt64,
    Bool,

    // Compound Types
    Array {
        elem_ty: Box<Type>,
        dims: Vec<usize>,
    },
    Tuple {
        fields: Vec<Type>,
    },
    Struct {
        name: String,
        fields: Vec<StructField>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
}

pub const BUILTIN_TYPES: &[Type] = &[Type::Unit, Type::UInt64, Type::Bool];

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "()"),
            Type::UInt64 => write!(f, "u64"),
            Type::Bool => write!(f, "bool"),
            Type::Array { elem_ty, dims } => {
                let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
                write!(f, "{}[{}]", elem_ty, dims_str.join(", "))
            }
            Type::Tuple { fields } => {
                let fields_str = fields.iter().map(|f| f.to_string()).collect::<Vec<_>>();
                write!(f, "({})", fields_str.join(", "))
            }
            Type::Struct { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, f.ty))
                    .collect::<Vec<_>>();
                write!(f, "{} {{ {} }}", name, fields_str.join(", "))
            }
        }
    }
}

impl Type {
    pub fn size_of(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::UInt64 => 8,
            Type::Bool => 1,
            Type::Array { elem_ty, dims } => {
                let total_elems: usize = dims.iter().product();
                total_elems * elem_ty.size_of()
            }
            Type::Tuple { fields } => {
                let total_size: usize = fields.iter().map(|f| f.size_of()).sum();
                total_size
            }
            Type::Struct { fields, .. } => {
                let total_size: usize = fields.iter().map(|f| f.ty.size_of()).sum();
                total_size
            }
            Type::Unknown => panic!("Unknown type"),
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            Type::Unit => 1,
            Type::UInt64 => 8,
            Type::Bool => 1,
            Type::Array { elem_ty, .. } => elem_ty.align_of(),
            Type::Tuple { fields } => fields.iter().map(|f| f.align_of()).max().unwrap_or(1),
            Type::Struct { fields, .. } => {
                fields.iter().map(|f| f.ty.align_of()).max().unwrap_or(1)
            }
            Type::Unknown => panic!("Unknown type"),
        }
    }

    pub fn is_compound(&self) -> bool {
        matches!(
            self,
            Type::Array { .. } | Type::Tuple { .. } | Type::Struct { .. }
        )
    }

    pub fn tuple_field_offset(&self, index: usize) -> usize {
        match self {
            Type::Tuple { fields } => {
                assert!(index < fields.len(), "Tuple field index out of bounds");
                fields.iter().take(index).map(|f| f.size_of()).sum()
            }
            _ => panic!("Expected tuple type"),
        }
    }

    pub fn struct_field_offset(&self, field_name: &str) -> usize {
        match self {
            Type::Struct { fields, .. } => {
                let mut offset = 0;
                for field in fields {
                    if field.name == field_name {
                        return offset;
                    }
                    offset += field.ty.size_of();
                }
                panic!("Field not found in struct");
            }
            _ => panic!("Expected struct type"),
        }
    }

    pub fn struct_field_type(&self, field_name: &str) -> Type {
        match self {
            Type::Struct { fields, .. } => fields
                .iter()
                .find(|f| f.name == field_name)
                .map(|f| f.ty.clone())
                .unwrap_or_else(|| panic!("Field not found in struct")),
            _ => panic!("Expected struct type"),
        }
    }

    pub fn has_field(&self, field_name: &str) -> bool {
        match self {
            Type::Struct { fields, .. } => fields.iter().any(|f| f.name == field_name),
            _ => false,
        }
    }
}
