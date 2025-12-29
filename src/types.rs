use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Eq)]
pub enum Type {
    Unknown,

    // Scalar Types
    Unit,
    UInt64,
    UInt32,
    UInt8,
    Bool,
    Char,
    Range {
        min: u64,
        max: u64,
    },

    // Compound Types
    String,
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
    Enum {
        name: String,
        variants: Vec<EnumVariant>,
    },

    // Internal Types
    Slice {
        elem_ty: Box<Type>,
    },
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Type::Range {
                    min: lmin,
                    max: lmax,
                },
                Type::Range {
                    min: rmin,
                    max: rmax,
                },
            ) => lmin == rmin && lmax == rmax,
            (Type::Unknown, Type::Unknown) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::UInt64, Type::UInt64) => true,
            (Type::UInt32, Type::UInt32) => true,
            (Type::UInt8, Type::UInt8) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Char, Type::Char) => true,
            (Type::String, Type::String) => true,
            (
                Type::Array {
                    elem_ty: e1,
                    dims: d1,
                },
                Type::Array {
                    elem_ty: e2,
                    dims: d2,
                },
            ) => e1 == e2 && d1 == d2,
            (Type::Tuple { fields: f1 }, Type::Tuple { fields: f2 }) => f1 == f2,
            (Type::Struct { name: n1, .. }, Type::Struct { name: n2, .. }) => n1 == n2,
            (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => n1 == n2,
            (Type::Slice { elem_ty: e1 }, Type::Slice { elem_ty: e2 }) => e1 == e2,
            _ => false,
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Unknown => {
                0u8.hash(state);
            }
            Type::Unit => {
                1u8.hash(state);
            }
            Type::UInt64 => {
                2u8.hash(state);
            }
            Type::UInt32 => {
                3u8.hash(state);
            }
            Type::UInt8 => {
                4u8.hash(state);
            }
            Type::Bool => {
                5u8.hash(state);
            }
            Type::Char => {
                6u8.hash(state);
            }
            Type::Range { min, max } => {
                7u8.hash(state);
                min.hash(state);
                max.hash(state);
            }
            Type::String => {
                8u8.hash(state);
            }
            Type::Array { elem_ty, dims } => {
                9u8.hash(state);
                elem_ty.hash(state);
                dims.hash(state);
            }
            Type::Tuple { fields } => {
                10u8.hash(state);
                fields.hash(state);
            }
            Type::Struct { name, .. } => {
                // Nominal type: only hash the name
                11u8.hash(state);
                name.hash(state);
            }
            Type::Enum { name, .. } => {
                // Nominal type: only hash the name
                12u8.hash(state);
                name.hash(state);
            }
            Type::Slice { elem_ty } => {
                13u8.hash(state);
                elem_ty.hash(state);
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub payload: Vec<Type>,
}

pub const BUILTIN_TYPES: &[Type] = &[
    Type::Unit,
    Type::UInt64,
    Type::UInt32,
    Type::UInt8,
    Type::Bool,
    Type::Char,
    Type::String,
];

pub fn is_builtin_type_name(name: &str) -> bool {
    matches!(
        name,
        "()" | "u8" | "u32" | "u64" | "bool" | "char" | "string"
    )
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "()"),
            Type::UInt64 => write!(f, "u64"),
            Type::UInt32 => write!(f, "u32"),
            Type::UInt8 => write!(f, "u8"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Range { min, max } => write!(f, "range({}, {})", min, max),
            Type::String => write!(f, "string"),
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
            Type::Enum { name, variants } => {
                let variant_names = variants
                    .iter()
                    .map(|v| {
                        if v.payload.is_empty() {
                            v.name.clone()
                        } else {
                            let payload_str = v
                                .payload
                                .iter()
                                .map(|p| p.to_string())
                                .collect::<Vec<_>>()
                                .join(", ");
                            format!("{}({})", v.name, payload_str)
                        }
                    })
                    .collect::<Vec<_>>();
                write!(f, "{} = {}", name, variant_names.join(" | "))
            }
            Type::Slice { elem_ty } => {
                write!(f, "{}[]", elem_ty)
            }
        }
    }
}

impl Type {
    pub fn size_of(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::UInt64 => 8,
            Type::UInt32 => 4,
            Type::UInt8 => 1,
            Type::Bool => 1,
            Type::Char => 4,
            Type::Range { .. } => 8,
            Type::String => 16,
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
            Type::Enum { .. } => {
                // 8 bytes for the scalar tag + max payload size
                8 + self.enum_max_payload_size()
            }
            Type::Slice { .. } => {
                // 8 bytes for the pointer + 8 bytes for the length
                16
            }
            Type::Unknown => panic!("Unknown type"),
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            Type::Unit => 1,
            Type::UInt64 => 8,
            Type::UInt32 => 4,
            Type::UInt8 => 1,
            Type::Bool => 1,
            Type::Char => 4,
            Type::Range { .. } => 8,
            Type::String => 8,
            Type::Array { elem_ty, .. } => elem_ty.align_of(),
            Type::Tuple { fields } => fields.iter().map(|f| f.align_of()).max().unwrap_or(1),
            Type::Struct { fields, .. } => {
                fields.iter().map(|f| f.ty.align_of()).max().unwrap_or(1)
            }
            Type::Enum { variants, .. } => {
                // max of 8 and the max alignment of any payload element across all variants.
                let max_payload_align = variants
                    .iter()
                    .map(|v| v.payload.iter().map(|p| p.align_of()).max().unwrap_or(0))
                    .max()
                    .unwrap_or(0);
                max_payload_align.max(8)
            }
            Type::Slice { .. } => 8,
            Type::Unknown => panic!("Unknown type"),
        }
    }

    pub fn is_compound(&self) -> bool {
        let is_compound = matches!(
            self,
            Type::Array { .. }
                | Type::Tuple { .. }
                | Type::Struct { .. }
                | Type::String
                | Type::Slice { .. }
        );

        // if it's an enum, check if it has a payload
        let has_payload = if let Type::Enum { variants, .. } = self {
            variants.iter().any(|v| !v.payload.is_empty())
        } else {
            false
        };

        is_compound || has_payload
    }

    pub fn is_scalar(&self) -> bool {
        !self.is_compound()
    }

    pub fn tuple_field_offset(&self, index: usize) -> usize {
        let Type::Tuple { fields } = self else {
            panic!("Expected tuple type");
        };
        assert!(index < fields.len(), "Tuple field index out of bounds");
        fields.iter().take(index).map(|f| f.size_of()).sum()
    }

    pub fn tuple_field_type(&self, index: usize) -> Type {
        let Type::Tuple { fields } = self else {
            panic!("Expected tuple type");
        };
        fields[index].clone()
    }

    pub fn struct_field_offset(&self, field_name: &str) -> usize {
        let Type::Struct { fields, .. } = self else {
            panic!("Expected struct type");
        };
        let mut offset = 0;
        for field in fields {
            if field.name == field_name {
                return offset;
            }
            offset += field.ty.size_of();
        }
        panic!("Field not found in struct");
    }

    pub fn struct_field_index(&self, field_name: &str) -> usize {
        let Type::Struct { fields, .. } = self else {
            panic!("Expected struct type");
        };
        fields
            .iter()
            .position(|f| f.name == field_name)
            .expect("Field not found in struct")
    }

    pub fn struct_field_type(&self, field_name: &str) -> Type {
        let Type::Struct { fields, .. } = self else {
            panic!("Expected struct type");
        };
        fields
            .iter()
            .find(|f| f.name == field_name)
            .map(|f| f.ty.clone())
            .unwrap_or_else(|| panic!("Field not found in struct"))
    }

    pub fn has_field(&self, field_name: &str) -> bool {
        let Type::Struct { fields, .. } = self else {
            panic!("Expected struct type");
        };
        fields.iter().any(|f| f.name == field_name)
    }

    pub fn enum_variant_index(&self, variant: &str) -> usize {
        let Type::Enum { variants, .. } = self else {
            panic!("Expected enum type");
        };
        variants
            .iter()
            .position(|v| v.name == variant)
            .unwrap_or_else(|| panic!("Variant not found in enum"))
    }

    pub fn enum_max_payload_size(&self) -> usize {
        let Type::Enum { variants, .. } = self else {
            panic!("Expected enum type");
        };
        // take the max payload size across all variants
        variants
            .iter()
            .map(|v| v.payload.iter().map(|p| p.size_of()).sum::<usize>())
            .max()
            .unwrap_or(0)
    }

    pub fn enum_variant_payload_offsets(&self, variant: &str) -> Vec<usize> {
        let Type::Enum { variants, .. } = self else {
            panic!("Expected enum type");
        };
        let v = variants
            .iter()
            .find(|v| v.name == variant)
            .expect("Variant not found in enum");
        let mut offsets = Vec::with_capacity(v.payload.len());
        let mut offset = 0;
        for ty in &v.payload {
            offsets.push(offset);
            offset += ty.size_of();
        }
        offsets
    }

    pub fn min_value(&self) -> u64 {
        match self {
            Type::UInt64 => 0,
            Type::UInt32 => 0,
            Type::UInt8 => 0,
            _ => panic!("Expected integer type"),
        }
    }

    pub fn max_value(&self) -> u64 {
        match self {
            Type::UInt64 => u64::MAX,
            Type::UInt32 => u32::MAX as u64,
            Type::UInt8 => u8::MAX as u64,
            _ => panic!("Expected integer type"),
        }
    }

    pub fn is_int_in_range(&self, value: u64) -> bool {
        value >= self.min_value() && value <= self.max_value()
    }
}
