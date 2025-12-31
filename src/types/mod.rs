mod format;
mod relations;

pub use relations::{TypeAssignability, ValueAssignability, type_assignable, value_assignable};

use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Eq)]
pub enum Type {
    Unknown,

    // Scalar Types
    Unit,
    Int {
        signed: bool,
        bits: u8,
    },
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
            (
                Type::Int {
                    signed: ls,
                    bits: lb,
                },
                Type::Int {
                    signed: rs,
                    bits: rb,
                },
            ) => ls == rs && lb == rb,
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
            Type::Int { signed, bits } => {
                2u8.hash(state);
                signed.hash(state);
                bits.hash(state);
            }
            Type::Bool => {
                3u8.hash(state);
            }
            Type::Char => {
                4u8.hash(state);
            }
            Type::Range { min, max } => {
                5u8.hash(state);
                min.hash(state);
                max.hash(state);
            }
            Type::String => {
                6u8.hash(state);
            }
            Type::Array { elem_ty, dims } => {
                7u8.hash(state);
                elem_ty.hash(state);
                dims.hash(state);
            }
            Type::Tuple { fields } => {
                8u8.hash(state);
                fields.hash(state);
            }
            Type::Struct { name, .. } => {
                // Nominal type: only hash the name
                9u8.hash(state);
                name.hash(state);
            }
            Type::Enum { name, .. } => {
                // Nominal type: only hash the name
                10u8.hash(state);
                name.hash(state);
            }
            Type::Slice { elem_ty } => {
                11u8.hash(state);
                elem_ty.hash(state);
            }
        }
    }
}

pub const BUILTIN_TYPES: &[Type] = &[
    Type::Unit,
    Type::Int {
        signed: false,
        bits: 8,
    },
    Type::Int {
        signed: false,
        bits: 16,
    },
    Type::Int {
        signed: false,
        bits: 32,
    },
    Type::Int {
        signed: false,
        bits: 64,
    },
    Type::Int {
        signed: true,
        bits: 8,
    },
    Type::Int {
        signed: true,
        bits: 16,
    },
    Type::Int {
        signed: true,
        bits: 32,
    },
    Type::Int {
        signed: true,
        bits: 64,
    },
    Type::Bool,
    Type::Char,
    Type::String,
];

pub fn is_builtin_type_name(name: &str) -> bool {
    matches!(
        name,
        "()" | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "bool"
            | "char"
            | "string"
    )
}

impl Type {
    pub fn uint(bits: u8) -> Self {
        Type::Int {
            signed: false,
            bits,
        }
    }

    pub fn sint(bits: u8) -> Self {
        Type::Int { signed: true, bits }
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int { .. })
    }

    pub fn int_signed_bits(&self) -> Option<(bool, u8)> {
        match self {
            Type::Int { signed, bits } => Some((*signed, *bits)),
            _ => None,
        }
    }

    pub fn size_of(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::Int { bits, .. } => (*bits as usize) / 8,
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
            Type::Int { bits, .. } => (*bits as usize) / 8,
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

    pub fn min_value(&self) -> i128 {
        match self {
            Type::Int { signed: false, .. } => 0,
            Type::Int { signed: true, bits } => -(1i128 << (*bits as u32 - 1)),
            _ => panic!("Expected integer type"),
        }
    }

    pub fn max_value(&self) -> i128 {
        match self {
            Type::Int {
                signed: false,
                bits,
            } => (1i128 << (*bits as u32)) - 1,
            Type::Int { signed: true, bits } => (1i128 << (*bits as u32 - 1)) - 1,
            _ => panic!("Expected integer type"),
        }
    }

    pub fn is_int_in_range(&self, value: i128) -> bool {
        value >= self.min_value() && value <= self.max_value()
    }
}
