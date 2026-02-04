mod format;
mod relations;
mod type_cache;

pub use relations::{
    TypeAssignability, ValueAssignability, array_to_slice_assignable, type_assignable,
    value_assignable,
};
pub use type_cache::{TypeCache, TypeId};

use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Eq)]
pub enum Type {
    Unknown,

    // Scalar Types
    Unit,
    Int {
        signed: bool,
        bits: u8,
        bounds: Option<IntBounds>,
        nonzero: bool,
    },
    Bool,
    Char,
    Range {
        elem_ty: Box<Type>,
    },
    Fn {
        params: Vec<FnParam>,
        ret_ty: Box<Type>,
    },

    // Compound Types
    String,
    Array {
        elem_ty: Box<Type>,
        dims: Vec<usize>,
    },
    Tuple {
        field_tys: Vec<Type>,
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
    Heap {
        elem_ty: Box<Type>,
    },
    Ref {
        mutable: bool,
        elem_ty: Box<Type>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnParam {
    pub mode: FnParamMode,
    pub ty: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IntBounds {
    pub min: i128,
    pub max_excl: i128,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FnParamMode {
    In,
    InOut,
    Out,
    Sink,
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
                Type::Int {
                    signed: lsigned,
                    bits: lbits,
                    bounds: lbounds,
                    nonzero: lnonzero,
                },
                Type::Int {
                    signed: rsigned,
                    bits: rbits,
                    bounds: rbounds,
                    nonzero: rnonzero,
                },
            ) => lsigned == rsigned && lbits == rbits && lbounds == rbounds && lnonzero == rnonzero,
            (Type::Range { elem_ty: l_elem }, Type::Range { elem_ty: r_elem }) => l_elem == r_elem,
            (
                Type::Fn {
                    params: p1,
                    ret_ty: r1,
                },
                Type::Fn {
                    params: p2,
                    ret_ty: r2,
                },
            ) => p1 == p2 && r1 == r2,
            (Type::Unknown, Type::Unknown) => true,
            (Type::Unit, Type::Unit) => true,
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
            (Type::Tuple { field_tys: f1 }, Type::Tuple { field_tys: f2 }) => f1 == f2,
            (Type::Struct { name: n1, .. }, Type::Struct { name: n2, .. }) => n1 == n2,
            (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => n1 == n2,
            (Type::Slice { elem_ty: e1 }, Type::Slice { elem_ty: e2 }) => e1 == e2,
            (Type::Heap { elem_ty: e1 }, Type::Heap { elem_ty: e2 }) => e1 == e2,
            (
                Type::Ref {
                    mutable: m1,
                    elem_ty: e1,
                },
                Type::Ref {
                    mutable: m2,
                    elem_ty: e2,
                },
            ) => m1 == m2 && e1 == e2,
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
            Type::Int {
                signed,
                bits,
                bounds,
                nonzero,
            } => {
                2u8.hash(state);
                signed.hash(state);
                bits.hash(state);
                bounds.hash(state);
                nonzero.hash(state);
            }
            Type::Bool => {
                3u8.hash(state);
            }
            Type::Char => {
                4u8.hash(state);
            }
            Type::Fn { params, ret_ty } => {
                6u8.hash(state);
                params.hash(state);
                ret_ty.hash(state);
            }
            Type::Range { elem_ty } => {
                7u8.hash(state);
                elem_ty.hash(state);
            }
            Type::String => {
                8u8.hash(state);
            }
            Type::Array { elem_ty, dims } => {
                9u8.hash(state);
                elem_ty.hash(state);
                dims.hash(state);
            }
            Type::Tuple { field_tys } => {
                10u8.hash(state);
                field_tys.hash(state);
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
            Type::Heap { elem_ty } => {
                14u8.hash(state);
                elem_ty.hash(state);
            }
            Type::Ref { mutable, elem_ty } => {
                15u8.hash(state);
                mutable.hash(state);
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
        bounds: None,
        nonzero: false,
    },
    Type::Int {
        signed: false,
        bits: 16,
        bounds: None,
        nonzero: false,
    },
    Type::Int {
        signed: false,
        bits: 32,
        bounds: None,
        nonzero: false,
    },
    Type::Int {
        signed: false,
        bits: 64,
        bounds: None,
        nonzero: false,
    },
    Type::Int {
        signed: true,
        bits: 8,
        bounds: None,
        nonzero: false,
    },
    Type::Int {
        signed: true,
        bits: 16,
        bounds: None,
        nonzero: false,
    },
    Type::Int {
        signed: true,
        bits: 32,
        bounds: None,
        nonzero: false,
    },
    Type::Int {
        signed: true,
        bits: 64,
        bounds: None,
        nonzero: false,
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
            bounds: None,
            nonzero: false,
        }
    }

    pub fn sint(bits: u8) -> Self {
        Type::Int {
            signed: true,
            bits,
            bounds: None,
            nonzero: false,
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int { .. })
    }

    pub fn int_signed_bits(&self) -> Option<(bool, u8)> {
        match self {
            Type::Int { signed, bits, .. } => Some((*signed, *bits)),
            _ => None,
        }
    }

    pub fn size_of(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::Int { bits, .. } => (*bits as usize) / 8,
            Type::Bool => 1,
            Type::Char => 4,
            Type::Range { elem_ty } => elem_ty.size_of(),
            Type::Fn { .. } => 8,
            Type::String => 16,
            Type::Array { elem_ty, dims } => {
                let total_elems: usize = dims.iter().product();
                total_elems * elem_ty.size_of()
            }
            Type::Tuple { field_tys } => {
                let total_size: usize = field_tys.iter().map(|f| f.size_of()).sum();
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
            Type::Heap { .. } => 8,
            Type::Ref { .. } => 8,
            Type::Unknown => panic!("Unknown type"),
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            Type::Unit => 1,
            Type::Int { bits, .. } => (*bits as usize) / 8,
            Type::Bool => 1,
            Type::Char => 4,
            Type::Range { elem_ty } => elem_ty.align_of(),
            Type::Fn { .. } => 8,
            Type::String => 8,
            Type::Array { elem_ty, .. } => elem_ty.align_of(),
            Type::Tuple { field_tys } => field_tys.iter().map(|t| t.align_of()).max().unwrap_or(1),
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
            Type::Heap { .. } => 8,
            Type::Ref { .. } => 8,
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

    pub fn is_heap(&self) -> bool {
        matches!(self, Type::Heap { .. })
    }

    pub fn peel_heap(&self) -> Type {
        let mut ty = self.clone();
        while let Type::Heap { elem_ty } = ty {
            ty = *elem_ty;
        }
        ty
    }

    pub fn peel_heap_with_count(&self) -> (Type, usize) {
        let mut ty = self.clone();
        let mut count = 0usize;
        while let Type::Heap { elem_ty } = ty {
            count += 1;
            ty = *elem_ty;
        }
        (ty, count)
    }

    pub fn is_move_tracked(&self) -> bool {
        self.is_compound() || self.is_heap()
    }

    pub fn needs_drop(&self) -> bool {
        match self {
            Type::Heap { .. } => true,
            Type::String => true,
            Type::Array { elem_ty, .. } => elem_ty.needs_drop(),
            Type::Tuple { field_tys } => field_tys.iter().any(Type::needs_drop),
            Type::Struct { fields, .. } => fields.iter().any(|f| f.ty.needs_drop()),
            Type::Enum { variants, .. } => variants
                .iter()
                .any(|v| v.payload.iter().any(Type::needs_drop)),
            _ => false,
        }
    }

    pub fn is_scalar(&self) -> bool {
        !self.is_compound()
    }

    pub fn array_item_type(&self) -> Option<Type> {
        let Type::Array { elem_ty, dims } = self else {
            return None;
        };
        if dims.is_empty() {
            return None;
        }
        if dims.len() == 1 {
            Some((**elem_ty).clone())
        } else {
            Some(Type::Array {
                elem_ty: elem_ty.clone(),
                dims: dims[1..].to_vec(),
            })
        }
    }

    pub fn tuple_field_offset(&self, index: usize) -> usize {
        let Type::Tuple { field_tys } = self else {
            panic!("Expected tuple type");
        };
        assert!(index < field_tys.len(), "Tuple field index out of bounds");
        field_tys.iter().take(index).map(|f| f.size_of()).sum()
    }

    pub fn tuple_field_type(&self, index: usize) -> Type {
        let Type::Tuple { field_tys } = self else {
            panic!("Expected tuple type");
        };
        field_tys[index].clone()
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
            Type::Int {
                signed: true, bits, ..
            } => -(1i128 << (*bits as u32 - 1)),
            _ => panic!("Expected integer type"),
        }
    }

    pub fn max_value(&self) -> i128 {
        match self {
            Type::Int {
                signed: false,
                bits,
                ..
            } => (1i128 << (*bits as u32)) - 1,
            Type::Int {
                signed: true, bits, ..
            } => (1i128 << (*bits as u32 - 1)) - 1,
            _ => panic!("Expected integer type"),
        }
    }

    pub fn is_int_in_range(&self, value: i128) -> bool {
        value >= self.min_value() && value <= self.max_value()
    }
}
