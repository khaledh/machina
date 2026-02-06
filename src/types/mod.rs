mod format;
mod relations;
mod type_cache;

pub use relations::{
    TypeAssignability, ValueAssignability, array_to_slice_assignable, type_assignable,
    value_assignable,
};
pub use type_cache::{TypeCache, TypeId};

use std::borrow::Cow;
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
    Var(TyVarId),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TyVarId(u32);

impl TyVarId {
    pub fn new(value: u32) -> Self {
        Self(value)
    }

    pub fn index(self) -> u32 {
        self.0
    }
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
            (Type::Var(v1), Type::Var(v2)) => v1 == v2,
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
            Type::Var(var) => {
                16u8.hash(state);
                var.hash(state);
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
            Type::Var(_) => panic!("Type variable has no size"),
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
            Type::Var(_) => panic!("Type variable has no alignment"),
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

    /// Bottom-up transform: recursively applies `f` to every child type first,
    /// then applies `f` to the rebuilt node. The closure sees a node whose
    /// children have already been transformed.
    pub fn map(self, f: &impl Fn(Type) -> Type) -> Type {
        let rebuilt = match self {
            Type::Fn { params, ret_ty } => Type::Fn {
                params: params
                    .into_iter()
                    .map(|p| FnParam {
                        mode: p.mode,
                        ty: p.ty.map(f),
                    })
                    .collect(),
                ret_ty: Box::new((*ret_ty).map(f)),
            },
            Type::Range { elem_ty } => Type::Range {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::Array { elem_ty, dims } => Type::Array {
                elem_ty: Box::new((*elem_ty).map(f)),
                dims,
            },
            Type::Tuple { field_tys } => Type::Tuple {
                field_tys: field_tys.into_iter().map(|ty| ty.map(f)).collect(),
            },
            Type::Struct { name, fields } => Type::Struct {
                name,
                fields: fields
                    .into_iter()
                    .map(|field| StructField {
                        name: field.name,
                        ty: field.ty.map(f),
                    })
                    .collect(),
            },
            Type::Enum { name, variants } => Type::Enum {
                name,
                variants: variants
                    .into_iter()
                    .map(|v| EnumVariant {
                        name: v.name,
                        payload: v.payload.into_iter().map(|ty| ty.map(f)).collect(),
                    })
                    .collect(),
            },
            Type::Slice { elem_ty } => Type::Slice {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::Heap { elem_ty } => Type::Heap {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::Ref { mutable, elem_ty } => Type::Ref {
                mutable,
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            // Leaf types: no children to recurse into.
            other => other,
        };
        f(rebuilt)
    }

    /// Like `map` but takes `&self` and clones.
    pub fn map_cloned(&self, f: &impl Fn(Type) -> Type) -> Type {
        self.clone().map(f)
    }

    /// Bottom-up transform over a borrowed type with clone-on-write behavior.
    ///
    /// Child nodes are transformed first. If no child changes and `f` returns
    /// `None` for the rebuilt node, this returns `Cow::Borrowed(self)`.
    /// Returning `Some(new_ty)` from `f` replaces the rebuilt node.
    pub fn map_cow<'a>(&'a self, f: &impl Fn(&Type) -> Option<Type>) -> Cow<'a, Type> {
        let rebuilt = match self {
            Type::Fn { params, ret_ty } => {
                let mapped_params = params
                    .iter()
                    .map(|param| (param.mode, param.ty.map_cow(f)))
                    .collect::<Vec<_>>();
                let mapped_ret = ret_ty.map_cow(f);
                let changed = matches!(mapped_ret, Cow::Owned(_))
                    || mapped_params
                        .iter()
                        .any(|(_, ty)| matches!(ty, Cow::Owned(_)));
                if changed {
                    Cow::Owned(Type::Fn {
                        params: mapped_params
                            .into_iter()
                            .map(|(mode, ty)| FnParam {
                                mode,
                                ty: ty.into_owned(),
                            })
                            .collect(),
                        ret_ty: Box::new(mapped_ret.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Range { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::Range {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Array { elem_ty, dims } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::Array {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                        dims: dims.clone(),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Tuple { field_tys } => {
                let mapped_fields = field_tys.iter().map(|ty| ty.map_cow(f)).collect::<Vec<_>>();
                if mapped_fields.iter().any(|ty| matches!(ty, Cow::Owned(_))) {
                    Cow::Owned(Type::Tuple {
                        field_tys: mapped_fields.into_iter().map(Cow::into_owned).collect(),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Struct { name, fields } => {
                let mapped_fields = fields
                    .iter()
                    .map(|field| (&field.name, field.ty.map_cow(f)))
                    .collect::<Vec<_>>();
                let changed = mapped_fields
                    .iter()
                    .any(|(_, ty)| matches!(ty, Cow::Owned(_)));
                if changed {
                    Cow::Owned(Type::Struct {
                        name: name.clone(),
                        fields: mapped_fields
                            .into_iter()
                            .map(|(field_name, ty)| StructField {
                                name: field_name.clone(),
                                ty: ty.into_owned(),
                            })
                            .collect(),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Enum { name, variants } => {
                let mapped_variants = variants
                    .iter()
                    .map(|variant| {
                        let payload = variant
                            .payload
                            .iter()
                            .map(|ty| ty.map_cow(f))
                            .collect::<Vec<_>>();
                        (&variant.name, payload)
                    })
                    .collect::<Vec<_>>();
                let changed = mapped_variants
                    .iter()
                    .any(|(_, payload)| payload.iter().any(|ty| matches!(ty, Cow::Owned(_))));
                if changed {
                    Cow::Owned(Type::Enum {
                        name: name.clone(),
                        variants: mapped_variants
                            .into_iter()
                            .map(|(variant_name, payload)| EnumVariant {
                                name: variant_name.clone(),
                                payload: payload.into_iter().map(Cow::into_owned).collect(),
                            })
                            .collect(),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Slice { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::Slice {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Heap { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::Heap {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Ref { mutable, elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::Ref {
                        mutable: *mutable,
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            _ => Cow::Borrowed(self),
        };

        if let Some(mapped) = f(rebuilt.as_ref()) {
            Cow::Owned(mapped)
        } else {
            rebuilt
        }
    }

    /// Returns `true` if `predicate` holds for this type or any nested child type.
    pub fn any(&self, predicate: &impl Fn(&Type) -> bool) -> bool {
        if predicate(self) {
            return true;
        }
        match self {
            Type::Fn { params, ret_ty } => {
                params.iter().any(|p| p.ty.any(predicate)) || ret_ty.any(predicate)
            }
            Type::Range { elem_ty }
            | Type::Array { elem_ty, .. }
            | Type::Slice { elem_ty }
            | Type::Heap { elem_ty }
            | Type::Ref { elem_ty, .. } => elem_ty.any(predicate),
            Type::Tuple { field_tys } => field_tys.iter().any(|ty| ty.any(predicate)),
            Type::Struct { fields, .. } => fields.iter().any(|f| f.ty.any(predicate)),
            Type::Enum { variants, .. } => variants
                .iter()
                .any(|v| v.payload.iter().any(|ty| ty.any(predicate))),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::borrow::Cow;

    #[test]
    fn test_map_cow_borrows_when_unchanged() {
        let ty = Type::Array {
            elem_ty: Box::new(Type::uint(64)),
            dims: vec![4],
        };

        let mapped = ty.map_cow(&|_| None);
        assert!(matches!(mapped, Cow::Borrowed(_)));
    }

    #[test]
    fn test_map_cow_rewrites_only_when_needed() {
        let ty = Type::Tuple {
            field_tys: vec![Type::Var(TyVarId::new(1)), Type::Bool],
        };

        let mapped = ty.map_cow(&|t| match t {
            Type::Var(var) if *var == TyVarId::new(1) => Some(Type::uint(64)),
            _ => None,
        });
        assert!(matches!(mapped, Cow::Owned(_)));
        assert_eq!(
            mapped.into_owned(),
            Type::Tuple {
                field_tys: vec![Type::uint(64), Type::Bool]
            }
        );
    }
}
