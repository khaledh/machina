mod format;
mod relations;
mod render;
mod type_cache;

pub use relations::{
    TypeAssignability, ValueAssignability, array_to_dyn_array_assignable,
    array_to_slice_assignable, dyn_array_to_slice_assignable, type_assignable, value_assignable,
};
pub use render::{TypeRenderConfig, render_type};
pub use type_cache::{TypeCache, TypeId};

use std::borrow::Cow;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Eq)]
pub enum Type {
    Unknown,

    // Scalar Types
    Unit,
    PAddr,
    NullablePAddr,
    VAddr,
    NullableVAddr,
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
    ErrorUnion {
        ok_ty: Box<Type>,
        err_tys: Vec<Type>,
    },

    // Compound Types
    String,
    Array {
        elem_ty: Box<Type>,
        dims: Vec<usize>,
    },
    DynArray {
        elem_ty: Box<Type>,
    },
    Pending {
        response_tys: Vec<Type>,
    },
    ReplyCap {
        response_tys: Vec<Type>,
    },
    View {
        elem_ty: Box<Type>,
    },
    NullableView {
        elem_ty: Box<Type>,
    },
    NullableViewSlice {
        elem_ty: Box<Type>,
    },
    NullableViewArray {
        elem_ty: Box<Type>,
    },
    RawPtr {
        elem_ty: Box<Type>,
    },
    ViewSlice {
        elem_ty: Box<Type>,
    },
    ViewArray {
        elem_ty: Box<Type>,
    },
    Set {
        elem_ty: Box<Type>,
    },
    Iterable {
        item_ty: Box<Type>,
    },
    Map {
        key_ty: Box<Type>,
        value_ty: Box<Type>,
    },
    Tuple {
        field_tys: Vec<Type>,
    },
    Struct {
        name: String,
        type_args: Vec<Type>,
        fields: Vec<StructField>,
    },
    Enum {
        name: String,
        type_args: Vec<Type>,
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

pub fn format_nominal_type_name(name: &str, type_args: &[Type]) -> String {
    if type_args.is_empty() {
        name.to_string()
    } else {
        let args = type_args
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        format!("{name}<{args}>")
    }
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
            (
                Type::ErrorUnion {
                    ok_ty: lok,
                    err_tys: lerrs,
                },
                Type::ErrorUnion {
                    ok_ty: rok,
                    err_tys: rerrs,
                },
            ) => lok == rok && lerrs == rerrs,
            (Type::Unknown, Type::Unknown) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::PAddr, Type::PAddr) => true,
            (Type::NullablePAddr, Type::NullablePAddr) => true,
            (Type::VAddr, Type::VAddr) => true,
            (Type::NullableVAddr, Type::NullableVAddr) => true,
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
            (Type::DynArray { elem_ty: e1 }, Type::DynArray { elem_ty: e2 }) => e1 == e2,
            (Type::Pending { response_tys: r1 }, Type::Pending { response_tys: r2 }) => r1 == r2,
            (Type::ReplyCap { response_tys: r1 }, Type::ReplyCap { response_tys: r2 }) => r1 == r2,
            (Type::View { elem_ty: e1 }, Type::View { elem_ty: e2 }) => e1 == e2,
            (Type::NullableView { elem_ty: e1 }, Type::NullableView { elem_ty: e2 }) => e1 == e2,
            (Type::NullableViewSlice { elem_ty: e1 }, Type::NullableViewSlice { elem_ty: e2 }) => {
                e1 == e2
            }
            (Type::NullableViewArray { elem_ty: e1 }, Type::NullableViewArray { elem_ty: e2 }) => {
                e1 == e2
            }
            (Type::RawPtr { elem_ty: e1 }, Type::RawPtr { elem_ty: e2 }) => e1 == e2,
            (Type::ViewSlice { elem_ty: e1 }, Type::ViewSlice { elem_ty: e2 }) => e1 == e2,
            (Type::ViewArray { elem_ty: e1 }, Type::ViewArray { elem_ty: e2 }) => e1 == e2,
            (Type::Set { elem_ty: e1 }, Type::Set { elem_ty: e2 }) => e1 == e2,
            (Type::Iterable { item_ty: e1 }, Type::Iterable { item_ty: e2 }) => e1 == e2,
            (
                Type::Map {
                    key_ty: lk,
                    value_ty: lv,
                },
                Type::Map {
                    key_ty: rk,
                    value_ty: rv,
                },
            ) => lk == rk && lv == rv,
            (Type::Tuple { field_tys: f1 }, Type::Tuple { field_tys: f2 }) => f1 == f2,
            (
                Type::Struct {
                    name: n1,
                    type_args: a1,
                    ..
                },
                Type::Struct {
                    name: n2,
                    type_args: a2,
                    ..
                },
            ) => n1 == n2 && a1 == a2,
            (
                Type::Enum {
                    name: n1,
                    type_args: a1,
                    ..
                },
                Type::Enum {
                    name: n2,
                    type_args: a2,
                    ..
                },
            ) => n1 == n2 && a1 == a2,
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
            Type::PAddr => {
                24u8.hash(state);
            }
            Type::NullablePAddr => {
                25u8.hash(state);
            }
            Type::VAddr => {
                26u8.hash(state);
            }
            Type::NullableVAddr => {
                27u8.hash(state);
            }
            Type::NullableView { elem_ty } => {
                28u8.hash(state);
                elem_ty.hash(state);
            }
            Type::NullableViewSlice { elem_ty } => {
                29u8.hash(state);
                elem_ty.hash(state);
            }
            Type::NullableViewArray { elem_ty } => {
                30u8.hash(state);
                elem_ty.hash(state);
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
            Type::ErrorUnion { ok_ty, err_tys } => {
                7u8.hash(state);
                ok_ty.hash(state);
                err_tys.hash(state);
            }
            Type::Range { elem_ty } => {
                8u8.hash(state);
                elem_ty.hash(state);
            }
            Type::String => {
                9u8.hash(state);
            }
            Type::Array { elem_ty, dims } => {
                10u8.hash(state);
                elem_ty.hash(state);
                dims.hash(state);
            }
            Type::DynArray { elem_ty } => {
                18u8.hash(state);
                elem_ty.hash(state);
            }
            Type::Pending { response_tys } => {
                21u8.hash(state);
                response_tys.hash(state);
            }
            Type::ReplyCap { response_tys } => {
                22u8.hash(state);
                response_tys.hash(state);
            }
            Type::View { elem_ty } => {
                28u8.hash(state);
                elem_ty.hash(state);
            }
            Type::RawPtr { elem_ty } => {
                31u8.hash(state);
                elem_ty.hash(state);
            }
            Type::ViewSlice { elem_ty } => {
                29u8.hash(state);
                elem_ty.hash(state);
            }
            Type::ViewArray { elem_ty } => {
                30u8.hash(state);
                elem_ty.hash(state);
            }
            Type::Set { elem_ty } => {
                19u8.hash(state);
                elem_ty.hash(state);
            }
            Type::Iterable { item_ty } => {
                23u8.hash(state);
                item_ty.hash(state);
            }
            Type::Map { key_ty, value_ty } => {
                20u8.hash(state);
                key_ty.hash(state);
                value_ty.hash(state);
            }
            Type::Tuple { field_tys } => {
                11u8.hash(state);
                field_tys.hash(state);
            }
            Type::Struct {
                name, type_args, ..
            } => {
                12u8.hash(state);
                name.hash(state);
                type_args.hash(state);
            }
            Type::Enum {
                name, type_args, ..
            } => {
                13u8.hash(state);
                name.hash(state);
                type_args.hash(state);
            }
            Type::Slice { elem_ty } => {
                14u8.hash(state);
                elem_ty.hash(state);
            }
            Type::Heap { elem_ty } => {
                15u8.hash(state);
                elem_ty.hash(state);
            }
            Type::Ref { mutable, elem_ty } => {
                16u8.hash(state);
                mutable.hash(state);
                elem_ty.hash(state);
            }
            Type::Var(var) => {
                17u8.hash(state);
                var.hash(state);
            }
        }
    }
}

pub const BUILTIN_TYPES: &[Type] = &[
    Type::Unit,
    Type::PAddr,
    Type::NullablePAddr,
    Type::VAddr,
    Type::NullableVAddr,
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
        "()" | "paddr"
            | "paddr?"
            | "vaddr"
            | "vaddr?"
            | "view"
            | "view?"
            | "view_slice"
            | "view_array"
            | "u8"
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

    pub fn is_address(&self) -> bool {
        matches!(self, Type::PAddr | Type::VAddr)
    }

    pub fn is_nullable_address(&self) -> bool {
        matches!(self, Type::NullablePAddr | Type::NullableVAddr)
    }

    pub fn is_nullable_view(&self) -> bool {
        matches!(
            self,
            Type::NullableView { .. }
                | Type::NullableViewSlice { .. }
                | Type::NullableViewArray { .. }
        )
    }

    pub fn nullable_address_payload(&self) -> Option<Type> {
        match self {
            Type::NullablePAddr => Some(Type::PAddr),
            Type::NullableVAddr => Some(Type::VAddr),
            _ => None,
        }
    }

    pub fn nullable_view_payload(&self) -> Option<Type> {
        match self {
            Type::NullableView { elem_ty } if !matches!(elem_ty.as_ref(), Type::Slice { .. }) => {
                Some(Type::View {
                    elem_ty: elem_ty.clone(),
                })
            }
            Type::NullableViewSlice { elem_ty } => Some(Type::ViewSlice {
                elem_ty: elem_ty.clone(),
            }),
            Type::NullableViewArray { elem_ty } => Some(Type::ViewArray {
                elem_ty: elem_ty.clone(),
            }),
            _ => None,
        }
    }

    pub fn shape_eq(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Unknown, Type::Unknown) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::PAddr, Type::PAddr) => true,
            (Type::NullablePAddr, Type::NullablePAddr) => true,
            (Type::VAddr, Type::VAddr) => true,
            (Type::NullableVAddr, Type::NullableVAddr) => true,
            (
                Type::Int {
                    signed: l_signed,
                    bits: l_bits,
                    bounds: l_bounds,
                    nonzero: l_nonzero,
                },
                Type::Int {
                    signed: r_signed,
                    bits: r_bits,
                    bounds: r_bounds,
                    nonzero: r_nonzero,
                },
            ) => {
                l_signed == r_signed
                    && l_bits == r_bits
                    && l_bounds == r_bounds
                    && l_nonzero == r_nonzero
            }
            (Type::Bool, Type::Bool) => true,
            (Type::Char, Type::Char) => true,
            (Type::String, Type::String) => true,
            (Type::Range { elem_ty: l }, Type::Range { elem_ty: r })
            | (Type::Slice { elem_ty: l }, Type::Slice { elem_ty: r })
            | (Type::DynArray { elem_ty: l }, Type::DynArray { elem_ty: r })
            | (Type::View { elem_ty: l }, Type::View { elem_ty: r })
            | (Type::NullableView { elem_ty: l }, Type::NullableView { elem_ty: r })
            | (Type::NullableViewSlice { elem_ty: l }, Type::NullableViewSlice { elem_ty: r })
            | (Type::NullableViewArray { elem_ty: l }, Type::NullableViewArray { elem_ty: r })
            | (Type::RawPtr { elem_ty: l }, Type::RawPtr { elem_ty: r })
            | (Type::ViewSlice { elem_ty: l }, Type::ViewSlice { elem_ty: r })
            | (Type::ViewArray { elem_ty: l }, Type::ViewArray { elem_ty: r })
            | (Type::Set { elem_ty: l }, Type::Set { elem_ty: r })
            | (Type::Iterable { item_ty: l }, Type::Iterable { item_ty: r })
            | (Type::Heap { elem_ty: l }, Type::Heap { elem_ty: r }) => l.shape_eq(r),
            (
                Type::Map {
                    key_ty: l_key,
                    value_ty: l_value,
                },
                Type::Map {
                    key_ty: r_key,
                    value_ty: r_value,
                },
            ) => l_key.shape_eq(r_key) && l_value.shape_eq(r_value),
            (
                Type::Ref {
                    mutable: l_mut,
                    elem_ty: l_elem,
                },
                Type::Ref {
                    mutable: r_mut,
                    elem_ty: r_elem,
                },
            ) => l_mut == r_mut && l_elem.shape_eq(r_elem),
            (
                Type::Fn {
                    params: l_params,
                    ret_ty: l_ret,
                },
                Type::Fn {
                    params: r_params,
                    ret_ty: r_ret,
                },
            ) => {
                l_params.len() == r_params.len()
                    && l_params
                        .iter()
                        .zip(r_params.iter())
                        .all(|(l, r)| l.mode == r.mode && l.ty.shape_eq(&r.ty))
                    && l_ret.shape_eq(r_ret)
            }
            (
                Type::Array {
                    elem_ty: l_elem,
                    dims: l_dims,
                },
                Type::Array {
                    elem_ty: r_elem,
                    dims: r_dims,
                },
            ) => l_dims == r_dims && l_elem.shape_eq(r_elem),
            (Type::Tuple { field_tys: l }, Type::Tuple { field_tys: r }) => {
                l.len() == r.len() && l.iter().zip(r.iter()).all(|(l, r)| l.shape_eq(r))
            }
            (
                Type::Struct {
                    name: l_name,
                    type_args: l_type_args,
                    fields: l_fields,
                },
                Type::Struct {
                    name: r_name,
                    type_args: r_type_args,
                    fields: r_fields,
                },
            ) => {
                l_name == r_name
                    && l_type_args.len() == r_type_args.len()
                    && l_type_args
                        .iter()
                        .zip(r_type_args.iter())
                        .all(|(l, r)| l.shape_eq(r))
                    && l_fields.len() == r_fields.len()
                    && l_fields
                        .iter()
                        .zip(r_fields.iter())
                        .all(|(l, r)| l.name == r.name && l.ty.shape_eq(&r.ty))
            }
            (
                Type::Enum {
                    name: l_name,
                    type_args: l_type_args,
                    variants: l_variants,
                },
                Type::Enum {
                    name: r_name,
                    type_args: r_type_args,
                    variants: r_variants,
                },
            ) => {
                l_name == r_name
                    && l_type_args.len() == r_type_args.len()
                    && l_type_args
                        .iter()
                        .zip(r_type_args.iter())
                        .all(|(l, r)| l.shape_eq(r))
                    && l_variants.len() == r_variants.len()
                    && l_variants.iter().zip(r_variants.iter()).all(|(l, r)| {
                        l.name == r.name
                            && l.payload.len() == r.payload.len()
                            && l.payload
                                .iter()
                                .zip(r.payload.iter())
                                .all(|(l, r)| l.shape_eq(r))
                    })
            }
            (
                Type::ErrorUnion {
                    ok_ty: l_ok,
                    err_tys: l_errs,
                },
                Type::ErrorUnion {
                    ok_ty: r_ok,
                    err_tys: r_errs,
                },
            ) => {
                l_errs.len() == r_errs.len()
                    && l_ok.shape_eq(r_ok)
                    && l_errs.iter().zip(r_errs.iter()).all(|(l, r)| l.shape_eq(r))
            }
            (
                Type::Pending {
                    response_tys: l_responses,
                },
                Type::Pending {
                    response_tys: r_responses,
                },
            )
            | (
                Type::ReplyCap {
                    response_tys: l_responses,
                },
                Type::ReplyCap {
                    response_tys: r_responses,
                },
            ) => {
                l_responses.len() == r_responses.len()
                    && l_responses
                        .iter()
                        .zip(r_responses.iter())
                        .all(|(l, r)| l.shape_eq(r))
            }
            (Type::Var(l), Type::Var(r)) => l == r,
            _ => false,
        }
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
            Type::PAddr | Type::NullablePAddr | Type::VAddr | Type::NullableVAddr => 8,
            Type::Int { bits, .. } => (*bits as usize) / 8,
            Type::Bool => 1,
            Type::Char => 4,
            Type::Range { elem_ty } => elem_ty.size_of(),
            Type::Fn { .. } => 8,
            Type::ErrorUnion { .. } => {
                // Tag + max payload to model a compact tagged-sum layout.
                8 + self.error_union_max_payload_size()
            }
            Type::String => 16,
            Type::Array { elem_ty, dims } => {
                let total_elems: usize = dims.iter().product();
                total_elems * elem_ty.size_of()
            }
            Type::DynArray { .. } => 16,
            Type::Pending { .. } => 8,
            Type::ReplyCap { .. } => 8,
            Type::View { .. } | Type::NullableView { .. } => 8,
            Type::NullableViewSlice { .. } | Type::NullableViewArray { .. } => 16,
            Type::RawPtr { .. } => 8,
            Type::ViewSlice { .. } | Type::ViewArray { .. } => 16,
            Type::Set { .. } => 16,
            Type::Iterable { .. } => 16,
            Type::Map { .. } => 16,
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
            Type::PAddr | Type::NullablePAddr | Type::VAddr | Type::NullableVAddr => 8,
            Type::Int { bits, .. } => (*bits as usize) / 8,
            Type::Bool => 1,
            Type::Char => 4,
            Type::Range { elem_ty } => elem_ty.align_of(),
            Type::Fn { .. } => 8,
            Type::ErrorUnion { .. } => self.error_union_max_payload_align().max(8),
            Type::String => 8,
            Type::Array { elem_ty, .. } => elem_ty.align_of(),
            Type::DynArray { .. } => 8,
            Type::Pending { .. } => 8,
            Type::ReplyCap { .. } => 8,
            Type::View { .. } | Type::NullableView { .. } => 8,
            Type::NullableViewSlice { .. } | Type::NullableViewArray { .. } => 8,
            Type::RawPtr { .. } => 8,
            Type::ViewSlice { .. } | Type::ViewArray { .. } => 8,
            Type::Set { .. } => 8,
            Type::Iterable { .. } => 8,
            Type::Map { .. } => 8,
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
                | Type::DynArray { .. }
                | Type::NullableViewSlice { .. }
                | Type::NullableViewArray { .. }
                | Type::ViewSlice { .. }
                | Type::ViewArray { .. }
                | Type::Set { .. }
                | Type::Iterable { .. }
                | Type::Map { .. }
                | Type::Tuple { .. }
                | Type::Struct { .. }
                | Type::String
                | Type::Slice { .. }
                | Type::ErrorUnion { .. }
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
            Type::DynArray { .. } => true,
            Type::Pending { .. } => false,
            Type::ReplyCap { .. } => false,
            Type::View { .. } => false,
            Type::NullableView { .. } => false,
            Type::NullableViewSlice { .. } => false,
            Type::NullableViewArray { .. } => false,
            Type::RawPtr { .. } => false,
            Type::ViewSlice { .. } => false,
            Type::ViewArray { .. } => false,
            Type::Set { .. } => true,
            Type::Iterable { .. } => true,
            Type::Map { .. } => true,
            Type::Tuple { field_tys } => field_tys.iter().any(Type::needs_drop),
            Type::Struct { fields, .. } => fields.iter().any(|f| f.ty.needs_drop()),
            Type::Enum { variants, .. } => variants
                .iter()
                .any(|v| v.payload.iter().any(Type::needs_drop)),
            Type::ErrorUnion { ok_ty, err_tys } => {
                ok_ty.needs_drop() || err_tys.iter().any(Type::needs_drop)
            }
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

    pub fn dyn_array_elem_type(&self) -> Option<Type> {
        let Type::DynArray { elem_ty } = self else {
            return None;
        };
        Some((**elem_ty).clone())
    }

    pub fn foreign_view_elem_type(&self) -> Option<Type> {
        match self {
            Type::View { elem_ty }
            | Type::NullableView { elem_ty }
            | Type::NullableViewSlice { elem_ty }
            | Type::NullableViewArray { elem_ty }
            | Type::RawPtr { elem_ty }
            | Type::ViewSlice { elem_ty }
            | Type::ViewArray { elem_ty } => Some((**elem_ty).clone()),
            _ => None,
        }
    }

    pub fn is_foreign_view_handle(&self) -> bool {
        matches!(
            self,
            Type::View { .. }
                | Type::NullableView { .. }
                | Type::NullableViewSlice { .. }
                | Type::NullableViewArray { .. }
                | Type::ViewSlice { .. }
                | Type::ViewArray { .. }
        )
    }

    pub fn is_nullable_single_view_field(&self) -> bool {
        matches!(self, Type::NullableView { .. })
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

    pub fn error_union_variants(&self) -> Vec<Type> {
        let Type::ErrorUnion { ok_ty, err_tys } = self else {
            panic!("Expected error-union type");
        };

        std::iter::once(ok_ty.as_ref())
            .chain(err_tys.iter())
            .cloned()
            .collect()
    }

    pub fn error_union_remainder_excluding(&self, matched: &[Type]) -> Option<Type> {
        let remaining = self
            .error_union_variants()
            .into_iter()
            .filter(|variant| !matched.iter().any(|matched| matched.shape_eq(variant)))
            .collect::<Vec<_>>();

        match remaining.as_slice() {
            [] => None,
            [single] => Some(single.clone()),
            [first, rest @ ..] => Some(Type::ErrorUnion {
                ok_ty: Box::new(first.clone()),
                err_tys: rest.to_vec(),
            }),
        }
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

    fn error_union_max_payload_size(&self) -> usize {
        let Type::ErrorUnion { ok_ty, err_tys } = self else {
            panic!("Expected error-union type");
        };

        std::iter::once(ok_ty.as_ref())
            .chain(err_tys.iter())
            .map(Type::size_of)
            .max()
            .unwrap_or(0)
    }

    fn error_union_max_payload_align(&self) -> usize {
        let Type::ErrorUnion { ok_ty, err_tys } = self else {
            panic!("Expected error-union type");
        };

        std::iter::once(ok_ty.as_ref())
            .chain(err_tys.iter())
            .map(Type::align_of)
            .max()
            .unwrap_or(1)
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
            Type::PAddr
            | Type::NullablePAddr
            | Type::VAddr
            | Type::NullableVAddr
            | Type::NullableView { .. }
            | Type::RawPtr { .. } => 0,
            Type::Int { signed: false, .. } => 0,
            Type::Int {
                signed: true, bits, ..
            } => -(1i128 << (*bits as u32 - 1)),
            _ => panic!("Expected integer type"),
        }
    }

    pub fn max_value(&self) -> i128 {
        match self {
            Type::PAddr
            | Type::NullablePAddr
            | Type::VAddr
            | Type::NullableVAddr
            | Type::NullableView { .. }
            | Type::RawPtr { .. } => u64::MAX as i128,
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
            Type::ErrorUnion { ok_ty, err_tys } => Type::ErrorUnion {
                ok_ty: Box::new((*ok_ty).map(f)),
                err_tys: err_tys.into_iter().map(|ty| ty.map(f)).collect(),
            },
            Type::Range { elem_ty } => Type::Range {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::Array { elem_ty, dims } => Type::Array {
                elem_ty: Box::new((*elem_ty).map(f)),
                dims,
            },
            Type::DynArray { elem_ty } => Type::DynArray {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::View { elem_ty } => Type::View {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::NullableView { elem_ty } => Type::NullableView {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::NullableViewSlice { elem_ty } => Type::NullableViewSlice {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::NullableViewArray { elem_ty } => Type::NullableViewArray {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::RawPtr { elem_ty } => Type::RawPtr {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::ViewSlice { elem_ty } => Type::ViewSlice {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::ViewArray { elem_ty } => Type::ViewArray {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::Set { elem_ty } => Type::Set {
                elem_ty: Box::new((*elem_ty).map(f)),
            },
            Type::Iterable { item_ty } => Type::Iterable {
                item_ty: Box::new((*item_ty).map(f)),
            },
            Type::Map { key_ty, value_ty } => Type::Map {
                key_ty: Box::new((*key_ty).map(f)),
                value_ty: Box::new((*value_ty).map(f)),
            },
            Type::Tuple { field_tys } => Type::Tuple {
                field_tys: field_tys.into_iter().map(|ty| ty.map(f)).collect(),
            },
            Type::Struct {
                name,
                type_args,
                fields,
            } => Type::Struct {
                name,
                type_args: type_args.into_iter().map(|ty| ty.map(f)).collect(),
                fields: fields
                    .into_iter()
                    .map(|field| StructField {
                        name: field.name,
                        ty: field.ty.map(f),
                    })
                    .collect(),
            },
            Type::Enum {
                name,
                type_args,
                variants,
            } => Type::Enum {
                name,
                type_args: type_args.into_iter().map(|ty| ty.map(f)).collect(),
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
            Type::ErrorUnion { ok_ty, err_tys } => {
                let mapped_ok = ok_ty.map_cow(f);
                let mapped_errs = err_tys.iter().map(|ty| ty.map_cow(f)).collect::<Vec<_>>();
                let changed = matches!(mapped_ok, Cow::Owned(_))
                    || mapped_errs.iter().any(|ty| matches!(ty, Cow::Owned(_)));
                if changed {
                    Cow::Owned(Type::ErrorUnion {
                        ok_ty: Box::new(mapped_ok.into_owned()),
                        err_tys: mapped_errs.into_iter().map(Cow::into_owned).collect(),
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
            Type::DynArray { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::DynArray {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::View { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::View {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::NullableView { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::NullableView {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::NullableViewSlice { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::NullableViewSlice {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::NullableViewArray { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::NullableViewArray {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::RawPtr { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::RawPtr {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::ViewSlice { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::ViewSlice {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::ViewArray { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::ViewArray {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Set { elem_ty } => {
                let mapped_elem = elem_ty.map_cow(f);
                if matches!(mapped_elem, Cow::Owned(_)) {
                    Cow::Owned(Type::Set {
                        elem_ty: Box::new(mapped_elem.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Iterable { item_ty } => {
                let mapped_item = item_ty.map_cow(f);
                if matches!(mapped_item, Cow::Owned(_)) {
                    Cow::Owned(Type::Iterable {
                        item_ty: Box::new(mapped_item.into_owned()),
                    })
                } else {
                    Cow::Borrowed(self)
                }
            }
            Type::Map { key_ty, value_ty } => {
                let mapped_key = key_ty.map_cow(f);
                let mapped_value = value_ty.map_cow(f);
                if matches!(mapped_key, Cow::Owned(_)) || matches!(mapped_value, Cow::Owned(_)) {
                    Cow::Owned(Type::Map {
                        key_ty: Box::new(mapped_key.into_owned()),
                        value_ty: Box::new(mapped_value.into_owned()),
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
            Type::Struct {
                name,
                type_args,
                fields,
            } => {
                let mapped_type_args = type_args.iter().map(|ty| ty.map_cow(f)).collect::<Vec<_>>();
                let mapped_fields = fields
                    .iter()
                    .map(|field| (&field.name, field.ty.map_cow(f)))
                    .collect::<Vec<_>>();
                let changed = mapped_type_args
                    .iter()
                    .any(|ty| matches!(ty, Cow::Owned(_)))
                    || mapped_fields
                        .iter()
                        .any(|(_, ty)| matches!(ty, Cow::Owned(_)));
                if changed {
                    Cow::Owned(Type::Struct {
                        name: name.clone(),
                        type_args: mapped_type_args.into_iter().map(Cow::into_owned).collect(),
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
            Type::Enum {
                name,
                type_args,
                variants,
            } => {
                let mapped_type_args = type_args.iter().map(|ty| ty.map_cow(f)).collect::<Vec<_>>();
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
                let changed = mapped_type_args
                    .iter()
                    .any(|ty| matches!(ty, Cow::Owned(_)))
                    || mapped_variants
                        .iter()
                        .any(|(_, payload)| payload.iter().any(|ty| matches!(ty, Cow::Owned(_))));
                if changed {
                    Cow::Owned(Type::Enum {
                        name: name.clone(),
                        type_args: mapped_type_args.into_iter().map(Cow::into_owned).collect(),
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

    /// Returns `true` when the type or any nested child contains `Unknown` or
    /// `Var`, i.e. the type is not fully resolved.
    pub fn contains_unresolved(&self) -> bool {
        self.any(&|t| matches!(t, Type::Unknown | Type::Var(_)))
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
            Type::ErrorUnion { ok_ty, err_tys } => {
                ok_ty.any(predicate) || err_tys.iter().any(|ty| ty.any(predicate))
            }
            Type::Range { elem_ty }
            | Type::Array { elem_ty, .. }
            | Type::DynArray { elem_ty }
            | Type::View { elem_ty }
            | Type::NullableView { elem_ty }
            | Type::NullableViewSlice { elem_ty }
            | Type::NullableViewArray { elem_ty }
            | Type::RawPtr { elem_ty }
            | Type::ViewSlice { elem_ty }
            | Type::ViewArray { elem_ty }
            | Type::Set { elem_ty }
            | Type::Iterable { item_ty: elem_ty }
            | Type::Slice { elem_ty }
            | Type::Heap { elem_ty }
            | Type::Ref { elem_ty, .. } => elem_ty.any(predicate),
            Type::Map { key_ty, value_ty } => key_ty.any(predicate) || value_ty.any(predicate),
            Type::Tuple { field_tys } => field_tys.iter().any(|ty| ty.any(predicate)),
            Type::Struct { fields, .. } => fields.iter().any(|f| f.ty.any(predicate)),
            Type::Enum { variants, .. } => variants
                .iter()
                .any(|v| v.payload.iter().any(|ty| ty.any(predicate))),
            Type::Pending { response_tys } | Type::ReplyCap { response_tys } => {
                response_tys.iter().any(|ty| ty.any(predicate))
            }
            _ => false,
        }
    }
}

#[cfg(test)]
#[path = "../../tests/core/types/t_types.rs"]
mod tests;
