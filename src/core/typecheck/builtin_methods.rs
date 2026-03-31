//! Shared builtin collection method contracts used by typecheck phases.
//!
//! Both solver and finalize need the same receiver/param/return shape for
//! builtin methods on dynamic arrays, sets, and maps. Keeping this in one
//! place prevents semantic drift between phases.

use crate::core::ast::ParamMode;
use crate::core::types::Type;

#[derive(Clone, Debug)]
pub(crate) struct BuiltinMethodParam {
    pub(crate) name: String,
    pub(crate) mode: ParamMode,
    pub(crate) ty: Type,
}

#[derive(Clone, Debug)]
pub(crate) enum BuiltinMethodRet {
    Unit,
    Bool,
    Value(Type),
    MapGet { value_ty: Type },
}

#[derive(Clone, Debug)]
pub(crate) struct BuiltinProperty {
    pub(crate) ty: Type,
    pub(crate) readable: bool,
    pub(crate) writable: bool,
}

#[derive(Clone, Debug)]
pub(crate) enum BuiltinMethod {
    DynArrayAppend { elem_ty: Type, elem_mode: ParamMode },
    AddrOffset,
    AddrAlignDown { addr_ty: Type },
    AddrAlignUp { addr_ty: Type },
    AddrIsAligned,
    NullableAddrIsSome,
    NullableAddrIsNone,
    NullableAddrUnwrap { addr_ty: Type },
    RawPtrRead { elem_ty: Type },
    RawPtrWrite { elem_ty: Type },
    SetInsert { elem_ty: Type },
    SetRemove { elem_ty: Type },
    SetContains { elem_ty: Type },
    SetClear { elem_ty: Type },
    MapInsert { key_ty: Type, value_ty: Type },
    MapRemove { key_ty: Type },
    MapContainsKey { key_ty: Type },
    MapGet { key_ty: Type, value_ty: Type },
    MapClear { key_ty: Type },
}

impl BuiltinMethod {
    pub(crate) fn receiver_mode(&self) -> ParamMode {
        match self {
            BuiltinMethod::DynArrayAppend { .. } => ParamMode::InOut,
            BuiltinMethod::AddrOffset
            | BuiltinMethod::AddrAlignDown { .. }
            | BuiltinMethod::AddrAlignUp { .. }
            | BuiltinMethod::AddrIsAligned
            | BuiltinMethod::NullableAddrIsSome
            | BuiltinMethod::NullableAddrIsNone
            | BuiltinMethod::NullableAddrUnwrap { .. }
            | BuiltinMethod::RawPtrRead { .. } => ParamMode::In,
            BuiltinMethod::RawPtrWrite { .. } => ParamMode::In,
            BuiltinMethod::SetInsert { .. }
            | BuiltinMethod::SetRemove { .. }
            | BuiltinMethod::SetClear { .. } => ParamMode::InOut,
            BuiltinMethod::SetContains { .. } => ParamMode::In,
            BuiltinMethod::MapInsert { .. }
            | BuiltinMethod::MapRemove { .. }
            | BuiltinMethod::MapClear { .. } => ParamMode::InOut,
            BuiltinMethod::MapContainsKey { .. } | BuiltinMethod::MapGet { .. } => ParamMode::In,
        }
    }

    pub(crate) fn params(&self) -> Vec<BuiltinMethodParam> {
        match self {
            BuiltinMethod::DynArrayAppend { elem_ty, elem_mode } => vec![BuiltinMethodParam {
                name: "value".to_string(),
                mode: elem_mode.clone(),
                ty: elem_ty.clone(),
            }],
            BuiltinMethod::AddrOffset => Vec::new(),
            BuiltinMethod::AddrAlignDown { .. }
            | BuiltinMethod::AddrAlignUp { .. }
            | BuiltinMethod::AddrIsAligned => vec![BuiltinMethodParam {
                name: "alignment".to_string(),
                mode: ParamMode::In,
                ty: Type::uint(64),
            }],
            BuiltinMethod::NullableAddrIsSome
            | BuiltinMethod::NullableAddrIsNone
            | BuiltinMethod::NullableAddrUnwrap { .. }
            | BuiltinMethod::RawPtrRead { .. } => Vec::new(),
            BuiltinMethod::RawPtrWrite { elem_ty } => vec![BuiltinMethodParam {
                name: "value".to_string(),
                mode: ParamMode::In,
                ty: elem_ty.clone(),
            }],
            BuiltinMethod::SetInsert { elem_ty }
            | BuiltinMethod::SetRemove { elem_ty }
            | BuiltinMethod::SetContains { elem_ty } => vec![BuiltinMethodParam {
                name: "value".to_string(),
                mode: ParamMode::In,
                ty: elem_ty.clone(),
            }],
            BuiltinMethod::SetClear { .. } => Vec::new(),
            BuiltinMethod::MapInsert { key_ty, value_ty } => vec![
                BuiltinMethodParam {
                    name: "key".to_string(),
                    mode: ParamMode::In,
                    ty: key_ty.clone(),
                },
                BuiltinMethodParam {
                    name: "value".to_string(),
                    mode: ParamMode::In,
                    ty: value_ty.clone(),
                },
            ],
            BuiltinMethod::MapRemove { key_ty }
            | BuiltinMethod::MapContainsKey { key_ty }
            | BuiltinMethod::MapGet { key_ty, .. } => vec![BuiltinMethodParam {
                name: "key".to_string(),
                mode: ParamMode::In,
                ty: key_ty.clone(),
            }],
            BuiltinMethod::MapClear { .. } => Vec::new(),
        }
    }

    pub(crate) fn ret_kind(&self) -> BuiltinMethodRet {
        match self {
            BuiltinMethod::DynArrayAppend { .. } => BuiltinMethodRet::Unit,
            BuiltinMethod::AddrOffset => BuiltinMethodRet::Value(Type::uint(64)),
            BuiltinMethod::AddrAlignDown { addr_ty } | BuiltinMethod::AddrAlignUp { addr_ty } => {
                BuiltinMethodRet::Value(addr_ty.clone())
            }
            BuiltinMethod::AddrIsAligned => BuiltinMethodRet::Bool,
            BuiltinMethod::NullableAddrIsSome | BuiltinMethod::NullableAddrIsNone => {
                BuiltinMethodRet::Bool
            }
            BuiltinMethod::NullableAddrUnwrap { addr_ty } => {
                BuiltinMethodRet::Value(addr_ty.clone())
            }
            BuiltinMethod::RawPtrRead { elem_ty } => BuiltinMethodRet::Value(elem_ty.clone()),
            BuiltinMethod::RawPtrWrite { .. } => BuiltinMethodRet::Unit,
            BuiltinMethod::SetInsert { .. }
            | BuiltinMethod::SetRemove { .. }
            | BuiltinMethod::SetContains { .. } => BuiltinMethodRet::Bool,
            BuiltinMethod::SetClear { .. } => BuiltinMethodRet::Unit,
            BuiltinMethod::MapInsert { .. }
            | BuiltinMethod::MapRemove { .. }
            | BuiltinMethod::MapContainsKey { .. } => BuiltinMethodRet::Bool,
            BuiltinMethod::MapGet { value_ty, .. } => BuiltinMethodRet::MapGet {
                value_ty: value_ty.clone(),
            },
            BuiltinMethod::MapClear { .. } => BuiltinMethodRet::Unit,
        }
    }

    pub(crate) fn hashable_ty(&self) -> Option<&Type> {
        match self {
            BuiltinMethod::SetInsert { elem_ty }
            | BuiltinMethod::SetRemove { elem_ty }
            | BuiltinMethod::SetContains { elem_ty }
            | BuiltinMethod::SetClear { elem_ty } => Some(elem_ty),
            BuiltinMethod::AddrOffset
            | BuiltinMethod::AddrAlignDown { .. }
            | BuiltinMethod::AddrAlignUp { .. }
            | BuiltinMethod::AddrIsAligned
            | BuiltinMethod::NullableAddrIsSome
            | BuiltinMethod::NullableAddrIsNone
            | BuiltinMethod::NullableAddrUnwrap { .. }
            | BuiltinMethod::RawPtrRead { .. }
            | BuiltinMethod::RawPtrWrite { .. } => None,
            BuiltinMethod::MapInsert { key_ty, .. }
            | BuiltinMethod::MapRemove { key_ty }
            | BuiltinMethod::MapContainsKey { key_ty }
            | BuiltinMethod::MapGet { key_ty, .. }
            | BuiltinMethod::MapClear { key_ty } => Some(key_ty),
            BuiltinMethod::DynArrayAppend { .. } => None,
        }
    }
}

pub(crate) fn has_builtin_surface(ty: &Type) -> bool {
    matches!(
        ty.peel_heap(),
        Type::PAddr
            | Type::VAddr
            | Type::NullablePAddr
            | Type::NullableVAddr
            | Type::RawPtr { .. }
            | Type::DynArray { .. }
            | Type::Set { .. }
            | Type::Map { .. }
    )
}

pub(crate) fn resolve_builtin_property(owner_ty: &Type, field: &str) -> Option<BuiltinProperty> {
    let owner = owner_ty.peel_heap();
    match field {
        "len"
            if matches!(
                owner,
                Type::Array { .. }
                    | Type::DynArray { .. }
                    | Type::Set { .. }
                    | Type::Map { .. }
                    | Type::Slice { .. }
                    | Type::ViewSlice { .. }
                    | Type::ViewArray { .. }
                    | Type::String
            ) =>
        {
            Some(BuiltinProperty {
                ty: Type::uint(64),
                readable: true,
                writable: false,
            })
        }
        "capacity"
            if matches!(
                owner,
                Type::DynArray { .. } | Type::Set { .. } | Type::Map { .. }
            ) =>
        {
            Some(BuiltinProperty {
                ty: Type::uint(64),
                readable: true,
                writable: false,
            })
        }
        "is_empty"
            if matches!(
                owner,
                Type::DynArray { .. } | Type::Set { .. } | Type::Map { .. }
            ) =>
        {
            Some(BuiltinProperty {
                ty: Type::Bool,
                readable: true,
                writable: false,
            })
        }
        _ => None,
    }
}

pub(crate) fn resolve_builtin_method(
    receiver_ty: &Type,
    method_name: &str,
) -> Option<BuiltinMethod> {
    let peeled = receiver_ty.peel_heap();
    match (&peeled, method_name) {
        (Type::DynArray { elem_ty }, "append") => {
            let elem_mode = if elem_ty.needs_drop() {
                ParamMode::Sink
            } else {
                ParamMode::In
            };
            Some(BuiltinMethod::DynArrayAppend {
                elem_ty: (**elem_ty).clone(),
                elem_mode,
            })
        }
        (Type::PAddr, "offset") => Some(BuiltinMethod::AddrOffset),
        (Type::PAddr, "align_down") => Some(BuiltinMethod::AddrAlignDown {
            addr_ty: Type::PAddr,
        }),
        (Type::PAddr, "align_up") => Some(BuiltinMethod::AddrAlignUp {
            addr_ty: Type::PAddr,
        }),
        (Type::PAddr, "is_aligned") => Some(BuiltinMethod::AddrIsAligned),
        (Type::VAddr, "offset") => Some(BuiltinMethod::AddrOffset),
        (Type::VAddr, "align_down") => Some(BuiltinMethod::AddrAlignDown {
            addr_ty: Type::VAddr,
        }),
        (Type::VAddr, "align_up") => Some(BuiltinMethod::AddrAlignUp {
            addr_ty: Type::VAddr,
        }),
        (Type::VAddr, "is_aligned") => Some(BuiltinMethod::AddrIsAligned),
        (Type::NullablePAddr, "is_some") | (Type::NullableVAddr, "is_some") => {
            Some(BuiltinMethod::NullableAddrIsSome)
        }
        (Type::NullablePAddr, "is_none") | (Type::NullableVAddr, "is_none") => {
            Some(BuiltinMethod::NullableAddrIsNone)
        }
        (Type::NullablePAddr, "unwrap") => Some(BuiltinMethod::NullableAddrUnwrap {
            addr_ty: Type::PAddr,
        }),
        (Type::NullableVAddr, "unwrap") => Some(BuiltinMethod::NullableAddrUnwrap {
            addr_ty: Type::VAddr,
        }),
        (Type::RawPtr { elem_ty }, "read") => Some(BuiltinMethod::RawPtrRead {
            elem_ty: (**elem_ty).clone(),
        }),
        (Type::RawPtr { elem_ty }, "write") => Some(BuiltinMethod::RawPtrWrite {
            elem_ty: (**elem_ty).clone(),
        }),
        (Type::Set { elem_ty }, "insert") => Some(BuiltinMethod::SetInsert {
            elem_ty: (**elem_ty).clone(),
        }),
        (Type::Set { elem_ty }, "remove") => Some(BuiltinMethod::SetRemove {
            elem_ty: (**elem_ty).clone(),
        }),
        (Type::Set { elem_ty }, "contains") => Some(BuiltinMethod::SetContains {
            elem_ty: (**elem_ty).clone(),
        }),
        (Type::Set { elem_ty }, "clear") => Some(BuiltinMethod::SetClear {
            elem_ty: (**elem_ty).clone(),
        }),
        (Type::Map { key_ty, value_ty }, "insert") => Some(BuiltinMethod::MapInsert {
            key_ty: (**key_ty).clone(),
            value_ty: (**value_ty).clone(),
        }),
        (Type::Map { key_ty, .. }, "remove") => Some(BuiltinMethod::MapRemove {
            key_ty: (**key_ty).clone(),
        }),
        (Type::Map { key_ty, .. }, "contains_key") => Some(BuiltinMethod::MapContainsKey {
            key_ty: (**key_ty).clone(),
        }),
        (Type::Map { key_ty, value_ty }, "get") => Some(BuiltinMethod::MapGet {
            key_ty: (**key_ty).clone(),
            value_ty: (**value_ty).clone(),
        }),
        (Type::Map { key_ty, .. }, "clear") => Some(BuiltinMethod::MapClear {
            key_ty: (**key_ty).clone(),
        }),
        _ => None,
    }
}
