//! Shared builtin collection method contracts used by typecheck phases.
//!
//! Both solver and finalize need the same receiver/param/return shape for
//! builtin methods on dynamic arrays, sets, and maps. Keeping this in one
//! place prevents semantic drift between phases.

use crate::tree::ParamMode;
use crate::types::Type;

#[derive(Clone, Debug)]
pub(crate) struct BuiltinMethodParam {
    pub(crate) mode: ParamMode,
    pub(crate) ty: Type,
}

#[derive(Clone, Debug)]
pub(crate) enum BuiltinMethodRet {
    Unit,
    Bool,
    MapGet { value_ty: Type },
}

#[derive(Clone, Debug)]
pub(crate) enum BuiltinMethod {
    DynArrayAppend { elem_ty: Type, elem_mode: ParamMode },
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
                mode: elem_mode.clone(),
                ty: elem_ty.clone(),
            }],
            BuiltinMethod::SetInsert { elem_ty }
            | BuiltinMethod::SetRemove { elem_ty }
            | BuiltinMethod::SetContains { elem_ty } => vec![BuiltinMethodParam {
                mode: ParamMode::In,
                ty: elem_ty.clone(),
            }],
            BuiltinMethod::SetClear { .. } => Vec::new(),
            BuiltinMethod::MapInsert { key_ty, value_ty } => vec![
                BuiltinMethodParam {
                    mode: ParamMode::In,
                    ty: key_ty.clone(),
                },
                BuiltinMethodParam {
                    mode: ParamMode::In,
                    ty: value_ty.clone(),
                },
            ],
            BuiltinMethod::MapRemove { key_ty }
            | BuiltinMethod::MapContainsKey { key_ty }
            | BuiltinMethod::MapGet { key_ty, .. } => vec![BuiltinMethodParam {
                mode: ParamMode::In,
                ty: key_ty.clone(),
            }],
            BuiltinMethod::MapClear { .. } => Vec::new(),
        }
    }

    pub(crate) fn ret_kind(&self) -> BuiltinMethodRet {
        match self {
            BuiltinMethod::DynArrayAppend { .. } => BuiltinMethodRet::Unit,
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
            BuiltinMethod::MapInsert { key_ty, .. }
            | BuiltinMethod::MapRemove { key_ty }
            | BuiltinMethod::MapContainsKey { key_ty }
            | BuiltinMethod::MapGet { key_ty, .. }
            | BuiltinMethod::MapClear { key_ty } => Some(key_ty),
            BuiltinMethod::DynArrayAppend { .. } => None,
        }
    }
}

pub(crate) fn is_builtin_collection_receiver(ty: &Type) -> bool {
    matches!(
        ty.peel_heap(),
        Type::DynArray { .. } | Type::Set { .. } | Type::Map { .. }
    )
}

pub(crate) fn is_builtin_collection_property(name: &str) -> bool {
    matches!(name, "len" | "capacity" | "is_empty")
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
