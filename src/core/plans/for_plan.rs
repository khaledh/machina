//! Semantic `for` loop plans selected during typecheck/finalize.

use std::collections::HashMap;

use crate::core::ast::NodeId;
use crate::core::types::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForPlan {
    pub item_ty: Type,
    pub kernel: ForKernel,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForKernel {
    Intrinsic(IntrinsicForKernel),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntrinsicForKernel {
    Range,
    Array,
    DynArray,
    Slice,
    String,
    Map,
}

pub type ForPlanMap = HashMap<NodeId, ForPlan>;

pub fn plan_for_iterable_type(ty: &Type) -> Option<ForPlan> {
    let (item_ty, kernel) = match ty {
        Type::Range { elem_ty } => ((**elem_ty).clone(), IntrinsicForKernel::Range),
        Type::Array { elem_ty, .. } => ((**elem_ty).clone(), IntrinsicForKernel::Array),
        Type::DynArray { elem_ty } => ((**elem_ty).clone(), IntrinsicForKernel::DynArray),
        Type::Slice { elem_ty } => ((**elem_ty).clone(), IntrinsicForKernel::Slice),
        Type::String => (Type::Char, IntrinsicForKernel::String),
        Type::Map { key_ty, value_ty }
            if (!key_ty.needs_drop() || matches!(key_ty.as_ref(), Type::String))
                && !value_ty.needs_drop() =>
        {
            (
                Type::Tuple {
                    field_tys: vec![(**key_ty).clone(), (**value_ty).clone()],
                },
                IntrinsicForKernel::Map,
            )
        }
        _ => return None,
    };

    Some(ForPlan {
        item_ty,
        kernel: ForKernel::Intrinsic(kernel),
    })
}
