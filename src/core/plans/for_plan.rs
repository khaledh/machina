//! Semantic `for` loop plans selected during typecheck/finalize.

use std::collections::HashMap;

use crate::core::ast::NodeId;
use crate::core::resolve::DefId;
use crate::core::typecheck::CollectedCallableSig;
use crate::core::types::Type;

use super::iterable_protocol::{
    ITER_DONE_TYPE_NAME, can_use_iterable_protocol_methods, check_iterable_protocol_with_methods,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForPlan {
    pub item_ty: Type,
    pub kernel: ForKernel,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForKernel {
    Intrinsic(IntrinsicForKernel),
    AbstractIterable(AbstractIterableForKernel),
    Protocol(ProtocolForKernel),
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbstractIterableForKernel {
    pub done_ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProtocolForKernel {
    pub iter_ty: Type,
    pub done_ty: Type,
    pub propagated_err_tys: Vec<Type>,
    pub iter_method: DefId,
    pub next_method: DefId,
    pub iter_method_type_args: Vec<Type>,
    pub next_method_type_args: Vec<Type>,
}

pub use super::iterable_protocol::IterableProtocolError as ProtocolForPlanError;

pub type ForPlanMap = HashMap<NodeId, ForPlan>;

pub fn plan_for_iterable_type(ty: &Type) -> Option<ForPlan> {
    let (item_ty, kernel) = match ty {
        Type::Range { elem_ty } => ((**elem_ty).clone(), IntrinsicForKernel::Range),
        Type::Array { elem_ty, .. } => ((**elem_ty).clone(), IntrinsicForKernel::Array),
        Type::DynArray { elem_ty } => ((**elem_ty).clone(), IntrinsicForKernel::DynArray),
        Type::Slice { elem_ty } => ((**elem_ty).clone(), IntrinsicForKernel::Slice),
        Type::ViewSlice { elem_ty } => ((**elem_ty).clone(), IntrinsicForKernel::Slice),
        Type::ViewArray { elem_ty } => ((**elem_ty).clone(), IntrinsicForKernel::Slice),
        Type::String => (Type::Char, IntrinsicForKernel::String),
        Type::Iterable { item_ty } => {
            return Some(ForPlan {
                item_ty: (**item_ty).clone(),
                kernel: ForKernel::AbstractIterable(AbstractIterableForKernel {
                    done_ty: iter_done_type(),
                }),
            });
        }
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

fn iter_done_type() -> Type {
    Type::Struct {
        name: ITER_DONE_TYPE_NAME.to_string(),
        type_args: Vec::new(),
        fields: Vec::new(),
    }
}

pub(crate) fn plan_for_iterable_type_with_methods(
    ty: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Option<ForPlan> {
    plan_for_iterable_type(ty).or_else(|| plan_protocol_iterable_type(ty, method_sigs))
}

pub(crate) fn diagnose_protocol_iterable_type(
    ty: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Option<ProtocolForPlanError> {
    if plan_for_iterable_type(ty).is_some() {
        return None;
    }
    if !can_use_iterable_protocol_methods(ty) {
        return None;
    }

    check_iterable_protocol_with_methods(ty, method_sigs).err()
}

fn plan_protocol_iterable_type(
    ty: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Option<ForPlan> {
    let witness = check_iterable_protocol_with_methods(ty, method_sigs).ok()?;

    Some(ForPlan {
        item_ty: witness.item_ty,
        kernel: ForKernel::Protocol(ProtocolForKernel {
            iter_ty: witness.iter_ty,
            done_ty: witness.done_ty,
            propagated_err_tys: witness.propagated_err_tys,
            iter_method: witness.iter_method,
            next_method: witness.next_method,
            iter_method_type_args: witness.iter_method_type_args,
            next_method_type_args: witness.next_method_type_args,
        }),
    })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::core::plans::iterable_protocol::is_iter_done_type;

    #[test]
    fn iterable_type_plan_builds_without_method_lookup() {
        let source_ty = Type::Iterable {
            item_ty: Box::new(Type::String),
        };

        let plan = plan_for_iterable_type(&source_ty).expect("expected iterable plan");
        assert_eq!(plan.item_ty, Type::String);
        match plan.kernel {
            ForKernel::AbstractIterable(AbstractIterableForKernel { done_ty }) => {
                assert!(is_iter_done_type(&done_ty));
            }
            other => panic!("expected abstract iterable kernel, found {other:?}"),
        }
    }

    #[test]
    fn protocol_plan_reuses_shared_iterable_protocol_witness() {
        let source_ty = Type::Struct {
            name: "Counter".to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let methods = HashMap::from([
            (
                "Counter".to_string(),
                HashMap::from([(
                    "iter".to_string(),
                    vec![CollectedCallableSig {
                        def_id: crate::core::resolve::DefId(1),
                        self_ty: Some(source_ty.clone()),
                        params: Vec::new(),
                        ret_ty: Type::Struct {
                            name: "CounterIter".to_string(),
                            type_args: Vec::new(),
                            fields: Vec::new(),
                        },
                        type_param_count: 0,
                        type_param_var_names: Default::default(),
                        type_param_bounds: Vec::new(),
                        self_mode: Some(crate::core::ast::ParamMode::In),
                        impl_trait: None,
                    }],
                )]),
            ),
            (
                "CounterIter".to_string(),
                HashMap::from([(
                    "next".to_string(),
                    vec![CollectedCallableSig {
                        def_id: crate::core::resolve::DefId(2),
                        self_ty: Some(Type::Struct {
                            name: "CounterIter".to_string(),
                            type_args: Vec::new(),
                            fields: Vec::new(),
                        }),
                        params: Vec::new(),
                        ret_ty: Type::ErrorUnion {
                            ok_ty: Box::new(Type::uint(64)),
                            err_tys: vec![Type::Struct {
                                name: ITER_DONE_TYPE_NAME.to_string(),
                                type_args: Vec::new(),
                                fields: Vec::new(),
                            }],
                        },
                        type_param_count: 0,
                        type_param_var_names: Default::default(),
                        type_param_bounds: Vec::new(),
                        self_mode: Some(crate::core::ast::ParamMode::InOut),
                        impl_trait: None,
                    }],
                )]),
            ),
        ]);

        let plan = plan_for_iterable_type_with_methods(&source_ty, &methods)
            .expect("expected protocol for-plan");
        assert_eq!(plan.item_ty, Type::uint(64));
    }
}
