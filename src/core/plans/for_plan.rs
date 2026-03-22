//! Semantic `for` loop plans selected during typecheck/finalize.

use std::collections::HashMap;

use crate::core::ast::NodeId;
use crate::core::ast::ParamMode;
use crate::core::resolve::DefId;
use crate::core::typecheck::CollectedCallableSig;
use crate::core::types::Type;

pub const ITER_DONE_TYPE_NAME: &str = "IterDone";

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForPlan {
    pub item_ty: Type,
    pub kernel: ForKernel,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForKernel {
    Intrinsic(IntrinsicForKernel),
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
pub struct ProtocolForKernel {
    pub iter_ty: Type,
    pub done_ty: Type,
    pub iter_method: DefId,
    pub next_method: DefId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProtocolForPlanError {
    MissingIter { source_ty: Type },
    MissingNext { iter_ty: Type },
    InvalidNextReturn { ret_ty: Type },
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

    let owner = protocol_owner_name(ty)?;
    let Some(iter_sig) = select_protocol_method(method_sigs, owner, "iter", ParamMode::In) else {
        return Some(ProtocolForPlanError::MissingIter {
            source_ty: ty.clone(),
        });
    };

    let iter_ty = iter_sig.ret_ty.clone();
    let Some(iter_owner) = protocol_owner_name(&iter_ty) else {
        return Some(ProtocolForPlanError::MissingNext { iter_ty });
    };

    let Some(next_sig) = select_protocol_method(method_sigs, iter_owner, "next", ParamMode::InOut)
    else {
        return Some(ProtocolForPlanError::MissingNext { iter_ty });
    };

    if protocol_next_item_and_done(&next_sig.ret_ty).is_none() {
        return Some(ProtocolForPlanError::InvalidNextReturn {
            ret_ty: next_sig.ret_ty.clone(),
        });
    }

    None
}

fn plan_protocol_iterable_type(
    ty: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Option<ForPlan> {
    let owner = protocol_owner_name(ty)?;
    let iter_sig = select_protocol_method(method_sigs, owner, "iter", ParamMode::In)?;
    let iter_ty = iter_sig.ret_ty.clone();
    let iter_owner = protocol_owner_name(&iter_ty)?;
    let next_sig = select_protocol_method(method_sigs, iter_owner, "next", ParamMode::InOut)?;
    let (item_ty, done_ty) = protocol_next_item_and_done(&next_sig.ret_ty)?;

    Some(ForPlan {
        item_ty,
        kernel: ForKernel::Protocol(ProtocolForKernel {
            iter_ty,
            done_ty,
            iter_method: iter_sig.def_id,
            next_method: next_sig.def_id,
        }),
    })
}

fn protocol_owner_name(ty: &Type) -> Option<&str> {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.as_str()),
        Type::String => Some("string"),
        _ => None,
    }
}

fn select_protocol_method<'a>(
    method_sigs: &'a HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
    owner: &str,
    method_name: &str,
    self_mode: ParamMode,
) -> Option<&'a CollectedCallableSig> {
    let overloads = method_sigs.get(owner)?.get(method_name)?;
    let mut best: Option<&CollectedCallableSig> = None;
    let mut best_priority = i32::MIN;
    let mut ambiguous = false;

    for sig in overloads {
        if sig.params.len() != 0
            || sig.type_param_count != 0
            || sig.self_mode != Some(self_mode.clone())
        {
            continue;
        }

        let priority = if sig.impl_trait.is_none() { 1 } else { 0 };
        match best {
            None => {
                best = Some(sig);
                best_priority = priority;
                ambiguous = false;
            }
            Some(_) if priority > best_priority => {
                best = Some(sig);
                best_priority = priority;
                ambiguous = false;
            }
            Some(_) if priority == best_priority => {
                ambiguous = true;
            }
            Some(_) => {}
        }
    }

    if ambiguous { None } else { best }
}

fn protocol_next_item_and_done(ret_ty: &Type) -> Option<(Type, Type)> {
    let Type::ErrorUnion { ok_ty, err_tys } = ret_ty else {
        return None;
    };
    if err_tys.len() != 1 {
        return None;
    }
    let done_ty = err_tys[0].clone();
    if !is_iter_done_type(&done_ty) {
        return None;
    }
    Some(((**ok_ty).clone(), done_ty))
}

fn is_iter_done_type(ty: &Type) -> bool {
    matches!(ty, Type::Struct { name, fields } if name == ITER_DONE_TYPE_NAME && fields.is_empty())
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};

    use super::*;
    use crate::core::resolve::DefId;
    use crate::core::typecheck::CollectedParamSig;

    fn method_sig(def_id: u32, self_mode: ParamMode, ret_ty: Type) -> CollectedCallableSig {
        CollectedCallableSig {
            def_id: DefId(def_id),
            params: Vec::<CollectedParamSig>::new(),
            ret_ty,
            type_param_count: 0,
            type_param_var_names: BTreeMap::new(),
            type_param_bounds: Vec::new(),
            self_mode: Some(self_mode),
            impl_trait: None,
        }
    }

    #[test]
    fn protocol_plan_builds_for_concrete_iterable_shape() {
        let source_ty = Type::Struct {
            name: "Counter".to_string(),
            fields: Vec::new(),
        };
        let iter_ty = Type::Struct {
            name: "CounterIter".to_string(),
            fields: Vec::new(),
        };
        let done_ty = Type::Struct {
            name: ITER_DONE_TYPE_NAME.to_string(),
            fields: Vec::new(),
        };
        let mut methods = HashMap::<String, HashMap<String, Vec<CollectedCallableSig>>>::new();
        methods.insert(
            "Counter".to_string(),
            HashMap::from([(
                "iter".to_string(),
                vec![method_sig(1, ParamMode::In, iter_ty.clone())],
            )]),
        );
        methods.insert(
            "CounterIter".to_string(),
            HashMap::from([(
                "next".to_string(),
                vec![method_sig(
                    2,
                    ParamMode::InOut,
                    Type::ErrorUnion {
                        ok_ty: Box::new(Type::uint(64)),
                        err_tys: vec![done_ty.clone()],
                    },
                )],
            )]),
        );

        let plan = plan_for_iterable_type_with_methods(&source_ty, &methods)
            .expect("expected protocol for-plan");
        assert_eq!(plan.item_ty, Type::uint(64));
        assert_eq!(
            plan.kernel,
            ForKernel::Protocol(ProtocolForKernel {
                iter_ty,
                done_ty,
                iter_method: DefId(1),
                next_method: DefId(2),
            })
        );
    }

    #[test]
    fn protocol_plan_rejects_non_iter_done_sentinel() {
        let source_ty = Type::Struct {
            name: "Counter".to_string(),
            fields: Vec::new(),
        };
        let iter_ty = Type::Struct {
            name: "CounterIter".to_string(),
            fields: Vec::new(),
        };
        let wrong_done_ty = Type::Struct {
            name: "Finished".to_string(),
            fields: Vec::new(),
        };
        let mut methods = HashMap::<String, HashMap<String, Vec<CollectedCallableSig>>>::new();
        methods.insert(
            "Counter".to_string(),
            HashMap::from([(
                "iter".to_string(),
                vec![method_sig(1, ParamMode::In, iter_ty.clone())],
            )]),
        );
        methods.insert(
            "CounterIter".to_string(),
            HashMap::from([(
                "next".to_string(),
                vec![method_sig(
                    2,
                    ParamMode::InOut,
                    Type::ErrorUnion {
                        ok_ty: Box::new(Type::uint(64)),
                        err_tys: vec![wrong_done_ty],
                    },
                )],
            )]),
        );

        assert!(plan_for_iterable_type_with_methods(&source_ty, &methods).is_none());
    }
}
