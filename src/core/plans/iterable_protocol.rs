//! Shared structural iterable protocol checking.
//!
//! This module recognizes concrete types that satisfy Machina's iterable
//! protocol through `iter()` / `next()` methods and extracts the witness data
//! needed by later stages.

use std::collections::HashMap;

use crate::core::ast::ParamMode;
use crate::core::resolve::DefId;
use crate::core::typecheck::CollectedCallableSig;
use crate::core::types::{TyVarId, Type};

pub const ITER_DONE_TYPE_NAME: &str = "IterDone";

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IterableProtocolWitness {
    pub item_ty: Type,
    pub iter_ty: Type,
    pub done_ty: Type,
    pub propagated_err_tys: Vec<Type>,
    pub iter_method: DefId,
    pub next_method: DefId,
    pub iter_method_type_args: Vec<Type>,
    pub next_method_type_args: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IterableProtocolError {
    MissingIter { source_ty: Type },
    MissingNext { iter_ty: Type },
    InvalidNextReturn { ret_ty: Type },
}

pub(crate) fn check_iterable_protocol_with_methods(
    ty: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Result<IterableProtocolWitness, IterableProtocolError> {
    let owner = protocol_owner_name(ty).ok_or_else(|| IterableProtocolError::MissingIter {
        source_ty: ty.clone(),
    })?;
    let iter_sig = select_protocol_method(method_sigs, owner, "iter", ParamMode::In, ty)
        .ok_or_else(|| IterableProtocolError::MissingIter {
            source_ty: ty.clone(),
        })?;

    let iter_ty = iter_sig.ret_ty.clone();
    let iter_owner =
        protocol_owner_name(&iter_ty).ok_or_else(|| IterableProtocolError::MissingNext {
            iter_ty: iter_ty.clone(),
        })?;
    let next_sig =
        select_protocol_method(method_sigs, iter_owner, "next", ParamMode::InOut, &iter_ty)
            .ok_or_else(|| IterableProtocolError::MissingNext {
                iter_ty: iter_ty.clone(),
            })?;

    let (item_ty, done_ty, propagated_err_tys) =
        protocol_next_shape(&next_sig.ret_ty).ok_or_else(|| {
            IterableProtocolError::InvalidNextReturn {
                ret_ty: next_sig.ret_ty.clone(),
            }
        })?;

    Ok(IterableProtocolWitness {
        item_ty,
        iter_ty,
        done_ty,
        propagated_err_tys,
        iter_method: iter_sig.def_id,
        next_method: next_sig.def_id,
        iter_method_type_args: iter_sig.type_args,
        next_method_type_args: next_sig.type_args,
    })
}

fn protocol_owner_name(ty: &Type) -> Option<&str> {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name),
        Type::String => Some("string"),
        _ => None,
    }
}

pub(crate) fn can_use_iterable_protocol_methods(ty: &Type) -> bool {
    protocol_owner_name(ty).is_some()
}

fn select_protocol_method(
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
    owner: &str,
    method_name: &str,
    self_mode: ParamMode,
    receiver_ty: &Type,
) -> Option<InstantiatedProtocolMethodSig> {
    let overloads = method_sigs.get(owner)?.get(method_name)?;
    let mut best: Option<InstantiatedProtocolMethodSig> = None;
    let mut best_priority = i32::MIN;
    let mut best_specialization_priority = i32::MIN;
    let mut ambiguous = false;

    for sig in overloads {
        if !sig.params.is_empty() || sig.self_mode != Some(self_mode.clone()) {
            continue;
        }

        let Some(instantiated) = instantiate_protocol_sig(sig, receiver_ty) else {
            continue;
        };

        let priority = if sig.impl_trait.is_none() { 1 } else { 0 };
        let specialization_priority = if sig.type_param_count == 0 { 1 } else { 0 };
        match best {
            None => {
                best = Some(instantiated);
                best_priority = priority;
                best_specialization_priority = specialization_priority;
                ambiguous = false;
            }
            Some(_) if priority > best_priority => {
                best = Some(instantiated);
                best_priority = priority;
                best_specialization_priority = specialization_priority;
                ambiguous = false;
            }
            Some(_)
                if priority == best_priority
                    && specialization_priority > best_specialization_priority =>
            {
                best = Some(instantiated);
                best_specialization_priority = specialization_priority;
                ambiguous = false;
            }
            Some(_)
                if priority == best_priority
                    && specialization_priority == best_specialization_priority =>
            {
                ambiguous = true;
            }
            Some(_) => {}
        }
    }

    if ambiguous { None } else { best }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct InstantiatedProtocolMethodSig {
    def_id: DefId,
    ret_ty: Type,
    type_args: Vec<Type>,
}

fn instantiate_protocol_sig(
    sig: &CollectedCallableSig,
    receiver_ty: &Type,
) -> Option<InstantiatedProtocolMethodSig> {
    let self_ty = sig.self_ty.as_ref()?;
    let normalized_self_ty = normalize_protocol_nominal_names(self_ty);
    let normalized_receiver_ty = normalize_protocol_nominal_names(receiver_ty);

    if sig.type_param_count == 0 {
        return (normalized_self_ty == normalized_receiver_ty).then(|| {
            InstantiatedProtocolMethodSig {
                def_id: sig.def_id,
                ret_ty: sig.ret_ty.clone(),
                type_args: Vec::new(),
            }
        });
    }

    let mut subst = HashMap::new();
    if !bind_protocol_type_vars(&normalized_self_ty, &normalized_receiver_ty, &mut subst) {
        return None;
    }
    if (0..sig.type_param_count).any(|i| !subst.contains_key(&TyVarId::new(i as u32))) {
        return None;
    }

    Some(InstantiatedProtocolMethodSig {
        def_id: sig.def_id,
        ret_ty: subst_type_vars(&sig.ret_ty, &subst),
        type_args: (0..sig.type_param_count)
            .map(|i| {
                subst
                    .get(&TyVarId::new(i as u32))
                    .cloned()
                    .expect("bound type arg")
            })
            .collect(),
    })
}

fn bind_protocol_type_vars(
    pattern: &Type,
    concrete: &Type,
    subst: &mut HashMap<TyVarId, Type>,
) -> bool {
    match pattern {
        Type::Var(var) => match subst.get(var) {
            Some(bound) => bound == concrete,
            None => {
                subst.insert(*var, concrete.clone());
                true
            }
        },
        Type::Fn { params, ret_ty } => {
            let Type::Fn {
                params: concrete_params,
                ret_ty: concrete_ret,
            } = concrete
            else {
                return false;
            };
            params.len() == concrete_params.len()
                && params
                    .iter()
                    .zip(concrete_params.iter())
                    .all(|(left, right)| {
                        left.mode == right.mode
                            && bind_protocol_type_vars(&left.ty, &right.ty, subst)
                    })
                && bind_protocol_type_vars(ret_ty, concrete_ret, subst)
        }
        Type::Range { elem_ty } => {
            let Type::Range {
                elem_ty: concrete_elem_ty,
            } = concrete
            else {
                return false;
            };
            bind_protocol_type_vars(elem_ty, concrete_elem_ty, subst)
        }
        Type::Array { elem_ty, dims } => {
            let Type::Array {
                elem_ty: concrete_elem_ty,
                dims: concrete_dims,
            } = concrete
            else {
                return false;
            };
            dims == concrete_dims && bind_protocol_type_vars(elem_ty, concrete_elem_ty, subst)
        }
        Type::DynArray { elem_ty } => {
            let Type::DynArray {
                elem_ty: concrete_elem_ty,
            } = concrete
            else {
                return false;
            };
            bind_protocol_type_vars(elem_ty, concrete_elem_ty, subst)
        }
        Type::Set { elem_ty } => {
            let Type::Set {
                elem_ty: concrete_elem_ty,
            } = concrete
            else {
                return false;
            };
            bind_protocol_type_vars(elem_ty, concrete_elem_ty, subst)
        }
        Type::Map { key_ty, value_ty } => {
            let Type::Map {
                key_ty: concrete_key_ty,
                value_ty: concrete_value_ty,
            } = concrete
            else {
                return false;
            };
            bind_protocol_type_vars(key_ty, concrete_key_ty, subst)
                && bind_protocol_type_vars(value_ty, concrete_value_ty, subst)
        }
        Type::Tuple { field_tys } => {
            let Type::Tuple {
                field_tys: concrete_field_tys,
            } = concrete
            else {
                return false;
            };
            field_tys.len() == concrete_field_tys.len()
                && field_tys
                    .iter()
                    .zip(concrete_field_tys.iter())
                    .all(|(left, right)| bind_protocol_type_vars(left, right, subst))
        }
        Type::Struct { name, fields, .. } => {
            let Type::Struct {
                name: concrete_name,
                type_args: _,
                fields: concrete_fields,
            } = concrete
            else {
                return false;
            };
            name == concrete_name
                && fields.len() == concrete_fields.len()
                && fields
                    .iter()
                    .zip(concrete_fields.iter())
                    .all(|(left, right)| {
                        left.name == right.name
                            && bind_protocol_type_vars(&left.ty, &right.ty, subst)
                    })
        }
        Type::Enum { name, variants, .. } => {
            let Type::Enum {
                name: concrete_name,
                type_args: _,
                variants: concrete_variants,
            } = concrete
            else {
                return false;
            };
            name == concrete_name
                && variants.len() == concrete_variants.len()
                && variants
                    .iter()
                    .zip(concrete_variants.iter())
                    .all(|(left, right)| {
                        left.name == right.name
                            && left.payload.len() == right.payload.len()
                            && left.payload.iter().zip(right.payload.iter()).all(
                                |(left_ty, right_ty)| {
                                    bind_protocol_type_vars(left_ty, right_ty, subst)
                                },
                            )
                    })
        }
        Type::Slice { elem_ty } => {
            let Type::Slice {
                elem_ty: concrete_elem_ty,
            } = concrete
            else {
                return false;
            };
            bind_protocol_type_vars(elem_ty, concrete_elem_ty, subst)
        }
        Type::Heap { elem_ty } => {
            let Type::Heap {
                elem_ty: concrete_elem_ty,
            } = concrete
            else {
                return false;
            };
            bind_protocol_type_vars(elem_ty, concrete_elem_ty, subst)
        }
        Type::Ref { mutable, elem_ty } => {
            let Type::Ref {
                mutable: concrete_mutable,
                elem_ty: concrete_elem_ty,
            } = concrete
            else {
                return false;
            };
            mutable == concrete_mutable && bind_protocol_type_vars(elem_ty, concrete_elem_ty, subst)
        }
        Type::ErrorUnion { ok_ty, err_tys } => {
            let Type::ErrorUnion {
                ok_ty: concrete_ok_ty,
                err_tys: concrete_err_tys,
            } = concrete
            else {
                return false;
            };
            err_tys.len() == concrete_err_tys.len()
                && bind_protocol_type_vars(ok_ty, concrete_ok_ty, subst)
                && err_tys
                    .iter()
                    .zip(concrete_err_tys.iter())
                    .all(|(left, right)| bind_protocol_type_vars(left, right, subst))
        }
        Type::Pending { response_tys } => {
            let Type::Pending {
                response_tys: concrete_response_tys,
            } = concrete
            else {
                return false;
            };
            response_tys.len() == concrete_response_tys.len()
                && response_tys
                    .iter()
                    .zip(concrete_response_tys.iter())
                    .all(|(left, right)| bind_protocol_type_vars(left, right, subst))
        }
        Type::ReplyCap { response_tys } => {
            let Type::ReplyCap {
                response_tys: concrete_response_tys,
            } = concrete
            else {
                return false;
            };
            response_tys.len() == concrete_response_tys.len()
                && response_tys
                    .iter()
                    .zip(concrete_response_tys.iter())
                    .all(|(left, right)| bind_protocol_type_vars(left, right, subst))
        }
        _ => pattern == concrete,
    }
}

fn normalize_protocol_nominal_names(ty: &Type) -> Type {
    ty.map_cloned(&|t| match t {
        Type::Struct {
            name,
            type_args,
            fields,
        } => Type::Struct {
            name,
            type_args,
            fields,
        },
        Type::Enum {
            name,
            type_args,
            variants,
        } => Type::Enum {
            name,
            type_args,
            variants,
        },
        other => other,
    })
}

fn subst_type_vars(ty: &Type, map: &HashMap<TyVarId, Type>) -> Type {
    ty.map_cloned(&|t| match t {
        Type::Var(var) => map.get(&var).cloned().unwrap_or(Type::Var(var)),
        other => other,
    })
}

fn protocol_next_shape(ret_ty: &Type) -> Option<(Type, Type, Vec<Type>)> {
    let Type::ErrorUnion { ok_ty, err_tys } = ret_ty else {
        return None;
    };

    let mut done_ty = None;
    let mut propagated_err_tys = Vec::new();
    for err_ty in err_tys {
        if is_iter_done_type(err_ty) {
            if done_ty.is_some() {
                return None;
            }
            done_ty = Some(err_ty.clone());
        } else {
            propagated_err_tys.push(err_ty.clone());
        }
    }

    let Some(done_ty) = done_ty else {
        return None;
    };

    Some(((**ok_ty).clone(), done_ty, propagated_err_tys))
}

pub fn is_iter_done_type(ty: &Type) -> bool {
    matches!(ty, Type::Struct { name, fields, .. } if name == ITER_DONE_TYPE_NAME && fields.is_empty())
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};

    use super::*;
    use crate::core::typecheck::CollectedParamSig;
    use crate::core::types::StructField;

    fn method_sig(
        def_id: u32,
        self_ty: Type,
        self_mode: ParamMode,
        ret_ty: Type,
    ) -> CollectedCallableSig {
        CollectedCallableSig {
            def_id: DefId(def_id),
            self_ty: Some(self_ty),
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
    fn iterable_protocol_witness_builds_for_concrete_iterable_shape() {
        let source_ty = Type::Struct {
            name: "Counter".to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let iter_ty = Type::Struct {
            name: "CounterIter".to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let done_ty = Type::Struct {
            name: ITER_DONE_TYPE_NAME.to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let mut methods = HashMap::<String, HashMap<String, Vec<CollectedCallableSig>>>::new();
        methods.insert(
            "Counter".to_string(),
            HashMap::from([(
                "iter".to_string(),
                vec![method_sig(
                    1,
                    source_ty.clone(),
                    ParamMode::In,
                    iter_ty.clone(),
                )],
            )]),
        );
        methods.insert(
            "CounterIter".to_string(),
            HashMap::from([(
                "next".to_string(),
                vec![method_sig(
                    2,
                    iter_ty.clone(),
                    ParamMode::InOut,
                    Type::ErrorUnion {
                        ok_ty: Box::new(Type::uint(64)),
                        err_tys: vec![done_ty.clone()],
                    },
                )],
            )]),
        );

        let witness = check_iterable_protocol_with_methods(&source_ty, &methods)
            .expect("expected iterable protocol witness");
        assert_eq!(
            witness,
            IterableProtocolWitness {
                item_ty: Type::uint(64),
                iter_ty,
                done_ty,
                propagated_err_tys: Vec::new(),
                iter_method: DefId(1),
                next_method: DefId(2),
                iter_method_type_args: Vec::new(),
                next_method_type_args: Vec::new(),
            }
        );
    }

    #[test]
    fn iterable_protocol_witness_accepts_fallible_next_shape() {
        let source_ty = Type::Struct {
            name: "Counter".to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let iter_ty = Type::Struct {
            name: "CounterIter".to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let done_ty = Type::Struct {
            name: ITER_DONE_TYPE_NAME.to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let parse_err_ty = Type::Struct {
            name: "ParseError".to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let mut methods = HashMap::<String, HashMap<String, Vec<CollectedCallableSig>>>::new();
        methods.insert(
            "Counter".to_string(),
            HashMap::from([(
                "iter".to_string(),
                vec![method_sig(
                    1,
                    source_ty.clone(),
                    ParamMode::In,
                    iter_ty.clone(),
                )],
            )]),
        );
        methods.insert(
            "CounterIter".to_string(),
            HashMap::from([(
                "next".to_string(),
                vec![method_sig(
                    2,
                    iter_ty.clone(),
                    ParamMode::InOut,
                    Type::ErrorUnion {
                        ok_ty: Box::new(Type::uint(64)),
                        err_tys: vec![parse_err_ty.clone(), done_ty.clone()],
                    },
                )],
            )]),
        );

        let witness = check_iterable_protocol_with_methods(&source_ty, &methods)
            .expect("expected fallible iterable protocol witness");
        assert_eq!(witness.item_ty, Type::uint(64));
        assert_eq!(witness.propagated_err_tys, vec![parse_err_ty]);
    }

    #[test]
    fn iterable_protocol_reports_invalid_next_return() {
        let source_ty = Type::Struct {
            name: "Counter".to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let iter_ty = Type::Struct {
            name: "CounterIter".to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let wrong_done_ty = Type::Struct {
            name: "Finished".to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let mut methods = HashMap::<String, HashMap<String, Vec<CollectedCallableSig>>>::new();
        methods.insert(
            "Counter".to_string(),
            HashMap::from([(
                "iter".to_string(),
                vec![method_sig(
                    1,
                    source_ty.clone(),
                    ParamMode::In,
                    iter_ty.clone(),
                )],
            )]),
        );
        methods.insert(
            "CounterIter".to_string(),
            HashMap::from([(
                "next".to_string(),
                vec![method_sig(
                    2,
                    iter_ty,
                    ParamMode::InOut,
                    Type::ErrorUnion {
                        ok_ty: Box::new(Type::uint(64)),
                        err_tys: vec![wrong_done_ty],
                    },
                )],
            )]),
        );

        assert_eq!(
            check_iterable_protocol_with_methods(&source_ty, &methods),
            Err(IterableProtocolError::InvalidNextReturn {
                ret_ty: Type::ErrorUnion {
                    ok_ty: Box::new(Type::uint(64)),
                    err_tys: vec![Type::Struct {
                        name: "Finished".to_string(),
                        type_args: Vec::new(),
                        fields: Vec::new(),
                    }],
                },
            })
        );
    }

    #[test]
    fn iterable_protocol_accepts_generic_receiver_methods_when_receiver_is_concrete() {
        let source_ty = Type::Struct {
            name: "MapIter".to_string(),
            type_args: vec![
                Type::Struct {
                    name: "CounterIter".to_string(),
                    type_args: Vec::new(),
                    fields: Vec::new(),
                },
                Type::uint(64),
                Type::uint(64),
            ],
            fields: vec![
                StructField {
                    name: "source".to_string(),
                    ty: Type::Struct {
                        name: "CounterIter".to_string(),
                        type_args: Vec::new(),
                        fields: Vec::new(),
                    },
                },
                StructField {
                    name: "f".to_string(),
                    ty: Type::Fn {
                        params: vec![crate::core::types::FnParam {
                            mode: crate::core::types::FnParamMode::In,
                            ty: Type::uint(64),
                        }],
                        ret_ty: Box::new(Type::uint(64)),
                    },
                },
            ],
        };
        let done_ty = Type::Struct {
            name: ITER_DONE_TYPE_NAME.to_string(),
            type_args: Vec::new(),
            fields: Vec::new(),
        };
        let mut methods = HashMap::<String, HashMap<String, Vec<CollectedCallableSig>>>::new();
        methods.insert(
            "MapIter".to_string(),
            HashMap::from([
                (
                    "iter".to_string(),
                    vec![CollectedCallableSig {
                        def_id: DefId(10),
                        self_ty: Some(Type::Struct {
                            name: "MapIter".to_string(),
                            type_args: vec![
                                Type::Var(TyVarId::new(0)),
                                Type::Var(TyVarId::new(1)),
                                Type::Var(TyVarId::new(2)),
                            ],
                            fields: vec![
                                StructField {
                                    name: "source".to_string(),
                                    ty: Type::Var(TyVarId::new(0)),
                                },
                                StructField {
                                    name: "f".to_string(),
                                    ty: Type::Fn {
                                        params: vec![crate::core::types::FnParam {
                                            mode: crate::core::types::FnParamMode::In,
                                            ty: Type::Var(TyVarId::new(1)),
                                        }],
                                        ret_ty: Box::new(Type::Var(TyVarId::new(2))),
                                    },
                                },
                            ],
                        }),
                        params: Vec::new(),
                        ret_ty: Type::Struct {
                            name: "MapIter".to_string(),
                            type_args: vec![
                                Type::Var(TyVarId::new(0)),
                                Type::Var(TyVarId::new(1)),
                                Type::Var(TyVarId::new(2)),
                            ],
                            fields: vec![
                                StructField {
                                    name: "source".to_string(),
                                    ty: Type::Var(TyVarId::new(0)),
                                },
                                StructField {
                                    name: "f".to_string(),
                                    ty: Type::Fn {
                                        params: vec![crate::core::types::FnParam {
                                            mode: crate::core::types::FnParamMode::In,
                                            ty: Type::Var(TyVarId::new(1)),
                                        }],
                                        ret_ty: Box::new(Type::Var(TyVarId::new(2))),
                                    },
                                },
                            ],
                        },
                        type_param_count: 3,
                        type_param_var_names: BTreeMap::new(),
                        type_param_bounds: Vec::new(),
                        self_mode: Some(ParamMode::In),
                        impl_trait: None,
                    }],
                ),
                (
                    "next".to_string(),
                    vec![CollectedCallableSig {
                        def_id: DefId(11),
                        self_ty: Some(Type::Struct {
                            name: "MapIter".to_string(),
                            type_args: vec![
                                Type::Var(TyVarId::new(0)),
                                Type::Var(TyVarId::new(1)),
                                Type::Var(TyVarId::new(2)),
                            ],
                            fields: vec![
                                StructField {
                                    name: "source".to_string(),
                                    ty: Type::Var(TyVarId::new(0)),
                                },
                                StructField {
                                    name: "f".to_string(),
                                    ty: Type::Fn {
                                        params: vec![crate::core::types::FnParam {
                                            mode: crate::core::types::FnParamMode::In,
                                            ty: Type::Var(TyVarId::new(1)),
                                        }],
                                        ret_ty: Box::new(Type::Var(TyVarId::new(2))),
                                    },
                                },
                            ],
                        }),
                        params: Vec::new(),
                        ret_ty: Type::ErrorUnion {
                            ok_ty: Box::new(Type::Var(TyVarId::new(2))),
                            err_tys: vec![done_ty.clone()],
                        },
                        type_param_count: 3,
                        type_param_var_names: BTreeMap::new(),
                        type_param_bounds: Vec::new(),
                        self_mode: Some(ParamMode::InOut),
                        impl_trait: None,
                    }],
                ),
            ]),
        );

        let witness = check_iterable_protocol_with_methods(&source_ty, &methods)
            .expect("expected generic receiver protocol witness");
        assert_eq!(witness.item_ty, Type::uint(64));
        assert_eq!(witness.done_ty, done_ty);
        assert_eq!(witness.iter_method, DefId(10));
        assert_eq!(witness.next_method, DefId(11));
    }
}
