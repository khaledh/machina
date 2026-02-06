use std::collections::{HashMap, HashSet};

use crate::diag::Span;
use crate::resolve::DefId;
use crate::tree::resolved::CallArg;
use crate::tree::{CallArgMode, ParamMode};
use crate::typeck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typeck::type_map::GenericInst;
use crate::typeck::unify::Unifier;
use crate::types::{
    EnumVariant, FnParam, StructField, TyVarId, Type, TypeAssignability, ValueAssignability,
    array_to_slice_assignable, value_assignable,
};

#[derive(Debug, Clone)]
pub(super) struct ParamSig {
    #[allow(dead_code)]
    pub(super) name: String,
    pub(super) ty: Type,
    #[allow(dead_code)]
    pub(super) mode: ParamMode,
}

#[derive(Debug, Clone)]
pub(super) struct OverloadSig {
    pub(super) def_id: DefId,
    pub(super) params: Vec<ParamSig>,
    pub(super) ret_ty: Type,
    pub(super) type_param_count: usize,
}

pub(super) struct CallResolution {
    pub(super) def_id: DefId,
    pub(super) param_types: Vec<Type>,
    pub(super) param_modes: Vec<ParamMode>,
    pub(super) ret_type: Type,
    pub(super) inst: Option<GenericInst>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub(super) enum ArgOverloadRank {
    Exact = 0,
    Assignable = 1,
}

pub(super) struct ResolvedOverload<'a> {
    pub(super) def_id: DefId,
    pub(super) sig: &'a OverloadSig,
    pub(super) arg_ranks: Vec<ArgOverloadRank>,
}

impl ResolvedOverload<'_> {
    fn score(&self) -> u32 {
        self.arg_ranks.iter().map(|r| *r as u32).sum()
    }
}

pub(super) struct OverloadResolver<'a> {
    call_span: Span,
    name: &'a str,
    args: &'a [CallArg],
    arg_types: &'a [Type],
}

impl<'a> OverloadResolver<'a> {
    pub(super) fn new(
        name: &'a str,
        args: &'a [CallArg],
        arg_types: &'a [Type],
        call_span: Span,
    ) -> Self {
        Self {
            call_span,
            name,
            args,
            arg_types,
        }
    }

    pub(super) fn resolve(
        self,
        overloads: &'a [OverloadSig],
    ) -> Result<ResolvedOverload<'a>, TypeCheckError> {
        // Score each candidate by per-arg assignability; keep the best score.
        // If only out-of-range errors are seen, surface that instead of "no match."
        let mut candidates = Vec::new();
        let mut range_err: Option<TypeCheckError> = None;

        for cand in overloads {
            match self.rank_overload(cand) {
                Ok(Some(ranks)) => candidates.push(ResolvedOverload {
                    def_id: cand.def_id,
                    sig: cand,
                    arg_ranks: ranks,
                }),
                Ok(None) => {}
                Err(err) => {
                    if matches!(err.kind(), TypeCheckErrorKind::ValueOutOfRange(_, _, _, _)) {
                        range_err.get_or_insert(err);
                    } else {
                        return Err(err);
                    }
                }
            }
        }

        if candidates.is_empty() {
            return Err(range_err.unwrap_or_else(|| {
                TypeCheckErrorKind::OverloadNoMatch(self.name.to_string(), self.call_span).into()
            }));
        }

        // Lower score wins (Exact < Assignable); ties are ambiguous.
        let best_score = candidates.iter().map(|c| c.score()).min().unwrap();
        let mut best: Vec<_> = candidates
            .into_iter()
            .filter(|c| c.score() == best_score)
            .collect();

        if best.len() != 1 {
            return Err(TypeCheckErrorKind::OverloadAmbiguous(
                self.name.to_string(),
                self.call_span,
            )
            .into());
        }

        Ok(best.pop().unwrap())
    }

    fn rank_overload(
        &self,
        sig: &OverloadSig,
    ) -> Result<Option<Vec<ArgOverloadRank>>, TypeCheckError> {
        // Reject wrong arity; otherwise classify each argument against its param.
        if sig.params.len() != self.arg_types.len() {
            return Ok(None);
        }

        let mut ranks = Vec::with_capacity(self.arg_types.len());
        for ((arg, arg_ty), param) in self
            .args
            .iter()
            .zip(self.arg_types.iter())
            .zip(sig.params.iter())
        {
            // Use value-aware assignability for literal narrowing and range checks.
            match value_assignable(&arg.expr, arg_ty, &param.ty) {
                ValueAssignability::Assignable(assignability) => match assignability {
                    TypeAssignability::Exact => ranks.push(ArgOverloadRank::Exact),
                    TypeAssignability::Incompatible => return Ok(None),
                    _ => ranks.push(ArgOverloadRank::Assignable),
                },
                ValueAssignability::ValueOutOfRange { value, min, max } => {
                    return Err(
                        TypeCheckErrorKind::ValueOutOfRange(value, min, max, arg.span).into(),
                    );
                }
                ValueAssignability::ValueNotNonZero { value } => {
                    return Err(TypeCheckErrorKind::ValueNotNonZero(value, arg.span).into());
                }
                ValueAssignability::Incompatible => {
                    if arg.mode != CallArgMode::Move
                        && arg.mode != CallArgMode::Out
                        && array_to_slice_assignable(arg_ty, &param.ty)
                    {
                        ranks.push(ArgOverloadRank::Assignable);
                    } else {
                        return Ok(None);
                    }
                }
            }
        }

        Ok(Some(ranks))
    }
}

pub(super) fn single_arity_param_types(
    overloads: &[OverloadSig],
    arg_count: usize,
) -> Option<Vec<Type>> {
    let mut matches = overloads.iter().filter(|sig| sig.params.len() == arg_count);
    let sig = matches.next()?;
    if matches.next().is_some() {
        return None;
    }
    Some(sig.params.iter().map(|param| param.ty.clone()).collect())
}

pub(super) fn resolve_call(
    name: &str,
    args: &[CallArg],
    arg_types: &[Type],
    overloads: &[OverloadSig],
    expected: Option<&Type>,
    allow_infer: bool,
    call_span: Span,
    fresh_infer: &mut impl FnMut() -> TyVarId,
) -> Result<CallResolution, TypeCheckError> {
    if !overloads
        .iter()
        .any(|sig| sig.params.len() == arg_types.len())
    {
        let mut counts = HashSet::new();
        for sig in overloads {
            counts.insert(sig.params.len());
        }
        if counts.len() == 1 {
            let expected = *counts.iter().next().unwrap();
            return Err(TypeCheckErrorKind::ArgCountMismatch(
                name.to_string(),
                expected,
                arg_types.len(),
                call_span,
            )
            .into());
        }
    }

    let mut generic_overloads = Vec::new();
    let mut concrete_overloads = Vec::new();
    for sig in overloads {
        if sig_has_type_vars(sig) {
            generic_overloads.push(sig);
        } else {
            concrete_overloads.push(sig);
        }
    }

    if !concrete_overloads.is_empty() {
        let concrete = concrete_overloads.into_iter().cloned().collect::<Vec<_>>();
        match OverloadResolver::new(name, args, arg_types, call_span).resolve(&concrete) {
            Ok(resolved) => {
                let param_types = resolved
                    .sig
                    .params
                    .iter()
                    .map(|param| param.ty.clone())
                    .collect::<Vec<_>>();
                let param_modes = resolved
                    .sig
                    .params
                    .iter()
                    .map(|param| param.mode.clone())
                    .collect::<Vec<_>>();
                let ret_type = resolved.sig.ret_ty.clone();
                return Ok(CallResolution {
                    def_id: resolved.def_id,
                    param_types,
                    param_modes,
                    ret_type,
                    inst: None,
                });
            }
            Err(err) => {
                if matches!(err.kind(), TypeCheckErrorKind::OverloadNoMatch(_, _))
                    && !generic_overloads.is_empty()
                {
                    let inst = resolve_generic_overload(
                        name,
                        args,
                        arg_types,
                        &generic_overloads,
                        expected,
                        allow_infer,
                        call_span,
                        fresh_infer,
                    )?;
                    let sig = generic_overloads
                        .iter()
                        .find(|sig| sig.def_id == inst.def_id)
                        .ok_or_else(|| {
                            TypeCheckError::from(TypeCheckErrorKind::OverloadNoMatch(
                                name.to_string(),
                                call_span,
                            ))
                        })?;
                    let (param_types, param_modes, ret_type) = apply_inst_to_sig(sig, &inst);
                    return Ok(CallResolution {
                        def_id: sig.def_id,
                        param_types,
                        param_modes,
                        ret_type,
                        inst: Some(inst),
                    });
                }
                return Err(err);
            }
        }
    }

    if !generic_overloads.is_empty() {
        let inst = resolve_generic_overload(
            name,
            args,
            arg_types,
            &generic_overloads,
            expected,
            allow_infer,
            call_span,
            fresh_infer,
        )?;
        let sig = generic_overloads
            .iter()
            .find(|sig| sig.def_id == inst.def_id)
            .ok_or_else(|| {
                TypeCheckError::from(TypeCheckErrorKind::OverloadNoMatch(
                    name.to_string(),
                    call_span,
                ))
            })?;
        let (param_types, param_modes, ret_type) = apply_inst_to_sig(sig, &inst);
        return Ok(CallResolution {
            def_id: sig.def_id,
            param_types,
            param_modes,
            ret_type,
            inst: Some(inst),
        });
    }

    OverloadResolver::new(name, args, arg_types, call_span)
        .resolve(overloads)
        .map(|resolved| {
            let param_types = resolved
                .sig
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect::<Vec<_>>();
            let param_modes = resolved
                .sig
                .params
                .iter()
                .map(|param| param.mode.clone())
                .collect::<Vec<_>>();
            let ret_type = resolved.sig.ret_ty.clone();
            CallResolution {
                def_id: resolved.def_id,
                param_types,
                param_modes,
                ret_type,
                inst: None,
            }
        })
}

fn resolve_generic_overload(
    name: &str,
    args: &[CallArg],
    arg_types: &[Type],
    overloads: &[&OverloadSig],
    expected: Option<&Type>,
    allow_infer: bool,
    call_span: Span,
    fresh_infer: &mut impl FnMut() -> TyVarId,
) -> Result<GenericInst, TypeCheckError> {
    let mut candidates = Vec::new();
    let mut range_err: Option<TypeCheckError> = None;

    let mut unifier_for_sig = |sig: &OverloadSig| -> Result<Option<Unifier>, TypeCheckError> {
        let mut unifier = Unifier::new();
        for ((arg, arg_ty), param) in args.iter().zip(arg_types).zip(sig.params.iter()) {
            let param_ty = &param.ty;
            let mut arg_ty_for_unify = arg_ty.clone();

            if arg.mode != CallArgMode::Move
                && arg.mode != CallArgMode::Out
                && matches!(param_ty, Type::Slice { .. })
                && let Some(item) = arg_ty.array_item_type()
            {
                arg_ty_for_unify = Type::Slice {
                    elem_ty: Box::new(item),
                };
            }

            if type_has_vars(param_ty) {
                if unifier.unify(param_ty, &arg_ty_for_unify).is_err() {
                    return Ok(None);
                }
                continue;
            }

            match value_assignable(&arg.expr, arg_ty, param_ty) {
                ValueAssignability::Assignable(assignability) => {
                    if matches!(assignability, TypeAssignability::Incompatible) {
                        return Ok(None);
                    }
                }
                ValueAssignability::ValueOutOfRange { value, min, max } => {
                    range_err.get_or_insert(
                        TypeCheckErrorKind::ValueOutOfRange(value, min, max, arg.span).into(),
                    );
                    return Ok(None);
                }
                ValueAssignability::ValueNotNonZero { value } => {
                    return Err(TypeCheckErrorKind::ValueNotNonZero(value, arg.span).into());
                }
                ValueAssignability::Incompatible => {
                    if arg.mode != CallArgMode::Move
                        && arg.mode != CallArgMode::Out
                        && array_to_slice_assignable(arg_ty, param_ty)
                    {
                        continue;
                    }
                    return Ok(None);
                }
            }
        }
        if let Some(expected_ty) = expected {
            if type_has_vars(&sig.ret_ty) {
                if unifier.unify(&sig.ret_ty, expected_ty).is_err() {
                    return Ok(None);
                }
            }
        }
        Ok(Some(unifier))
    };

    for sig in overloads {
        if sig.params.len() != arg_types.len() {
            continue;
        }

        let Some(unifier) = unifier_for_sig(sig)? else {
            continue;
        };

        let mut type_args = (0..sig.type_param_count)
            .map(|index| unifier.apply(&Type::Var(TyVarId::new(index as u32))))
            .collect::<Vec<_>>();

        if allow_infer {
            let mut replacements = HashMap::new();
            type_args = type_args
                .iter()
                .map(|ty| {
                    replace_param_vars_with_infer(
                        ty,
                        sig.type_param_count,
                        &mut replacements,
                        fresh_infer,
                    )
                })
                .collect();
        }

        if type_args
            .iter()
            .any(|ty| type_contains_param_var(ty, sig.type_param_count))
        {
            continue;
        }

        candidates.push(GenericInst {
            def_id: sig.def_id,
            type_args,
            call_span,
        });
    }

    if candidates.is_empty() {
        return Err(range_err.unwrap_or_else(|| {
            TypeCheckErrorKind::OverloadNoMatch(name.to_string(), call_span).into()
        }));
    }

    if candidates.len() != 1 {
        return Err(TypeCheckErrorKind::OverloadAmbiguous(name.to_string(), call_span).into());
    }

    Ok(candidates.pop().unwrap())
}

fn sig_has_type_vars(sig: &OverloadSig) -> bool {
    sig.type_param_count > 0
        || sig.params.iter().any(|param| type_has_vars(&param.ty))
        || type_has_vars(&sig.ret_ty)
}

fn apply_inst_to_sig(sig: &OverloadSig, inst: &GenericInst) -> (Vec<Type>, Vec<ParamMode>, Type) {
    let mut unifier = Unifier::new();
    for (index, ty) in inst.type_args.iter().enumerate() {
        let var = TyVarId::new(index as u32);
        {
            let _ = unifier.unify(&Type::Var(var), ty);
        }
    }

    let param_types = sig
        .params
        .iter()
        .map(|param| {
            let ty = unifier.apply(&param.ty);
            rewrite_nominal_names(&ty, &inst.type_args)
        })
        .collect::<Vec<_>>();
    let param_modes = sig
        .params
        .iter()
        .map(|param| param.mode.clone())
        .collect::<Vec<_>>();
    let ret_type = unifier.apply(&sig.ret_ty);
    let ret_type = rewrite_nominal_names(&ret_type, &inst.type_args);
    (param_types, param_modes, ret_type)
}

fn type_has_vars(ty: &Type) -> bool {
    match ty {
        Type::Var(_) => true,
        Type::Array { elem_ty, .. }
        | Type::Slice { elem_ty }
        | Type::Heap { elem_ty }
        | Type::Ref { elem_ty, .. }
        | Type::Range { elem_ty } => type_has_vars(elem_ty),
        Type::Tuple { field_tys } => field_tys.iter().any(type_has_vars),
        Type::Struct { fields, .. } => fields.iter().any(|field| type_has_vars(&field.ty)),
        Type::Enum { variants, .. } => variants
            .iter()
            .any(|variant| variant.payload.iter().any(type_has_vars)),
        Type::Fn { params, ret_ty } => {
            params.iter().any(|param| type_has_vars(&param.ty)) || type_has_vars(ret_ty)
        }
        _ => false,
    }
}

fn rewrite_nominal_names(ty: &Type, type_args: &[Type]) -> Type {
    match ty {
        Type::Struct { name, fields } => Type::Struct {
            name: subst_type_vars_in_name(name, type_args),
            fields: fields
                .iter()
                .map(|field| StructField {
                    name: field.name.clone(),
                    ty: rewrite_nominal_names(&field.ty, type_args),
                })
                .collect(),
        },
        Type::Enum { name, variants } => Type::Enum {
            name: subst_type_vars_in_name(name, type_args),
            variants: variants
                .iter()
                .map(|variant| EnumVariant {
                    name: variant.name.clone(),
                    payload: variant
                        .payload
                        .iter()
                        .map(|ty| rewrite_nominal_names(ty, type_args))
                        .collect(),
                })
                .collect(),
        },
        Type::Array { elem_ty, dims } => Type::Array {
            elem_ty: Box::new(rewrite_nominal_names(elem_ty, type_args)),
            dims: dims.clone(),
        },
        Type::Slice { elem_ty } => Type::Slice {
            elem_ty: Box::new(rewrite_nominal_names(elem_ty, type_args)),
        },
        Type::Heap { elem_ty } => Type::Heap {
            elem_ty: Box::new(rewrite_nominal_names(elem_ty, type_args)),
        },
        Type::Ref { mutable, elem_ty } => Type::Ref {
            mutable: *mutable,
            elem_ty: Box::new(rewrite_nominal_names(elem_ty, type_args)),
        },
        Type::Range { elem_ty } => Type::Range {
            elem_ty: Box::new(rewrite_nominal_names(elem_ty, type_args)),
        },
        Type::Tuple { field_tys } => Type::Tuple {
            field_tys: field_tys
                .iter()
                .map(|ty| rewrite_nominal_names(ty, type_args))
                .collect(),
        },
        Type::Fn { params, ret_ty } => Type::Fn {
            params: params
                .iter()
                .map(|param| FnParam {
                    mode: param.mode,
                    ty: rewrite_nominal_names(&param.ty, type_args),
                })
                .collect(),
            ret_ty: Box::new(rewrite_nominal_names(ret_ty, type_args)),
        },
        _ => ty.clone(),
    }
}

fn subst_type_vars_in_name(name: &str, type_args: &[Type]) -> String {
    let mut out = String::with_capacity(name.len());
    let mut chars = name.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == 'T' {
            let mut digits = String::new();
            while let Some(next) = chars.peek() {
                if next.is_ascii_digit() {
                    digits.push(*next);
                    chars.next();
                } else {
                    break;
                }
            }
            if !digits.is_empty() {
                if let Ok(index) = digits.parse::<usize>() {
                    if let Some(ty) = type_args.get(index) {
                        out.push_str(&type_arg_name(ty));
                        continue;
                    }
                }
                out.push('T');
                out.push_str(&digits);
                continue;
            }
        }
        out.push(ch);
    }
    out
}

fn type_arg_name(ty: &Type) -> String {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        Type::Var(var) => format!("T{}", var.index()),
        _ => ty.to_string(),
    }
}

fn type_contains_param_var(ty: &Type, type_param_count: usize) -> bool {
    let param_limit = type_param_count as u32;
    match ty {
        Type::Var(var) => var.index() < param_limit,
        Type::Array { elem_ty, .. }
        | Type::Slice { elem_ty }
        | Type::Heap { elem_ty }
        | Type::Ref { elem_ty, .. }
        | Type::Range { elem_ty } => type_contains_param_var(elem_ty, type_param_count),
        Type::Tuple { field_tys } => field_tys
            .iter()
            .any(|ty| type_contains_param_var(ty, type_param_count)),
        Type::Struct { fields, .. } => fields
            .iter()
            .any(|field| type_contains_param_var(&field.ty, type_param_count)),
        Type::Enum { variants, .. } => variants.iter().any(|variant| {
            variant
                .payload
                .iter()
                .any(|ty| type_contains_param_var(ty, type_param_count))
        }),
        Type::Fn { params, ret_ty } => {
            params
                .iter()
                .any(|param| type_contains_param_var(&param.ty, type_param_count))
                || type_contains_param_var(ret_ty, type_param_count)
        }
        _ => false,
    }
}

fn replace_param_vars_with_infer(
    ty: &Type,
    type_param_count: usize,
    replacements: &mut HashMap<u32, TyVarId>,
    fresh_infer: &mut impl FnMut() -> TyVarId,
) -> Type {
    let param_limit = type_param_count as u32;
    match ty {
        Type::Var(var) if var.index() < param_limit => {
            let entry = replacements
                .entry(var.index())
                .or_insert_with(|| fresh_infer());
            Type::Var(*entry)
        }
        Type::Array { elem_ty, dims } => Type::Array {
            elem_ty: Box::new(replace_param_vars_with_infer(
                elem_ty,
                type_param_count,
                replacements,
                fresh_infer,
            )),
            dims: dims.clone(),
        },
        Type::Slice { elem_ty } => Type::Slice {
            elem_ty: Box::new(replace_param_vars_with_infer(
                elem_ty,
                type_param_count,
                replacements,
                fresh_infer,
            )),
        },
        Type::Heap { elem_ty } => Type::Heap {
            elem_ty: Box::new(replace_param_vars_with_infer(
                elem_ty,
                type_param_count,
                replacements,
                fresh_infer,
            )),
        },
        Type::Ref { mutable, elem_ty } => Type::Ref {
            mutable: *mutable,
            elem_ty: Box::new(replace_param_vars_with_infer(
                elem_ty,
                type_param_count,
                replacements,
                fresh_infer,
            )),
        },
        Type::Range { elem_ty } => Type::Range {
            elem_ty: Box::new(replace_param_vars_with_infer(
                elem_ty,
                type_param_count,
                replacements,
                fresh_infer,
            )),
        },
        Type::Tuple { field_tys } => Type::Tuple {
            field_tys: field_tys
                .iter()
                .map(|ty| {
                    replace_param_vars_with_infer(ty, type_param_count, replacements, fresh_infer)
                })
                .collect(),
        },
        Type::Struct { name, fields } => Type::Struct {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|field| StructField {
                    name: field.name.clone(),
                    ty: replace_param_vars_with_infer(
                        &field.ty,
                        type_param_count,
                        replacements,
                        fresh_infer,
                    ),
                })
                .collect(),
        },
        Type::Enum { name, variants } => Type::Enum {
            name: name.clone(),
            variants: variants
                .iter()
                .map(|variant| EnumVariant {
                    name: variant.name.clone(),
                    payload: variant
                        .payload
                        .iter()
                        .map(|ty| {
                            replace_param_vars_with_infer(
                                ty,
                                type_param_count,
                                replacements,
                                fresh_infer,
                            )
                        })
                        .collect(),
                })
                .collect(),
        },
        Type::Fn { params, ret_ty } => Type::Fn {
            params: params
                .iter()
                .map(|param| FnParam {
                    mode: param.mode,
                    ty: replace_param_vars_with_infer(
                        &param.ty,
                        type_param_count,
                        replacements,
                        fresh_infer,
                    ),
                })
                .collect(),
            ret_ty: Box::new(replace_param_vars_with_infer(
                ret_ty,
                type_param_count,
                replacements,
                fresh_infer,
            )),
        },
        _ => ty.clone(),
    }
}
