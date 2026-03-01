//! Pass 5 of the type checker: finalize side tables.
//!
//! This pass materializes concrete `TypeMap`/`CallSigMap`/`GenericInstMap`
//! outputs from solver results and collected obligations, then builds the
//! typed tree context used by downstream compiler stages.

use std::collections::{HashMap, HashSet};

use crate::core::context::TypeCheckedContext;
use crate::core::resolve::DefId;
use crate::core::tree::NodeId;
use crate::core::tree::visit::Visitor;
use crate::core::tree::*;
use crate::core::typecheck::Unifier;
use crate::core::typecheck::builtin_methods;
use crate::core::typecheck::constraints::{CallCallee, ExprObligation};
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::engine::{
    CollectedCallableSig, CollectedPropertySig, CollectedTraitSig,
};
use crate::core::typecheck::errors::TypeCheckError;
use crate::core::typecheck::nominal_infer::NominalKeyResolver;
use crate::core::typecheck::property_access;
use crate::core::typecheck::type_map::{
    CallParam, CallSig, CallSigMap, GenericInst, GenericInstMap, TypeMap, TypeMapBuilder,
};
use crate::core::typecheck::utils::fn_param_mode;
use crate::core::types::{FnParam, TyVarId, Type};

#[cfg(test)]
fn infer_type_args_from_instance(
    template: &Type,
    concrete: &Type,
    param_count: usize,
) -> Option<Vec<Type>> {
    crate::core::typecheck::nominal_infer::infer_type_args_from_instance(
        template,
        concrete,
        param_count,
    )
}

#[derive(Debug, Clone)]
pub(crate) struct FinalizeOutput {
    pub(crate) type_map: TypeMap,
    pub(crate) call_sigs: CallSigMap,
    pub(crate) generic_insts: GenericInstMap,
}

/// Pass 5: finalize side tables.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    if !engine.state().diags.is_empty() {
        return Err(engine.state().diags.clone());
    }

    let finalized = build_outputs(engine);
    engine.state_mut().finalize = Some(finalized);
    Ok(())
}

/// Partial finalize used by analysis mode: always materialize side tables even
/// when earlier phases produced diagnostics.
pub(crate) fn run_partial(engine: &mut TypecheckEngine) {
    let finalized = build_outputs(engine);
    engine.state_mut().finalize = Some(finalized);
}

pub(crate) fn materialize(
    engine: TypecheckEngine,
) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    let finalized = engine
        .state()
        .finalize
        .clone()
        .expect("finalize output must be populated before materialize");

    let type_checked_context = engine.context().clone().with_type_map(
        finalized.type_map,
        finalized.call_sigs,
        finalized.generic_insts,
    );

    Ok(type_checked_context)
}

fn build_outputs(engine: &TypecheckEngine) -> FinalizeOutput {
    let mut builder = TypeMapBuilder::new();
    let nominal_keys = NominalKeyResolver::new(engine.context(), &engine.env().imported_facts);

    // Seed all payload nodes so every AST node has a type entry, even if a
    // specific node was not explicitly solved.
    let all_payload_nodes = collect_payload_nodes(&engine.context().module);
    for node_id in all_payload_nodes {
        let ty = resolved_node_type_or_unknown(engine, node_id);
        record_node_type_with_nominal(&mut builder, node_id, ty, &nominal_keys);
    }
    for (node_id, ty) in &engine.state().solve.resolved_node_types {
        record_node_type_with_nominal(&mut builder, *node_id, ty.clone(), &nominal_keys);
    }

    // Ensure callable definition nodes always have a return type entry.
    for func_def in engine.context().module.func_defs() {
        let ty = resolved_node_type_or_unknown(engine, func_def.id);
        record_node_type_with_nominal(&mut builder, func_def.id, ty, &nominal_keys);
    }
    for method_block in engine.context().module.method_blocks() {
        for method_item in &method_block.method_items {
            if let MethodItem::Def(method_def) = method_item {
                let ty = resolved_node_type_or_unknown(engine, method_def.id);
                record_node_type_with_nominal(&mut builder, method_def.id, ty, &nominal_keys);
            }
        }
    }

    let callable_types = collect_callable_def_types(engine);
    let callable_type_param_names = collect_callable_type_param_names(engine);

    // Fill definition-side types, with callable fallback for unresolved defs.
    for def in engine.context().def_table.clone() {
        let def_id = def.id;
        let mut ty = engine
            .state()
            .solve
            .resolved_def_types
            .get(&def_id)
            .cloned()
            .unwrap_or(Type::Unknown);
        if is_unresolved_type(&ty)
            && let Some(callable_ty) = callable_types.get(&def_id)
        {
            ty = callable_ty.clone();
        }
        let nominal = nominal_keys.infer(&ty);
        builder.record_def_type_with_nominal(def, ty, nominal);
        if let Some(names) = callable_type_param_names.get(&def_id) {
            builder.record_def_type_param_names(def_id, names.clone());
        }
    }

    // Materialize call signatures and generic instantiations from call
    // obligations. Prefer solver-selected overload def ids when available.
    for obligation in &engine.state().constrain.call_obligations {
        let arg_types = obligation
            .arg_terms
            .iter()
            .map(|term| resolve_term(term, engine))
            .collect::<Vec<_>>();
        let expected_ret = resolve_term(&obligation.ret_ty, engine);
        let selected_def_id = engine
            .state()
            .solve
            .resolved_call_defs
            .get(&obligation.call_node)
            .copied();
        let resolved = match &obligation.callee {
            CallCallee::NamedFunction { name, .. } => {
                if let Some(def_id) = selected_def_id {
                    resolve_named_call_by_def_id(
                        engine,
                        name,
                        def_id,
                        &arg_types,
                        obligation.span,
                        &expected_ret,
                    )
                } else {
                    resolve_named_call(engine, name, &arg_types, obligation.span, &expected_ret)
                }
            }
            CallCallee::Method { name } => {
                if let Some(def_id) = selected_def_id {
                    resolve_method_call_by_def_id(
                        engine,
                        obligation.receiver.as_ref(),
                        name,
                        def_id,
                        &arg_types,
                        obligation.span,
                        &expected_ret,
                    )
                } else {
                    resolve_method_call(
                        engine,
                        obligation.receiver.as_ref(),
                        name,
                        &arg_types,
                        obligation.span,
                        &expected_ret,
                    )
                }
            }
            CallCallee::Dynamic { .. } => None,
        };
        let builtin = match &obligation.callee {
            CallCallee::Method { name } => {
                resolve_builtin_method_call(engine, obligation.receiver.as_ref(), name)
            }
            _ => None,
        };
        let (def_id, receiver, params, generic_inst) = if let Some(resolved) = resolved {
            (
                Some(resolved.def_id),
                resolved.receiver,
                resolved.params,
                resolved.inst,
            )
        } else if let Some((receiver, params)) = builtin {
            (None, Some(receiver), params, None)
        } else {
            let params = arg_types
                .into_iter()
                .map(|ty| CallParam {
                    mode: crate::core::tree::ParamMode::In,
                    ty,
                })
                .collect::<Vec<_>>();
            (None, None, params, None)
        };
        builder.record_call_sig(
            obligation.call_node,
            CallSig {
                def_id,
                receiver,
                params,
            },
        );
        if let Some(inst) = generic_inst {
            // Intrinsic calls are lowered by compiler intent and do not require
            // monomorphized function clones, even if their declarations are generic.
            let skip_monomorph = def_id
                .and_then(|id| engine.context().def_table.lookup_def(id))
                .is_some_and(|def| def.is_intrinsic());
            if !skip_monomorph {
                builder.record_generic_inst(obligation.call_node, inst);
            }
        }
    }

    // Property dot-syntax (`obj.prop`, `obj.prop = v`) is represented as
    // struct-field obligations and rewritten by normalize when a call signature
    // entry exists for the field node id. Materialize those entries here.
    record_property_access_call_sigs(engine, &mut builder);

    let (mut type_map, call_sigs, generic_insts) = builder.finish();
    nominal_keys.hydrate(&mut type_map);
    FinalizeOutput {
        type_map,
        call_sigs,
        generic_insts,
    }
}

fn resolved_node_type_or_unknown(engine: &TypecheckEngine, node_id: NodeId) -> Type {
    engine
        .state()
        .solve
        .resolved_node_types
        .get(&node_id)
        .cloned()
        .unwrap_or(Type::Unknown)
}

fn record_node_type_with_nominal(
    builder: &mut TypeMapBuilder,
    node_id: NodeId,
    ty: Type,
    nominal_keys: &NominalKeyResolver,
) {
    let nominal = nominal_keys.infer(&ty);
    builder.record_node_type_with_nominal(node_id, ty, nominal);
}

fn record_property_access_call_sigs(engine: &TypecheckEngine, builder: &mut TypeMapBuilder) {
    for obligation in &engine.state().constrain.expr_obligations {
        match obligation {
            ExprObligation::StructField {
                expr_id,
                target,
                field,
                ..
            } => {
                let target_ty = resolve_term(target, engine);
                let owner_ty = target_ty.clone().peel_heap();
                let Some(prop) = resolve_property_access_for_finalize(
                    &owner_ty,
                    field,
                    &engine.env().property_sigs,
                    &engine.env().trait_sigs,
                    &engine.state().constrain.var_trait_bounds,
                ) else {
                    continue;
                };
                if !prop.readable {
                    continue;
                }
                builder.record_call_sig(
                    *expr_id,
                    CallSig {
                        def_id: prop.getter_def,
                        receiver: Some(CallParam {
                            mode: crate::core::tree::ParamMode::In,
                            ty: target_ty,
                        }),
                        params: Vec::new(),
                    },
                );
            }
            ExprObligation::StructFieldAssign {
                assignee_expr_id,
                target,
                field,
                value,
                ..
            } => {
                let target_ty = resolve_term(target, engine);
                let owner_ty = target_ty.clone().peel_heap();
                let Some(prop) = resolve_property_access_for_finalize(
                    &owner_ty,
                    field,
                    &engine.env().property_sigs,
                    &engine.env().trait_sigs,
                    &engine.state().constrain.var_trait_bounds,
                ) else {
                    continue;
                };
                if !prop.writable {
                    continue;
                }
                builder.record_call_sig(
                    *assignee_expr_id,
                    CallSig {
                        def_id: prop.setter_def,
                        receiver: Some(CallParam {
                            mode: crate::core::tree::ParamMode::InOut,
                            ty: target_ty,
                        }),
                        params: vec![CallParam {
                            mode: crate::core::tree::ParamMode::In,
                            ty: resolve_term(value, engine),
                        }],
                    },
                );
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone)]
struct ResolvedPropertyAccess {
    readable: bool,
    writable: bool,
    getter_def: Option<DefId>,
    setter_def: Option<DefId>,
}

fn resolve_property_access_for_finalize(
    owner_ty: &Type,
    field: &str,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
    trait_sigs: &HashMap<String, CollectedTraitSig>,
    var_trait_bounds: &HashMap<TyVarId, Vec<String>>,
) -> Option<ResolvedPropertyAccess> {
    let prop =
        property_access::lookup(owner_ty, field, property_sigs, trait_sigs, var_trait_bounds)
            .ok()?;
    Some(ResolvedPropertyAccess {
        readable: prop.has_get,
        writable: prop.has_set,
        getter_def: prop.getter_def,
        setter_def: prop.setter_def,
    })
}

fn resolve_term(ty: &Type, engine: &TypecheckEngine) -> Type {
    engine.type_vars().apply(ty)
}

fn is_unresolved_type(ty: &Type) -> bool {
    matches!(ty, Type::Unknown | Type::Var(_))
}

fn collect_callable_def_types(engine: &TypecheckEngine) -> std::collections::HashMap<DefId, Type> {
    let mut out = HashMap::new();
    for overloads in engine.env().func_sigs.values() {
        for sig in overloads {
            out.insert(sig.def_id, callable_sig_to_fn_type(sig));
        }
    }
    for by_name in engine.env().method_sigs.values() {
        for overloads in by_name.values() {
            for sig in overloads {
                out.insert(sig.def_id, callable_sig_to_fn_type(sig));
            }
        }
    }
    out
}

fn collect_callable_type_param_names(
    engine: &TypecheckEngine,
) -> std::collections::HashMap<DefId, std::collections::BTreeMap<u32, String>> {
    let mut out = HashMap::new();
    for overloads in engine.env().func_sigs.values() {
        for sig in overloads {
            if !sig.type_param_var_names.is_empty() {
                out.insert(sig.def_id, sig.type_param_var_names.clone());
            }
        }
    }
    for by_name in engine.env().method_sigs.values() {
        for overloads in by_name.values() {
            for sig in overloads {
                if !sig.type_param_var_names.is_empty() {
                    out.insert(sig.def_id, sig.type_param_var_names.clone());
                }
            }
        }
    }
    out
}

fn callable_sig_to_fn_type(sig: &CollectedCallableSig) -> Type {
    let params = sig
        .params
        .iter()
        .map(|param| FnParam {
            mode: fn_param_mode(param.mode.clone()),
            ty: param.ty.clone(),
        })
        .collect::<Vec<_>>();
    Type::Fn {
        params,
        ret_ty: Box::new(sig.ret_ty.clone()),
    }
}

struct ResolvedCall {
    def_id: DefId,
    receiver: Option<CallParam>,
    params: Vec<CallParam>,
    inst: Option<GenericInst>,
}

fn resolve_named_call(
    engine: &TypecheckEngine,
    name: &str,
    arg_types: &[Type],
    span: crate::core::diag::Span,
    expected_ret: &Type,
) -> Option<ResolvedCall> {
    let overloads = engine.env().func_sigs.get(name)?;
    let sig = pick_overload(overloads, arg_types.len())?;
    Some(instantiate_call_sig(
        sig,
        arg_types,
        None,
        span,
        expected_ret,
    ))
}

fn resolve_named_call_by_def_id(
    engine: &TypecheckEngine,
    name: &str,
    def_id: DefId,
    arg_types: &[Type],
    span: crate::core::diag::Span,
    expected_ret: &Type,
) -> Option<ResolvedCall> {
    let overloads = engine.env().func_sigs.get(name)?;
    let sig = overloads
        .iter()
        .find(|sig| sig.def_id == def_id && sig.params.len() == arg_types.len())?;
    Some(instantiate_call_sig(
        sig,
        arg_types,
        None,
        span,
        expected_ret,
    ))
}

fn resolve_method_call(
    engine: &TypecheckEngine,
    receiver: Option<&Type>,
    method_name: &str,
    arg_types: &[Type],
    span: crate::core::diag::Span,
    expected_ret: &Type,
) -> Option<ResolvedCall> {
    let receiver_ty = receiver.map(|term| resolve_term(term, engine))?;
    let owner = match &receiver_ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        Type::String => "string".to_string(),
        _ => return None,
    };
    let by_name = engine.env().method_sigs.get(&owner)?;
    let overloads = by_name.get(method_name)?;
    let sig = pick_overload(overloads, arg_types.len())?;
    let receiver = Some(CallParam {
        mode: sig
            .self_mode
            .clone()
            .unwrap_or(crate::core::tree::ParamMode::In),
        ty: receiver_ty,
    });
    Some(instantiate_call_sig(
        sig,
        arg_types,
        receiver,
        span,
        expected_ret,
    ))
}

fn resolve_builtin_method_call(
    engine: &TypecheckEngine,
    receiver: Option<&Type>,
    method_name: &str,
) -> Option<(CallParam, Vec<CallParam>)> {
    let receiver_ty = receiver.map(|term| resolve_term(term, engine))?;
    let builtin = builtin_methods::resolve_builtin_method(&receiver_ty, method_name)?;
    let receiver_mode = builtin.receiver_mode();
    let params = builtin
        .params()
        .into_iter()
        .map(|param| CallParam {
            mode: param.mode,
            ty: param.ty,
        })
        .collect();

    Some((
        CallParam {
            mode: receiver_mode,
            ty: receiver_ty.clone(),
        },
        params,
    ))
}

fn resolve_method_call_by_def_id(
    engine: &TypecheckEngine,
    receiver: Option<&Type>,
    method_name: &str,
    def_id: DefId,
    arg_types: &[Type],
    span: crate::core::diag::Span,
    expected_ret: &Type,
) -> Option<ResolvedCall> {
    let receiver_ty = receiver.map(|term| resolve_term(term, engine))?;
    let owner = match &receiver_ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        Type::String => "string".to_string(),
        _ => return None,
    };
    let by_name = engine.env().method_sigs.get(&owner)?;
    let overloads = by_name.get(method_name)?;
    let sig = overloads
        .iter()
        .find(|sig| sig.def_id == def_id && sig.params.len() == arg_types.len())?;
    let receiver = Some(CallParam {
        mode: sig
            .self_mode
            .clone()
            .unwrap_or(crate::core::tree::ParamMode::In),
        ty: receiver_ty,
    });
    Some(instantiate_call_sig(
        sig,
        arg_types,
        receiver,
        span,
        expected_ret,
    ))
}

fn pick_overload(
    overloads: &[CollectedCallableSig],
    arity: usize,
) -> Option<&CollectedCallableSig> {
    let mut matches = overloads.iter().filter(|sig| sig.params.len() == arity);
    let first = matches.next()?;
    if matches.next().is_some() {
        // Ambiguous arity-only match: let caller handle as unresolved.
        return None;
    }
    Some(first)
}

fn instantiate_call_sig(
    sig: &CollectedCallableSig,
    arg_types: &[Type],
    receiver: Option<CallParam>,
    span: crate::core::diag::Span,
    expected_ret: &Type,
) -> ResolvedCall {
    if sig.type_param_count == 0 {
        let params = sig
            .params
            .iter()
            .map(|param| CallParam {
                mode: param.mode.clone(),
                ty: param.ty.clone(),
            })
            .collect::<Vec<_>>();
        return ResolvedCall {
            def_id: sig.def_id,
            receiver,
            params,
            inst: None,
        };
    }

    let mut unifier = Unifier::new();
    for (param, arg_ty) in sig.params.iter().zip(arg_types.iter()) {
        let _ = unifier.unify(&param.ty, arg_ty);
    }
    let _ = unifier.unify(&sig.ret_ty, expected_ret);

    let type_args = (0..sig.type_param_count)
        .map(|i| unifier.apply(&Type::Var(TyVarId::new(i as u32))))
        .collect::<Vec<_>>();
    let inst = if type_args.iter().any(|ty| matches!(ty, Type::Var(_))) {
        None
    } else {
        Some(GenericInst {
            def_id: sig.def_id,
            type_args: type_args.clone(),
            call_span: span,
        })
    };

    let params = sig
        .params
        .iter()
        .map(|param| CallParam {
            mode: param.mode.clone(),
            ty: unifier.apply(&param.ty),
        })
        .collect::<Vec<_>>();

    ResolvedCall {
        def_id: sig.def_id,
        receiver,
        params,
        inst,
    }
}

struct PayloadNodeCollector {
    nodes: HashSet<NodeId>,
}

impl PayloadNodeCollector {
    fn collect(module: &Module) -> HashSet<NodeId> {
        let mut collector = Self {
            nodes: HashSet::new(),
        };
        collector.visit_module(module);
        collector.nodes
    }
}

impl Visitor for PayloadNodeCollector {
    fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
        self.nodes.insert(type_expr.id);
        visit::walk_type_expr(self, type_expr);
    }

    fn visit_bind_pattern(&mut self, pattern: &BindPattern) {
        self.nodes.insert(pattern.id);
        visit::walk_bind_pattern(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Binding { id, .. }
            | MatchPattern::TypedBinding { id, .. }
            | MatchPattern::EnumVariant { id, .. } => {
                self.nodes.insert(*id);
            }
            MatchPattern::Wildcard { .. }
            | MatchPattern::BoolLit { .. }
            | MatchPattern::IntLit { .. }
            | MatchPattern::Tuple { .. } => {}
        }
        visit::walk_match_pattern(self, pattern);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        self.nodes.insert(stmt.id);
        visit::walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        self.nodes.insert(expr.id);
        visit::walk_expr(self, expr);
    }
}

fn collect_payload_nodes(module: &Module) -> HashSet<NodeId> {
    PayloadNodeCollector::collect(module)
}

#[cfg(test)]
#[path = "../../tests/typecheck/t_finalize.rs"]
mod tests;
