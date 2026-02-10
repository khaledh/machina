//! Pass 5 of the type checker: finalize side tables.
//!
//! This pass materializes concrete `TypeMap`/`CallSigMap`/`GenericInstMap`
//! outputs from solver results and collected obligations, then builds the
//! typed tree context used by downstream compiler stages.

use std::collections::{HashMap, HashSet};

use crate::context::TypeCheckedContext;
use crate::resolve::DefId;
use crate::tree::NodeId;
use crate::tree::map::TreeMapper;
use crate::tree::resolved as res;
use crate::tree::typed::build_module;
use crate::typecheck::Unifier;
use crate::typecheck::builtin_methods;
use crate::typecheck::constraints::{CallCallee, ExprObligation};
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::engine::{CollectedCallableSig, CollectedPropertySig, CollectedTraitSig};
use crate::typecheck::errors::TypeCheckError;
use crate::typecheck::nominal::NominalKey;
use crate::typecheck::property_access;
use crate::typecheck::type_map::{
    CallParam, CallSig, CallSigMap, GenericInst, GenericInstMap, TypeMap, TypeMapBuilder,
    resolve_type_def_with_args, resolve_type_expr,
};
use crate::typecheck::utils::{fn_param_mode, nominal_key_concreteness};
use crate::types::{FnParam, TyVarId, Type};

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

pub(crate) fn materialize(
    engine: TypecheckEngine,
) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    let finalized = engine
        .state()
        .finalize
        .clone()
        .expect("finalize output must be populated before materialize");
    let typed_module = build_module(&finalized.type_map, &engine.context().module);
    Ok(engine.context().clone().with_type_map(
        finalized.type_map,
        finalized.call_sigs,
        finalized.generic_insts,
        typed_module,
    ))
}

fn build_outputs(engine: &TypecheckEngine) -> FinalizeOutput {
    let mut builder = TypeMapBuilder::new();
    let nominal_keys = NominalKeyResolver::new(engine.context());

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
            if let res::MethodItem::Def(method_def) = method_item {
                let ty = resolved_node_type_or_unknown(engine, method_def.id);
                record_node_type_with_nominal(&mut builder, method_def.id, ty, &nominal_keys);
            }
        }
    }

    let callable_types = collect_callable_def_types(engine);

    // Fill definition-side types, with callable fallback for unresolved defs.
    for def in engine.context().def_table.clone() {
        let mut ty = engine
            .state()
            .solve
            .resolved_def_types
            .get(&def.id)
            .cloned()
            .unwrap_or(Type::Unknown);
        if is_unresolved_type(&ty)
            && let Some(callable_ty) = callable_types.get(&def.id)
        {
            ty = callable_ty.clone();
        }
        let nominal = nominal_keys.infer(&ty);
        builder.record_def_type_with_nominal(def, ty, nominal);
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
                    mode: crate::tree::ParamMode::In,
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
            builder.record_generic_inst(obligation.call_node, inst);
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
                            mode: crate::tree::ParamMode::In,
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
                            mode: crate::tree::ParamMode::InOut,
                            ty: target_ty,
                        }),
                        params: vec![CallParam {
                            mode: crate::tree::ParamMode::In,
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

#[derive(Debug, Clone)]
struct NominalTemplate {
    def_id: DefId,
    param_count: usize,
    ty: Type,
}

#[derive(Debug, Clone)]
struct NominalKeyResolver {
    explicit_nominal_keys: HashMap<String, NominalKey>,
    nominal_templates: Vec<NominalTemplate>,
}

impl NominalKeyResolver {
    fn new(resolved: &crate::context::ResolvedContext) -> Self {
        Self {
            explicit_nominal_keys: collect_explicit_nominal_keys(resolved),
            nominal_templates: collect_nominal_templates(resolved),
        }
    }

    fn infer(&self, ty: &Type) -> Option<NominalKey> {
        let nominal_name = match ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => name.as_str(),
            _ => return None,
        };

        if let Some(key) = self.explicit_nominal_keys.get(nominal_name) {
            return Some(key.clone());
        }

        infer_nominal_key_from_templates(ty, &self.nominal_templates)
    }

    fn hydrate(&self, type_map: &mut TypeMap) {
        let entries = type_map
            .type_table()
            .entries()
            .map(|(id, ty)| (id, ty.clone()))
            .collect::<Vec<_>>();

        for (type_id, ty) in entries {
            if type_map.lookup_nominal_key_for_type_id(type_id).is_some() {
                continue;
            }
            let Some(key) = self.infer(&ty) else {
                continue;
            };
            type_map.record_nominal_key_for_type_id(type_id, key);
        }
    }
}

fn infer_type_args_from_instance(
    template: &Type,
    concrete: &Type,
    param_count: usize,
) -> Option<Vec<Type>> {
    let mut bindings: HashMap<TyVarId, Type> = HashMap::new();
    if !match_template_type(template, concrete, &mut bindings) {
        return None;
    }

    let mut args = Vec::with_capacity(param_count);
    for index in 0..param_count {
        let var = TyVarId::new(index as u32);
        let arg = bindings.get(&var)?.clone();
        args.push(arg);
    }
    Some(args)
}

fn collect_nominal_templates(resolved: &crate::context::ResolvedContext) -> Vec<NominalTemplate> {
    let mut templates = Vec::new();
    for type_def in resolved.module.type_defs() {
        if !matches!(
            type_def.kind,
            res::TypeDefKind::Struct { .. } | res::TypeDefKind::Enum { .. }
        ) {
            continue;
        }
        let param_count = type_def.type_params.len();
        let type_args = (0..param_count)
            .map(|i| Type::Var(TyVarId::new(i as u32)))
            .collect::<Vec<_>>();
        let Ok(ty) = resolve_type_def_with_args(
            &resolved.def_table,
            &resolved.module,
            type_def.def_id,
            &type_args,
        ) else {
            continue;
        };
        if !matches!(ty, Type::Struct { .. } | Type::Enum { .. }) {
            continue;
        }
        templates.push(NominalTemplate {
            def_id: type_def.def_id,
            param_count,
            ty,
        });
    }
    templates
}

fn infer_nominal_key_from_templates(
    ty: &Type,
    nominal_templates: &[NominalTemplate],
) -> Option<NominalKey> {
    let mut candidates = Vec::new();
    for template in nominal_templates {
        let Some(type_args) = infer_type_args_from_instance(&template.ty, ty, template.param_count)
        else {
            continue;
        };
        candidates.push(NominalKey::new(template.def_id, type_args));
    }

    if candidates.is_empty() {
        return None;
    }

    candidates.sort_by_key(nominal_key_concreteness);
    let best = candidates.pop().expect("checked non-empty");
    if candidates
        .iter()
        .any(|other| nominal_key_concreteness(other) == nominal_key_concreteness(&best))
    {
        // Ambiguous structural match across multiple nominal defs.
        return None;
    }
    Some(best)
}

fn match_template_type(
    template: &Type,
    concrete: &Type,
    bindings: &mut HashMap<TyVarId, Type>,
) -> bool {
    match (template, concrete) {
        (Type::Var(var), concrete) => match bindings.get(var) {
            Some(bound) => bound == concrete,
            None => {
                bindings.insert(*var, concrete.clone());
                true
            }
        },
        (Type::Unknown, Type::Unknown) => true,
        (Type::Unit, Type::Unit) => true,
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
        | (Type::Set { elem_ty: l }, Type::Set { elem_ty: r })
        | (Type::Heap { elem_ty: l }, Type::Heap { elem_ty: r }) => {
            match_template_type(l, r, bindings)
        }
        (
            Type::Map {
                key_ty: l_key,
                value_ty: l_value,
            },
            Type::Map {
                key_ty: r_key,
                value_ty: r_value,
            },
        ) => {
            match_template_type(l_key, r_key, bindings)
                && match_template_type(l_value, r_value, bindings)
        }
        (
            Type::Ref {
                mutable: l_mut,
                elem_ty: l_elem,
            },
            Type::Ref {
                mutable: r_mut,
                elem_ty: r_elem,
            },
        ) => *l_mut == *r_mut && match_template_type(l_elem, r_elem, bindings),
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
            if l_params.len() != r_params.len() {
                return false;
            }
            if l_params
                .iter()
                .zip(r_params.iter())
                .any(|(l, r)| l.mode != r.mode || !match_template_type(&l.ty, &r.ty, bindings))
            {
                return false;
            }
            match_template_type(l_ret, r_ret, bindings)
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
        ) => l_dims == r_dims && match_template_type(l_elem, r_elem, bindings),
        (Type::Tuple { field_tys: l }, Type::Tuple { field_tys: r }) => {
            l.len() == r.len()
                && l.iter()
                    .zip(r.iter())
                    .all(|(lt, rt)| match_template_type(lt, rt, bindings))
        }
        (
            Type::Struct {
                name: _l_name,
                fields: l_fields,
            },
            Type::Struct {
                name: _r_name,
                fields: r_fields,
            },
        ) => {
            l_fields.len() == r_fields.len()
                && l_fields.iter().zip(r_fields.iter()).all(|(lf, rf)| {
                    lf.name == rf.name && match_template_type(&lf.ty, &rf.ty, bindings)
                })
        }
        (
            Type::Enum {
                name: _l_name,
                variants: l_variants,
            },
            Type::Enum {
                name: _r_name,
                variants: r_variants,
            },
        ) => {
            l_variants.len() == r_variants.len()
                && l_variants.iter().zip(r_variants.iter()).all(|(lv, rv)| {
                    lv.name == rv.name
                        && lv.payload.len() == rv.payload.len()
                        && lv
                            .payload
                            .iter()
                            .zip(rv.payload.iter())
                            .all(|(lt, rt)| match_template_type(lt, rt, bindings))
                })
        }
        _ => false,
    }
}

#[derive(Debug, Clone)]
struct ExplicitNominalUse {
    def_id: DefId,
    type_args: Vec<res::TypeExpr>,
}

struct ExplicitNominalCollector<'a> {
    def_table: &'a crate::resolve::DefTable,
    uses: Vec<ExplicitNominalUse>,
}

impl<'a> ExplicitNominalCollector<'a> {
    fn collect(
        def_table: &'a crate::resolve::DefTable,
        module: &res::Module,
    ) -> Vec<ExplicitNominalUse> {
        let mut collector = Self {
            def_table,
            uses: Vec::new(),
        };
        let mut ctx = ();
        let _ = collector.map_module(module, &mut ctx);
        collector.uses
    }

    fn push_use(&mut self, def_id: DefId, type_args: &[res::TypeExpr]) {
        self.uses.push(ExplicitNominalUse {
            def_id,
            type_args: type_args.to_vec(),
        });
    }
}

impl TreeMapper for ExplicitNominalCollector<'_> {
    type Context = ();
    type InD = DefId;
    type InT = ();
    type OutD = DefId;
    type OutT = ();

    fn map_def_id(
        &mut self,
        _node_id: NodeId,
        def_id: &Self::InD,
        _ctx: &mut Self::Context,
    ) -> Self::OutD {
        *def_id
    }

    fn map_type_payload(
        &mut self,
        _node_id: NodeId,
        _payload: &Self::InT,
        _ctx: &mut Self::Context,
    ) -> Self::OutT {
    }

    fn map_type_expr(
        &mut self,
        type_expr: &res::TypeExpr,
        ctx: &mut Self::Context,
    ) -> res::TypeExpr {
        if let res::TypeExprKind::Named {
            def_id, type_args, ..
        } = &type_expr.kind
        {
            self.push_use(*def_id, type_args);
        }
        crate::tree::map::walk_type_expr(self, type_expr, ctx)
    }

    fn map_expr_kind(
        &mut self,
        expr_id: NodeId,
        expr: &res::ExprKind,
        ctx: &mut Self::Context,
    ) -> res::ExprKind {
        match expr {
            res::ExprKind::StructLit { type_args, .. }
            | res::ExprKind::EnumVariant { type_args, .. } => {
                if let Some(def_id) = self.def_table.lookup_node_def_id(expr_id) {
                    self.push_use(def_id, type_args);
                }
            }
            _ => {}
        }
        crate::tree::map::walk_expr_kind(self, expr_id, expr, ctx)
    }

    fn map_match_pattern(
        &mut self,
        pattern: &res::MatchPattern,
        ctx: &mut Self::Context,
    ) -> res::MatchPattern {
        if let res::MatchPattern::EnumVariant {
            id,
            enum_name: Some(_),
            type_args,
            ..
        } = pattern
            && let Some(def_id) = self.def_table.lookup_node_def_id(*id)
        {
            self.push_use(def_id, type_args);
        }
        crate::tree::map::walk_match_pattern(self, pattern, ctx)
    }
}

fn collect_explicit_nominal_keys(
    resolved: &crate::context::ResolvedContext,
) -> HashMap<String, NominalKey> {
    let mut out = HashMap::new();
    let uses = ExplicitNominalCollector::collect(&resolved.def_table, &resolved.module);

    for usage in uses {
        let Some(type_def) = resolved.module.type_def_by_id(usage.def_id) else {
            continue;
        };
        let is_nominal = matches!(
            type_def.kind,
            res::TypeDefKind::Struct { .. } | res::TypeDefKind::Enum { .. }
        );
        if !is_nominal {
            continue;
        }

        let resolved_args = if usage.type_args.is_empty() {
            if type_def.type_params.is_empty() {
                Vec::new()
            } else {
                // Omitted type args for generic nominals are solved implicitly
                // from expression context and are not reconstructible from
                // syntax alone here.
                continue;
            }
        } else {
            let mut args = Vec::with_capacity(usage.type_args.len());
            let mut ok = true;
            for arg in &usage.type_args {
                match resolve_type_expr(&resolved.def_table, &resolved.module, arg) {
                    Ok(ty) => args.push(ty),
                    Err(_) => {
                        ok = false;
                        break;
                    }
                }
            }
            if !ok || args.len() != type_def.type_params.len() {
                continue;
            }
            args
        };

        let Ok(inst_ty) = resolve_type_def_with_args(
            &resolved.def_table,
            &resolved.module,
            usage.def_id,
            &resolved_args,
        ) else {
            continue;
        };
        let inst_name = match inst_ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => name,
            _ => continue,
        };
        let key = NominalKey::new(usage.def_id, resolved_args);
        if let Some(existing) = out.get(&inst_name) {
            debug_assert_eq!(existing, &key);
            continue;
        }
        out.insert(inst_name, key);
    }

    out
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
    span: crate::diag::Span,
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
    span: crate::diag::Span,
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
    span: crate::diag::Span,
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
        mode: sig.self_mode.clone().unwrap_or(crate::tree::ParamMode::In),
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
    span: crate::diag::Span,
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
        mode: sig.self_mode.clone().unwrap_or(crate::tree::ParamMode::In),
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

fn pick_overload<'a>(
    overloads: &'a [CollectedCallableSig],
    arity: usize,
) -> Option<&'a CollectedCallableSig> {
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
    span: crate::diag::Span,
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
    fn collect(module: &res::Module) -> HashSet<NodeId> {
        let mut collector = Self {
            nodes: HashSet::new(),
        };
        let mut ctx = ();
        let _mapped = collector.map_module(module, &mut ctx);
        collector.nodes
    }
}

impl TreeMapper for PayloadNodeCollector {
    type Context = ();
    type InD = DefId;
    type InT = ();
    type OutD = DefId;
    type OutT = ();

    fn map_def_id(
        &mut self,
        _node_id: NodeId,
        def_id: &Self::InD,
        _ctx: &mut Self::Context,
    ) -> Self::OutD {
        *def_id
    }

    fn map_type_payload(
        &mut self,
        node_id: NodeId,
        _payload: &Self::InT,
        _ctx: &mut Self::Context,
    ) -> Self::OutT {
        self.nodes.insert(node_id);
    }
}

fn collect_payload_nodes(module: &res::Module) -> HashSet<NodeId> {
    PayloadNodeCollector::collect(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::ParsedContext;
    use crate::lexer::{LexError, Lexer, Token};
    use crate::parse::Parser;
    use crate::resolve::resolve;
    use crate::typecheck::{collect, constraints, solver, validate};

    fn resolve_source(source: &str) -> crate::context::ResolvedContext {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .expect("Failed to tokenize");
        let mut parser = Parser::new(&tokens);
        let module = parser.parse().expect("Failed to parse");
        let id_gen = parser.into_id_gen();
        let ast_context = ParsedContext::new(module, id_gen);
        resolve(ast_context).expect("Failed to resolve")
    }

    #[test]
    fn test_finalize_materializes_typechecked_context() {
        let source = r#"
            fn test() -> u64 {
                let x = 1;
                x
            }
        "#;

        let resolved = resolve_source(source);
        let mut engine = TypecheckEngine::new(resolved);
        collect::run(&mut engine).expect("collect pass failed");
        constraints::run(&mut engine).expect("constrain pass failed");
        solver::run(&mut engine).expect("solve pass failed");
        validate::run(&mut engine).expect("validate pass failed");
        run(&mut engine).expect("finalize pass failed");

        let checked = materialize(engine).expect("materialize failed");
        let func_body_id = checked.module.func_defs()[0].body.id;
        assert!(checked.type_map.lookup_node_type(func_body_id).is_some());
    }

    #[test]
    fn test_finalize_records_nominal_keys_for_generic_instantiations() {
        let source = r#"
            type Box<T> = { value: T }

            fn main() -> u64 {
                let b: Box<u64> = Box<u64>{ value: 7 };
                b.value
            }
        "#;

        let resolved = resolve_source(source);
        let mut engine = TypecheckEngine::new(resolved);
        collect::run(&mut engine).expect("collect pass failed");
        constraints::run(&mut engine).expect("constrain pass failed");
        solver::run(&mut engine).expect("solve pass failed");
        validate::run(&mut engine).expect("validate pass failed");
        run(&mut engine).expect("finalize pass failed");

        let checked = materialize(engine).expect("materialize failed");
        let box_def_id = checked
            .module
            .type_defs()
            .into_iter()
            .find(|type_def| type_def.name == "Box")
            .map(|type_def| type_def.def_id)
            .expect("missing Box type def");
        let main_body = &checked
            .module
            .func_defs()
            .into_iter()
            .find(|func| func.sig.name == "main")
            .expect("missing main")
            .body;
        let local_b_def_id = match &main_body.kind {
            crate::tree::typed::ExprKind::Block { items, .. } => items
                .iter()
                .find_map(|item| {
                    let crate::tree::typed::BlockItem::Stmt(stmt) = item else {
                        return None;
                    };
                    let crate::tree::typed::StmtExprKind::LetBind { pattern, .. } = &stmt.kind
                    else {
                        return None;
                    };
                    match &pattern.kind {
                        crate::tree::typed::BindPatternKind::Name { ident, def_id }
                            if ident == "b" =>
                        {
                            Some(*def_id)
                        }
                        _ => None,
                    }
                })
                .expect("missing local binding for b"),
            _ => panic!("main body is not a block"),
        };
        let local_b = checked
            .def_table
            .lookup_def(local_b_def_id)
            .expect("missing def for local b");
        let box_ty_id = checked
            .type_map
            .lookup_def_type_id(local_b)
            .expect("missing type for local b");
        let key = checked
            .type_map
            .lookup_nominal_key_for_type_id(box_ty_id)
            .expect("missing nominal key for Box<u64>");
        assert_eq!(key.def_id, box_def_id);
        assert_eq!(key.type_args, vec![Type::uint(64)]);
    }

    #[test]
    fn test_infer_type_args_from_instance_matches_generic_nominal_shape() {
        let template = Type::Struct {
            name: "Box<T0>".to_string(),
            fields: vec![crate::types::StructField {
                name: "value".to_string(),
                ty: Type::Var(TyVarId::new(0)),
            }],
        };
        let concrete = Type::Struct {
            name: "Box<i32>".to_string(),
            fields: vec![crate::types::StructField {
                name: "value".to_string(),
                ty: Type::sint(32),
            }],
        };

        let args = infer_type_args_from_instance(&template, &concrete, 1)
            .expect("expected inferred type args");
        assert_eq!(args, vec![Type::sint(32)]);
    }

    #[test]
    fn test_infer_type_args_from_instance_allows_var_bindings() {
        let template = Type::Enum {
            name: "Option<T0>".to_string(),
            variants: vec![
                crate::types::EnumVariant {
                    name: "None".to_string(),
                    payload: Vec::new(),
                },
                crate::types::EnumVariant {
                    name: "Some".to_string(),
                    payload: vec![Type::Var(TyVarId::new(0))],
                },
            ],
        };
        let concrete = Type::Enum {
            name: "Option<T42>".to_string(),
            variants: vec![
                crate::types::EnumVariant {
                    name: "None".to_string(),
                    payload: Vec::new(),
                },
                crate::types::EnumVariant {
                    name: "Some".to_string(),
                    payload: vec![Type::Var(TyVarId::new(42))],
                },
            ],
        };

        let args = infer_type_args_from_instance(&template, &concrete, 1)
            .expect("expected inferred var type args");
        assert_eq!(args, vec![Type::Var(TyVarId::new(42))]);
    }
}
