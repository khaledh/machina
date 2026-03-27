//! Sparse retype pass and type-map merge helpers after monomorphization.

use std::collections::{HashMap, HashSet};

use crate::core::ast::*;
use crate::core::context::{ResolvedContext, TypeCheckedContext};
use crate::core::resolve::{DefId, DefTable, ImportedFacts};
use crate::core::symbol_id::SelectedCallable;
use crate::core::typecheck::TypeCheckError;
use crate::core::typecheck::type_check_with_imported_facts;
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::typecheck::type_map::{CallSigMap, TypeMap};
use crate::core::types::Type;

use super::MonomorphizePlan;
use super::subst::type_expr_from_type;
use crate::core::plans::ForKernel;

pub(crate) fn retype_after_monomorphize(
    monomorphized_context: &ResolvedContext,
    first_pass: TypeCheckedContext,
    plan: &MonomorphizePlan,
) -> Result<(ResolvedContext, TypeCheckedContext), Vec<TypeCheckError>> {
    let mut rewritten_context = monomorphized_context.clone();
    let mut retype_def_ids = plan.retype_def_ids.clone();

    let second_pass = loop {
        let retype_context = build_retype_context(&rewritten_context, &retype_def_ids);
        let typed = type_check_with_imported_facts(retype_context, ImportedFacts::default())?;
        if rewrite_direct_forwarding_returns(&mut rewritten_context, &typed, &retype_def_ids) {
            retype_def_ids = all_callable_def_ids(&rewritten_context);
            continue;
        }
        break typed;
    };

    // Merge patch tables over the first pass so unaffected nodes/defs keep
    // their original entries.
    let typed = merge_typecheck_results(
        &rewritten_context,
        first_pass,
        second_pass,
        &plan.call_rewrites,
        &plan.for_plan_rewrites,
    );

    Ok((rewritten_context, typed))
}

pub(crate) fn build_retype_context(
    monomorphized_context: &ResolvedContext,
    retype_def_ids: &HashSet<DefId>,
) -> ResolvedContext {
    let mut module = monomorphized_context.module.clone();
    module.top_level_items = module
        .top_level_items
        .into_iter()
        .map(|item| retype_sparse_item(item, retype_def_ids, &monomorphized_context.def_table))
        .collect();

    let mut ctx = monomorphized_context.clone();
    ctx.module = module;
    ctx
}

fn retype_sparse_item(
    item: TopLevelItem,
    retype_def_ids: &HashSet<DefId>,
    def_table: &DefTable,
) -> TopLevelItem {
    match item {
        TopLevelItem::FuncDef(func_def) => {
            if retype_def_ids.contains(&def_table.def_id(func_def.id)) {
                TopLevelItem::FuncDef(func_def)
            } else {
                TopLevelItem::FuncDecl(FuncDecl {
                    id: func_def.id,
                    doc: func_def.doc,
                    attrs: func_def.attrs,
                    sig: func_def.sig,
                    span: func_def.span,
                })
            }
        }
        TopLevelItem::MethodBlock(mut method_block) => {
            method_block.method_items = method_block
                .method_items
                .into_iter()
                .map(|method_item| match method_item {
                    MethodItem::Def(method_def)
                        if !retype_def_ids.contains(&def_table.def_id(method_def.id)) =>
                    {
                        MethodItem::Decl(MethodDecl {
                            id: method_def.id,
                            doc: method_def.doc,
                            attrs: method_def.attrs,
                            sig: method_def.sig,
                            span: method_def.span,
                        })
                    }
                    other => other,
                })
                .collect();
            TopLevelItem::MethodBlock(method_block)
        }
        other => other,
    }
}

fn merge_typecheck_results(
    monomorphized_context: &ResolvedContext,
    first_pass: TypeCheckedContext,
    second_pass: TypeCheckedContext,
    call_rewrites: &HashMap<NodeId, DefId>,
    for_plan_rewrites: &HashMap<NodeId, super::ProtocolForRewrite>,
) -> TypeCheckedContext {
    // Types: patch over first-pass map with second-pass entries.
    let mut merged_type_map = first_pass.type_map.clone();
    merge_type_maps(&mut merged_type_map, &second_pass.type_map);

    // Call signatures: union both maps and then force callsite def rewrites
    // produced during monomorphization.
    let mut merged_call_sigs = first_pass.call_sigs.clone();
    merged_call_sigs.extend(second_pass.call_sigs.clone());
    apply_call_rewrites(&mut merged_call_sigs, call_rewrites);

    // Generic instantiations: keep first pass and fill any second-pass
    // additions. For call nodes that were reretyped in the sparse pass, the
    // sparse result is authoritative: drop any stale first-pass inst request
    // before layering the sparse pass back on top.
    //
    // Important: these instantiations must stay anchored to the original
    // generic template defs, not the specialized clone ids from call_rewrites.
    // Later monomorphization rounds discover nested instantiations from sparse
    // retype, and those requests need to target the generic template again.
    // Rewriting them to clone ids corrupts the next round's substitution basis.
    let mut merged_generic_insts = first_pass.generic_insts.clone();
    for call_id in second_pass.call_sigs.keys() {
        merged_generic_insts.remove(call_id);
    }
    merged_generic_insts.extend(second_pass.generic_insts.clone());

    let mut merged_for_plans = first_pass.for_plans.clone();
    merged_for_plans.extend(second_pass.for_plans.clone());
    apply_for_plan_rewrites(&mut merged_for_plans, for_plan_rewrites);

    // The AST carries no type payload; keep the monomorphized module.
    monomorphized_context.clone().with_type_map(
        merged_type_map,
        first_pass.opaque_bindings.clone(),
        first_pass.exposed_types.clone(),
        merged_call_sigs,
        merged_generic_insts,
        merged_for_plans,
    )
}

fn apply_for_plan_rewrites(
    for_plans: &mut crate::core::plans::ForPlanMap,
    rewrites: &HashMap<NodeId, super::ProtocolForRewrite>,
) {
    for (stmt_id, rewrite) in rewrites {
        let Some(plan) = for_plans.get_mut(stmt_id) else {
            continue;
        };
        let ForKernel::Protocol(kernel) = &mut plan.kernel else {
            continue;
        };
        kernel.iter_method = rewrite.iter_method;
        kernel.next_method = rewrite.next_method;
    }
}

fn merge_type_maps(base: &mut TypeMap, patch: &TypeMap) {
    // The second-pass retype module strips non-specialized function bodies to
    // FuncDecls, so their parameter/local defs and body nodes are never
    // constrained and finalize defaults them to Unknown. Skip these to avoid
    // overwriting correct first-pass types. But if the base doesn't already
    // have an entry, this is a new def/node from specialization — keep it
    // even if Unknown.
    for (def, patch_type_id) in patch.iter_def_type_ids() {
        let ty = patch.type_table().get(patch_type_id).clone();
        if matches!(ty, Type::Unknown) && base.lookup_def_type_id(def).is_some() {
            continue;
        }
        let new_type_id = base.insert_def_type(def.clone(), ty);
        if let Some(nominal) = patch.lookup_nominal_key_for_type_id(patch_type_id).cloned() {
            base.record_nominal_key_for_type_id(new_type_id, nominal);
        }
    }

    for (node_id, patch_type_id) in patch.iter_node_type_ids() {
        let ty = patch.type_table().get(patch_type_id).clone();
        if matches!(ty, Type::Unknown) && base.lookup_node_type_id(node_id).is_some() {
            continue;
        }
        let new_type_id = base.insert_node_type(node_id, ty);
        if let Some(nominal) = patch.lookup_nominal_key_for_type_id(patch_type_id).cloned() {
            base.record_nominal_key_for_type_id(new_type_id, nominal);
        }
    }
}

fn apply_call_rewrites(call_sigs: &mut CallSigMap, call_rewrites: &HashMap<NodeId, DefId>) {
    for (call_id, rewritten_def_id) in call_rewrites {
        if let Some(sig) = call_sigs.get_mut(call_id) {
            sig.def_id = Some(*rewritten_def_id);
            if let Some(SelectedCallable::Local(local_def_id)) = &mut sig.selected {
                *local_def_id = *rewritten_def_id;
            }
        }
    }
}

fn rewrite_direct_forwarding_returns(
    context: &mut ResolvedContext,
    typed: &TypeCheckedContext,
    retype_def_ids: &HashSet<DefId>,
) -> bool {
    let lookup_ctx = context.clone();
    let rewrites = context
        .module
        .top_level_items
        .iter()
        .flat_map(|item| match item {
            TopLevelItem::FuncDef(func_def) => {
                let def_id = lookup_ctx.def_table.def_id(func_def.id);
                if !retype_def_ids.contains(&def_id) {
                    return Vec::new();
                }
                widened_direct_forwarding_return(
                    &lookup_ctx,
                    typed,
                    Some(func_def.sig.name.as_str()),
                    func_def.id,
                    &func_def.sig.ret_ty_expr,
                    &func_def.body,
                )
                .map(|ret_ty| (func_def.id, ret_ty, func_def.sig.ret_ty_expr.span))
                .into_iter()
                .collect::<Vec<_>>()
            }
            TopLevelItem::MethodBlock(method_block) => method_block
                .method_items
                .iter()
                .filter_map(|method_item| {
                    let MethodItem::Def(method_def) = method_item else {
                        return None;
                    };
                    let def_id = lookup_ctx.def_table.def_id(method_def.id);
                    if !retype_def_ids.contains(&def_id) {
                        return None;
                    }
                    widened_direct_forwarding_return(
                        &lookup_ctx,
                        typed,
                        Some(method_def.sig.name.as_str()),
                        method_def.id,
                        &method_def.sig.ret_ty_expr,
                        &method_def.body,
                    )
                    .map(|ret_ty| (method_def.id, ret_ty, method_def.sig.ret_ty_expr.span))
                })
                .collect::<Vec<_>>(),
            _ => Vec::new(),
        })
        .collect::<Vec<_>>();

    if rewrites.is_empty() {
        return false;
    }

    let mut rewritten = HashMap::new();
    for (node_id, ret_ty, span) in rewrites {
        let Ok(ret_ty_expr) = type_expr_from_type(
            &ret_ty,
            &lookup_ctx.def_table,
            &lookup_ctx,
            &mut context.node_id_gen,
            span,
        ) else {
            continue;
        };
        rewritten.insert(node_id, ret_ty_expr);
    }

    let mut changed = false;
    for item in &mut context.module.top_level_items {
        match item {
            TopLevelItem::FuncDef(func_def) => {
                if let Some(ret_ty_expr) = rewritten.remove(&func_def.id) {
                    if ret_ty_expr != func_def.sig.ret_ty_expr {
                        func_def.sig.ret_ty_expr = ret_ty_expr;
                        changed = true;
                    }
                }
            }
            TopLevelItem::MethodBlock(method_block) => {
                for method_item in &mut method_block.method_items {
                    let MethodItem::Def(method_def) = method_item else {
                        continue;
                    };
                    if let Some(ret_ty_expr) = rewritten.remove(&method_def.id) {
                        if ret_ty_expr != method_def.sig.ret_ty_expr {
                            method_def.sig.ret_ty_expr = ret_ty_expr;
                            changed = true;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    changed
}

fn widened_direct_forwarding_return(
    context: &ResolvedContext,
    typed: &TypeCheckedContext,
    _callable_name: Option<&str>,
    callable_node_id: NodeId,
    ret_ty_expr: &TypeExpr,
    body: &Expr,
) -> Option<Type> {
    let match_exprs = direct_forwarding_match_exprs(body);
    if match_exprs.is_empty() {
        return None;
    }
    let declared_ret_ty = typed
        .type_map
        .lookup_node_type(callable_node_id)
        .filter(|ty| !matches!(ty, Type::Unknown))
        .or_else(|| resolve_type_expr(&context.def_table, context, ret_ty_expr).ok())?;

    let mut join_terms = vec![declared_ret_ty.clone()];
    for match_expr in match_exprs {
        let ExprKind::Match { scrutinee, arms } = &match_expr.kind else {
            unreachable!("direct_forwarding_match_exprs only returns match expressions");
        };
        let forwarding_arm = arms.last()?;
        if !is_direct_forwarding_arm(forwarding_arm, &context.def_table) {
            continue;
        }

        let Some(scrutinee_ty) = typed.type_map.lookup_node_type(scrutinee.id) else {
            continue;
        };
        let matched = arms[..arms.len().saturating_sub(1)]
            .iter()
            .map(|arm| {
                if arm.patterns.len() != 1 {
                    return None;
                }
                let MatchPattern::TypedBinding { ty_expr, .. } = &arm.patterns[0] else {
                    return None;
                };
                typed
                    .type_map
                    .lookup_node_type(ty_expr.id)
                    .filter(|ty| !matches!(ty, Type::Unknown))
                    .or_else(|| resolve_type_expr(&context.def_table, context, ty_expr).ok())
            })
            .collect::<Option<Vec<_>>>()?;
        let Some(remainder) = scrutinee_ty.error_union_remainder_excluding(&matched) else {
            continue;
        };
        join_terms.push(remainder);
    }

    if join_terms.len() == 1 {
        return None;
    }
    let widened = infer_join_type_from_arms(&join_terms);
    if widened.as_ref() == Some(&declared_ret_ty) {
        return None;
    }
    widened
}

fn direct_forwarding_match_exprs<'a>(expr: &'a Expr) -> Vec<&'a Expr> {
    match &expr.kind {
        ExprKind::Match { .. } => vec![expr],
        ExprKind::Block {
            tail: Some(tail), ..
        } => direct_forwarding_match_exprs(tail),
        ExprKind::If {
            then_body,
            else_body,
            ..
        } => {
            let mut out = direct_forwarding_match_exprs(then_body);
            out.extend(direct_forwarding_match_exprs(else_body));
            out
        }
        _ => Vec::new(),
    }
}

fn is_direct_forwarding_arm(arm: &MatchArm, def_table: &DefTable) -> bool {
    if arm.patterns.len() != 1 {
        return false;
    }
    let MatchPattern::Binding { id, ident, .. } = &arm.patterns[0] else {
        return false;
    };
    let ExprKind::Var { ident: body_ident } = &arm.body.kind else {
        return false;
    };
    if ident != body_ident {
        return false;
    }
    match (
        def_table.lookup_node_def_id(*id),
        def_table.lookup_node_def_id(arm.body.id),
    ) {
        (Some(pattern_def), Some(body_def)) => pattern_def == body_def,
        _ => true,
    }
}

fn all_callable_def_ids(context: &ResolvedContext) -> HashSet<DefId> {
    let mut out = HashSet::new();
    for func_def in context.module.func_defs() {
        out.insert(context.def_table.def_id(func_def.id));
    }
    for method_block in context.module.method_blocks() {
        for method_item in &method_block.method_items {
            if let MethodItem::Def(method_def) = method_item {
                out.insert(context.def_table.def_id(method_def.id));
            }
        }
    }
    out
}

fn infer_join_type_from_arms(arms: &[Type]) -> Option<Type> {
    let mut variants = Vec::new();
    for arm_ty in arms {
        collect_join_variants(arm_ty, &mut variants);
    }
    variants.dedup();
    if variants.is_empty() {
        return None;
    }
    if variants.len() == 1 {
        return variants.into_iter().next();
    }
    let ok_ty = variants[0].clone();
    let err_tys = variants.into_iter().skip(1).collect::<Vec<_>>();
    Some(Type::ErrorUnion {
        ok_ty: Box::new(ok_ty),
        err_tys,
    })
}

fn collect_join_variants(ty: &Type, out: &mut Vec<Type>) {
    match ty {
        Type::ErrorUnion { ok_ty, err_tys } => {
            collect_join_variants(ok_ty, out);
            for err_ty in err_tys {
                collect_join_variants(err_ty, out);
            }
        }
        _ if !out.iter().any(|existing| existing == ty) => out.push(ty.clone()),
        _ => {}
    }
}
