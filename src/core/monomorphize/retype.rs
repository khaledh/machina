//! Sparse retype pass and type-map merge helpers after monomorphization.

use std::collections::{HashMap, HashSet};

use crate::core::context::{ResolvedContext, TypeCheckedContext};
use crate::core::resolve::{DefId, DefTable, ImportedFacts};
use crate::core::tree::*;
use crate::core::typecheck::TypeCheckError;
use crate::core::typecheck::type_check_with_imported_facts;
use crate::core::typecheck::type_map::{CallSigMap, TypeMap};
use crate::core::types::Type;

use super::MonomorphizePlan;

pub(crate) fn retype_after_monomorphize(
    monomorphized_context: &ResolvedContext,
    first_pass: TypeCheckedContext,
    plan: &MonomorphizePlan,
) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    // Build a sparse module where unchanged function/method bodies become decls.
    let retype_context = build_retype_context(monomorphized_context, &plan.retype_def_ids);

    // Re-run typecheck only across this sparse module.
    let second_pass = type_check_with_imported_facts(retype_context, ImportedFacts::default())?;

    // Merge patch tables over the first pass so unaffected nodes/defs keep
    // their original entries.
    Ok(merge_typecheck_results(
        monomorphized_context,
        first_pass,
        second_pass,
        &plan.call_rewrites,
    ))
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
) -> TypeCheckedContext {
    // Types: patch over first-pass map with second-pass entries.
    let mut merged_type_map = first_pass.type_map.clone();
    merge_type_maps(&mut merged_type_map, &second_pass.type_map);

    // Call signatures: union both maps and then force callsite def rewrites
    // produced during monomorphization.
    let mut merged_call_sigs = first_pass.call_sigs.clone();
    merged_call_sigs.extend(second_pass.call_sigs.clone());
    apply_call_rewrites(&mut merged_call_sigs, call_rewrites);

    // Generic instantiations: keep first pass, fill any second-pass additions,
    // and update def ids to rewritten specialized defs.
    let mut merged_generic_insts = first_pass.generic_insts.clone();
    merged_generic_insts.extend(second_pass.generic_insts.clone());
    for (call_id, def_id) in call_rewrites {
        if let Some(inst) = merged_generic_insts.get_mut(call_id) {
            inst.def_id = *def_id;
        }
    }

    // The tree carries no type payload; keep the monomorphized module.
    monomorphized_context.clone().with_type_map(
        merged_type_map,
        merged_call_sigs,
        merged_generic_insts,
    )
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
        }
    }
}
