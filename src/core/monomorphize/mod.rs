mod reseed;
mod retype;
mod subst;

use std::collections::{HashMap, HashSet};

use crate::core::capsule::ModuleId;
use crate::core::context::{ResolvedContext, TypeCheckedContext};
use crate::core::diag::{Span, SpannedError};
use crate::core::resolve::{DefId, attach_def_owners};
use crate::core::tree::*;
use crate::core::typecheck::TypeCheckError;
use crate::core::typecheck::type_map::{GenericInst, GenericInstMap};
use crate::core::types::Type;
use reseed::{
    collect_node_ids_in_method_item, collect_node_ids_in_top_level_item, register_item_def_id,
    register_method_item_def_id, remap_local_defs_in_item, remap_local_defs_in_method_item,
    replay_node_def_mappings, reseed_ids_in_item, reseed_ids_in_method_item, rewrite_calls_in_item,
    rewrite_calls_in_method_item,
};
use retype::retype_after_monomorphize;
use subst::{
    apply_inst_to_func_decl, apply_inst_to_func_def, apply_inst_to_method_decl,
    apply_inst_to_method_def, def_name,
};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct MonomorphizeStats {
    pub requested_instantiations: usize,
    pub unique_instantiations: usize,
    pub reused_requests: usize,
}

#[derive(Debug, Clone, Default)]
pub struct MonomorphizePlan {
    pub retype_def_ids: HashSet<DefId>,
    pub call_rewrites: HashMap<NodeId, DefId>,
}

#[derive(Debug)]
pub enum MonomorphizePipelineError {
    Monomorphize(MonomorphizeError),
    Retype(Vec<TypeCheckError>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct InstKey {
    def_id: DefId,
    type_args: Vec<Type>,
}

#[derive(Debug, Error)]
pub enum MonomorphizeErrorKind {
    #[error("generic function `{name}` is used with multiple type arguments")]
    MultipleInstantiations { name: String },

    #[error("generic function `{name}` expects {expected} type arguments, got {got}")]
    ArityMismatch {
        name: String,
        expected: usize,
        got: usize,
    },

    #[error("unknown type `{name}`")]
    UnknownType { name: String },

    #[error("unsupported type in monomorphization")]
    UnsupportedType,
}

pub type MonomorphizeError = SpannedError<MonomorphizeErrorKind>;

impl MonomorphizeErrorKind {
    pub fn at(self, span: Span) -> MonomorphizeError {
        MonomorphizeError::new(self, span)
    }
}

pub fn monomorphize(
    resolved_context: ResolvedContext,
    first_pass_typed: TypeCheckedContext,
    top_level_owners: Option<&HashMap<NodeId, ModuleId>>,
) -> Result<(ResolvedContext, TypeCheckedContext, MonomorphizeStats), MonomorphizePipelineError> {
    // Fast-path: no generic call instantiations requested by typecheck.
    if first_pass_typed.generic_insts.is_empty() {
        return Ok((
            resolved_context,
            first_pass_typed,
            MonomorphizeStats::default(),
        ));
    }

    // 1) Clone specialized defs/decls and rewrite callsites to point to clones.
    let (monomorphized_context, stats, plan) =
        monomorphize_with_plan(resolved_context, &first_pass_typed.generic_insts)
            .map_err(MonomorphizePipelineError::Monomorphize)?;

    // 2) Recompute def->owner map for cloned defs in multi-module capsules.
    let monomorphized_context = if let Some(owners) = top_level_owners {
        attach_def_owners(monomorphized_context, owners)
    } else {
        monomorphized_context
    };

    // 3) Re-typecheck only instantiated callables and merge patch tables.
    let typed_context = retype_after_monomorphize(&monomorphized_context, first_pass_typed, &plan)
        .map_err(MonomorphizePipelineError::Retype)?;

    Ok((monomorphized_context, typed_context, stats))
}

#[cfg(test)]
pub(crate) fn monomorphize_resolved(
    ctx: ResolvedContext,
    generic_insts: &GenericInstMap,
) -> Result<ResolvedContext, MonomorphizeError> {
    let (ctx, _stats, _plan) = monomorphize_with_plan(ctx, generic_insts)?;
    Ok(ctx)
}

#[cfg(test)]
pub(crate) fn monomorphize_resolved_with_stats(
    ctx: ResolvedContext,
    generic_insts: &GenericInstMap,
) -> Result<(ResolvedContext, MonomorphizeStats), MonomorphizeError> {
    let (ctx, stats, _plan) = monomorphize_with_plan(ctx, generic_insts)?;
    Ok((ctx, stats))
}

pub(crate) fn monomorphize_with_plan(
    ctx: ResolvedContext,
    generic_insts: &GenericInstMap,
) -> Result<(ResolvedContext, MonomorphizeStats, MonomorphizePlan), MonomorphizeError> {
    let ResolvedContext {
        mut module,
        payload: tables,
    } = ctx;
    let mut def_table = tables.def_table;
    let def_owners = tables.def_owners;
    let mut node_id_gen = tables.node_id_gen;
    let typestate_role_impls = tables.typestate_role_impls;
    let protocol_index = tables.protocol_index;
    let mut stats = MonomorphizeStats {
        requested_instantiations: generic_insts.len(),
        ..MonomorphizeStats::default()
    };

    if generic_insts.is_empty() {
        let symbols = crate::core::symtab::SymbolTable::new(&module, &def_table);
        return Ok((
            ResolvedContext {
                module,
                payload: crate::core::context::ResolvedTables {
                    def_table,
                    def_owners,
                    symbols,
                    node_id_gen,
                    typestate_role_impls,
                    protocol_index: protocol_index.clone(),
                },
            },
            stats,
            MonomorphizePlan::default(),
        ));
    }

    // Deduplicate by (callable def, concrete type args) so repeated callsites
    // of the same instantiation only clone once.
    let mut unique_inst_reqs: HashMap<InstKey, GenericInst> = HashMap::new();
    for inst in generic_insts.values() {
        unique_inst_reqs
            .entry(inst_key_for_inst(inst))
            .or_insert_with(|| inst.clone());
    }
    stats.unique_instantiations = unique_inst_reqs.len();
    stats.reused_requests = stats
        .requested_instantiations
        .saturating_sub(stats.unique_instantiations);

    let mut insts_by_def: HashMap<DefId, Vec<GenericInst>> = HashMap::new();
    for inst in unique_inst_reqs.values() {
        insts_by_def
            .entry(inst.def_id)
            .or_default()
            .push(inst.clone());
    }

    // Reserve new DefIds for every unique instantiation.
    let mut inst_to_def: HashMap<InstKey, DefId> = HashMap::new();
    for (def_id, insts) in insts_by_def.iter() {
        let def = def_table
            .lookup_def(*def_id)
            .ok_or_else(|| {
                MonomorphizeErrorKind::UnknownType {
                    name: def_name(&def_table, *def_id),
                }
                .at(insts.first().map(|inst| inst.call_span).unwrap_or_default())
            })?
            .clone();
        for inst in insts {
            let new_def_id = def_table.add_def(def.name.clone(), def.kind.clone());
            inst_to_def.insert(
                InstKey {
                    def_id: *def_id,
                    type_args: inst.type_args.clone(),
                },
                new_def_id,
            );
        }
    }

    // Record callsite rewrites from original def id -> specialized def id.
    let mut call_inst_map: HashMap<NodeId, DefId> = HashMap::new();
    for (call_id, inst) in generic_insts {
        if let Some(def_id) = inst_to_def.get(&inst_key_for_inst(inst)) {
            call_inst_map.insert(*call_id, *def_id);
        }
    }

    let mut new_items = Vec::with_capacity(module.top_level_items.len());

    for mut item in module.top_level_items.into_iter() {
        match &mut item {
            TopLevelItem::FuncDef(func_def) => {
                if func_def.sig.type_params.is_empty() {
                    rewrite_calls_in_item(&mut item, &call_inst_map);
                    new_items.push(item);
                    continue;
                }

                let func_def_id = def_table.def_id(func_def.id);
                if let Some(insts) = insts_by_def.get(&func_def_id) {
                    for inst in insts {
                        let mut cloned = func_def.clone();
                        let new_def_id = *inst_to_def
                            .get(&InstKey {
                                def_id: func_def_id,
                                type_args: inst.type_args.clone(),
                            })
                            .expect("compiler bug: missing instantiated def id for function");
                        apply_inst_to_func_def(&mut cloned, inst, &def_table, &mut node_id_gen)?;

                        let mut cloned_item = TopLevelItem::FuncDef(cloned);
                        cloned_item = remap_local_defs_in_item(cloned_item, &mut def_table);
                        rewrite_calls_in_item(&mut cloned_item, &call_inst_map);

                        let old_node_ids = collect_node_ids_in_top_level_item(&cloned_item);
                        reseed_ids_in_item(&mut cloned_item, &mut node_id_gen);

                        let new_node_ids = collect_node_ids_in_top_level_item(&cloned_item);
                        replay_node_def_mappings(&mut def_table, &old_node_ids, &new_node_ids);
                        register_item_def_id(&mut def_table, &cloned_item, new_def_id);

                        new_items.push(cloned_item);
                    }
                }
            }
            TopLevelItem::FuncDecl(func_decl) => {
                if func_decl.sig.type_params.is_empty() {
                    rewrite_calls_in_item(&mut item, &call_inst_map);
                    new_items.push(item);
                    continue;
                }

                let func_decl_id = def_table.def_id(func_decl.id);
                if let Some(insts) = insts_by_def.get(&func_decl_id) {
                    for inst in insts {
                        let mut cloned = func_decl.clone();
                        let new_def_id = *inst_to_def
                            .get(&InstKey {
                                def_id: func_decl_id,
                                type_args: inst.type_args.clone(),
                            })
                            .expect("compiler bug: missing instantiated def id for function decl");
                        apply_inst_to_func_decl(&mut cloned, inst, &def_table, &mut node_id_gen)?;

                        let mut cloned_item = TopLevelItem::FuncDecl(cloned);
                        cloned_item = remap_local_defs_in_item(cloned_item, &mut def_table);
                        rewrite_calls_in_item(&mut cloned_item, &call_inst_map);

                        let old_node_ids = collect_node_ids_in_top_level_item(&cloned_item);
                        reseed_ids_in_item(&mut cloned_item, &mut node_id_gen);

                        let new_node_ids = collect_node_ids_in_top_level_item(&cloned_item);
                        replay_node_def_mappings(&mut def_table, &old_node_ids, &new_node_ids);
                        register_item_def_id(&mut def_table, &cloned_item, new_def_id);

                        new_items.push(cloned_item);
                    }
                }
            }
            TopLevelItem::MethodBlock(method_block) => {
                let mut kept_items = Vec::with_capacity(method_block.method_items.len());
                for mut method_item in method_block.method_items.drain(..) {
                    match &mut method_item {
                        MethodItem::Def(method_def) => {
                            if method_def.sig.type_params.is_empty() {
                                rewrite_calls_in_method_item(&mut method_item, &call_inst_map);
                                kept_items.push(method_item);
                                continue;
                            }
                            let method_def_id = def_table.def_id(method_def.id);
                            if let Some(insts) = insts_by_def.get(&method_def_id) {
                                for inst in insts {
                                    let mut cloned = method_def.clone();
                                    let new_def_id = *inst_to_def
                                        .get(&InstKey {
                                            def_id: method_def_id,
                                            type_args: inst.type_args.clone(),
                                        })
                                        .expect(
                                            "compiler bug: missing instantiated def id for method",
                                        );
                                    apply_inst_to_method_def(
                                        &mut cloned,
                                        inst,
                                        &def_table,
                                        &mut node_id_gen,
                                    )?;

                                    let mut cloned_item = MethodItem::Def(cloned);
                                    cloned_item = remap_local_defs_in_method_item(
                                        cloned_item,
                                        &mut def_table,
                                    );
                                    rewrite_calls_in_method_item(&mut cloned_item, &call_inst_map);

                                    let old_node_ids =
                                        collect_node_ids_in_method_item(&cloned_item);
                                    reseed_ids_in_method_item(&mut cloned_item, &mut node_id_gen);

                                    let new_node_ids =
                                        collect_node_ids_in_method_item(&cloned_item);
                                    replay_node_def_mappings(
                                        &mut def_table,
                                        &old_node_ids,
                                        &new_node_ids,
                                    );
                                    register_method_item_def_id(
                                        &mut def_table,
                                        &cloned_item,
                                        new_def_id,
                                    );

                                    kept_items.push(cloned_item);
                                }
                            }
                        }
                        MethodItem::Decl(method_decl) => {
                            if method_decl.sig.type_params.is_empty() {
                                rewrite_calls_in_method_item(&mut method_item, &call_inst_map);
                                kept_items.push(method_item);
                                continue;
                            }
                            let method_decl_id = def_table.def_id(method_decl.id);
                            if let Some(insts) = insts_by_def.get(&method_decl_id) {
                                for inst in insts {
                                    let mut cloned = method_decl.clone();
                                    let new_def_id = *inst_to_def
                                        .get(&InstKey {
                                            def_id: method_decl_id,
                                            type_args: inst.type_args.clone(),
                                        })
                                        .expect(
                                            "compiler bug: missing instantiated def id for method decl",
                                        );
                                    apply_inst_to_method_decl(
                                        &mut cloned,
                                        inst,
                                        &def_table,
                                        &mut node_id_gen,
                                    )?;

                                    let mut cloned_item = MethodItem::Decl(cloned);
                                    cloned_item = remap_local_defs_in_method_item(
                                        cloned_item,
                                        &mut def_table,
                                    );
                                    rewrite_calls_in_method_item(&mut cloned_item, &call_inst_map);

                                    let old_node_ids =
                                        collect_node_ids_in_method_item(&cloned_item);
                                    reseed_ids_in_method_item(&mut cloned_item, &mut node_id_gen);

                                    let new_node_ids =
                                        collect_node_ids_in_method_item(&cloned_item);
                                    replay_node_def_mappings(
                                        &mut def_table,
                                        &old_node_ids,
                                        &new_node_ids,
                                    );
                                    register_method_item_def_id(
                                        &mut def_table,
                                        &cloned_item,
                                        new_def_id,
                                    );

                                    kept_items.push(cloned_item);
                                }
                            }
                        }
                    }
                }

                if !kept_items.is_empty() {
                    method_block.method_items = kept_items;
                    method_block.id = node_id_gen.new_id();
                    rewrite_calls_in_item(&mut item, &call_inst_map);
                    new_items.push(item);
                }
            }
            _ => {
                rewrite_calls_in_item(&mut item, &call_inst_map);
                new_items.push(item);
            }
        }
    }

    module.top_level_items = new_items;
    let symbols = crate::core::symtab::SymbolTable::new(&module, &def_table);
    Ok((
        ResolvedContext {
            module,
            payload: crate::core::context::ResolvedTables {
                def_table,
                def_owners,
                symbols,
                node_id_gen,
                typestate_role_impls,
                protocol_index,
            },
        },
        stats,
        MonomorphizePlan {
            retype_def_ids: inst_to_def.values().copied().collect(),
            call_rewrites: call_inst_map,
        },
    ))
}

fn inst_key_for_inst(inst: &GenericInst) -> InstKey {
    InstKey {
        def_id: inst.def_id,
        type_args: inst.type_args.clone(),
    }
}

/// Kept as a top-level helper for compile-path tests that assert sparse retype shape.
#[allow(dead_code)]
pub(crate) fn build_retype_context(
    monomorphized_context: &ResolvedContext,
    retype_def_ids: &HashSet<DefId>,
) -> ResolvedContext {
    retype::build_retype_context(monomorphized_context, retype_def_ids)
}

#[cfg(test)]
#[path = "../../tests/monomorphize/t_monomorphize.rs"]
mod tests;
