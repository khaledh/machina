mod reseed;
mod retype;
mod subst;

use std::collections::{HashMap, HashSet};

use crate::core::ast::*;
use crate::core::capsule::ModuleId;
use crate::core::context::{ResolvedContext, TypeCheckedContext};
use crate::core::diag::{Span, SpannedError};
use crate::core::plans::{ForKernel, ForPlanMap};
use crate::core::resolve::{DefId, attach_def_owners};
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
    apply_inst_to_func_decl, apply_inst_to_func_def, apply_inst_to_method_block,
    apply_inst_to_method_decl, apply_inst_to_method_def, def_name, method_block_type_param_count,
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
    pub for_plan_rewrites: HashMap<NodeId, ProtocolForRewrite>,
    pub specialized_insts: HashMap<DefId, GenericInst>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProtocolForRewrite {
    pub iter_method: DefId,
    pub next_method: DefId,
}

#[derive(Debug)]
pub enum MonomorphizePipelineError {
    Monomorphize(MonomorphizeError),
    Retype(Vec<TypeCheckError>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct InstKey {
    def_id: DefId,
    type_args: Vec<Type>,
    iterable_param_tys: Vec<Type>,
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
    let (func_templates, method_templates) = collect_generic_callable_templates(&resolved_context);
    let mut current_resolved = resolved_context;
    let mut current_typed = first_pass_typed;
    let mut processed_inst_keys = HashSet::new();
    let mut stats = MonomorphizeStats::default();

    loop {
        if current_typed.generic_insts.is_empty()
            && !for_plans_require_monomorphization(&current_typed.for_plans)
        {
            break;
        }

        let (monomorphized_context, round_stats, plan) = monomorphize_with_plan(
            current_resolved,
            &current_typed.generic_insts,
            &current_typed.for_plans,
            &processed_inst_keys,
            &func_templates,
            &method_templates,
        )
        .map_err(MonomorphizePipelineError::Monomorphize)?;

        if round_stats.unique_instantiations == 0 {
            current_resolved = monomorphized_context;
            break;
        }

        stats.requested_instantiations += round_stats.requested_instantiations;
        processed_inst_keys.extend(plan.specialized_insts.values().map(inst_key_for_inst));
        stats.unique_instantiations = processed_inst_keys.len();
        stats.reused_requests = stats
            .requested_instantiations
            .saturating_sub(stats.unique_instantiations);

        let monomorphized_context = if let Some(owners) = top_level_owners {
            attach_def_owners(monomorphized_context, owners)
        } else {
            monomorphized_context
        };

        let (next_resolved, next_typed) =
            retype_after_monomorphize(&monomorphized_context, current_typed, &plan)
                .map_err(MonomorphizePipelineError::Retype)?;
        current_resolved = next_resolved;
        current_typed = next_typed;
    }

    Ok((current_resolved, current_typed, stats))
}

#[cfg(test)]
pub(crate) fn monomorphize_resolved(
    ctx: ResolvedContext,
    generic_insts: &GenericInstMap,
) -> Result<ResolvedContext, MonomorphizeError> {
    let (func_templates, method_templates) = collect_generic_callable_templates(&ctx);
    let (ctx, _stats, _plan) = monomorphize_with_plan(
        ctx,
        generic_insts,
        &ForPlanMap::default(),
        &HashSet::new(),
        &func_templates,
        &method_templates,
    )?;
    Ok(ctx)
}

#[cfg(test)]
pub(crate) fn monomorphize_resolved_with_stats(
    ctx: ResolvedContext,
    generic_insts: &GenericInstMap,
) -> Result<(ResolvedContext, MonomorphizeStats), MonomorphizeError> {
    let (func_templates, method_templates) = collect_generic_callable_templates(&ctx);
    let (ctx, stats, _plan) = monomorphize_with_plan(
        ctx,
        generic_insts,
        &ForPlanMap::default(),
        &HashSet::new(),
        &func_templates,
        &method_templates,
    )?;
    Ok((ctx, stats))
}

pub(crate) fn monomorphize_with_plan(
    ctx: ResolvedContext,
    generic_insts: &GenericInstMap,
    for_plans: &ForPlanMap,
    already_specialized: &HashSet<InstKey>,
    func_templates: &HashMap<DefId, FuncDef>,
    method_templates: &HashMap<DefId, MethodDef>,
) -> Result<(ResolvedContext, MonomorphizeStats, MonomorphizePlan), MonomorphizeError> {
    let ResolvedContext {
        mut module,
        payload: tables,
    } = ctx;
    let mut def_table = tables.def_table;
    let module_path = tables.module_path;
    let def_owners = tables.def_owners;
    let symbol_ids = tables.symbol_ids;
    let mut node_id_gen = tables.node_id_gen;
    let linear_index = tables.linear_index;
    let protocol_inst_reqs = collect_protocol_for_instantiations(&module, &def_table, for_plans);
    let mut stats = MonomorphizeStats {
        requested_instantiations: generic_insts
            .values()
            .filter(|inst| !already_specialized.contains(&inst_key_for_inst(inst)))
            .count()
            + protocol_inst_reqs
                .iter()
                .filter(|(_, inst)| !already_specialized.contains(&inst_key_for_inst(inst)))
                .count(),
        ..MonomorphizeStats::default()
    };

    if stats.requested_instantiations == 0 {
        let symbols = crate::core::codegen_names::CodegenNameTable::new(&module, &def_table);
        return Ok((
            ResolvedContext {
                module,
                payload: crate::core::context::ResolvedTables {
                    def_table,
                    module_path,
                    def_owners,
                    symbol_ids,
                    symbols,
                    node_id_gen,
                    linear_index: linear_index.clone(),
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
        let key = inst_key_for_inst(inst);
        if already_specialized.contains(&key) {
            continue;
        }
        unique_inst_reqs.entry(key).or_insert_with(|| inst.clone());
    }
    for (_, inst) in &protocol_inst_reqs {
        let key = inst_key_for_inst(inst);
        if already_specialized.contains(&key) {
            continue;
        }
        unique_inst_reqs.entry(key).or_insert_with(|| inst.clone());
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
                    iterable_param_tys: inst.iterable_param_tys.clone(),
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
    let for_plan_rewrites =
        collect_protocol_for_rewrites(&module, &def_table, for_plans, &inst_to_def);

    let mut new_items = Vec::with_capacity(module.top_level_items.len());

    let module_lookup = module.clone();
    let top_level_items = std::mem::take(&mut module.top_level_items);
    for mut item in top_level_items.into_iter() {
        match &mut item {
            TopLevelItem::FuncDef(func_def) => {
                if func_def.sig.type_params.is_empty()
                    && !insts_by_def.contains_key(&def_table.def_id(func_def.id))
                {
                    rewrite_calls_in_item(&mut item, &call_inst_map);
                    new_items.push(item);
                    continue;
                }

                let func_def_id = def_table.def_id(func_def.id);
                if let Some(insts) = insts_by_def.get(&func_def_id) {
                    if !func_def.sig.type_params.is_empty() {
                        new_items.push(TopLevelItem::FuncDecl(FuncDecl {
                            id: func_def.id,
                            attrs: func_def.attrs.clone(),
                            sig: func_def.sig.clone(),
                            span: func_def.span,
                        }));
                    }
                    for inst in insts {
                        let mut cloned = func_def.clone();
                        let new_def_id = *inst_to_def
                            .get(&InstKey {
                                def_id: func_def_id,
                                type_args: inst.type_args.clone(),
                                iterable_param_tys: inst.iterable_param_tys.clone(),
                            })
                            .expect("compiler bug: missing instantiated def id for function");
                        apply_inst_to_func_def(
                            &mut cloned,
                            inst,
                            &def_table,
                            &module_lookup,
                            &mut node_id_gen,
                        )?;

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
                if func_decl.sig.type_params.is_empty()
                    && !insts_by_def.contains_key(&def_table.def_id(func_decl.id))
                {
                    rewrite_calls_in_item(&mut item, &call_inst_map);
                    new_items.push(item);
                    continue;
                }

                let func_decl_id = def_table.def_id(func_decl.id);
                if let Some(insts) = insts_by_def.get(&func_decl_id) {
                    if !func_decl.sig.type_params.is_empty() {
                        new_items.push(TopLevelItem::FuncDecl(func_decl.clone()));
                    }
                    for inst in insts {
                        if let Some(template_def) = func_templates.get(&func_decl_id) {
                            let mut cloned = template_def.clone();
                            let new_def_id = *inst_to_def
                                .get(&InstKey {
                                    def_id: func_decl_id,
                                    type_args: inst.type_args.clone(),
                                    iterable_param_tys: inst.iterable_param_tys.clone(),
                                })
                                .expect(
                                    "compiler bug: missing instantiated def id for function decl",
                                );
                            apply_inst_to_func_def(
                                &mut cloned,
                                inst,
                                &def_table,
                                &module_lookup,
                                &mut node_id_gen,
                            )?;

                            let mut cloned_item = TopLevelItem::FuncDef(cloned);
                            cloned_item = remap_local_defs_in_item(cloned_item, &mut def_table);
                            rewrite_calls_in_item(&mut cloned_item, &call_inst_map);

                            let old_node_ids = collect_node_ids_in_top_level_item(&cloned_item);
                            reseed_ids_in_item(&mut cloned_item, &mut node_id_gen);

                            let new_node_ids = collect_node_ids_in_top_level_item(&cloned_item);
                            replay_node_def_mappings(&mut def_table, &old_node_ids, &new_node_ids);
                            register_item_def_id(&mut def_table, &cloned_item, new_def_id);

                            new_items.push(cloned_item);
                            continue;
                        }

                        let mut cloned = func_decl.clone();
                        let new_def_id = *inst_to_def
                            .get(&InstKey {
                                def_id: func_decl_id,
                                type_args: inst.type_args.clone(),
                                iterable_param_tys: inst.iterable_param_tys.clone(),
                            })
                            .expect("compiler bug: missing instantiated def id for function decl");
                        apply_inst_to_func_decl(
                            &mut cloned,
                            inst,
                            &def_table,
                            &module_lookup,
                            &mut node_id_gen,
                        )?;

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
                let receiver_type_args = method_block.type_args.clone();
                let receiver_type_param_count =
                    method_block_type_param_count(&def_table, &receiver_type_args);
                let preserve_generic_block = receiver_type_param_count > 0
                    || method_block
                        .method_items
                        .iter()
                        .any(|method_item| match method_item {
                            MethodItem::Decl(method_decl) => {
                                !method_decl.sig.type_params.is_empty()
                            }
                            MethodItem::Def(method_def) => !method_def.sig.type_params.is_empty(),
                        });
                let preserved_generic_block = preserve_generic_block.then(|| {
                    let mut block = method_block.clone();
                    block.method_items = block
                        .method_items
                        .into_iter()
                        .map(|method_item| match method_item {
                            MethodItem::Def(method_def) => MethodItem::Decl(MethodDecl {
                                id: method_def.id,
                                attrs: method_def.attrs,
                                sig: method_def.sig,
                                span: method_def.span,
                            }),
                            other => other,
                        })
                        .collect();
                    block
                });
                let mut kept_items = Vec::with_capacity(method_block.method_items.len());
                let mut specialized_blocks: Vec<MethodBlock> = Vec::new();
                let mut specialized_block_indices: HashMap<Vec<Type>, usize> = HashMap::new();
                for mut method_item in method_block.method_items.drain(..) {
                    match &mut method_item {
                        MethodItem::Def(method_def) => {
                            if receiver_type_param_count == 0
                                && method_def.sig.type_params.is_empty()
                                && !insts_by_def.contains_key(&def_table.def_id(method_def.id))
                            {
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
                                            iterable_param_tys: inst.iterable_param_tys.clone(),
                                        })
                                        .expect(
                                            "compiler bug: missing instantiated def id for method",
                                        );
                                    apply_inst_to_method_def(
                                        &receiver_type_args,
                                        &mut cloned,
                                        inst,
                                        &def_table,
                                        &module_lookup,
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
                                    if receiver_type_param_count == 0 {
                                        kept_items.push(cloned_item);
                                    } else {
                                        let receiver_inst_args =
                                            inst.type_args[..receiver_type_param_count].to_vec();
                                        let block_idx = *specialized_block_indices
                                            .entry(receiver_inst_args.clone())
                                            .or_insert_with(|| {
                                                let mut cloned_block = MethodBlock {
                                                    id: node_id_gen.new_id(),
                                                    type_name: method_block.type_name.clone(),
                                                    type_args: method_block.type_args.clone(),
                                                    trait_name: method_block.trait_name.clone(),
                                                    method_items: Vec::new(),
                                                    span: method_block.span,
                                                };
                                                apply_inst_to_method_block(
                                                    &mut cloned_block,
                                                    &receiver_inst_args,
                                                    &def_table,
                                                    &module_lookup,
                                                    &mut node_id_gen,
                                                )
                                                .expect(
                                                    "compiler bug: failed to specialize generic method block receiver",
                                                );
                                                if let Some(type_def_id) =
                                                    def_table.lookup_type_def_id(&cloned_block.type_name)
                                                {
                                                    def_table.record_use(cloned_block.id, type_def_id);
                                                }
                                                specialized_blocks.push(cloned_block);
                                                specialized_blocks.len() - 1
                                            });
                                        specialized_blocks[block_idx]
                                            .method_items
                                            .push(cloned_item);
                                    }
                                }
                            }
                        }
                        MethodItem::Decl(method_decl) => {
                            if receiver_type_param_count == 0
                                && method_decl.sig.type_params.is_empty()
                                && !insts_by_def.contains_key(&def_table.def_id(method_decl.id))
                            {
                                rewrite_calls_in_method_item(&mut method_item, &call_inst_map);
                                kept_items.push(method_item);
                                continue;
                            }
                            let method_decl_id = def_table.def_id(method_decl.id);
                            if let Some(insts) = insts_by_def.get(&method_decl_id) {
                                for inst in insts {
                                    if let Some(template_def) =
                                        method_templates.get(&method_decl_id)
                                    {
                                        let mut cloned = template_def.clone();
                                        let new_def_id = *inst_to_def
                                            .get(&InstKey {
                                                def_id: method_decl_id,
                                                type_args: inst.type_args.clone(),
                                                iterable_param_tys: inst.iterable_param_tys.clone(),
                                            })
                                            .expect(
                                                "compiler bug: missing instantiated def id for method decl",
                                            );
                                        apply_inst_to_method_def(
                                            &receiver_type_args,
                                            &mut cloned,
                                            inst,
                                            &def_table,
                                            &module_lookup,
                                            &mut node_id_gen,
                                        )?;

                                        let mut cloned_item = MethodItem::Def(cloned);
                                        cloned_item = remap_local_defs_in_method_item(
                                            cloned_item,
                                            &mut def_table,
                                        );
                                        rewrite_calls_in_method_item(
                                            &mut cloned_item,
                                            &call_inst_map,
                                        );

                                        let old_node_ids =
                                            collect_node_ids_in_method_item(&cloned_item);
                                        reseed_ids_in_method_item(
                                            &mut cloned_item,
                                            &mut node_id_gen,
                                        );

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
                                        if receiver_type_param_count == 0 {
                                            kept_items.push(cloned_item);
                                        } else {
                                            let receiver_inst_args = inst.type_args
                                                [..receiver_type_param_count]
                                                .to_vec();
                                            let block_idx = *specialized_block_indices
                                                .entry(receiver_inst_args.clone())
                                                .or_insert_with(|| {
                                                    let mut cloned_block = MethodBlock {
                                                        id: node_id_gen.new_id(),
                                                        type_name: method_block.type_name.clone(),
                                                        type_args: method_block.type_args.clone(),
                                                        trait_name: method_block.trait_name.clone(),
                                                        method_items: Vec::new(),
                                                        span: method_block.span,
                                                    };
                                                    apply_inst_to_method_block(
                                                        &mut cloned_block,
                                                        &receiver_inst_args,
                                                        &def_table,
                                                        &module_lookup,
                                                        &mut node_id_gen,
                                                    )
                                                    .expect(
                                                        "compiler bug: failed to specialize generic method block receiver",
                                                    );
                                                    if let Some(type_def_id) = def_table
                                                        .lookup_type_def_id(&cloned_block.type_name)
                                                    {
                                                        def_table.record_use(
                                                            cloned_block.id,
                                                            type_def_id,
                                                        );
                                                    }
                                                    specialized_blocks.push(cloned_block);
                                                    specialized_blocks.len() - 1
                                                });
                                            specialized_blocks[block_idx]
                                                .method_items
                                                .push(cloned_item);
                                        }
                                        continue;
                                    }

                                    let mut cloned = method_decl.clone();
                                    let new_def_id = *inst_to_def
                                        .get(&InstKey {
                                            def_id: method_decl_id,
                                            type_args: inst.type_args.clone(),
                                            iterable_param_tys: inst.iterable_param_tys.clone(),
                                        })
                                        .expect(
                                            "compiler bug: missing instantiated def id for method decl",
                                        );
                                    apply_inst_to_method_decl(
                                        &receiver_type_args,
                                        &mut cloned,
                                        inst,
                                        &def_table,
                                        &module_lookup,
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
                                    if receiver_type_param_count == 0 {
                                        kept_items.push(cloned_item);
                                    } else {
                                        let receiver_inst_args =
                                            inst.type_args[..receiver_type_param_count].to_vec();
                                        let block_idx = *specialized_block_indices
                                            .entry(receiver_inst_args.clone())
                                            .or_insert_with(|| {
                                                let mut cloned_block = MethodBlock {
                                                    id: node_id_gen.new_id(),
                                                    type_name: method_block.type_name.clone(),
                                                    type_args: method_block.type_args.clone(),
                                                    trait_name: method_block.trait_name.clone(),
                                                    method_items: Vec::new(),
                                                    span: method_block.span,
                                                };
                                                apply_inst_to_method_block(
                                                    &mut cloned_block,
                                                    &receiver_inst_args,
                                                    &def_table,
                                                    &module_lookup,
                                                    &mut node_id_gen,
                                                )
                                                .expect(
                                                    "compiler bug: failed to specialize generic method block receiver",
                                                );
                                                if let Some(type_def_id) =
                                                    def_table.lookup_type_def_id(&cloned_block.type_name)
                                                {
                                                    def_table.record_use(cloned_block.id, type_def_id);
                                                }
                                                specialized_blocks.push(cloned_block);
                                                specialized_blocks.len() - 1
                                            });
                                        specialized_blocks[block_idx]
                                            .method_items
                                            .push(cloned_item);
                                    }
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

                if let Some(block) = preserved_generic_block {
                    new_items.push(TopLevelItem::MethodBlock(block));
                }

                for block in specialized_blocks {
                    new_items.push(TopLevelItem::MethodBlock(block));
                }
            }
            _ => {
                rewrite_calls_in_item(&mut item, &call_inst_map);
                new_items.push(item);
            }
        }
    }

    module.top_level_items = new_items;
    let symbols = crate::core::codegen_names::CodegenNameTable::new(&module, &def_table);
    let specialized_insts = unique_inst_reqs
        .values()
        .filter_map(|inst| {
            inst_to_def
                .get(&inst_key_for_inst(inst))
                .copied()
                .map(|specialized_def_id| (specialized_def_id, inst.clone()))
        })
        .collect();
    Ok((
        ResolvedContext {
            module,
            payload: crate::core::context::ResolvedTables {
                def_table,
                module_path,
                def_owners,
                symbol_ids,
                symbols,
                node_id_gen,
                linear_index,
            },
        },
        stats,
        MonomorphizePlan {
            retype_def_ids: inst_to_def.values().copied().collect(),
            call_rewrites: call_inst_map,
            for_plan_rewrites,
            specialized_insts,
        },
    ))
}

fn inst_key_for_inst(inst: &GenericInst) -> InstKey {
    InstKey {
        def_id: inst.def_id,
        type_args: inst.type_args.clone(),
        iterable_param_tys: inst.iterable_param_tys.clone(),
    }
}

fn for_plans_require_monomorphization(for_plans: &ForPlanMap) -> bool {
    for_plans.values().any(|plan| match &plan.kernel {
        ForKernel::Protocol(kernel) => {
            !kernel.iter_method_type_args.is_empty() || !kernel.next_method_type_args.is_empty()
        }
        _ => false,
    })
}

fn collect_protocol_for_instantiations(
    module: &Module,
    def_table: &crate::core::resolve::DefTable,
    for_plans: &ForPlanMap,
) -> Vec<(NodeId, GenericInst)> {
    let mut out = Vec::new();
    for (stmt_id, plan) in for_plans {
        let ForKernel::Protocol(kernel) = &plan.kernel else {
            continue;
        };
        if !kernel.iter_method_type_args.is_empty()
            && callable_requires_monomorphization(module, def_table, kernel.iter_method)
        {
            out.push((
                *stmt_id,
                GenericInst {
                    def_id: kernel.iter_method,
                    type_args: kernel.iter_method_type_args.clone(),
                    iterable_param_tys: Vec::new(),
                    call_span: Span::default(),
                },
            ));
        }
        if !kernel.next_method_type_args.is_empty()
            && callable_requires_monomorphization(module, def_table, kernel.next_method)
        {
            out.push((
                *stmt_id,
                GenericInst {
                    def_id: kernel.next_method,
                    type_args: kernel.next_method_type_args.clone(),
                    iterable_param_tys: Vec::new(),
                    call_span: Span::default(),
                },
            ));
        }
    }
    out
}

fn collect_protocol_for_rewrites(
    module: &Module,
    def_table: &crate::core::resolve::DefTable,
    for_plans: &ForPlanMap,
    inst_to_def: &HashMap<InstKey, DefId>,
) -> HashMap<NodeId, ProtocolForRewrite> {
    let mut rewrites = HashMap::new();
    for (stmt_id, plan) in for_plans {
        let ForKernel::Protocol(kernel) = &plan.kernel else {
            continue;
        };
        let iter_method = if kernel.iter_method_type_args.is_empty()
            || !callable_requires_monomorphization(module, def_table, kernel.iter_method)
        {
            kernel.iter_method
        } else {
            *inst_to_def
                .get(&InstKey {
                    def_id: kernel.iter_method,
                    type_args: kernel.iter_method_type_args.clone(),
                    iterable_param_tys: Vec::new(),
                })
                .expect("compiler bug: missing specialized iter method def id")
        };
        let next_method = if kernel.next_method_type_args.is_empty()
            || !callable_requires_monomorphization(module, def_table, kernel.next_method)
        {
            kernel.next_method
        } else {
            *inst_to_def
                .get(&InstKey {
                    def_id: kernel.next_method,
                    type_args: kernel.next_method_type_args.clone(),
                    iterable_param_tys: Vec::new(),
                })
                .expect("compiler bug: missing specialized next method def id")
        };
        rewrites.insert(
            *stmt_id,
            ProtocolForRewrite {
                iter_method,
                next_method,
            },
        );
    }
    rewrites
}

fn callable_requires_monomorphization(
    module: &Module,
    def_table: &crate::core::resolve::DefTable,
    def_id: DefId,
) -> bool {
    for item in &module.top_level_items {
        match item {
            TopLevelItem::FuncDecl(func_decl) if def_table.def_id(func_decl.id) == def_id => {
                return !func_decl.sig.type_params.is_empty();
            }
            TopLevelItem::FuncDef(func_def) if def_table.def_id(func_def.id) == def_id => {
                return !func_def.sig.type_params.is_empty();
            }
            TopLevelItem::MethodBlock(block) => {
                let receiver_generic =
                    method_block_type_param_count(def_table, &block.type_args) > 0;
                for method_item in &block.method_items {
                    match method_item {
                        MethodItem::Decl(method_decl)
                            if def_table.def_id(method_decl.id) == def_id =>
                        {
                            return receiver_generic || !method_decl.sig.type_params.is_empty();
                        }
                        MethodItem::Def(method_def)
                            if def_table.def_id(method_def.id) == def_id =>
                        {
                            return receiver_generic || !method_def.sig.type_params.is_empty();
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    false
}

fn collect_generic_callable_templates(
    ctx: &ResolvedContext,
) -> (HashMap<DefId, FuncDef>, HashMap<DefId, MethodDef>) {
    let mut func_templates = HashMap::new();
    let mut method_templates = HashMap::new();
    for item in &ctx.module.top_level_items {
        match item {
            TopLevelItem::FuncDef(func_def) if !func_def.sig.type_params.is_empty() => {
                func_templates.insert(ctx.def_table.def_id(func_def.id), func_def.clone());
            }
            TopLevelItem::MethodBlock(method_block) => {
                let receiver_generic =
                    method_block_type_param_count(&ctx.def_table, &method_block.type_args) > 0;
                for method_item in &method_block.method_items {
                    let MethodItem::Def(method_def) = method_item else {
                        continue;
                    };
                    if receiver_generic || !method_def.sig.type_params.is_empty() {
                        method_templates
                            .insert(ctx.def_table.def_id(method_def.id), method_def.clone());
                    }
                }
            }
            _ => {}
        }
    }
    (func_templates, method_templates)
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
#[path = "../../tests/core/monomorphize/t_monomorphize.rs"]
mod tests;
