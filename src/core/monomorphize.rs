use std::collections::{HashMap, HashSet};

use crate::core::capsule::ModuleId;
use crate::core::context::{ResolvedContext, TypeCheckedContext};
use crate::core::diag::{Span, SpannedError};
use crate::core::resolve::{DefId, DefTable, ImportedFacts, attach_def_owners};
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::visit_mut::{VisitorMut, walk_expr, walk_type_expr};
use crate::core::tree::*;
use crate::core::typecheck::TypeCheckError;
use crate::core::typecheck::type_check_with_imported_facts;
use crate::core::typecheck::type_map::{CallSigMap, GenericInst, GenericInstMap, TypeMap};
use crate::core::types::{FnParamMode, Type};
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
        if matches!(ty, Type::Unknown) && base.lookup_def_type_id(&def).is_some() {
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

struct CallInstRewriter<'a> {
    call_inst_map: &'a HashMap<NodeId, DefId>,
}

impl<'a> VisitorMut for CallInstRewriter<'a> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr);
        let _ = self.call_inst_map.get(&expr.id);
    }
}

fn register_item_def_id(def_table: &mut DefTable, item: &TopLevelItem, def_id: DefId) {
    let (node_id, span) = match item {
        TopLevelItem::FuncDef(func_def) => (func_def.id, func_def.span),
        TopLevelItem::FuncDecl(func_decl) => (func_decl.id, func_decl.span),
        _ => return,
    };
    def_table.record_use(node_id, def_id);
    def_table.record_def_node(def_id, node_id, span);
}

fn register_method_item_def_id(def_table: &mut DefTable, item: &MethodItem, def_id: DefId) {
    let (node_id, span) = match item {
        MethodItem::Def(method_def) => (method_def.id, method_def.span),
        MethodItem::Decl(method_decl) => (method_decl.id, method_decl.span),
    };
    def_table.record_use(node_id, def_id);
    def_table.record_def_node(def_id, node_id, span);
}

fn replay_node_def_mappings(def_table: &mut DefTable, old_ids: &[NodeId], new_ids: &[NodeId]) {
    assert_eq!(
        old_ids.len(),
        new_ids.len(),
        "compiler bug: reseed changed node visitation shape ({} -> {})",
        old_ids.len(),
        new_ids.len()
    );
    for (old_id, new_id) in old_ids.iter().zip(new_ids) {
        if let Some(def_id) = def_table.lookup_node_def_id(*old_id) {
            def_table.record_use(*new_id, def_id);
        }
    }
}

fn collect_node_ids_in_top_level_item(item: &TopLevelItem) -> Vec<NodeId> {
    let mut collector = NodeIdCollector { ids: Vec::new() };
    match item {
        TopLevelItem::ProtocolDef(protocol_def) => collector.visit_protocol_def(protocol_def),
        TopLevelItem::TraitDef(trait_def) => collector.visit_trait_def(trait_def),
        TopLevelItem::TypeDef(type_def) => collector.visit_type_def(type_def),
        TopLevelItem::TypestateDef(typestate_def) => collector.visit_typestate_def(typestate_def),
        TopLevelItem::FuncDecl(func_decl) => collector.visit_func_decl(func_decl),
        TopLevelItem::FuncDef(func_def) => collector.visit_func_def(func_def),
        TopLevelItem::MethodBlock(method_block) => collector.visit_method_block(method_block),
        TopLevelItem::ClosureDef(closure_def) => collector.visit_closure_def(closure_def),
    }
    collector.ids
}

fn collect_node_ids_in_method_item(item: &MethodItem) -> Vec<NodeId> {
    let mut collector = NodeIdCollector { ids: Vec::new() };
    match item {
        MethodItem::Decl(method_decl) => collector.visit_method_decl(method_decl),
        MethodItem::Def(method_def) => collector.visit_method_def(method_def),
    }
    collector.ids
}

struct NodeIdCollector {
    ids: Vec<NodeId>,
}

impl Visitor for NodeIdCollector {
    fn visit_protocol_def(&mut self, protocol_def: &ProtocolDef) {
        self.ids.push(protocol_def.id);
        visit::walk_protocol_def(self, protocol_def);
    }

    fn visit_protocol_role(&mut self, role: &ProtocolRole) {
        self.ids.push(role.id);
        visit::walk_protocol_role(self, role);
    }

    fn visit_protocol_message(&mut self, message: &ProtocolMessage) {
        self.ids.push(message.id);
        visit::walk_protocol_message(self, message);
    }

    fn visit_protocol_request_contract(&mut self, contract: &ProtocolRequestContract) {
        self.ids.push(contract.id);
        visit::walk_protocol_request_contract(self, contract);
    }

    fn visit_protocol_state(&mut self, state: &ProtocolState) {
        self.ids.push(state.id);
        visit::walk_protocol_state(self, state);
    }

    fn visit_protocol_transition(&mut self, transition: &ProtocolTransition) {
        self.ids.push(transition.id);
        visit::walk_protocol_transition(self, transition);
    }

    fn visit_protocol_trigger(&mut self, trigger: &ProtocolTrigger) {
        visit::walk_protocol_trigger(self, trigger);
    }

    fn visit_protocol_effect(&mut self, effect: &ProtocolEffect) {
        visit::walk_protocol_effect(self, effect);
    }

    fn visit_trait_def(&mut self, trait_def: &TraitDef) {
        self.ids.push(trait_def.id);
        visit::walk_trait_def(self, trait_def);
    }

    fn visit_trait_method(&mut self, method: &TraitMethod) {
        self.ids.push(method.id);
        visit::walk_trait_method(self, method);
    }

    fn visit_trait_property(&mut self, property: &TraitProperty) {
        self.ids.push(property.id);
        visit::walk_trait_property(self, property);
    }

    fn visit_typestate_def(&mut self, typestate_def: &TypestateDef) {
        self.ids.push(typestate_def.id);
        visit::walk_typestate_def(self, typestate_def);
    }

    fn visit_typestate_role_impl(&mut self, role_impl: &TypestateRoleImpl) {
        self.ids.push(role_impl.id);
        visit::walk_typestate_role_impl(self, role_impl);
    }

    fn visit_typestate_fields(&mut self, fields: &TypestateFields) {
        self.ids.push(fields.id);
        visit::walk_typestate_fields(self, fields);
    }

    fn visit_typestate_state(&mut self, state: &TypestateState) {
        self.ids.push(state.id);
        visit::walk_typestate_state(self, state);
    }

    fn visit_typestate_on_handler(&mut self, handler: &TypestateOnHandler) {
        self.ids.push(handler.id);
        visit::walk_typestate_on_handler(self, handler);
    }

    fn visit_type_def(&mut self, type_def: &TypeDef) {
        self.ids.push(type_def.id);
        visit::walk_type_def(self, type_def);
    }

    fn visit_struct_def_field(&mut self, field: &StructDefField) {
        self.ids.push(field.id);
        visit::walk_struct_def_field(self, field);
    }

    fn visit_enum_def_variant(&mut self, variant: &EnumDefVariant) {
        self.ids.push(variant.id);
        visit::walk_enum_def_variant(self, variant);
    }

    fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
        self.ids.push(type_expr.id);
        visit::walk_type_expr(self, type_expr);
    }

    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        self.ids.push(func_decl.id);
        visit::walk_func_decl(self, func_decl);
    }

    fn visit_func_def(&mut self, func_def: &FuncDef) {
        self.ids.push(func_def.id);
        visit::walk_func_def(self, func_def);
    }

    fn visit_type_param(&mut self, param: &TypeParam) {
        self.ids.push(param.id);
        visit::walk_type_param(self, param);
    }

    fn visit_self_param(&mut self, self_param: &SelfParam) {
        self.ids.push(self_param.id);
        visit::walk_self_param(self, self_param);
    }

    fn visit_param(&mut self, param: &Param) {
        self.ids.push(param.id);
        visit::walk_param(self, param);
    }

    fn visit_method_block(&mut self, method_block: &MethodBlock) {
        self.ids.push(method_block.id);
        visit::walk_method_block(self, method_block);
    }

    fn visit_method_decl(&mut self, method_decl: &MethodDecl) {
        self.ids.push(method_decl.id);
        visit::walk_method_decl(self, method_decl);
    }

    fn visit_method_def(&mut self, method_def: &MethodDef) {
        self.ids.push(method_def.id);
        visit::walk_method_def(self, method_def);
    }

    fn visit_closure_def(&mut self, closure_def: &ClosureDef) {
        self.ids.push(closure_def.id);
        visit::walk_closure_def(self, closure_def);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        self.ids.push(stmt.id);
        if let StmtExprKind::VarDecl { .. } = &stmt.kind {
            self.ids.push(stmt.id);
        }
        visit::walk_stmt_expr(self, stmt);
    }

    fn visit_bind_pattern(&mut self, pattern: &BindPattern) {
        self.ids.push(pattern.id);
        visit::walk_bind_pattern(self, pattern);
    }

    fn visit_match_arm(&mut self, arm: &MatchArm) {
        self.ids.push(arm.id);
        visit::walk_match_arm(self, arm);
    }

    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Binding { id, .. } | MatchPattern::TypedBinding { id, .. } => {
                self.ids.push(*id);
            }
            MatchPattern::EnumVariant { id, .. } => self.ids.push(*id),
            _ => {}
        }
        visit::walk_match_pattern(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding) {
        if let MatchPatternBinding::Named { id, .. } = binding {
            self.ids.push(*id);
        }
        visit::walk_match_pattern_binding(self, binding);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        self.ids.push(expr.id);
        if let ExprKind::Closure { captures, .. } = &expr.kind {
            for capture in captures {
                let CaptureSpec::Move { id, .. } = capture;
                self.ids.push(*id);
            }
        }
        visit::walk_expr(self, expr);
    }
}

fn rewrite_calls_in_item(item: &mut TopLevelItem, call_inst_map: &HashMap<NodeId, DefId>) {
    let mut rewriter = CallInstRewriter { call_inst_map };
    match item {
        TopLevelItem::FuncDef(func_def) => rewriter.visit_func_def(func_def),
        TopLevelItem::FuncDecl(func_decl) => rewriter.visit_func_decl(func_decl),
        TopLevelItem::MethodBlock(method_block) => rewriter.visit_method_block(method_block),
        TopLevelItem::ClosureDef(closure_def) => rewriter.visit_closure_def(closure_def),
        TopLevelItem::ProtocolDef(_)
        | TopLevelItem::TypeDef(_)
        | TopLevelItem::TraitDef(_)
        | TopLevelItem::TypestateDef(_) => {}
    }
}

fn rewrite_calls_in_method_item(item: &mut MethodItem, call_inst_map: &HashMap<NodeId, DefId>) {
    let mut rewriter = CallInstRewriter { call_inst_map };
    match item {
        MethodItem::Def(method_def) => rewriter.visit_method_def(method_def),
        MethodItem::Decl(method_decl) => rewriter.visit_method_decl(method_decl),
    }
}

fn remap_local_defs_in_item(item: TopLevelItem, def_table: &mut DefTable) -> TopLevelItem {
    let node_ids = collect_node_ids_in_top_level_item(&item);
    remap_local_defs_for_nodes(def_table, &node_ids);
    item
}

fn remap_local_defs_in_method_item(item: MethodItem, def_table: &mut DefTable) -> MethodItem {
    let node_ids = collect_node_ids_in_method_item(&item);
    remap_local_defs_for_nodes(def_table, &node_ids);
    item
}

fn remap_local_defs_for_nodes(def_table: &mut DefTable, node_ids: &[NodeId]) {
    let node_set: HashSet<NodeId> = node_ids.iter().copied().collect();
    let mut old_to_new = HashMap::<DefId, DefId>::new();

    for node_id in node_ids {
        let Some(old_def_id) = def_table.lookup_node_def_id(*node_id) else {
            continue;
        };
        if old_to_new.contains_key(&old_def_id) {
            continue;
        }
        let Some(def) = def_table.lookup_def(old_def_id) else {
            continue;
        };
        if !matches!(
            def.kind,
            crate::core::resolve::DefKind::Param { .. }
                | crate::core::resolve::DefKind::LocalVar { .. }
        ) {
            continue;
        }

        let name = def.name.clone();
        let kind = def.kind.clone();
        let new_def_id = def_table.add_def(name, kind);
        old_to_new.insert(old_def_id, new_def_id);

        if let Some(def_node) = def_table.lookup_def_node_id(old_def_id)
            && node_set.contains(&def_node)
        {
            let span = def_table.lookup_def_span(old_def_id).unwrap_or_default();
            def_table.record_def_node(new_def_id, def_node, span);
        }
    }

    if old_to_new.is_empty() {
        return;
    }

    for node_id in node_ids {
        let Some(old_def_id) = def_table.lookup_node_def_id(*node_id) else {
            continue;
        };
        if let Some(new_def_id) = old_to_new.get(&old_def_id) {
            def_table.record_use(*node_id, *new_def_id);
        }
    }
}

fn reseed_ids_in_item(item: &mut TopLevelItem, node_id_gen: &mut NodeIdGen) {
    match item {
        TopLevelItem::ProtocolDef(protocol_def) => {
            protocol_def.id = node_id_gen.new_id();
            reseed_protocol_def(protocol_def, node_id_gen);
        }
        TopLevelItem::FuncDef(func_def) => reseed_func_def(func_def, node_id_gen),
        TopLevelItem::FuncDecl(func_decl) => reseed_func_decl(func_decl, node_id_gen),
        TopLevelItem::MethodBlock(method_block) => reseed_method_block(method_block, node_id_gen),
        TopLevelItem::ClosureDef(closure_def) => reseed_closure_def(closure_def, node_id_gen),
        TopLevelItem::TraitDef(trait_def) => reseed_trait_def(trait_def, node_id_gen),
        TopLevelItem::TypestateDef(typestate_def) => {
            typestate_def.id = node_id_gen.new_id();
            reseed_typestate_def(typestate_def, node_id_gen);
        }
        TopLevelItem::TypeDef(type_def) => {
            type_def.id = node_id_gen.new_id();
            reseed_type_def(type_def, node_id_gen);
        }
    }
}

fn reseed_protocol_def(protocol_def: &mut ProtocolDef, node_id_gen: &mut NodeIdGen) {
    for message in &mut protocol_def.messages {
        message.id = node_id_gen.new_id();
        reseed_type_expr(&mut message.ty, node_id_gen);
    }
    for contract in &mut protocol_def.request_contracts {
        contract.id = node_id_gen.new_id();
        reseed_type_expr(&mut contract.request_ty, node_id_gen);
        for response_ty in &mut contract.response_tys {
            reseed_type_expr(response_ty, node_id_gen);
        }
    }
    for role in &mut protocol_def.roles {
        role.id = node_id_gen.new_id();
        for state in &mut role.states {
            state.id = node_id_gen.new_id();
            for transition in &mut state.transitions {
                transition.id = node_id_gen.new_id();
                reseed_type_expr(&mut transition.trigger.selector_ty, node_id_gen);
                for effect in &mut transition.effects {
                    reseed_type_expr(&mut effect.payload_ty, node_id_gen);
                }
            }
        }
    }
}

fn reseed_trait_def(trait_def: &mut TraitDef, node_id_gen: &mut NodeIdGen) {
    trait_def.id = node_id_gen.new_id();
    for method in &mut trait_def.methods {
        method.id = node_id_gen.new_id();
        reseed_method_sig(&mut method.sig, node_id_gen);
    }
    for property in &mut trait_def.properties {
        property.id = node_id_gen.new_id();
        reseed_type_expr(&mut property.ty, node_id_gen);
    }
}

fn reseed_typestate_def(typestate_def: &mut TypestateDef, node_id_gen: &mut NodeIdGen) {
    for role_impl in &mut typestate_def.role_impls {
        role_impl.id = node_id_gen.new_id();
    }
    for item in &mut typestate_def.items {
        match item {
            TypestateItem::Fields(fields) => reseed_typestate_fields(fields, node_id_gen),
            TypestateItem::Constructor(constructor) => {
                reseed_func_def(constructor, node_id_gen);
            }
            TypestateItem::Handler(handler) => {
                reseed_typestate_on_handler(handler, node_id_gen);
            }
            TypestateItem::State(state) => reseed_typestate_state(state, node_id_gen),
        }
    }
}

fn reseed_typestate_state(state: &mut TypestateState, node_id_gen: &mut NodeIdGen) {
    state.id = node_id_gen.new_id();
    for item in &mut state.items {
        match item {
            TypestateStateItem::Fields(fields) => reseed_typestate_fields(fields, node_id_gen),
            TypestateStateItem::Method(method) => reseed_func_def(method, node_id_gen),
            TypestateStateItem::Handler(handler) => {
                reseed_typestate_on_handler(handler, node_id_gen);
            }
        }
    }
}

fn reseed_typestate_on_handler(handler: &mut TypestateOnHandler, node_id_gen: &mut NodeIdGen) {
    handler.id = node_id_gen.new_id();
    reseed_type_expr(&mut handler.selector_ty, node_id_gen);
    for param in &mut handler.params {
        reseed_param(param, node_id_gen);
    }
    if let Some(provenance) = &mut handler.provenance {
        reseed_param(&mut provenance.param, node_id_gen);
    }
    reseed_type_expr(&mut handler.ret_ty_expr, node_id_gen);
    reseed_expr(&mut handler.body, node_id_gen);
}

fn reseed_typestate_fields(fields: &mut TypestateFields, node_id_gen: &mut NodeIdGen) {
    fields.id = node_id_gen.new_id();
    for field in &mut fields.fields {
        field.id = node_id_gen.new_id();
        reseed_type_expr(&mut field.ty, node_id_gen);
    }
}

fn reseed_ids_in_method_item(item: &mut MethodItem, node_id_gen: &mut NodeIdGen) {
    match item {
        MethodItem::Def(method_def) => reseed_method_def(method_def, node_id_gen),
        MethodItem::Decl(method_decl) => reseed_method_decl(method_decl, node_id_gen),
    }
}

fn reseed_func_def(func_def: &mut FuncDef, node_id_gen: &mut NodeIdGen) {
    func_def.id = node_id_gen.new_id();
    reseed_func_sig(&mut func_def.sig, node_id_gen);
    reseed_expr(&mut func_def.body, node_id_gen);
}

fn reseed_func_decl(func_decl: &mut FuncDecl, node_id_gen: &mut NodeIdGen) {
    func_decl.id = node_id_gen.new_id();
    reseed_func_sig(&mut func_decl.sig, node_id_gen);
}

fn reseed_method_block(method_block: &mut MethodBlock, node_id_gen: &mut NodeIdGen) {
    method_block.id = node_id_gen.new_id();
    for item in &mut method_block.method_items {
        reseed_ids_in_method_item(item, node_id_gen);
    }
}

fn reseed_method_def(method_def: &mut MethodDef, node_id_gen: &mut NodeIdGen) {
    method_def.id = node_id_gen.new_id();
    reseed_method_sig(&mut method_def.sig, node_id_gen);
    reseed_expr(&mut method_def.body, node_id_gen);
}

fn reseed_method_decl(method_decl: &mut MethodDecl, node_id_gen: &mut NodeIdGen) {
    method_decl.id = node_id_gen.new_id();
    reseed_method_sig(&mut method_decl.sig, node_id_gen);
}

fn reseed_closure_def(closure_def: &mut ClosureDef, node_id_gen: &mut NodeIdGen) {
    closure_def.id = node_id_gen.new_id();
    reseed_closure_sig(&mut closure_def.sig, node_id_gen);
    reseed_expr(&mut closure_def.body, node_id_gen);
}

fn reseed_func_sig(func_sig: &mut FunctionSig, node_id_gen: &mut NodeIdGen) {
    for param in &mut func_sig.type_params {
        reseed_type_param(param, node_id_gen);
    }
    for param in &mut func_sig.params {
        reseed_param(param, node_id_gen);
    }
    reseed_type_expr(&mut func_sig.ret_ty_expr, node_id_gen);
}

fn reseed_method_sig(method_sig: &mut MethodSig, node_id_gen: &mut NodeIdGen) {
    method_sig.self_param.id = node_id_gen.new_id();
    for param in &mut method_sig.type_params {
        reseed_type_param(param, node_id_gen);
    }
    for param in &mut method_sig.params {
        reseed_param(param, node_id_gen);
    }
    reseed_type_expr(&mut method_sig.ret_ty_expr, node_id_gen);
}

fn reseed_closure_sig(closure_sig: &mut ClosureSig, node_id_gen: &mut NodeIdGen) {
    for param in &mut closure_sig.params {
        reseed_param(param, node_id_gen);
    }
    reseed_type_expr(&mut closure_sig.return_ty, node_id_gen);
}

fn reseed_param(param: &mut Param, node_id_gen: &mut NodeIdGen) {
    param.id = node_id_gen.new_id();
    reseed_type_expr(&mut param.typ, node_id_gen);
}

fn reseed_type_param(param: &mut TypeParam, node_id_gen: &mut NodeIdGen) {
    param.id = node_id_gen.new_id();
    if let Some(bound) = &mut param.bound {
        bound.id = node_id_gen.new_id();
    }
}

fn reseed_type_def(type_def: &mut TypeDef, node_id_gen: &mut NodeIdGen) {
    match &mut type_def.kind {
        TypeDefKind::Alias { aliased_ty } => reseed_type_expr(aliased_ty, node_id_gen),
        TypeDefKind::Struct { fields } => {
            for field in fields {
                field.id = node_id_gen.new_id();
                reseed_type_expr(&mut field.ty, node_id_gen);
            }
        }
        TypeDefKind::Enum { variants } => {
            for variant in variants {
                variant.id = node_id_gen.new_id();
                for payload in &mut variant.payload {
                    reseed_type_expr(payload, node_id_gen);
                }
            }
        }
    }
}

fn reseed_type_expr(type_expr: &mut TypeExpr, node_id_gen: &mut NodeIdGen) {
    type_expr.id = node_id_gen.new_id();
    match &mut type_expr.kind {
        TypeExprKind::Infer => {}
        TypeExprKind::Union { variants } => {
            for variant in variants {
                reseed_type_expr(variant, node_id_gen);
            }
        }
        TypeExprKind::Named { type_args, .. } => {
            for arg in type_args {
                reseed_type_expr(arg, node_id_gen);
            }
        }
        TypeExprKind::Refined { base_ty_expr, .. } => {
            reseed_type_expr(base_ty_expr, node_id_gen);
        }
        TypeExprKind::Array { elem_ty_expr, .. } => {
            reseed_type_expr(elem_ty_expr, node_id_gen);
        }
        TypeExprKind::DynArray { elem_ty_expr } => {
            reseed_type_expr(elem_ty_expr, node_id_gen);
        }
        TypeExprKind::Tuple { field_ty_exprs } => {
            for field in field_ty_exprs {
                reseed_type_expr(field, node_id_gen);
            }
        }
        TypeExprKind::Slice { elem_ty_expr } => {
            reseed_type_expr(elem_ty_expr, node_id_gen);
        }
        TypeExprKind::Heap { elem_ty_expr } => {
            reseed_type_expr(elem_ty_expr, node_id_gen);
        }
        TypeExprKind::Ref { elem_ty_expr, .. } => {
            reseed_type_expr(elem_ty_expr, node_id_gen);
        }
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => {
            for param in params {
                reseed_type_expr(&mut param.ty_expr, node_id_gen);
            }
            reseed_type_expr(ret_ty_expr, node_id_gen);
        }
    }
}

fn reseed_stmt_expr(stmt: &mut StmtExpr, node_id_gen: &mut NodeIdGen) {
    stmt.id = node_id_gen.new_id();
    match &mut stmt.kind {
        StmtExprKind::LetBind {
            pattern,
            decl_ty,
            value,
        }
        | StmtExprKind::VarBind {
            pattern,
            decl_ty,
            value,
        } => {
            reseed_bind_pattern(pattern, node_id_gen);
            if let Some(ty) = decl_ty {
                reseed_type_expr(ty, node_id_gen);
            }
            reseed_expr(value, node_id_gen);
        }
        StmtExprKind::VarDecl { decl_ty, .. } => {
            reseed_type_expr(decl_ty, node_id_gen);
        }
        StmtExprKind::Assign {
            assignee, value, ..
        } => {
            reseed_expr(assignee, node_id_gen);
            reseed_expr(value, node_id_gen);
        }
        StmtExprKind::CompoundAssign {
            assignee, value, ..
        } => {
            reseed_expr(assignee, node_id_gen);
            reseed_expr(value, node_id_gen);
        }
        StmtExprKind::While { cond, body } => {
            reseed_expr(cond, node_id_gen);
            reseed_expr(body, node_id_gen);
        }
        StmtExprKind::For {
            pattern,
            iter,
            body,
        } => {
            reseed_bind_pattern(pattern, node_id_gen);
            reseed_expr(iter, node_id_gen);
            reseed_expr(body, node_id_gen);
        }
        StmtExprKind::Break | StmtExprKind::Continue => {}
        StmtExprKind::Return { value } => {
            if let Some(value) = value {
                reseed_expr(value, node_id_gen);
            }
        }
    }
}

fn reseed_bind_pattern(pattern: &mut BindPattern, node_id_gen: &mut NodeIdGen) {
    pattern.id = node_id_gen.new_id();
    match &mut pattern.kind {
        BindPatternKind::Name { .. } => {
            pattern.id = node_id_gen.new_id();
        }
        BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
            for pattern in patterns {
                reseed_bind_pattern(pattern, node_id_gen);
            }
        }
        BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                reseed_bind_pattern(&mut field.pattern, node_id_gen);
            }
        }
    }
}

fn reseed_match_arm(arm: &mut MatchArm, node_id_gen: &mut NodeIdGen) {
    arm.id = node_id_gen.new_id();
    reseed_match_pattern(&mut arm.pattern, node_id_gen);
    reseed_expr(&mut arm.body, node_id_gen);
}

fn reseed_match_pattern(pattern: &mut MatchPattern, node_id_gen: &mut NodeIdGen) {
    match pattern {
        MatchPattern::Binding { id, .. } => *id = node_id_gen.new_id(),
        MatchPattern::TypedBinding { id, ty_expr, .. } => {
            *id = node_id_gen.new_id();
            reseed_type_expr(ty_expr, node_id_gen);
        }
        MatchPattern::Tuple { patterns, .. } => {
            for pattern in patterns {
                reseed_match_pattern(pattern, node_id_gen);
            }
        }
        MatchPattern::EnumVariant { id, bindings, .. } => {
            *id = node_id_gen.new_id();
            for binding in bindings {
                reseed_match_pattern_binding(binding, node_id_gen);
            }
        }
        MatchPattern::Wildcard { .. }
        | MatchPattern::BoolLit { .. }
        | MatchPattern::IntLit { .. } => {}
    }
}

fn reseed_match_pattern_binding(binding: &mut MatchPatternBinding, node_id_gen: &mut NodeIdGen) {
    if let MatchPatternBinding::Named { id, .. } = binding {
        *id = node_id_gen.new_id();
    }
}

fn reseed_struct_lit_field(field: &mut StructLitField, node_id_gen: &mut NodeIdGen) {
    field.id = node_id_gen.new_id();
    reseed_expr(&mut field.value, node_id_gen);
}

fn reseed_struct_update_field(field: &mut StructUpdateField, node_id_gen: &mut NodeIdGen) {
    field.id = node_id_gen.new_id();
    reseed_expr(&mut field.value, node_id_gen);
}

fn reseed_capture_spec(spec: &mut CaptureSpec, node_id_gen: &mut NodeIdGen) {
    match spec {
        CaptureSpec::Move { id, .. } => *id = node_id_gen.new_id(),
    }
}

fn reseed_expr(expr: &mut Expr, node_id_gen: &mut NodeIdGen) {
    expr.id = node_id_gen.new_id();
    match &mut expr.kind {
        ExprKind::Block { items, tail } => {
            for item in items {
                match item {
                    BlockItem::Stmt(stmt) => reseed_stmt_expr(stmt, node_id_gen),
                    BlockItem::Expr(expr) => reseed_expr(expr, node_id_gen),
                }
            }
            if let Some(tail) = tail {
                reseed_expr(tail, node_id_gen);
            }
        }
        ExprKind::StringFmt { segments } => {
            for segment in segments {
                if let StringFmtSegment::Expr { expr, .. } = segment {
                    reseed_expr(expr, node_id_gen);
                }
            }
        }
        ExprKind::ArrayLit { init, elem_ty } => {
            if let Some(elem_ty) = elem_ty {
                reseed_type_expr(elem_ty, node_id_gen);
            }
            match init {
                ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        reseed_expr(elem, node_id_gen);
                    }
                }
                ArrayLitInit::Repeat(expr, _) => reseed_expr(expr, node_id_gen),
            }
        }
        ExprKind::SetLit { elem_ty, elems } => {
            if let Some(elem_ty) = elem_ty {
                reseed_type_expr(elem_ty, node_id_gen);
            }
            for elem in elems {
                reseed_expr(elem, node_id_gen);
            }
        }
        ExprKind::MapLit {
            key_ty,
            value_ty,
            entries,
        } => {
            if let Some(key_ty) = key_ty {
                reseed_type_expr(key_ty, node_id_gen);
            }
            if let Some(value_ty) = value_ty {
                reseed_type_expr(value_ty, node_id_gen);
            }
            for entry in entries {
                entry.id = node_id_gen.new_id();
                reseed_expr(&mut entry.key, node_id_gen);
                reseed_expr(&mut entry.value, node_id_gen);
            }
        }
        ExprKind::TupleLit(fields) => {
            for field in fields {
                reseed_expr(field, node_id_gen);
            }
        }
        ExprKind::StructLit { fields, .. } => {
            for field in fields {
                reseed_struct_lit_field(field, node_id_gen);
            }
        }
        ExprKind::EnumVariant { payload, .. } => {
            for expr in payload {
                reseed_expr(expr, node_id_gen);
            }
        }
        ExprKind::StructUpdate { target, fields } => {
            reseed_expr(target, node_id_gen);
            for field in fields {
                reseed_struct_update_field(field, node_id_gen);
            }
        }
        ExprKind::BinOp { left, right, .. } => {
            reseed_expr(left, node_id_gen);
            reseed_expr(right, node_id_gen);
        }
        ExprKind::UnaryOp { expr, .. } => {
            reseed_expr(expr, node_id_gen);
        }
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            reseed_expr(fallible_expr, node_id_gen);
            if let Some(handler) = on_error {
                reseed_expr(handler, node_id_gen);
            }
        }
        ExprKind::HeapAlloc { expr }
        | ExprKind::Move { expr }
        | ExprKind::AddrOf { expr }
        | ExprKind::Deref { expr }
        | ExprKind::ImplicitMove { expr }
        | ExprKind::Coerce { expr, .. } => {
            reseed_expr(expr, node_id_gen);
        }
        ExprKind::ArrayIndex { target, indices } => {
            reseed_expr(target, node_id_gen);
            for index in indices {
                reseed_expr(index, node_id_gen);
            }
        }
        ExprKind::TupleField { target, .. } | ExprKind::StructField { target, .. } => {
            reseed_expr(target, node_id_gen);
        }
        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => {
            reseed_expr(cond, node_id_gen);
            reseed_expr(then_body, node_id_gen);
            reseed_expr(else_body, node_id_gen);
        }
        ExprKind::Range { start, end } => {
            reseed_expr(start, node_id_gen);
            reseed_expr(end, node_id_gen);
        }
        ExprKind::Slice { target, start, end } => {
            reseed_expr(target, node_id_gen);
            if let Some(start) = start {
                reseed_expr(start, node_id_gen);
            }
            if let Some(end) = end {
                reseed_expr(end, node_id_gen);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            reseed_expr(scrutinee, node_id_gen);
            for arm in arms {
                reseed_match_arm(arm, node_id_gen);
            }
        }
        ExprKind::Call { callee, args } => {
            reseed_expr(callee, node_id_gen);
            for arg in args {
                reseed_expr(&mut arg.expr, node_id_gen);
            }
        }
        ExprKind::MethodCall { callee, args, .. } => {
            reseed_expr(callee, node_id_gen);
            for arg in args {
                reseed_expr(&mut arg.expr, node_id_gen);
            }
        }
        ExprKind::Emit { kind } => match kind {
            EmitKind::Send { to, payload }
            | EmitKind::Request {
                to,
                payload,
                request_site_label: _,
            } => {
                reseed_expr(to, node_id_gen);
                reseed_expr(payload, node_id_gen);
            }
        },
        ExprKind::Reply { cap, value } => {
            reseed_expr(cap, node_id_gen);
            reseed_expr(value, node_id_gen);
        }
        ExprKind::Closure {
            params,
            return_ty,
            body,
            captures,
            ..
        } => {
            for param in params {
                reseed_param(param, node_id_gen);
            }
            for capture in captures {
                reseed_capture_spec(capture, node_id_gen);
            }
            reseed_type_expr(return_ty, node_id_gen);
            reseed_expr(body, node_id_gen);
        }
        ExprKind::UnitLit
        | ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::Var { .. } => {}
    }
}

fn apply_inst_to_func_def(
    func_def: &mut FuncDef,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&func_def.sig.type_params, inst, def_table)?;
    func_def.sig.type_params.clear();
    let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
    substituter.visit_func_def(func_def);
    substituter.finish()
}

fn apply_inst_to_func_decl(
    func_decl: &mut FuncDecl,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&func_decl.sig.type_params, inst, def_table)?;
    func_decl.sig.type_params.clear();
    let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
    substituter.visit_func_decl(func_decl);
    substituter.finish()
}

fn apply_inst_to_method_def(
    method_def: &mut MethodDef,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&method_def.sig.type_params, inst, def_table)?;
    method_def.sig.type_params.clear();
    let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
    substituter.visit_method_def(method_def);
    substituter.finish()
}

fn apply_inst_to_method_decl(
    method_decl: &mut MethodDecl,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&method_decl.sig.type_params, inst, def_table)?;
    method_decl.sig.type_params.clear();
    let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
    substituter.visit_method_decl(method_decl);
    substituter.finish()
}

fn build_subst(
    type_params: &[TypeParam],
    inst: &GenericInst,
    def_table: &DefTable,
) -> Result<HashMap<DefId, Type>, MonomorphizeError> {
    if type_params.len() != inst.type_args.len() {
        let name = def_name(def_table, inst.def_id);
        return Err(MonomorphizeErrorKind::ArityMismatch {
            name,
            expected: type_params.len(),
            got: inst.type_args.len(),
        }
        .at(inst.call_span));
    }

    Ok(type_params
        .iter()
        .zip(inst.type_args.iter().cloned())
        .map(|(param, ty)| (def_table.def_id(param.id), ty))
        .collect())
}

struct TypeExprSubstitutor<'a> {
    subst: &'a HashMap<DefId, Type>,
    def_table: &'a DefTable,
    node_id_gen: &'a mut NodeIdGen,
    error: Option<MonomorphizeError>,
}

impl<'a> TypeExprSubstitutor<'a> {
    fn new(
        subst: &'a HashMap<DefId, Type>,
        def_table: &'a DefTable,
        node_id_gen: &'a mut NodeIdGen,
    ) -> Self {
        Self {
            subst,
            def_table,
            node_id_gen,
            error: None,
        }
    }

    fn finish(self) -> Result<(), MonomorphizeError> {
        if let Some(err) = self.error {
            Err(err)
        } else {
            Ok(())
        }
    }
}

impl<'a> VisitorMut for TypeExprSubstitutor<'a> {
    fn visit_type_expr(&mut self, type_expr: &mut TypeExpr) {
        if self.error.is_some() {
            return;
        }

        if let TypeExprKind::Named { .. } = &type_expr.kind
            && let Some(def_id) = self.def_table.lookup_node_def_id(type_expr.id)
            && let Some(ty) = self.subst.get(&def_id)
        {
            match type_expr_from_type(ty, self.def_table, self.node_id_gen, type_expr.span) {
                Ok(new_expr) => {
                    *type_expr = new_expr;
                    return;
                }
                Err(err) => {
                    self.error = Some(err);
                    return;
                }
            }
        }

        walk_type_expr(self, type_expr);
    }
}

fn type_expr_from_type(
    ty: &Type,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Result<TypeExpr, MonomorphizeError> {
    let id = node_id_gen.new_id();
    let kind = match ty {
        Type::Unit => {
            return named_type_expr("()", def_table, node_id_gen, span);
        }
        Type::Int {
            signed,
            bits,
            bounds,
            nonzero,
        } => {
            let name = match (*signed, *bits) {
                (false, 8) => "u8",
                (false, 16) => "u16",
                (false, 32) => "u32",
                (false, 64) => "u64",
                (true, 8) => "i8",
                (true, 16) => "i16",
                (true, 32) => "i32",
                (true, 64) => "i64",
                _ => return Err(MonomorphizeErrorKind::UnsupportedType.at(span)),
            };
            let mut refinements = Vec::new();
            if let Some(bounds) = bounds {
                refinements.push(RefinementKind::Bounds {
                    min: bounds.min,
                    max: bounds.max_excl,
                });
            }
            if *nonzero {
                refinements.push(RefinementKind::NonZero);
            }
            if refinements.is_empty() {
                return named_type_expr(name, def_table, node_id_gen, span);
            }
            let base_expr = named_type_expr(name, def_table, node_id_gen, span)?;
            TypeExprKind::Refined {
                base_ty_expr: Box::new(base_expr),
                refinements,
            }
        }
        Type::Bool => {
            return named_type_expr("bool", def_table, node_id_gen, span);
        }
        Type::Char => {
            return named_type_expr("char", def_table, node_id_gen, span);
        }
        Type::String => {
            return named_type_expr("string", def_table, node_id_gen, span);
        }
        Type::Array { elem_ty, dims } => TypeExprKind::Array {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
            dims: dims.clone(),
        },
        Type::DynArray { elem_ty } => TypeExprKind::DynArray {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Pending { response_tys } => TypeExprKind::Named {
            ident: "Pending".to_string(),
            type_args: vec![response_set_type_arg_expr(
                response_tys,
                def_table,
                node_id_gen,
                span,
            )?],
        },
        Type::ReplyCap { response_tys } => TypeExprKind::Named {
            ident: "ReplyCap".to_string(),
            type_args: vec![response_set_type_arg_expr(
                response_tys,
                def_table,
                node_id_gen,
                span,
            )?],
        },
        Type::Set { elem_ty } => TypeExprKind::Named {
            ident: "set".to_string(),
            type_args: vec![type_expr_from_type(elem_ty, def_table, node_id_gen, span)?],
        },
        Type::Map { key_ty, value_ty } => TypeExprKind::Named {
            ident: "map".to_string(),
            type_args: vec![
                type_expr_from_type(key_ty, def_table, node_id_gen, span)?,
                type_expr_from_type(value_ty, def_table, node_id_gen, span)?,
            ],
        },
        Type::Tuple { field_tys } => TypeExprKind::Tuple {
            field_ty_exprs: field_tys
                .iter()
                .map(|ty| type_expr_from_type(ty, def_table, node_id_gen, span))
                .collect::<Result<Vec<_>, _>>()?,
        },
        Type::Slice { elem_ty } => TypeExprKind::Slice {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Heap { elem_ty } => TypeExprKind::Heap {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Ref { mutable, elem_ty } => TypeExprKind::Ref {
            mutable: *mutable,
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Fn { params, ret_ty } => {
            let params = params
                .iter()
                .map(|param| {
                    let mode = match param.mode {
                        FnParamMode::In => ParamMode::In,
                        FnParamMode::InOut => ParamMode::InOut,
                        FnParamMode::Out => ParamMode::Out,
                        FnParamMode::Sink => ParamMode::Sink,
                    };
                    Ok(FnTypeParam {
                        mode,
                        ty_expr: type_expr_from_type(&param.ty, def_table, node_id_gen, span)?,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            TypeExprKind::Fn {
                params,
                ret_ty_expr: Box::new(type_expr_from_type(ret_ty, def_table, node_id_gen, span)?),
            }
        }
        Type::ErrorUnion { ok_ty, err_tys } => TypeExprKind::Union {
            variants: std::iter::once(ok_ty.as_ref())
                .chain(err_tys.iter())
                .map(|ty| type_expr_from_type(ty, def_table, node_id_gen, span))
                .collect::<Result<Vec<_>, _>>()?,
        },
        Type::Struct { name, .. } | Type::Enum { name, .. } => {
            return named_type_expr(name, def_table, node_id_gen, span);
        }
        Type::Range { .. } => return Err(MonomorphizeErrorKind::UnsupportedType.at(span)),
        Type::Unknown | Type::Var(_) => {
            return Err(MonomorphizeErrorKind::UnsupportedType.at(span));
        }
    };

    Ok(TypeExpr { id, kind, span })
}

fn response_set_type_arg_expr(
    response_tys: &[Type],
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Result<TypeExpr, MonomorphizeError> {
    if response_tys.len() <= 1 {
        return type_expr_from_type(
            response_tys
                .first()
                .ok_or(MonomorphizeErrorKind::UnsupportedType.at(span))?,
            def_table,
            node_id_gen,
            span,
        );
    }
    Ok(TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Union {
            variants: response_tys
                .iter()
                .map(|ty| type_expr_from_type(ty, def_table, node_id_gen, span))
                .collect::<Result<Vec<_>, _>>()?,
        },
        span,
    })
}

fn named_type_expr(
    name: &str,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Result<TypeExpr, MonomorphizeError> {
    if def_table.lookup_type_def_id(name).is_none() {
        return Err(MonomorphizeErrorKind::UnknownType {
            name: name.to_string(),
        }
        .at(span));
    }
    Ok(TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Named {
            ident: name.to_string(),
            type_args: Vec::new(),
        },
        span,
    })
}

fn def_name(def_table: &DefTable, def_id: DefId) -> String {
    def_table
        .lookup_def(def_id)
        .map(|def| def.name.clone())
        .unwrap_or_else(|| format!("def_{def_id:?}"))
}

#[cfg(test)]
#[path = "../tests/monomorphize/t_monomorphize.rs"]
mod tests;
