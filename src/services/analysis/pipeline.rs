//! Shared analysis pipeline helpers.
//!
//! This module centralizes the repeated lex -> parse -> resolve -> typecheck
//! orchestration used by analysis queries, while preserving per-stage
//! diagnostics and query cache boundaries.

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use crate::core::api::{
    FrontendPolicy, ParseModuleError, ParseModuleOptions, ResolveInputs,
    parse_module_with_id_gen_and_options, resolve_stage_with_policy, semcheck_stage_with_policy,
    typecheck_stage_with_policy,
};
use crate::core::capsule::ModuleId;
use crate::core::resolve::{
    ImportedCallableSig, ImportedFacts, ImportedModule, ImportedParamSig, ImportedSymbol,
    ImportedTraitMethodSig, ImportedTraitPropertySig, ImportedTraitSig,
};
use crate::core::tree::NodeId;
use crate::core::tree::NodeIdGen;
use crate::services::analysis::diagnostics::Diagnostic;
use crate::services::analysis::query::{QueryKey, QueryKind, QueryResult, QueryRuntime};

/// Sentinel used when a stage fails before node-level attribution is possible.
pub(crate) const ROOT_POISON_NODE: NodeId = NodeId(0);

#[derive(Clone)]
pub(crate) struct StageOutput<T> {
    pub product: Option<T>,
    pub diagnostics: Vec<Diagnostic>,
    pub poisoned_nodes: HashSet<NodeId>,
}

impl<T> Default for StageOutput<T> {
    fn default() -> Self {
        Self {
            product: None,
            diagnostics: Vec::new(),
            poisoned_nodes: HashSet::new(),
        }
    }
}

pub(crate) type ParseStageOutput = StageOutput<crate::core::context::ParsedContext>;
pub(crate) type ResolveStageOutput = StageOutput<crate::core::context::ResolvedContext>;
pub(crate) type TypecheckStageOutput = StageOutput<crate::core::context::TypeCheckedContext>;
pub(crate) type SemcheckStageOutput = StageOutput<crate::core::context::SemanticCheckedContext>;

#[derive(Clone, Default)]
pub(crate) struct ModulePipelineState {
    pub parsed: ParseStageOutput,
    pub resolved: ResolveStageOutput,
    pub typechecked: TypecheckStageOutput,
    pub semchecked: SemcheckStageOutput,
}

#[derive(Clone, Default)]
pub(crate) struct LookupState {
    pub resolved: Option<crate::core::context::ResolvedContext>,
    pub typed: Option<crate::core::context::TypeCheckedContext>,
    pub poisoned_nodes: HashSet<NodeId>,
}

/// Analysis-side overlays/controls layered on top of core stage contexts.
///
/// These are intentionally kept out of core `ResolvedContext`/`TypeCheckedContext`.
#[derive(Clone, Default)]
pub(crate) struct ModulePipelineInputs {
    pub parsed_override: Option<crate::core::context::ParsedContext>,
    pub imported_modules: HashMap<String, ImportedModule>,
    pub imported_symbols: HashMap<String, ImportedSymbol>,
    pub skip_typecheck: bool,
    pub experimental_typestate: bool,
}

#[cfg(test)]
pub(crate) fn run_module_pipeline(
    rt: &mut QueryRuntime,
    module_id: ModuleId,
    revision: u64,
    source: Arc<str>,
) -> QueryResult<ModulePipelineState> {
    run_module_pipeline_with_query_input(rt, module_id, revision, source, 0, false)
}

pub(crate) fn run_module_pipeline_with_query_input(
    rt: &mut QueryRuntime,
    module_id: ModuleId,
    revision: u64,
    source: Arc<str>,
    query_input: u64,
    experimental_typestate: bool,
) -> QueryResult<ModulePipelineState> {
    let inputs = ModulePipelineInputs {
        experimental_typestate,
        ..ModulePipelineInputs::default()
    };
    run_module_pipeline_with_inputs(rt, module_id, revision, source, query_input, inputs)
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_module_pipeline_with_parsed_and_imports(
    rt: &mut QueryRuntime,
    module_id: ModuleId,
    revision: u64,
    source: Arc<str>,
    parsed_override: Option<crate::core::context::ParsedContext>,
    imported_modules: HashMap<String, ImportedModule>,
    imported_symbols: HashMap<String, ImportedSymbol>,
    skip_typecheck: bool,
    experimental_typestate: bool,
) -> QueryResult<ModulePipelineState> {
    run_module_pipeline_with_parsed_and_imports_with_query_input(
        rt,
        module_id,
        revision,
        source,
        0,
        parsed_override,
        imported_modules,
        imported_symbols,
        skip_typecheck,
        experimental_typestate,
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_module_pipeline_with_parsed_and_imports_with_query_input(
    rt: &mut QueryRuntime,
    module_id: ModuleId,
    revision: u64,
    source: Arc<str>,
    query_input: u64,
    parsed_override: Option<crate::core::context::ParsedContext>,
    imported_modules: HashMap<String, ImportedModule>,
    imported_symbols: HashMap<String, ImportedSymbol>,
    skip_typecheck: bool,
    experimental_typestate: bool,
) -> QueryResult<ModulePipelineState> {
    let inputs = ModulePipelineInputs {
        parsed_override,
        imported_modules,
        imported_symbols,
        skip_typecheck,
        experimental_typestate,
    };
    run_module_pipeline_with_inputs(rt, module_id, revision, source, query_input, inputs)
}

fn run_module_pipeline_with_inputs(
    rt: &mut QueryRuntime,
    module_id: ModuleId,
    revision: u64,
    source: Arc<str>,
    query_input: u64,
    inputs: ModulePipelineInputs,
) -> QueryResult<ModulePipelineState> {
    #[derive(Clone, Default)]
    struct ResolveEval {
        state: ResolveStageOutput,
        imported_facts: ImportedFacts,
    }

    let ModulePipelineInputs {
        parsed_override,
        imported_modules,
        imported_symbols,
        skip_typecheck,
        experimental_typestate,
    } = inputs;

    let semantic_fingerprint = semantic_input_fingerprint(
        parsed_override.as_ref(),
        &imported_modules,
        &imported_symbols,
        skip_typecheck,
        experimental_typestate,
    );
    let query_revision = revision ^ semantic_fingerprint;

    let parse_key = QueryKey::with_input(
        QueryKind::ParseModule,
        module_id,
        query_revision,
        query_input,
    );
    let source_for_parse = source.clone();
    let parsed_override_for_parse = parsed_override.clone();
    let parsed = rt.execute(parse_key, move |_rt| {
        let mut state = ParseStageOutput::default();
        if let Some(parsed_context) = parsed_override_for_parse {
            state.product = Some(parsed_context);
            return Ok(state);
        }

        match parse_module_with_id_gen_and_options(
            &source_for_parse,
            NodeIdGen::new(),
            ParseModuleOptions {
                experimental_typestate,
            },
        ) {
            Ok((module, id_gen)) => {
                state.product = Some(crate::core::context::ParsedContext::new(module, id_gen));
            }
            Err(ParseModuleError::Lex(error)) => {
                state.diagnostics.push(Diagnostic::from_lex_error(&error));
                state.poisoned_nodes.insert(ROOT_POISON_NODE);
                return Ok(state);
            }
            Err(ParseModuleError::Parse(error)) => {
                state.diagnostics.push(Diagnostic::from_parse_error(&error));
                state.poisoned_nodes.insert(ROOT_POISON_NODE);
            }
        }
        Ok(state)
    })?;

    let resolve_key = QueryKey::with_input(
        QueryKind::ResolveModule,
        module_id,
        query_revision,
        query_input,
    );
    let resolve_input = parsed.product.clone();
    let imported_modules_for_resolve = imported_modules.clone();
    let imported_symbols_for_resolve = imported_symbols.clone();
    let resolved_eval = rt.execute(resolve_key, move |_rt| {
        let mut eval = ResolveEval::default();
        if let Some(parsed) = resolve_input {
            let resolved = resolve_stage_with_policy(
                parsed,
                ResolveInputs {
                    imported_modules: imported_modules_for_resolve,
                    imported_symbols: imported_symbols_for_resolve,
                },
                FrontendPolicy::Partial,
            );
            let has_errors = resolved.has_errors();
            eval.state.product = resolved.context;
            eval.imported_facts = resolved.imported_facts;
            if has_errors {
                eval.state
                    .diagnostics
                    .extend(resolved.errors.iter().map(Diagnostic::from_resolve_error));
                eval.state.poisoned_nodes.insert(ROOT_POISON_NODE);
            }
        }
        Ok(eval)
    })?;
    let resolved = resolved_eval.state;
    let imported_facts_for_typecheck = resolved_eval.imported_facts;

    let typecheck_key = QueryKey::with_input(
        QueryKind::TypecheckModule,
        module_id,
        query_revision,
        query_input,
    );
    // Do not run type checking when resolution emitted errors. This keeps
    // lookup behavior stable (no leaked inference vars on unresolved symbols).
    let typecheck_input = if !skip_typecheck && resolved.diagnostics.is_empty() {
        resolved
            .product
            .clone()
            .map(|resolved| (resolved, imported_facts_for_typecheck.clone()))
    } else {
        None
    };
    let typechecked = rt.execute(typecheck_key, move |_rt| {
        let mut state = TypecheckStageOutput::default();
        if let Some((resolved, imported_facts)) = typecheck_input {
            let typed =
                typecheck_stage_with_policy(resolved, imported_facts, FrontendPolicy::Partial);
            let has_errors = typed.has_errors();
            state.product = typed.context;
            if has_errors {
                state
                    .diagnostics
                    .extend(typed.errors.iter().map(Diagnostic::from_typecheck_error));
                state.poisoned_nodes.insert(ROOT_POISON_NODE);
            }
        }
        Ok(state)
    })?;

    let mut upstream_poisoned = parsed.poisoned_nodes.clone();
    upstream_poisoned.extend(resolved.poisoned_nodes.iter().copied());
    upstream_poisoned.extend(typechecked.poisoned_nodes.iter().copied());

    let semcheck_key = QueryKey::with_input(
        QueryKind::SemcheckModule,
        module_id,
        query_revision,
        query_input,
    );
    let semcheck_input = if !skip_typecheck && typechecked.diagnostics.is_empty() {
        typechecked
            .product
            .clone()
            .map(|typed| (typed, upstream_poisoned))
    } else {
        None
    };
    let semchecked = rt.execute(semcheck_key, move |_rt| {
        let mut state = SemcheckStageOutput::default();
        if let Some((typed, upstream_poisoned_nodes)) = semcheck_input {
            let semchecked = semcheck_stage_with_policy(
                typed,
                FrontendPolicy::Partial,
                &upstream_poisoned_nodes,
            );
            let has_errors = semchecked.has_errors();
            state.product = semchecked.context;
            state.poisoned_nodes = semchecked.poisoned_nodes;
            if has_errors {
                state.diagnostics.extend(
                    semchecked
                        .errors
                        .iter()
                        .map(Diagnostic::from_semcheck_error),
                );
            }
        }
        Ok(state)
    })?;

    Ok(ModulePipelineState {
        parsed,
        resolved,
        typechecked,
        semchecked,
    })
}

fn semantic_input_fingerprint(
    parsed_override: Option<&crate::core::context::ParsedContext>,
    imported_modules: &HashMap<String, ImportedModule>,
    imported_symbols: &HashMap<String, ImportedSymbol>,
    skip_typecheck: bool,
    experimental_typestate: bool,
) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    // Keep file-local and program-mode pipelines disjoint in cache identity.
    parsed_override.is_some().hash(&mut hasher);
    skip_typecheck.hash(&mut hasher);
    experimental_typestate.hash(&mut hasher);
    hash_imported_modules(&mut hasher, imported_modules);
    hash_imported_symbols(&mut hasher, imported_symbols);
    hasher.finish()
}

fn hash_imported_modules(
    hasher: &mut std::collections::hash_map::DefaultHasher,
    imported_modules: &HashMap<String, ImportedModule>,
) {
    let mut aliases: Vec<_> = imported_modules.keys().collect();
    aliases.sort();
    for alias in aliases {
        alias.hash(hasher);
        if let Some(module) = imported_modules.get(alias) {
            module.path.hash(hasher);
            let mut members: Vec<_> = module.members.iter().collect();
            members.sort();
            for member in members {
                member.hash(hasher);
            }
        }
    }
}

fn hash_imported_symbols(
    hasher: &mut std::collections::hash_map::DefaultHasher,
    imported_symbols: &HashMap<String, ImportedSymbol>,
) {
    let mut aliases: Vec<_> = imported_symbols.keys().collect();
    aliases.sort();
    for alias in aliases {
        alias.hash(hasher);
        if let Some(symbol) = imported_symbols.get(alias) {
            symbol.has_callable.hash(hasher);
            symbol.has_type.hash(hasher);
            symbol.has_trait.hash(hasher);
            if let Some(ty) = symbol.type_ty.as_ref() {
                ty.hash(hasher);
            }
            for sig in canonicalize_callable_sigs(&symbol.callable_sigs) {
                sig.hash(hasher);
            }
            if let Some(trait_sig) = symbol.trait_sig.as_ref() {
                hash_trait_sig(hasher, trait_sig);
            }
        }
    }
}

fn canonicalize_callable_sigs(callable_sigs: &[ImportedCallableSig]) -> Vec<String> {
    let mut rendered: Vec<_> = callable_sigs.iter().map(render_callable_sig).collect();
    rendered.sort();
    rendered
}

fn render_callable_sig(sig: &ImportedCallableSig) -> String {
    let params = sig
        .params
        .iter()
        .map(render_param_sig)
        .collect::<Vec<_>>()
        .join(",");
    format!("({params})->{}", sig.ret_ty)
}

fn render_param_sig(sig: &ImportedParamSig) -> String {
    format!("{}:{}", param_mode_tag(&sig.mode), sig.ty)
}

fn hash_trait_sig(
    hasher: &mut std::collections::hash_map::DefaultHasher,
    trait_sig: &ImportedTraitSig,
) {
    let mut method_names: Vec<_> = trait_sig.methods.keys().collect();
    method_names.sort();
    for name in method_names {
        name.hash(hasher);
        if let Some(method) = trait_sig.methods.get(name) {
            hash_trait_method_sig(hasher, method);
        }
    }

    let mut property_names: Vec<_> = trait_sig.properties.keys().collect();
    property_names.sort();
    for name in property_names {
        name.hash(hasher);
        if let Some(property) = trait_sig.properties.get(name) {
            hash_trait_property_sig(hasher, property);
        }
    }
}

fn hash_trait_method_sig(
    hasher: &mut std::collections::hash_map::DefaultHasher,
    method: &ImportedTraitMethodSig,
) {
    method.name.hash(hasher);
    param_mode_tag(&method.self_mode).hash(hasher);
    method.type_param_count.hash(hasher);
    for bound in &method.type_param_bounds {
        bound.hash(hasher);
    }
    for param in &method.params {
        render_param_sig(param).hash(hasher);
    }
    method.ret_ty.hash(hasher);
}

fn hash_trait_property_sig(
    hasher: &mut std::collections::hash_map::DefaultHasher,
    property: &ImportedTraitPropertySig,
) {
    property.name.hash(hasher);
    property.ty.hash(hasher);
    property.has_get.hash(hasher);
    property.has_set.hash(hasher);
}

fn param_mode_tag(mode: &crate::core::tree::ParamMode) -> u8 {
    match mode {
        crate::core::tree::ParamMode::In => 0,
        crate::core::tree::ParamMode::InOut => 1,
        crate::core::tree::ParamMode::Out => 2,
        crate::core::tree::ParamMode::Sink => 3,
    }
}

pub(crate) fn collect_sorted_diagnostics(state: &ModulePipelineState) -> Vec<Diagnostic> {
    let mut diagnostics = state.parsed.diagnostics.clone();
    diagnostics.extend(state.resolved.diagnostics.iter().cloned());
    diagnostics.extend(state.typechecked.diagnostics.iter().cloned());
    diagnostics.extend(state.semchecked.diagnostics.iter().cloned());
    diagnostics.sort_by_key(|diag| {
        (
            diag.phase,
            diag.span.start.line,
            diag.span.start.column,
            diag.code.clone(),
        )
    });
    diagnostics
}

pub(crate) fn to_lookup_state(state: &ModulePipelineState) -> LookupState {
    let mut poisoned_nodes = state.parsed.poisoned_nodes.clone();
    poisoned_nodes.extend(state.resolved.poisoned_nodes.iter().copied());
    poisoned_nodes.extend(state.typechecked.poisoned_nodes.iter().copied());
    poisoned_nodes.extend(state.semchecked.poisoned_nodes.iter().copied());

    LookupState {
        resolved: state.resolved.product.clone(),
        typed: state.typechecked.product.clone(),
        poisoned_nodes,
    }
}

#[cfg(test)]
#[path = "../../tests/analysis/t_pipeline.rs"]
mod tests;
