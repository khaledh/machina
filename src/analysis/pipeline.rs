//! Shared analysis pipeline helpers.
//!
//! This module centralizes the repeated lex -> parse -> resolve -> typecheck
//! orchestration used by analysis queries, while preserving per-stage
//! diagnostics and query cache boundaries.

use std::collections::HashSet;
use std::sync::Arc;

use crate::analysis::diagnostics::Diagnostic;
use crate::analysis::query::{QueryKey, QueryKind, QueryResult, QueryRuntime};
use crate::frontend::ModuleId;
use crate::lexer::{LexError, Lexer};
use crate::parse::Parser;
use crate::resolve::resolve_partial;
use crate::tree::NodeId;
use crate::tree::NodeIdGen;
use crate::typecheck::type_check_partial;

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

pub(crate) type ParseStageOutput = StageOutput<crate::context::ParsedContext>;
pub(crate) type ResolveStageOutput = StageOutput<crate::context::ResolvedContext>;
pub(crate) type TypecheckStageOutput = StageOutput<crate::context::TypeCheckedContext>;

#[derive(Clone, Default)]
pub(crate) struct ModulePipelineState {
    pub parsed: ParseStageOutput,
    pub resolved: ResolveStageOutput,
    pub typechecked: TypecheckStageOutput,
}

#[derive(Clone, Default)]
pub(crate) struct LookupState {
    pub resolved: Option<crate::context::ResolvedContext>,
    pub typed: Option<crate::context::TypeCheckedContext>,
    pub poisoned_nodes: HashSet<NodeId>,
}

pub(crate) fn run_module_pipeline(
    rt: &mut QueryRuntime,
    module_id: ModuleId,
    revision: u64,
    source: Arc<str>,
) -> QueryResult<ModulePipelineState> {
    run_module_pipeline_with_parsed(rt, module_id, revision, source, None)
}

pub(crate) fn run_module_pipeline_with_parsed(
    rt: &mut QueryRuntime,
    module_id: ModuleId,
    revision: u64,
    source: Arc<str>,
    parsed_override: Option<crate::context::ParsedContext>,
) -> QueryResult<ModulePipelineState> {
    let parse_key = QueryKey::new(QueryKind::ParseModule, module_id, revision);
    let source_for_parse = source.clone();
    let parsed_override_for_parse = parsed_override.clone();
    let parsed = rt.execute(parse_key, move |_rt| {
        let mut state = ParseStageOutput::default();
        if let Some(parsed_context) = parsed_override_for_parse {
            state.product = Some(parsed_context);
            return Ok(state);
        }

        let lexer = Lexer::new(&source_for_parse);
        let tokens = match lexer.tokenize().collect::<Result<Vec<_>, LexError>>() {
            Ok(tokens) => tokens,
            Err(error) => {
                state.diagnostics.push(Diagnostic::from_lex_error(&error));
                state.poisoned_nodes.insert(ROOT_POISON_NODE);
                return Ok(state);
            }
        };

        let id_gen = NodeIdGen::new();
        let mut parser = Parser::new_with_id_gen(&tokens, id_gen);
        match parser.parse() {
            Ok(module) => {
                state.product = Some(crate::context::ParsedContext::new(
                    module,
                    parser.into_id_gen(),
                ));
            }
            Err(error) => {
                state.diagnostics.push(Diagnostic::from_parse_error(&error));
                state.poisoned_nodes.insert(ROOT_POISON_NODE);
            }
        }
        Ok(state)
    })?;

    let resolve_key = QueryKey::new(QueryKind::ResolveModule, module_id, revision);
    let resolve_input = parsed.product.clone();
    let resolved = rt.execute(resolve_key, move |_rt| {
        let mut state = ResolveStageOutput::default();
        if let Some(parsed) = resolve_input {
            let resolved = resolve_partial(parsed);
            state.product = Some(resolved.context);
            if !resolved.errors.is_empty() {
                state
                    .diagnostics
                    .extend(resolved.errors.iter().map(Diagnostic::from_resolve_error));
                state.poisoned_nodes.insert(ROOT_POISON_NODE);
            }
        }
        Ok(state)
    })?;

    let typecheck_key = QueryKey::new(QueryKind::TypecheckModule, module_id, revision);
    // Do not run type checking when resolution emitted errors. This keeps
    // lookup behavior stable (no leaked inference vars on unresolved symbols).
    let typecheck_input = if resolved.diagnostics.is_empty() {
        resolved.product.clone()
    } else {
        None
    };
    let typechecked = rt.execute(typecheck_key, move |_rt| {
        let mut state = TypecheckStageOutput::default();
        if let Some(resolved) = typecheck_input {
            let typed = type_check_partial(resolved);
            state.product = Some(typed.context);
            if !typed.errors.is_empty() {
                state
                    .diagnostics
                    .extend(typed.errors.iter().map(Diagnostic::from_typecheck_error));
                state.poisoned_nodes.insert(ROOT_POISON_NODE);
            }
        }
        Ok(state)
    })?;

    Ok(ModulePipelineState {
        parsed,
        resolved,
        typechecked,
    })
}

pub(crate) fn collect_sorted_diagnostics(state: &ModulePipelineState) -> Vec<Diagnostic> {
    let mut diagnostics = state.parsed.diagnostics.clone();
    diagnostics.extend(state.resolved.diagnostics.iter().cloned());
    diagnostics.extend(state.typechecked.diagnostics.iter().cloned());
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

    LookupState {
        resolved: state.resolved.product.clone(),
        typed: state.typechecked.product.clone(),
        poisoned_nodes,
    }
}

#[cfg(test)]
#[path = "../tests/analysis/t_pipeline.rs"]
mod tests;
