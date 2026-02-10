//! Shared analysis pipeline helpers.
//!
//! This module centralizes the repeated lex -> parse -> resolve -> typecheck
//! orchestration used by analysis queries, while preserving per-stage
//! diagnostics and query cache boundaries.

use std::sync::Arc;

use crate::analysis::diagnostics::Diagnostic;
use crate::analysis::query::{QueryKey, QueryKind, QueryResult, QueryRuntime};
use crate::frontend::ModuleId;
use crate::lexer::{LexError, Lexer};
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::tree::NodeIdGen;
use crate::typecheck::type_check;

#[derive(Clone, Default)]
pub(crate) struct ParseDiagState {
    pub parsed: Option<crate::context::ParsedContext>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, Default)]
pub(crate) struct ResolveDiagState {
    pub resolved: Option<crate::context::ResolvedContext>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, Default)]
pub(crate) struct TypecheckDiagState {
    pub typed: Option<crate::context::TypeCheckedContext>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, Default)]
pub(crate) struct ModulePipelineState {
    pub parsed: ParseDiagState,
    pub resolved: ResolveDiagState,
    pub typechecked: TypecheckDiagState,
}

#[derive(Clone, Default)]
pub(crate) struct LookupState {
    pub resolved: Option<crate::context::ResolvedContext>,
    pub typed: Option<crate::context::TypeCheckedContext>,
}

pub(crate) fn run_module_pipeline(
    rt: &mut QueryRuntime,
    module_id: ModuleId,
    revision: u64,
    source: Arc<str>,
) -> QueryResult<ModulePipelineState> {
    let parse_key = QueryKey::new(QueryKind::ParseModule, module_id, revision);
    let source_for_parse = source.clone();
    let parsed = rt.execute(parse_key, move |_rt| {
        let mut state = ParseDiagState::default();
        let lexer = Lexer::new(&source_for_parse);
        let tokens = match lexer.tokenize().collect::<Result<Vec<_>, LexError>>() {
            Ok(tokens) => tokens,
            Err(error) => {
                state.diagnostics.push(Diagnostic::from_lex_error(&error));
                return Ok(state);
            }
        };

        let id_gen = NodeIdGen::new();
        let mut parser = Parser::new_with_id_gen(&tokens, id_gen);
        match parser.parse() {
            Ok(module) => {
                state.parsed = Some(crate::context::ParsedContext::new(
                    module,
                    parser.into_id_gen(),
                ));
            }
            Err(error) => state.diagnostics.push(Diagnostic::from_parse_error(&error)),
        }
        Ok(state)
    })?;

    let resolve_key = QueryKey::new(QueryKind::ResolveModule, module_id, revision);
    let resolve_input = parsed.parsed.clone();
    let resolved = rt.execute(resolve_key, move |_rt| {
        let mut state = ResolveDiagState::default();
        if let Some(parsed) = resolve_input {
            match resolve(parsed) {
                Ok(resolved) => state.resolved = Some(resolved),
                Err(errors) => {
                    state
                        .diagnostics
                        .extend(errors.iter().map(Diagnostic::from_resolve_error));
                }
            }
        }
        Ok(state)
    })?;

    let typecheck_key = QueryKey::new(QueryKind::TypecheckModule, module_id, revision);
    let typecheck_input = resolved.resolved.clone();
    let typechecked = rt.execute(typecheck_key, move |_rt| {
        let mut state = TypecheckDiagState::default();
        if let Some(resolved) = typecheck_input {
            match type_check(resolved) {
                Ok(typed) => state.typed = Some(typed),
                Err(errors) => {
                    state
                        .diagnostics
                        .extend(errors.iter().map(Diagnostic::from_typecheck_error));
                }
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
    LookupState {
        resolved: state.resolved.resolved.clone(),
        typed: state.typechecked.typed.clone(),
    }
}
