//! Analysis database facade.
//!
//! This composes the foundational query runtime pieces:
//! - revisioned source snapshots,
//! - query memoization/dependency tracking,
//! - module-closure invalidation.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::analysis::diagnostics::Diagnostic;
use crate::analysis::module_graph::ModuleGraph;
use crate::analysis::query::{CacheStats, CancellationToken, QueryKey, QueryResult, QueryRuntime};
use crate::analysis::snapshot::{AnalysisSnapshot, FileId, SourceStore};
use crate::frontend::ModuleId;
use crate::lexer::{LexError, Lexer};
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::tree::NodeIdGen;
use crate::typecheck::type_check;

#[derive(Default)]
pub struct AnalysisDb {
    runtime: QueryRuntime,
    sources: SourceStore,
    module_graph: ModuleGraph,
}

#[derive(Clone, Default)]
struct ParseDiagState {
    parsed: Option<crate::context::ParsedContext>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, Default)]
struct ResolveDiagState {
    resolved: Option<crate::context::ResolvedContext>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, Default)]
struct TypecheckDiagState {
    diagnostics: Vec<Diagnostic>,
}

impl AnalysisDb {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_cancellation_token(token: CancellationToken) -> Self {
        Self {
            runtime: QueryRuntime::with_cancellation_token(token),
            ..Self::default()
        }
    }

    pub fn cancellation_token(&self) -> CancellationToken {
        self.runtime.cancellation_token()
    }

    pub fn set_cancellation_token(&mut self, token: CancellationToken) {
        self.runtime.set_cancellation_token(token);
    }

    pub fn set_module_graph(&mut self, graph: ModuleGraph) {
        self.module_graph = graph;
    }

    pub fn module_graph(&self) -> &ModuleGraph {
        &self.module_graph
    }

    pub fn upsert_disk_text<S>(&mut self, path: PathBuf, text: S) -> FileId
    where
        S: Into<std::sync::Arc<str>>,
    {
        self.sources.upsert_disk_text(path, text)
    }

    pub fn set_overlay<S>(&mut self, file_id: FileId, text: S)
    where
        S: Into<std::sync::Arc<str>>,
    {
        self.sources.set_overlay(file_id, text);
    }

    pub fn clear_overlay(&mut self, file_id: FileId) {
        self.sources.clear_overlay(file_id);
    }

    pub fn snapshot(&self) -> AnalysisSnapshot {
        self.sources.snapshot()
    }

    pub fn diagnostics_for_path(&mut self, path: &Path) -> QueryResult<Vec<Diagnostic>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(Vec::new());
        };
        self.diagnostics_for_file(file_id)
    }

    pub fn diagnostics_for_file(&mut self, file_id: FileId) -> QueryResult<Vec<Diagnostic>> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id) else {
            return Ok(Vec::new());
        };
        let revision = snapshot.revision();
        let module_id = ModuleId(file_id.0);

        let diagnostics_key = QueryKey::new(
            crate::analysis::query::QueryKind::Diagnostics,
            module_id,
            revision,
        );
        self.execute_query(diagnostics_key, move |rt| {
            let parse_key = QueryKey::new(
                crate::analysis::query::QueryKind::ParseModule,
                module_id,
                revision,
            );
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

            let resolve_key = QueryKey::new(
                crate::analysis::query::QueryKind::ResolveModule,
                module_id,
                revision,
            );
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

            let typecheck_key = QueryKey::new(
                crate::analysis::query::QueryKind::TypecheckModule,
                module_id,
                revision,
            );
            let typecheck_input = resolved.resolved.clone();
            let typechecked = rt.execute(typecheck_key, move |_rt| {
                let mut state = TypecheckDiagState::default();
                if let Some(resolved) = typecheck_input
                    && let Err(errors) = type_check(resolved)
                {
                    state
                        .diagnostics
                        .extend(errors.iter().map(Diagnostic::from_typecheck_error));
                }
                Ok(state)
            })?;

            let mut diagnostics = parsed.diagnostics;
            diagnostics.extend(resolved.diagnostics);
            diagnostics.extend(typechecked.diagnostics);

            diagnostics.sort_by_key(|diag| {
                (
                    diag.phase,
                    diag.span.start.line,
                    diag.span.start.column,
                    diag.code.clone(),
                )
            });
            Ok(diagnostics)
        })
    }

    pub fn execute_query<T, F>(&mut self, key: QueryKey, compute: F) -> QueryResult<T>
    where
        T: Clone + Send + Sync + 'static,
        F: FnOnce(&mut QueryRuntime) -> QueryResult<T>,
    {
        self.runtime.execute(key, compute)
    }

    pub fn cache_stats(&self) -> CacheStats {
        self.runtime.cache_stats()
    }

    pub fn clear_cache_stats(&mut self) {
        self.runtime.clear_stats();
    }

    pub fn invalidate_query(&mut self, key: QueryKey) {
        self.runtime.invalidate(key);
    }

    /// Invalidate changed modules and all transitive dependents.
    pub fn invalidate_changed_modules(&mut self, changed: &HashSet<ModuleId>) {
        let impacted = self.module_graph.invalidation_closure(changed);
        self.runtime.invalidate_modules(&impacted);
    }
}

#[cfg(test)]
#[path = "../tests/analysis/t_analysis_db.rs"]
mod tests;
