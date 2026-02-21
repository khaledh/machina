//! Analysis database facade.
//!
//! This composes the foundational query runtime pieces:
//! - revisioned source snapshots,
//! - query memoization/dependency tracking,
//! - module-closure invalidation.

mod completion_helpers;
mod pipeline_helpers;
mod signature_helpers;

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::core::capsule::ModuleId;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, GlobalDefId};
use crate::core::tree::NodeId;
use crate::core::types::Type;
use crate::services::analysis::diagnostics::Diagnostic;
use crate::services::analysis::lookups::{
    code_actions_for_range, def_at_span, def_location_at_span, document_symbols,
    hover_at_span_in_file, semantic_tokens, type_at_span,
};
use crate::services::analysis::module_graph::ModuleGraph;
use crate::services::analysis::pipeline::{
    collect_sorted_diagnostics, run_module_pipeline_with_query_input,
};
use crate::services::analysis::program_pipeline::{
    resolve_imported_symbol_target_from_import_env,
};
use crate::services::analysis::query::{
    CacheStats, CancellationToken, QueryKey, QueryResult, QueryRuntime,
};
use crate::services::analysis::rename::{
    references as collect_references, rename_plan as build_rename_plan,
};
use crate::services::analysis::results::{
    CodeAction, CompletionItem, DocumentSymbol, HoverInfo, Location, RenamePlan, SemanticToken,
    SignatureHelp,
};
use crate::services::analysis::snapshot::{AnalysisSnapshot, FileId, SourceStore};

#[derive(Default)]
pub struct AnalysisDb {
    runtime: QueryRuntime,
    sources: SourceStore,
    module_graph: ModuleGraph,
    experimental_typestate: bool,
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

    pub fn set_experimental_typestate(&mut self, enabled: bool) {
        self.experimental_typestate = enabled;
    }

    pub fn experimental_typestate(&self) -> bool {
        self.experimental_typestate
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

    fn with_file_id<R, F, D>(&mut self, path: &Path, default: D, f: F) -> QueryResult<R>
    where
        F: FnOnce(&mut Self, FileId) -> QueryResult<R>,
        D: FnOnce() -> R,
    {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(default());
        };
        f(self, file_id)
    }

    pub fn diagnostics_for_path(&mut self, path: &Path) -> QueryResult<Vec<Diagnostic>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.diagnostics_for_file(file_id)
        })
    }

    pub fn diagnostics_for_file(&mut self, file_id: FileId) -> QueryResult<Vec<Diagnostic>> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id) else {
            return Ok(Vec::new());
        };
        let revision = snapshot.revision();
        let module_id = ModuleId(file_id.0);
        let experimental_typestate = self.experimental_typestate;
        let query_input = if experimental_typestate { 1 } else { 0 };

        let diagnostics_key = QueryKey::with_input(
            crate::services::analysis::query::QueryKind::Diagnostics,
            module_id,
            revision,
            query_input,
        );
        let source_for_pipeline = source.clone();
        self.execute_query(diagnostics_key, move |rt| {
            let state = run_module_pipeline_with_query_input(
                rt,
                module_id,
                revision,
                source_for_pipeline,
                query_input,
                experimental_typestate,
            )?;
            Ok(collect_sorted_diagnostics(&state))
        })
    }

    /// Program-aware diagnostics for an entry file, loading dependency modules from
    /// disk or snapshot overlays.
    pub fn diagnostics_for_program_path(&mut self, path: &Path) -> QueryResult<Vec<Diagnostic>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.diagnostics_for_program_file(file_id)
        })
    }

    /// Program-aware diagnostics for an entry file id.
    ///
    /// This is intended for IDE/check workflows where dependency overlays may exist.
    pub fn diagnostics_for_program_file(
        &mut self,
        file_id: FileId,
    ) -> QueryResult<Vec<Diagnostic>> {
        Ok(self.program_pipeline_for_file(file_id)?.diagnostics)
    }

    pub fn poisoned_nodes_for_path(&mut self, path: &Path) -> QueryResult<HashSet<NodeId>> {
        self.with_file_id(path, HashSet::new, |db, file_id| {
            db.poisoned_nodes_for_file(file_id)
        })
    }

    pub fn poisoned_nodes_for_file(&mut self, file_id: FileId) -> QueryResult<HashSet<NodeId>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(state.poisoned_nodes)
    }

    pub fn def_at_path(&mut self, path: &Path, query_span: Span) -> QueryResult<Option<DefId>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.def_at_file(file_id, query_span),
        )
    }

    pub fn def_at_file(&mut self, file_id: FileId, query_span: Span) -> QueryResult<Option<DefId>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(def_at_span(&state, query_span))
    }

    pub fn def_location_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<Location>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.def_location_at_file(file_id, query_span),
        )
    }

    pub fn def_location_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<Location>> {
        let snapshot = self.snapshot();
        let state = self.lookup_state_for_file(file_id)?;
        Ok(def_location_at_span(&snapshot, file_id, &state, query_span))
    }

    pub fn def_location_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<Location>> {
        let snapshot = self.snapshot();
        let program_lookup = self.program_pipeline_for_file(file_id)?;
        let Some(entry_module_id) = program_lookup.entry_module_id else {
            return Ok(None);
        };
        let module_states = &program_lookup.module_states;
        let Some(entry_state) = module_states.get(&entry_module_id) else {
            return Ok(None);
        };
        let Some(def_id) = def_at_span(entry_state, query_span) else {
            return Ok(None);
        };
        let Some(entry_resolved) = entry_state.resolved.as_ref() else {
            return Ok(None);
        };

        let target = entry_resolved
            .def_table
            .lookup_def(def_id)
            .and_then(|def| {
                resolve_imported_symbol_target_from_import_env(
                    entry_module_id,
                    def,
                    &program_lookup.import_env_by_module,
                )
            })
            .unwrap_or_else(|| GlobalDefId::new(entry_module_id, def_id));
        let Some(target_state) = module_states.get(&target.module_id) else {
            return Ok(None);
        };
        let Some(target_resolved) = target_state.resolved.as_ref() else {
            return Ok(None);
        };
        let Some(loc) = target_resolved.def_table.lookup_def_location(target.def_id) else {
            return Ok(None);
        };
        let target_file_id = loc
            .path
            .as_deref()
            .and_then(|path| snapshot.file_id(path))
            .unwrap_or(file_id);
        Ok(Some(Location {
            file_id: target_file_id,
            path: loc.path,
            span: loc.span,
        }))
    }

    pub fn type_at_path(&mut self, path: &Path, query_span: Span) -> QueryResult<Option<Type>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.type_at_file(file_id, query_span),
        )
    }

    pub fn type_at_file(&mut self, file_id: FileId, query_span: Span) -> QueryResult<Option<Type>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(type_at_span(&state, query_span))
    }

    pub fn type_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<Type>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.type_at_program_file(file_id, query_span),
        )
    }

    pub fn type_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<Type>> {
        let state = if let Some(state) = self.entry_lookup_state_for_program_file(file_id)? {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        Ok(type_at_span(&state, query_span))
    }

    pub fn hover_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.hover_at_file(file_id, query_span),
        )
    }

    pub fn hover_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        let state = self.lookup_state_for_file(file_id)?;
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id);
        Ok(hover_at_span_in_file(
            &state,
            query_span,
            snapshot.path(file_id),
            source.as_deref(),
        ))
    }

    pub fn hover_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.hover_at_program_file(file_id, query_span),
        )
    }

    pub fn hover_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        let program = self.program_pipeline_for_file(file_id)?;
        let state = if let Some(state) = program
            .entry_module_id
            .and_then(|entry| program.module_states.get(&entry).cloned())
        {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id);
        Ok(hover_at_span_in_file(
            &state,
            query_span,
            snapshot.path(file_id),
            source.as_deref(),
        ))
    }

    pub fn completions_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.completions_at_file(file_id, query_span)
        })
    }

    pub fn completions_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id).map(|s| s.to_string()) else {
            return Ok(Vec::new());
        };
        let state = self.lookup_state_for_file(file_id)?;
        self.completions_for_state(file_id, query_span, source, state)
    }

    pub fn completions_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.completions_at_program_file(file_id, query_span)
        })
    }

    pub fn completions_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id).map(|s| s.to_string()) else {
            return Ok(Vec::new());
        };
        let program = self.program_pipeline_for_file(file_id)?;
        let state = if let Some(state) = program
            .entry_module_id
            .and_then(|entry| program.module_states.get(&entry).cloned())
        {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        self.completions_for_state(file_id, query_span, source, state)
    }

    pub fn signature_help_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.signature_help_at_file(file_id, query_span),
        )
    }

    pub fn signature_help_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id).map(|s| s.to_string());
        let state = self.lookup_state_for_file(file_id)?;
        if let Some(sig) = self.signature_help_for_state(&state, query_span, source.as_deref()) {
            return Ok(Some(sig));
        }
        self.signature_help_with_synthetic_fallback(file_id, query_span)
    }

    pub fn signature_help_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.signature_help_at_program_file(file_id, query_span),
        )
    }

    pub fn signature_help_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id).map(|s| s.to_string());
        let state = if let Some(state) = self.entry_lookup_state_for_program_file(file_id)? {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        if let Some(sig) = self.signature_help_for_state(&state, query_span, source.as_deref()) {
            return Ok(Some(sig));
        }
        if let Some(sig) = self.signature_help_with_synthetic_fallback(file_id, query_span)? {
            return Ok(Some(sig));
        }
        // Best-effort fallback: when program-aware state is unavailable due
        // transient graph/import failures while editing, keep signature help
        // useful by trying the file-local pipeline.
        self.signature_help_at_file(file_id, query_span)
    }

    pub fn document_symbols_at_path(&mut self, path: &Path) -> QueryResult<Vec<DocumentSymbol>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.document_symbols_at_file(file_id)
        })
    }

    pub fn document_symbols_at_file(
        &mut self,
        file_id: FileId,
    ) -> QueryResult<Vec<DocumentSymbol>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(document_symbols(&state))
    }

    pub fn semantic_tokens_at_path(&mut self, path: &Path) -> QueryResult<Vec<SemanticToken>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.semantic_tokens_at_file(file_id)
        })
    }

    pub fn semantic_tokens_at_file(&mut self, file_id: FileId) -> QueryResult<Vec<SemanticToken>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(semantic_tokens(&state))
    }

    pub fn code_actions_at_path(
        &mut self,
        path: &Path,
        range: Span,
    ) -> QueryResult<Vec<CodeAction>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.code_actions_at_file(file_id, range)
        })
    }

    pub fn code_actions_at_file(
        &mut self,
        file_id: FileId,
        range: Span,
    ) -> QueryResult<Vec<CodeAction>> {
        let diagnostics = self.diagnostics_for_file(file_id)?;
        self.code_actions_for_diagnostics_at_file(file_id, range, diagnostics)
    }

    pub fn code_actions_for_diagnostics_at_file(
        &mut self,
        file_id: FileId,
        range: Span,
        diagnostics: Vec<Diagnostic>,
    ) -> QueryResult<Vec<CodeAction>> {
        let source = self.snapshot().text(file_id);
        Ok(code_actions_for_range(
            &diagnostics,
            range,
            source.as_deref(),
        ))
    }

    pub fn references(&mut self, def_id: DefId) -> QueryResult<Vec<Location>> {
        let snapshot = self.snapshot();
        collect_references(&snapshot, def_id, |file_id| {
            self.lookup_state_for_file(file_id)
        })
    }

    pub fn rename_plan(&mut self, def_id: DefId, new_name: &str) -> QueryResult<RenamePlan> {
        let snapshot = self.snapshot();
        build_rename_plan(&snapshot, def_id, new_name, |file_id| {
            self.lookup_state_for_file(file_id)
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
#[path = "../../tests/analysis/t_analysis_db.rs"]
mod tests;
