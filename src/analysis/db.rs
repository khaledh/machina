//! Analysis database facade.
//!
//! This composes the foundational query runtime pieces:
//! - revisioned source snapshots,
//! - query memoization/dependency tracking,
//! - module-closure invalidation.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::analysis::completion::{
    collect as collect_completions, synthesize_member_completion_source,
};
use crate::analysis::diagnostics::Diagnostic;
use crate::analysis::frontend_support::{
    SnapshotOverlayLoader, frontend_error_diagnostics, infer_project_root, stable_source_revision,
};
use crate::analysis::lookups::{
    code_actions_for_range, def_at_span, def_location_at_span, document_symbols, hover_at_span,
    semantic_tokens, signature_help_at_span, type_at_span,
};
use crate::analysis::module_graph::ModuleGraph;
use crate::analysis::pipeline::{
    LookupState, collect_sorted_diagnostics, run_module_pipeline, to_lookup_state,
};
use crate::analysis::query::{CacheStats, CancellationToken, QueryKey, QueryResult, QueryRuntime};
use crate::analysis::results::{
    CodeAction, CompletionItem, DocumentSymbol, HoverInfo, Location, RenameConflict, RenameEdit,
    RenamePlan, SemanticToken, SignatureHelp,
};
use crate::analysis::snapshot::{AnalysisSnapshot, FileId, SourceStore};
use crate::analysis::syntax_index::node_span_map;
use crate::diag::Span;
use crate::frontend::program::flatten_program;
use crate::frontend::{self, ModuleId, ModulePath};
use crate::resolve::DefId;
use crate::tree::NodeId;
use crate::types::Type;

#[derive(Default)]
pub struct AnalysisDb {
    runtime: QueryRuntime,
    sources: SourceStore,
    module_graph: ModuleGraph,
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
        let source_for_pipeline = source.clone();
        self.execute_query(diagnostics_key, move |rt| {
            let state = run_module_pipeline(rt, module_id, revision, source_for_pipeline)?;
            Ok(collect_sorted_diagnostics(&state))
        })
    }

    /// Program-aware diagnostics for an entry file, loading dependency modules from
    /// disk or snapshot overlays.
    pub fn diagnostics_for_program_path(&mut self, path: &Path) -> QueryResult<Vec<Diagnostic>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(Vec::new());
        };
        self.diagnostics_for_program_file(file_id)
    }

    /// Program-aware diagnostics for an entry file id.
    ///
    /// This is intended for IDE/check workflows where dependency overlays may exist.
    pub fn diagnostics_for_program_file(
        &mut self,
        file_id: FileId,
    ) -> QueryResult<Vec<Diagnostic>> {
        let snapshot = self.snapshot();
        let Some(entry_source) = snapshot.text(file_id) else {
            return Ok(Vec::new());
        };
        let Some(entry_path) = snapshot.path(file_id).map(Path::to_path_buf) else {
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
            let project_root = infer_project_root(&entry_path);
            let entry_module_path = match ModulePath::from_file(&entry_path, &project_root) {
                Ok(path) => path,
                Err(err) => return Ok(frontend_error_diagnostics(err)),
            };
            let loader = SnapshotOverlayLoader::new(snapshot.clone(), project_root);

            let program = match frontend::discover_and_parse_program_with_loader(
                &entry_source,
                &entry_path,
                entry_module_path,
                &loader,
            ) {
                Ok(program) => program,
                Err(err) => return Ok(frontend_error_diagnostics(err)),
            };
            let program_context = crate::context::ProgramParsedContext::new(program);

            if let Err(errs) = flatten_program(&program_context) {
                let mut diagnostics = Vec::new();
                for err in errs {
                    diagnostics.extend(frontend_error_diagnostics(err));
                }
                diagnostics.sort_by_key(|diag| {
                    (
                        diag.phase,
                        diag.span.start.line,
                        diag.span.start.column,
                        diag.code.clone(),
                    )
                });
                return Ok(diagnostics);
            }

            let mut all = Vec::new();
            for module_id in program_context.dependency_order_from_entry() {
                let Some(parsed) = program_context.module(module_id) else {
                    continue;
                };
                let source = std::sync::Arc::<str>::from(parsed.source.source.as_str());
                let module_revision = stable_source_revision(&parsed.source.source);
                let state = run_module_pipeline(rt, module_id, module_revision, source)?;
                all.extend(collect_sorted_diagnostics(&state));
            }
            all.sort_by_key(|diag| {
                (
                    diag.phase,
                    diag.span.start.line,
                    diag.span.start.column,
                    diag.code.clone(),
                )
            });
            Ok(all)
        })
    }

    pub fn poisoned_nodes_for_path(&mut self, path: &Path) -> QueryResult<HashSet<NodeId>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(HashSet::new());
        };
        self.poisoned_nodes_for_file(file_id)
    }

    pub fn poisoned_nodes_for_file(&mut self, file_id: FileId) -> QueryResult<HashSet<NodeId>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(state.poisoned_nodes)
    }

    pub fn def_at_path(&mut self, path: &Path, query_span: Span) -> QueryResult<Option<DefId>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(None);
        };
        self.def_at_file(file_id, query_span)
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
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(None);
        };
        self.def_location_at_file(file_id, query_span)
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

    pub fn type_at_path(&mut self, path: &Path, query_span: Span) -> QueryResult<Option<Type>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(None);
        };
        self.type_at_file(file_id, query_span)
    }

    pub fn type_at_file(&mut self, file_id: FileId, query_span: Span) -> QueryResult<Option<Type>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(type_at_span(&state, query_span))
    }

    pub fn hover_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(None);
        };
        self.hover_at_file(file_id, query_span)
    }

    pub fn hover_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(hover_at_span(&state, query_span))
    }

    pub fn completions_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(Vec::new());
        };
        self.completions_at_file(file_id, query_span)
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
        let cursor = Span {
            start: query_span.end,
            end: query_span.end,
        };

        let mut active_source = source;
        let mut state = self.lookup_state_for_file(file_id)?;
        if state.resolved.is_none()
            && let Some(synthetic_source) =
                synthesize_member_completion_source(&active_source, cursor.start)
            && let Some(synthetic_state) =
                self.lookup_state_for_source(file_id, synthetic_source.clone())?
        {
            active_source = synthetic_source;
            state = synthetic_state;
        }

        let LookupState {
            resolved,
            typed,
            poisoned_nodes: _,
        } = state;
        let Some(resolved) = resolved else {
            return Ok(Vec::new());
        };
        let mut out = collect_completions(&active_source, cursor, &resolved, typed.as_ref());
        out.sort_by(|a, b| a.label.cmp(&b.label).then(a.def_id.0.cmp(&b.def_id.0)));
        out.dedup_by(|a, b| a.label == b.label && a.kind == b.kind && a.def_id == b.def_id);
        Ok(out)
    }

    pub fn signature_help_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(None);
        };
        self.signature_help_at_file(file_id, query_span)
    }

    pub fn signature_help_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(signature_help_at_span(&state, query_span))
    }

    pub fn document_symbols_at_path(&mut self, path: &Path) -> QueryResult<Vec<DocumentSymbol>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(Vec::new());
        };
        self.document_symbols_at_file(file_id)
    }

    pub fn document_symbols_at_file(
        &mut self,
        file_id: FileId,
    ) -> QueryResult<Vec<DocumentSymbol>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(document_symbols(&state))
    }

    pub fn semantic_tokens_at_path(&mut self, path: &Path) -> QueryResult<Vec<SemanticToken>> {
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(Vec::new());
        };
        self.semantic_tokens_at_file(file_id)
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
        let snapshot = self.snapshot();
        let Some(file_id) = snapshot.file_id(path) else {
            return Ok(Vec::new());
        };
        self.code_actions_at_file(file_id, range)
    }

    pub fn code_actions_at_file(
        &mut self,
        file_id: FileId,
        range: Span,
    ) -> QueryResult<Vec<CodeAction>> {
        let diagnostics = self.diagnostics_for_file(file_id)?;
        Ok(code_actions_for_range(&diagnostics, range))
    }

    pub fn references(&mut self, def_id: DefId) -> QueryResult<Vec<Location>> {
        let snapshot = self.snapshot();
        let mut out = Vec::new();

        for file_id in snapshot.file_ids() {
            let state = self.lookup_state_for_file(file_id)?;
            let Some(resolved) = state.resolved else {
                continue;
            };

            let node_spans = node_span_map(&resolved.module);
            for (node_id, mapped_def_id) in resolved.def_table.node_def_entries() {
                if mapped_def_id != def_id {
                    continue;
                }
                let Some(span) = node_spans.get(&node_id).copied() else {
                    continue;
                };
                out.push(Location {
                    file_id,
                    path: snapshot.path(file_id).map(Path::to_path_buf),
                    span,
                });
            }
        }

        out.sort_by_key(|loc| {
            (
                loc.path.clone(),
                loc.span.start.line,
                loc.span.start.column,
                loc.span.end.line,
                loc.span.end.column,
                loc.file_id,
            )
        });
        out.dedup();
        Ok(out)
    }

    pub fn rename_plan(&mut self, def_id: DefId, new_name: &str) -> QueryResult<RenamePlan> {
        let references = self.references(def_id)?;
        let mut plan = RenamePlan {
            def_id,
            old_name: None,
            new_name: new_name.to_string(),
            edits: Vec::new(),
            conflicts: Vec::new(),
        };

        if !is_identifier_name(new_name) {
            plan.conflicts.push(RenameConflict {
                message: format!("`{new_name}` is not a valid identifier"),
                existing_def: None,
            });
            return Ok(plan);
        }

        let snapshot = self.snapshot();
        let mut owner_module = None;
        let mut existing_same_name = HashSet::new();
        for file_id in snapshot.file_ids() {
            let state = self.lookup_state_for_file(file_id)?;
            let Some(resolved) = state.resolved else {
                continue;
            };

            if plan.old_name.is_none()
                && let Some(def) = resolved.def_table.lookup_def(def_id)
            {
                plan.old_name = Some(def.name.clone());
                owner_module = resolved
                    .def_owners
                    .get(&def_id)
                    .copied()
                    .or(Some(ModuleId(file_id.0)));
            }

            let Some(target_owner) = owner_module else {
                continue;
            };
            for def in resolved.def_table.defs() {
                if def.id == def_id || def.name != new_name {
                    continue;
                }
                let owner = resolved
                    .def_owners
                    .get(&def.id)
                    .copied()
                    .or(Some(ModuleId(file_id.0)));
                if owner == Some(target_owner) && existing_same_name.insert(def.id) {
                    plan.conflicts.push(RenameConflict {
                        message: format!("name `{new_name}` already exists as {}", def.kind),
                        existing_def: Some(def.id),
                    });
                }
            }
        }

        if let Some(old_name) = &plan.old_name
            && old_name == new_name
        {
            plan.conflicts.push(RenameConflict {
                message: "new name matches existing symbol name".to_string(),
                existing_def: Some(def_id),
            });
        }

        for location in references {
            plan.edits.push(RenameEdit {
                location,
                replacement: new_name.to_string(),
            });
        }

        Ok(plan)
    }

    fn lookup_state_for_file(&mut self, file_id: FileId) -> QueryResult<LookupState> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id) else {
            return Ok(LookupState::default());
        };
        let revision = snapshot.revision();
        let module_id = ModuleId(file_id.0);

        let lookup_key = QueryKey::new(
            crate::analysis::query::QueryKind::LookupState,
            module_id,
            revision,
        );
        self.execute_query(lookup_key, move |rt| {
            let state = run_module_pipeline(rt, module_id, revision, source)?;
            Ok(to_lookup_state(&state))
        })
    }

    fn lookup_state_for_source(
        &mut self,
        file_id: FileId,
        source: String,
    ) -> QueryResult<Option<LookupState>> {
        let snapshot = self.snapshot();
        if source.is_empty() {
            return Ok(None);
        }

        let revision = snapshot.revision() ^ stable_source_revision(&source) | (1u64 << 63);
        let module_id = ModuleId(file_id.0);
        let pipeline = run_module_pipeline(
            &mut self.runtime,
            module_id,
            revision,
            std::sync::Arc::<str>::from(source),
        )?;
        Ok(Some(to_lookup_state(&pipeline)))
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

fn is_identifier_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

#[cfg(test)]
#[path = "../tests/analysis/t_analysis_db.rs"]
mod tests;
