//! Analysis database facade.
//!
//! This composes the foundational query runtime pieces:
//! - revisioned source snapshots,
//! - query memoization/dependency tracking,
//! - module-closure invalidation.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use crate::core::capsule::compose::merge_modules;
use crate::core::capsule::{self, ModuleId, ModulePath, RequireKind};
use crate::core::context::{
    ImportEnv, ImportedSymbolBinding, ModuleExportFacts, ModuleImportBinding,
};
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefKind, DefLocation, GlobalDefId};
use crate::core::tree::NodeId;
use crate::core::types::Type;
use crate::core::{api, resolve};
use crate::services::analysis::completion::{
    collect as collect_completions, synthesize_member_completion_source,
};
use crate::services::analysis::diagnostics::{ANALYSIS_FILE_PATH_KEY, Diagnostic, DiagnosticValue};
use crate::services::analysis::frontend_support::{
    SnapshotOverlayLoader, frontend_error_diagnostics, infer_project_root, stable_source_revision,
};
use crate::services::analysis::lookups::{
    code_actions_for_range, def_at_span, def_location_at_span, document_symbols, hover_at_span,
    semantic_tokens, signature_help_at_span, type_at_span,
};
use crate::services::analysis::module_graph::ModuleGraph;
use crate::services::analysis::pipeline::{
    LookupState, collect_sorted_diagnostics, run_module_pipeline,
    run_module_pipeline_with_parsed_and_imports, to_lookup_state,
};
use crate::services::analysis::program_imports::ProgramImportFactsCache;
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
use crate::services::analysis::signature_help::synthesize_call_signature_sources;
use crate::services::analysis::snapshot::{AnalysisSnapshot, FileId, SourceStore};

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

        let diagnostics_key = QueryKey::new(
            crate::services::analysis::query::QueryKind::Diagnostics,
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
        Ok(hover_at_span(&state, query_span))
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
        Ok(hover_at_span(&state, query_span))
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

    fn completions_for_state(
        &mut self,
        file_id: FileId,
        query_span: Span,
        source: String,
        mut state: LookupState,
    ) -> QueryResult<Vec<CompletionItem>> {
        let cursor = Span {
            start: query_span.end,
            end: query_span.end,
        };

        let mut active_source = source;
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
        // Keep member completions useful when unrelated resolve errors suppress
        // typed lookup-state output by running a local best-effort typecheck.
        let fallback_typed = if typed.is_none() {
            Some(
                api::typecheck_stage_partial(resolved.clone(), resolve::ImportedFacts::default())
                    .context,
            )
        } else {
            None
        };
        let typed_for_completion = typed.as_ref().or(fallback_typed.as_ref());
        let mut out = collect_completions(&active_source, cursor, &resolved, typed_for_completion);
        out.sort_by(|a, b| a.label.cmp(&b.label).then(a.def_id.0.cmp(&b.def_id.0)));
        out.dedup_by(|a, b| a.label == b.label && a.kind == b.kind && a.def_id == b.def_id);
        Ok(out)
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
        let state = self.lookup_state_for_file(file_id)?;
        if let Some(sig) = signature_help_at_span(&state, query_span) {
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
        let state = if let Some(state) = self.entry_lookup_state_for_program_file(file_id)? {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        if let Some(sig) = signature_help_at_span(&state, query_span) {
            return Ok(Some(sig));
        }
        self.signature_help_with_synthetic_fallback(file_id, query_span)
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

    fn lookup_state_for_file(&mut self, file_id: FileId) -> QueryResult<LookupState> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id) else {
            return Ok(LookupState::default());
        };
        let revision = snapshot.revision();
        let module_id = ModuleId(file_id.0);

        let lookup_key = QueryKey::new(
            crate::services::analysis::query::QueryKind::LookupState,
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

    fn entry_lookup_state_for_program_file(
        &mut self,
        file_id: FileId,
    ) -> QueryResult<Option<LookupState>> {
        let program = self.program_pipeline_for_file(file_id)?;
        Ok(program
            .entry_module_id
            .and_then(|entry| program.module_states.get(&entry).cloned()))
    }

    fn signature_help_with_synthetic_fallback(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id).map(|s| s.to_string()) else {
            return Ok(None);
        };

        let mut fallback_sig = None;
        for synthetic_source in synthesize_call_signature_sources(&source, query_span.start) {
            let Some(synthetic_state) = self.lookup_state_for_source(file_id, synthetic_source)?
            else {
                continue;
            };
            if let Some(sig) = signature_help_at_span(&synthetic_state, query_span) {
                if sig.def_id.is_some() {
                    return Ok(Some(sig));
                }
                if fallback_sig.is_none() {
                    fallback_sig = Some(sig);
                }
            }
        }
        Ok(fallback_sig)
    }

    fn program_pipeline_for_file(&mut self, file_id: FileId) -> QueryResult<ProgramPipelineResult> {
        let snapshot = self.snapshot();
        let Some(entry_source) = snapshot.text(file_id) else {
            return Ok(ProgramPipelineResult::default());
        };
        let Some(entry_path) = snapshot.path(file_id).map(Path::to_path_buf) else {
            return Ok(ProgramPipelineResult::default());
        };
        let key = QueryKey::new(
            crate::services::analysis::query::QueryKind::ProgramPipeline,
            ModuleId(file_id.0),
            snapshot.revision(),
        );
        self.execute_query(key, move |rt| {
            let mut result = ProgramPipelineResult::default();
            let tag_with_entry_path = |mut diagnostics: Vec<Diagnostic>| {
                let entry_file_path = entry_path.to_string_lossy().to_string();
                for diag in &mut diagnostics {
                    diag.metadata
                        .entry(ANALYSIS_FILE_PATH_KEY.to_string())
                        .or_insert_with(|| DiagnosticValue::String(entry_file_path.clone()));
                }
                diagnostics
            };
            let project_root = infer_project_root(&entry_path);
            let entry_module_path = match ModulePath::from_file(&entry_path, &project_root) {
                Ok(path) => path,
                Err(err) => {
                    result.diagnostics = tag_with_entry_path(frontend_error_diagnostics(err));
                    return Ok(result);
                }
            };
            let loader = SnapshotOverlayLoader::new(snapshot.clone(), project_root);
            let program = match capsule::discover_and_parse_capsule_with_loader(
                &entry_source,
                &entry_path,
                entry_module_path,
                &loader,
            ) {
                Ok(program) => program,
                Err(err) => {
                    result.diagnostics = tag_with_entry_path(frontend_error_diagnostics(err));
                    return Ok(result);
                }
            };
            let program_context = crate::core::context::CapsuleParsedContext::new(program);
            let entry_module_id = program_context.entry();
            let mut import_facts = ProgramImportFactsCache::default();
            let prelude_module = parsed_prelude_decl_module(program_context.next_node_id_gen());
            let mut all_diagnostics = Vec::new();
            let mut module_states = HashMap::<ModuleId, LookupState>::new();
            let mut exports_by_module = HashMap::<ModuleId, ModuleExportFacts>::new();
            let mut import_env_by_module = HashMap::<ModuleId, ImportEnv>::new();

            for module_id in program_context.dependency_order_from_entry() {
                let Some(parsed) = program_context.module(module_id) else {
                    continue;
                };
                let source = std::sync::Arc::<str>::from(parsed.source.source.as_str());
                let module_revision = stable_source_revision(&parsed.source.source);
                let parsed_context = crate::core::context::ParsedContext::new(
                    module_with_implicit_prelude(parsed, prelude_module.as_ref()),
                    program_context.next_node_id_gen().clone(),
                )
                .with_source_path(parsed.source.file_path.clone());
                let imported_modules =
                    import_facts.imported_modules_for(&program_context, module_id);
                let imported_symbols =
                    import_facts.imported_symbols_for(&program_context, module_id);
                let skip_typecheck =
                    ProgramImportFactsCache::should_skip_typecheck(&imported_symbols);
                let mut state = run_module_pipeline_with_parsed_and_imports(
                    rt,
                    module_id,
                    module_revision,
                    source,
                    Some(parsed_context),
                    imported_modules,
                    imported_symbols,
                    skip_typecheck,
                )?;
                if let Some(resolved) = &mut state.resolved.product {
                    apply_prelude_runtime_def_locations(parsed, resolved);
                    import_facts.ingest_resolved(module_id, resolved);
                    let exports = collect_module_export_facts(
                        module_id,
                        parsed.source.path.clone(),
                        resolved,
                    );
                    let import_env =
                        collect_module_import_env(&program_context, module_id, &exports_by_module);
                    exports_by_module.insert(module_id, exports);
                    if !import_env.module_aliases.is_empty()
                        || !import_env.symbol_aliases.is_empty()
                    {
                        import_env_by_module.insert(module_id, import_env);
                    }
                }
                if let Some(typed) = &state.typechecked.product {
                    import_facts.ingest_typed(module_id, typed);
                }
                let mut module_diags = collect_sorted_diagnostics(&state);
                let file_path = parsed.source.file_path.to_string_lossy().to_string();
                for diag in &mut module_diags {
                    diag.metadata.insert(
                        ANALYSIS_FILE_PATH_KEY.to_string(),
                        DiagnosticValue::String(file_path.clone()),
                    );
                }
                all_diagnostics.extend(module_diags);
                module_states.insert(module_id, to_lookup_state(&state));
            }

            all_diagnostics.sort_by_key(|diag| {
                (
                    diag.phase,
                    diag.span.start.line,
                    diag.span.start.column,
                    diag.code.clone(),
                )
            });
            result.diagnostics = all_diagnostics;
            result.entry_module_id = Some(entry_module_id);
            result.module_states = module_states;
            result.import_env_by_module = import_env_by_module;
            Ok(result)
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

#[derive(Clone, Default)]
struct ProgramPipelineResult {
    diagnostics: Vec<Diagnostic>,
    entry_module_id: Option<ModuleId>,
    module_states: HashMap<ModuleId, LookupState>,
    import_env_by_module: HashMap<ModuleId, ImportEnv>,
}

fn collect_module_export_facts(
    module_id: ModuleId,
    module_path: ModulePath,
    resolved: &crate::core::context::ResolvedContext,
) -> ModuleExportFacts {
    let mut out = ModuleExportFacts {
        module_id,
        module_path: Some(module_path),
        callables: HashMap::new(),
        types: HashMap::new(),
        traits: HashMap::new(),
    };
    for def in resolved.def_table.defs() {
        if !def.is_public() {
            continue;
        }
        match def.kind {
            DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => {
                out.callables
                    .entry(def.name.clone())
                    .or_default()
                    .push(GlobalDefId::new(module_id, def.id));
            }
            DefKind::TypeDef { .. } => {
                out.types
                    .entry(def.name.clone())
                    .or_insert_with(|| GlobalDefId::new(module_id, def.id));
            }
            DefKind::TraitDef { .. } => {
                out.traits
                    .entry(def.name.clone())
                    .or_insert_with(|| GlobalDefId::new(module_id, def.id));
            }
            _ => {}
        }
    }
    for overloads in out.callables.values_mut() {
        overloads.sort_by_key(|id| id.def_id);
        overloads.dedup();
    }
    out
}

fn collect_module_import_env(
    program: &crate::core::context::CapsuleParsedContext,
    module_id: ModuleId,
    exports_by_module: &HashMap<ModuleId, ModuleExportFacts>,
) -> ImportEnv {
    let mut out = ImportEnv::default();
    let Some(parsed) = program.module(module_id) else {
        return out;
    };
    for req in &parsed.requires {
        let Some(dep_module_id) = program.capsule.by_path.get(&req.module_path).copied() else {
            continue;
        };
        let Some(dep_exports) = exports_by_module.get(&dep_module_id) else {
            continue;
        };
        match req.kind {
            RequireKind::Module => {
                out.module_aliases.insert(
                    req.alias.clone(),
                    ModuleImportBinding {
                        module_id: dep_module_id,
                        module_path: req.module_path.clone(),
                        exports: dep_exports.clone(),
                    },
                );
            }
            RequireKind::Symbol => {
                let Some(member) = &req.member else {
                    continue;
                };
                let binding = collect_imported_symbol_binding_from_exports(
                    dep_module_id,
                    &req.module_path,
                    dep_exports,
                    member,
                );
                if !binding.is_empty() {
                    out.symbol_aliases.insert(req.alias.clone(), binding);
                }
            }
        }
    }
    out
}

fn collect_imported_symbol_binding_from_exports(
    dep_module_id: ModuleId,
    dep_module_path: &ModulePath,
    dep_exports: &ModuleExportFacts,
    member: &str,
) -> ImportedSymbolBinding {
    let callables = dep_exports
        .callables
        .get(member)
        .cloned()
        .unwrap_or_default();
    let type_def = dep_exports.types.get(member).copied();
    let trait_def = dep_exports.traits.get(member).copied();
    ImportedSymbolBinding {
        module_id: dep_module_id,
        module_path: dep_module_path.clone(),
        callables,
        type_def,
        trait_def,
    }
}

fn resolve_imported_symbol_target_from_import_env(
    module_id: ModuleId,
    local_def: &crate::core::resolve::Def,
    import_env_by_module: &HashMap<ModuleId, ImportEnv>,
) -> Option<GlobalDefId> {
    let import_env = import_env_by_module.get(&module_id)?;
    let binding = import_env.symbol_aliases.get(&local_def.name)?;
    let target = match local_def.kind {
        DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => binding.callables.first().copied(),
        DefKind::TypeDef { .. } => binding.type_def,
        DefKind::TraitDef { .. } => binding.trait_def,
        _ => None,
    };
    if target.is_some() {
        return target;
    }

    // Partial/errored resolve may classify imported symbol aliases conservatively.
    // If the import binding has only one possible export target, use it.
    let mut candidates = Vec::new();
    if let Some(callable) = binding.callables.first().copied() {
        candidates.push(callable);
    }
    if let Some(type_def) = binding.type_def {
        candidates.push(type_def);
    }
    if let Some(trait_def) = binding.trait_def {
        candidates.push(trait_def);
    }
    candidates.dedup();
    if candidates.len() == 1 {
        return candidates.first().copied();
    }
    None
}

fn module_with_implicit_prelude(
    parsed: &crate::core::capsule::ParsedModule,
    prelude_module: Option<&crate::core::tree::parsed::Module>,
) -> crate::core::tree::parsed::Module {
    let Some(prelude_module) = prelude_module else {
        return parsed.module.clone();
    };
    if is_std_prelude_decl(parsed) {
        parsed.module.clone()
    } else {
        merge_modules(prelude_module, &parsed.module)
    }
}

fn is_std_prelude_decl(parsed: &crate::core::capsule::ParsedModule) -> bool {
    matches!(
        parsed.source.path.segments(),
        [std_seg, prelude_seg] if std_seg == "std" && prelude_seg == "prelude_decl"
    )
}

fn parsed_prelude_decl_module(
    id_gen: &crate::core::tree::NodeIdGen,
) -> Option<crate::core::tree::parsed::Module> {
    let prelude_path = prelude_decl_path();
    let prelude_src = std::fs::read_to_string(prelude_path).ok()?;
    let (module, _) = api::parse_module_with_id_gen(&prelude_src, id_gen.clone()).ok()?;
    Some(module)
}

fn prelude_decl_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("std")
        .join("prelude_decl.mc")
}

fn apply_prelude_runtime_def_locations(
    parsed: &crate::core::capsule::ParsedModule,
    resolved: &mut crate::core::context::ResolvedContext,
) {
    if is_std_prelude_decl(parsed) {
        return;
    }
    let runtime_locations = prelude_decl_runtime_locations();
    if runtime_locations.is_empty() {
        return;
    }
    let runtime_defs: Vec<(DefId, String)> = resolved
        .def_table
        .defs()
        .iter()
        .filter(|def| def.is_runtime())
        .map(|def| (def.id, def.name.clone()))
        .collect();
    for (def_id, name) in runtime_defs {
        if let Some(loc) = runtime_locations.get(&name) {
            resolved.def_table.set_def_location(def_id, loc.clone());
        }
    }
}

fn prelude_decl_runtime_locations() -> &'static HashMap<String, DefLocation> {
    static PRELUDE_RUNTIME_LOCATIONS: OnceLock<HashMap<String, DefLocation>> = OnceLock::new();
    PRELUDE_RUNTIME_LOCATIONS.get_or_init(build_prelude_runtime_locations)
}

fn build_prelude_runtime_locations() -> HashMap<String, DefLocation> {
    let prelude_path = prelude_decl_path();
    let Ok(prelude_src) = std::fs::read_to_string(prelude_path) else {
        return HashMap::new();
    };
    let Ok((module, id_gen)) =
        api::parse_module_with_id_gen(&prelude_src, crate::core::tree::NodeIdGen::new())
    else {
        return HashMap::new();
    };
    let parsed = crate::core::context::ParsedContext::new(module, id_gen)
        .with_source_path(prelude_decl_path());
    let resolved = api::resolve_stage_partial(parsed, HashMap::new(), HashMap::new());
    let mut locations = HashMap::new();
    for def in resolved.context.def_table.defs() {
        if !def.is_runtime() {
            continue;
        }
        let Some(loc) = resolved.context.def_table.lookup_def_location(def.id) else {
            continue;
        };
        locations.insert(def.name.clone(), loc);
    }
    locations
}

#[cfg(test)]
#[path = "../../tests/analysis/t_analysis_db.rs"]
mod tests;
