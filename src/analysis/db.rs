//! Analysis database facade.
//!
//! This composes the foundational query runtime pieces:
//! - revisioned source snapshots,
//! - query memoization/dependency tracking,
//! - module-closure invalidation.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::analysis::completion::{
    collect as collect_completions, synthesize_member_completion_source,
};
use crate::analysis::diagnostics::{ANALYSIS_FILE_PATH_KEY, Diagnostic, DiagnosticValue};
use crate::analysis::frontend_support::{
    SnapshotOverlayLoader, frontend_error_diagnostics, infer_project_root, stable_source_revision,
};
use crate::analysis::lookups::{
    code_actions_for_range, def_at_span, def_location_at_span, document_symbols, hover_at_span,
    semantic_tokens, signature_help_at_span, type_at_span,
};
use crate::analysis::module_graph::ModuleGraph;
use crate::analysis::pipeline::{
    LookupState, collect_sorted_diagnostics, run_module_pipeline,
    run_module_pipeline_with_parsed_and_imports, to_lookup_state,
};
use crate::analysis::query::{CacheStats, CancellationToken, QueryKey, QueryResult, QueryRuntime};
use crate::analysis::rename::{references as collect_references, rename_plan as build_rename_plan};
use crate::analysis::results::{
    CodeAction, CompletionItem, DocumentSymbol, HoverInfo, Location, RenamePlan, SemanticToken,
    SignatureHelp,
};
use crate::analysis::snapshot::{AnalysisSnapshot, FileId, SourceStore};
use crate::context::{ProgramParsedContext, TypeCheckedContext};
use crate::diag::Span;
use crate::frontend::bind::ProgramBindings;
use crate::frontend::{self, ModuleId, ModulePath, RequireKind};
use crate::resolve::{
    DefId, ImportedCallableSig, ImportedModule, ImportedParamSig, ImportedSymbol,
};
use crate::tree::NodeId;
use crate::tree::ParamMode;
use crate::typecheck::type_check_partial;
use crate::typecheck::type_map::resolve_type_def_with_args;
use crate::types::{FnParamMode, Type};

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
                Err(err) => return Ok(tag_with_entry_path(frontend_error_diagnostics(err))),
            };
            let loader = SnapshotOverlayLoader::new(snapshot.clone(), project_root);

            let program = match frontend::discover_and_parse_program_with_loader(
                &entry_source,
                &entry_path,
                entry_module_path,
                &loader,
            ) {
                Ok(program) => program,
                Err(err) => return Ok(tag_with_entry_path(frontend_error_diagnostics(err))),
            };
            let program_context = crate::context::ProgramParsedContext::new(program);
            let bindings = ProgramBindings::build(&program_context);
            let mut callable_sigs_by_module: HashMap<
                ModuleId,
                HashMap<String, Vec<ImportedCallableSig>>,
            > = HashMap::new();
            let mut type_tys_by_module: HashMap<ModuleId, HashMap<String, Type>> = HashMap::new();

            let mut all = Vec::new();
            for module_id in program_context.dependency_order_from_entry() {
                let Some(parsed) = program_context.module(module_id) else {
                    continue;
                };
                let source = std::sync::Arc::<str>::from(parsed.source.source.as_str());
                let module_revision = stable_source_revision(&parsed.source.source);
                let parsed_context = crate::context::ParsedContext::new(
                    parsed.module.clone(),
                    program_context.next_node_id_gen().clone(),
                );
                let imported_modules =
                    build_analysis_imported_modules(&program_context, &bindings, module_id);
                let imported_symbols = build_analysis_imported_symbols(
                    &program_context,
                    &bindings,
                    &callable_sigs_by_module,
                    &type_tys_by_module,
                    module_id,
                );
                let skip_typecheck = imported_symbols.values().any(|imported| {
                    (imported.has_type && imported.type_ty.is_none())
                        || imported.has_trait
                        || (imported.has_callable && imported.callable_sigs.is_empty())
                });
                let state = run_module_pipeline_with_parsed_and_imports(
                    rt,
                    module_id,
                    module_revision,
                    source,
                    Some(parsed_context),
                    imported_modules,
                    imported_symbols,
                    skip_typecheck,
                )?;
                if let Some(typed) = &state.typechecked.product {
                    callable_sigs_by_module.insert(module_id, collect_public_callable_sigs(typed));
                    type_tys_by_module.insert(module_id, collect_public_type_tys(typed));
                }
                let mut module_diags = collect_sorted_diagnostics(&state);
                let file_path = parsed.source.file_path.to_string_lossy().to_string();
                for diag in &mut module_diags {
                    diag.metadata.insert(
                        ANALYSIS_FILE_PATH_KEY.to_string(),
                        DiagnosticValue::String(file_path.clone()),
                    );
                }
                all.extend(module_diags);
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
        // Keep member completions useful when unrelated resolve errors suppress
        // typed lookup-state output by running a local best-effort typecheck.
        let fallback_typed = if typed.is_none() {
            Some(type_check_partial(resolved.clone()).context)
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
        Ok(signature_help_at_span(&state, query_span))
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
        Ok(code_actions_for_range(&diagnostics, range))
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

fn build_analysis_imported_modules(
    program_context: &ProgramParsedContext,
    bindings: &ProgramBindings,
    module_id: ModuleId,
) -> HashMap<String, ImportedModule> {
    let mut out = HashMap::new();
    let Some(parsed) = program_context.module(module_id) else {
        return out;
    };

    for req in &parsed.requires {
        if req.kind != RequireKind::Module {
            continue;
        }
        let Some(dep_id) = program_context
            .program
            .by_path
            .get(&req.module_path)
            .copied()
        else {
            continue;
        };
        let Some(exports) = bindings.exports_for(dep_id) else {
            continue;
        };
        let mut members = std::collections::HashSet::new();
        for (name, attrs) in &exports.callables {
            if attrs.public {
                members.insert(name.clone());
            }
        }
        for (name, attrs) in &exports.types {
            if attrs.public {
                members.insert(name.clone());
            }
        }
        for (name, attrs) in &exports.traits {
            if attrs.public {
                members.insert(name.clone());
            }
        }
        out.insert(
            req.alias.clone(),
            ImportedModule {
                path: req.module_path.to_string(),
                members,
            },
        );
    }

    out
}

fn build_analysis_imported_symbols(
    program_context: &ProgramParsedContext,
    bindings: &ProgramBindings,
    callable_sigs_by_module: &HashMap<ModuleId, HashMap<String, Vec<ImportedCallableSig>>>,
    type_tys_by_module: &HashMap<ModuleId, HashMap<String, Type>>,
    module_id: ModuleId,
) -> HashMap<String, ImportedSymbol> {
    let mut out = HashMap::new();
    let Some(parsed) = program_context.module(module_id) else {
        return out;
    };

    for req in &parsed.requires {
        if req.kind != RequireKind::Symbol {
            continue;
        }
        let Some(member) = &req.member else {
            continue;
        };
        let Some(dep_id) = program_context
            .program
            .by_path
            .get(&req.module_path)
            .copied()
        else {
            continue;
        };
        let Some(exports) = bindings.exports_for(dep_id) else {
            continue;
        };
        let dep_callable_sigs = callable_sigs_by_module.get(&dep_id);
        let dep_type_tys = type_tys_by_module.get(&dep_id);
        let has_callable = exports
            .callables
            .get(member)
            .is_some_and(|attrs| attrs.public);
        let has_type = exports.types.get(member).is_some_and(|attrs| attrs.public);
        let imported = ImportedSymbol {
            has_callable,
            callable_sigs: if has_callable {
                dep_callable_sigs
                    .and_then(|module_sigs| module_sigs.get(member))
                    .cloned()
                    .unwrap_or_default()
            } else {
                Vec::new()
            },
            has_type,
            type_ty: if has_type {
                dep_type_tys
                    .and_then(|module_types| module_types.get(member))
                    .cloned()
            } else {
                None
            },
            has_trait: exports.traits.get(member).is_some_and(|attrs| attrs.public),
        };
        if imported.has_callable || imported.has_type || imported.has_trait {
            out.insert(req.alias.clone(), imported);
        }
    }

    out
}

fn collect_public_callable_sigs(
    typed: &TypeCheckedContext,
) -> HashMap<String, Vec<ImportedCallableSig>> {
    let mut out = HashMap::<String, Vec<ImportedCallableSig>>::new();
    for item in &typed.module.top_level_items {
        let callable = match item {
            crate::tree::typed::TopLevelItem::FuncDecl(decl) => Some((&decl.sig.name, decl.def_id)),
            crate::tree::typed::TopLevelItem::FuncDef(def) => Some((&def.sig.name, def.def_id)),
            _ => None,
        };
        let Some((name, def_id)) = callable else {
            continue;
        };
        let Some(def) = typed.def_table.lookup_def(def_id) else {
            continue;
        };
        if !def.is_public() {
            continue;
        }
        let Some(def_ty) = typed.type_map.lookup_def_type(def) else {
            continue;
        };
        let Type::Fn { params, ret_ty } = def_ty else {
            continue;
        };
        out.entry(name.clone())
            .or_default()
            .push(ImportedCallableSig {
                params: params
                    .into_iter()
                    .map(|param| ImportedParamSig {
                        mode: param_mode_from_fn_param(param.mode),
                        ty: param.ty,
                    })
                    .collect(),
                ret_ty: *ret_ty,
            });
    }
    out
}

fn collect_public_type_tys(typed: &TypeCheckedContext) -> HashMap<String, Type> {
    let mut out = HashMap::<String, Type>::new();
    for type_def in typed.module.type_defs() {
        if !type_def.type_params.is_empty() {
            continue;
        }
        let Some(def) = typed.def_table.lookup_def(type_def.def_id) else {
            continue;
        };
        if !def.is_public() {
            continue;
        }
        let Ok(ty) =
            resolve_type_def_with_args(&typed.def_table, &typed.module, type_def.def_id, &[])
        else {
            continue;
        };
        out.insert(type_def.name.clone(), ty);
    }
    out
}

fn param_mode_from_fn_param(mode: FnParamMode) -> ParamMode {
    match mode {
        FnParamMode::In => ParamMode::In,
        FnParamMode::InOut => ParamMode::InOut,
        FnParamMode::Out => ParamMode::Out,
        FnParamMode::Sink => ParamMode::Sink,
    }
}

#[cfg(test)]
#[path = "../tests/analysis/t_analysis_db.rs"]
mod tests;
