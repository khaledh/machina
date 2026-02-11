//! Analysis database facade.
//!
//! This composes the foundational query runtime pieces:
//! - revisioned source snapshots,
//! - query memoization/dependency tracking,
//! - module-closure invalidation.

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use crate::analysis::code_actions::code_actions_for_diagnostic;
use crate::analysis::diagnostics::Diagnostic;
use crate::analysis::module_graph::ModuleGraph;
use crate::analysis::pipeline::{
    LookupState, collect_sorted_diagnostics, run_module_pipeline, to_lookup_state,
};
use crate::analysis::query::{CacheStats, CancellationToken, QueryKey, QueryResult, QueryRuntime};
use crate::analysis::results::{
    CodeAction, CompletionItem, CompletionKind, DocumentSymbol, HoverInfo, Location,
    RenameConflict, RenameEdit, RenamePlan, SemanticToken, SemanticTokenKind, SignatureHelp,
};
use crate::analysis::snapshot::{AnalysisSnapshot, FileId, SourceStore};
use crate::analysis::syntax_index::{
    active_param_index, call_site_at_span, document_symbol_nodes, node_at_span, node_span_map,
    position_leq, span_intersects_span,
};
use crate::diag::Span;
use crate::frontend::program::flatten_program;
use crate::frontend::{self, FrontendError, ModuleId, ModuleLoader, ModulePath};
use crate::resolve::{DefId, DefKind, UNKNOWN_DEF_ID};
use crate::tree::resolved as res;
use crate::tree::{BlockItem, NodeId, StmtExprKind};
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
        let Some(resolved) = state.resolved else {
            return Ok(None);
        };
        let Some(node_id) = node_at_span(&resolved.module, query_span) else {
            return Ok(None);
        };
        if state.poisoned_nodes.contains(&node_id) {
            return Ok(None);
        }
        Ok(resolved
            .def_table
            .lookup_node_def_id(node_id)
            .filter(|id| *id != UNKNOWN_DEF_ID))
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
        let Some(resolved) = state.resolved else {
            return Ok(None);
        };

        let Some(use_node_id) = node_at_span(&resolved.module, query_span) else {
            return Ok(None);
        };
        if state.poisoned_nodes.contains(&use_node_id) {
            return Ok(None);
        }
        let Some(def_id) = resolved.def_table.lookup_node_def_id(use_node_id) else {
            return Ok(None);
        };
        if def_id == UNKNOWN_DEF_ID {
            return Ok(None);
        }
        let Some(def_node_id) = resolved.def_table.lookup_def_node_id(def_id) else {
            return Ok(None);
        };

        let node_spans = node_span_map(&resolved.module);
        let Some(def_span) = node_spans.get(&def_node_id).copied() else {
            return Ok(None);
        };

        Ok(Some(Location {
            file_id,
            path: snapshot.path(file_id).map(Path::to_path_buf),
            span: def_span,
        }))
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
        let Some(typed) = state.typed else {
            return Ok(None);
        };
        let Some(node_id) = node_at_span(&typed.module, query_span) else {
            return Ok(None);
        };
        if state.poisoned_nodes.contains(&node_id) {
            return Ok(None);
        }
        Ok(typed
            .type_map
            .lookup_node_type(node_id)
            .filter(|ty| !matches!(ty, Type::Unknown)))
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
        let LookupState {
            resolved,
            typed,
            poisoned_nodes,
        } = self.lookup_state_for_file(file_id)?;

        if let Some(typed) = typed {
            if let Some(node_id) = node_at_span(&typed.module, query_span) {
                if poisoned_nodes.contains(&node_id) {
                    return Ok(None);
                }
                let def_id = typed
                    .def_table
                    .lookup_node_def_id(node_id)
                    .filter(|id| *id != UNKNOWN_DEF_ID);
                let def_name =
                    def_id.and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()));
                let ty = typed
                    .type_map
                    .lookup_node_type(node_id)
                    .filter(|ty| !matches!(ty, Type::Unknown));
                if def_id.is_none() && ty.is_none() {
                    return Ok(None);
                }
                let display = format_hover_label(def_name.as_deref(), ty.as_ref());
                return Ok(Some(HoverInfo {
                    node_id,
                    span: query_span,
                    def_id,
                    def_name,
                    ty,
                    display,
                }));
            }
            return Ok(None);
        }

        let Some(resolved) = resolved else {
            return Ok(None);
        };
        let Some(node_id) = node_at_span(&resolved.module, query_span) else {
            return Ok(None);
        };
        if poisoned_nodes.contains(&node_id) {
            return Ok(None);
        }
        let def_id = resolved
            .def_table
            .lookup_node_def_id(node_id)
            .filter(|id| *id != UNKNOWN_DEF_ID);
        let Some(def_id) = def_id else {
            return Ok(None);
        };
        let def_name = resolved
            .def_table
            .lookup_def(def_id)
            .map(|def| def.name.clone());
        let display = format_hover_label(def_name.as_deref(), None);
        Ok(Some(HoverInfo {
            node_id,
            span: query_span,
            def_id: Some(def_id),
            def_name,
            ty: None,
            display,
        }))
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

        let Some(resolved) = state.resolved else {
            return Ok(Vec::new());
        };
        let context = completion_context(&active_source, cursor, state.typed.as_ref());
        let mut out = match context {
            CompletionContext::Member {
                prefix,
                receiver_ty,
                caller_def_id,
            } => {
                let members = member_completions(&resolved, &receiver_ty, caller_def_id);
                members
                    .into_iter()
                    .filter(|item| item.label.starts_with(&prefix))
                    .collect()
            }
            CompletionContext::Scope { prefix } => {
                let mut items = scope_completions(&resolved, cursor.start);
                items.retain(|item| item.label.starts_with(&prefix));
                items
            }
        };
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
        let Some(typed) = state.typed else {
            return Ok(None);
        };
        let Some(call) = call_site_at_span(&typed.module, query_span) else {
            return Ok(None);
        };
        let Some(sig) = typed.call_sigs.get(&call.node_id) else {
            return Ok(None);
        };

        let mut params = Vec::with_capacity(sig.params.len());
        for param in &sig.params {
            params.push(format!("{} {}", param_mode_name(&param.mode), param.ty));
        }
        let active_parameter = active_param_index(&call.arg_spans, query_span.start);
        let name = sig
            .def_id
            .and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()))
            .unwrap_or_else(|| "<call>".to_string());
        let label = format!("{name}({})", params.join(", "));

        Ok(Some(SignatureHelp {
            label,
            def_id: sig.def_id,
            active_parameter,
            parameters: params,
        }))
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
        let Some(resolved) = state.resolved else {
            return Ok(Vec::new());
        };

        let node_spans = node_span_map(&resolved.module);
        let nodes = document_symbol_nodes(&resolved.module);

        let mut out = Vec::new();
        for (node_id, kind) in nodes {
            let Some(def_id) = resolved.def_table.lookup_node_def_id(node_id) else {
                continue;
            };
            let Some(def) = resolved.def_table.lookup_def(def_id) else {
                continue;
            };
            let Some(span) = node_spans.get(&node_id).copied() else {
                continue;
            };
            out.push(DocumentSymbol {
                name: def.name.clone(),
                kind: kind.clone(),
                def_id,
                span,
                detail: Some(def.kind.to_string()),
            });
        }

        out.sort_by_key(|sym| {
            (
                sym.span.start.line,
                sym.span.start.column,
                sym.span.end.line,
                sym.span.end.column,
                sym.name.clone(),
                sym.def_id,
            )
        });
        out.dedup_by(|a, b| a.def_id == b.def_id && a.kind == b.kind && a.span == b.span);
        Ok(out)
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
        let Some(resolved) = state.resolved else {
            return Ok(Vec::new());
        };

        let node_spans = node_span_map(&resolved.module);
        let mut out = Vec::new();
        for (node_id, def_id) in resolved.def_table.node_def_entries() {
            let Some(def) = resolved.def_table.lookup_def(def_id) else {
                continue;
            };
            let Some(kind) = semantic_token_kind_for_def(&def.kind) else {
                continue;
            };
            let Some(span) = node_spans.get(&node_id).copied() else {
                continue;
            };
            out.push(SemanticToken { span, kind, def_id });
        }
        out.sort_by_key(|tok| {
            (
                tok.span.start.line,
                tok.span.start.column,
                tok.span.end.line,
                tok.span.end.column,
                tok.def_id,
            )
        });
        out.dedup_by(|a, b| a.span == b.span && a.kind == b.kind && a.def_id == b.def_id);
        Ok(out)
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
        let mut actions = Vec::new();
        for diag in diagnostics {
            if !span_intersects_span(diag.span, range) {
                continue;
            }
            actions.extend(code_actions_for_diagnostic(&diag));
        }
        actions.sort_by_key(|action| {
            (
                action.title.clone(),
                action.diagnostic_code.clone(),
                action
                    .edits
                    .first()
                    .map(|e| (e.span.start.line, e.span.start.column))
                    .unwrap_or((0, 0)),
            )
        });
        actions.dedup_by(|a, b| {
            a.title == b.title && a.diagnostic_code == b.diagnostic_code && a.edits == b.edits
        });
        Ok(actions)
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

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        source.hash(&mut hasher);
        let source_hash = hasher.finish();

        let revision = snapshot.revision() ^ source_hash | (1u64 << 63);
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

fn format_hover_label(def_name: Option<&str>, ty: Option<&Type>) -> String {
    match (def_name, ty) {
        (Some(name), Some(ty)) => format!("{name}: {ty}"),
        (Some(name), None) => name.to_string(),
        (None, Some(ty)) => ty.to_string(),
        (None, None) => String::new(),
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

fn completion_kind_for_def(kind: &DefKind) -> Option<CompletionKind> {
    match kind {
        DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => Some(CompletionKind::Function),
        DefKind::TypeDef { .. } => Some(CompletionKind::Type),
        DefKind::TraitDef { .. } => Some(CompletionKind::Trait),
        DefKind::LocalVar { .. } => Some(CompletionKind::Variable),
        DefKind::Param { .. } => Some(CompletionKind::Parameter),
        DefKind::TypeParam => Some(CompletionKind::TypeParameter),
        DefKind::EnumVariantName => Some(CompletionKind::EnumVariant),
    }
}

#[derive(Debug, Clone)]
enum CompletionContext {
    Scope {
        prefix: String,
    },
    Member {
        prefix: String,
        receiver_ty: Type,
        caller_def_id: Option<DefId>,
    },
}

fn completion_context(
    source: &str,
    query_span: Span,
    typed: Option<&crate::context::TypeCheckedContext>,
) -> CompletionContext {
    let offset = offset_for_position(source, query_span.start).unwrap_or(source.len());
    let bytes = source.as_bytes();
    let mut prefix_start = offset;
    while prefix_start > 0 && is_ident_byte(bytes[prefix_start - 1]) {
        prefix_start -= 1;
    }
    let prefix = source[prefix_start..offset].to_string();

    let mut dot_probe = prefix_start;
    while dot_probe > 0 && bytes[dot_probe - 1].is_ascii_whitespace() {
        dot_probe -= 1;
    }
    if dot_probe == 0 || bytes[dot_probe - 1] != b'.' {
        return CompletionContext::Scope { prefix };
    }
    let Some(typed) = typed else {
        return CompletionContext::Scope { prefix };
    };

    let mut recv_end = dot_probe - 1;
    while recv_end > 0 && bytes[recv_end - 1].is_ascii_whitespace() {
        recv_end -= 1;
    }
    if recv_end == 0 {
        return CompletionContext::Scope { prefix };
    }
    let receiver_char = recv_end - 1;
    let Some(receiver_span) = single_char_span(source, receiver_char) else {
        return CompletionContext::Scope { prefix };
    };
    let Some(node_id) = node_at_span(&typed.module, receiver_span) else {
        return CompletionContext::Scope { prefix };
    };
    let Some(receiver_ty) = typed.type_map.lookup_node_type(node_id) else {
        return CompletionContext::Scope { prefix };
    };

    CompletionContext::Member {
        prefix,
        receiver_ty,
        caller_def_id: enclosing_callable_def_id(&typed.module, query_span.start),
    }
}

fn scope_completions(
    resolved: &crate::context::ResolvedContext,
    cursor: crate::diag::Position,
) -> Vec<CompletionItem> {
    let mut scopes = vec![global_scope(resolved)];

    if let Some(callable) = containing_callable(&resolved.module, cursor) {
        scopes.push(HashMap::new());
        add_callable_params_to_scope(&callable, &resolved.def_table, scopes.last_mut().unwrap());
        collect_defs_before_cursor_in_expr(
            callable.body(),
            cursor,
            &resolved.def_table,
            &mut scopes,
        );
    }

    let mut merged = HashMap::<String, CompletionItem>::new();
    for scope in scopes {
        for (label, item) in scope {
            merged.insert(label, item);
        }
    }
    merged.into_values().collect()
}

fn member_completions(
    resolved: &crate::context::ResolvedContext,
    receiver_ty: &Type,
    caller_def_id: Option<DefId>,
) -> Vec<CompletionItem> {
    let mut out = Vec::new();
    let owner = receiver_ty.peel_heap();

    if let Type::Struct { name, fields } = &owner {
        if struct_fields_accessible(resolved, name, caller_def_id) {
            for field in fields {
                out.push(CompletionItem {
                    label: field.name.clone(),
                    kind: CompletionKind::Variable,
                    def_id: UNKNOWN_DEF_ID,
                    detail: Some(field.ty.to_string()),
                });
            }
        }
    }

    out.extend(builtin_member_completions(&owner));
    out.extend(nominal_method_completions(resolved, &owner, caller_def_id));
    out
}

fn builtin_member_completions(owner: &Type) -> Vec<CompletionItem> {
    let mut out = Vec::new();
    match owner {
        Type::String | Type::Array { .. } | Type::Slice { .. } => {
            push_builtin_prop(&mut out, "len", "u64");
        }
        Type::DynArray { .. } => {
            push_builtin_prop(&mut out, "len", "u64");
            push_builtin_prop(&mut out, "capacity", "u64");
            push_builtin_prop(&mut out, "is_empty", "bool");
            push_builtin_method(&mut out, "append", "builtin");
        }
        Type::Set { .. } => {
            push_builtin_prop(&mut out, "len", "u64");
            push_builtin_prop(&mut out, "capacity", "u64");
            push_builtin_prop(&mut out, "is_empty", "bool");
            push_builtin_method(&mut out, "insert", "builtin");
            push_builtin_method(&mut out, "remove", "builtin");
            push_builtin_method(&mut out, "contains", "builtin");
            push_builtin_method(&mut out, "clear", "builtin");
        }
        Type::Map { .. } => {
            push_builtin_prop(&mut out, "len", "u64");
            push_builtin_prop(&mut out, "capacity", "u64");
            push_builtin_prop(&mut out, "is_empty", "bool");
            push_builtin_method(&mut out, "insert", "builtin");
            push_builtin_method(&mut out, "remove", "builtin");
            push_builtin_method(&mut out, "contains_key", "builtin");
            push_builtin_method(&mut out, "get", "builtin");
            push_builtin_method(&mut out, "clear", "builtin");
        }
        _ => {}
    }
    out
}

fn push_builtin_prop(out: &mut Vec<CompletionItem>, label: &str, detail: &str) {
    out.push(CompletionItem {
        label: label.to_string(),
        kind: CompletionKind::Variable,
        def_id: UNKNOWN_DEF_ID,
        detail: Some(detail.to_string()),
    });
}

fn push_builtin_method(out: &mut Vec<CompletionItem>, label: &str, detail: &str) {
    out.push(CompletionItem {
        label: label.to_string(),
        kind: CompletionKind::Function,
        def_id: UNKNOWN_DEF_ID,
        detail: Some(detail.to_string()),
    });
}

fn nominal_method_completions(
    resolved: &crate::context::ResolvedContext,
    owner_ty: &Type,
    caller_def_id: Option<DefId>,
) -> Vec<CompletionItem> {
    let owner_name = match owner_ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name,
        _ => return Vec::new(),
    };

    let mut out = Vec::new();
    for block in resolved.module.method_blocks() {
        if block.type_name != *owner_name {
            continue;
        }
        for item in &block.method_items {
            let (def_id, name, attrs) = match item {
                res::MethodItem::Decl(decl) => (decl.def_id, decl.sig.name.clone(), &decl.attrs),
                res::MethodItem::Def(def) => (def.def_id, def.sig.name.clone(), &def.attrs),
            };
            if !method_accessible(resolved, def_id, caller_def_id) {
                continue;
            }
            let is_prop_get = attrs.iter().any(|a| a.name == "__property_get");
            let is_prop_set = attrs.iter().any(|a| a.name == "__property_set");
            let kind = if is_prop_get || is_prop_set {
                CompletionKind::Variable
            } else {
                CompletionKind::Function
            };
            out.push(CompletionItem {
                label: name,
                kind,
                def_id,
                detail: Some(
                    resolved
                        .def_table
                        .lookup_def(def_id)
                        .map_or_else(|| "member".to_string(), |d| d.kind.to_string()),
                ),
            });
        }
    }
    out
}

fn method_accessible(
    resolved: &crate::context::ResolvedContext,
    target_def_id: DefId,
    caller_def_id: Option<DefId>,
) -> bool {
    let Some(def) = resolved.def_table.lookup_def(target_def_id) else {
        return false;
    };
    if def.is_public() {
        return true;
    }
    let Some(caller_def_id) = caller_def_id else {
        // Best-effort fallback for query contexts where we cannot identify an
        // enclosing callable yet.
        return true;
    };
    let caller_module = resolved
        .def_owners
        .get(&caller_def_id)
        .copied()
        .or(Some(ModuleId(0)));
    let target_module = resolved
        .def_owners
        .get(&target_def_id)
        .copied()
        .or(Some(ModuleId(0)));
    caller_module == target_module
}

fn struct_fields_accessible(
    resolved: &crate::context::ResolvedContext,
    type_name: &str,
    caller_def_id: Option<DefId>,
) -> bool {
    let Some(type_def_id) = resolved.def_table.lookup_type_def_id(type_name) else {
        return true;
    };
    let Some(type_def) = resolved.def_table.lookup_def(type_def_id) else {
        return true;
    };
    if !type_def.is_opaque() {
        return true;
    }
    let Some(caller_def_id) = caller_def_id else {
        return false;
    };
    let caller_module = resolved
        .def_owners
        .get(&caller_def_id)
        .copied()
        .or(Some(ModuleId(0)));
    let owner_module = resolved
        .def_owners
        .get(&type_def_id)
        .copied()
        .or(Some(ModuleId(0)));
    caller_module == owner_module
}

fn global_scope(resolved: &crate::context::ResolvedContext) -> HashMap<String, CompletionItem> {
    let mut allowed_nodes = HashSet::new();
    for item in &resolved.module.top_level_items {
        match item {
            res::TopLevelItem::TraitDef(trait_def) => {
                allowed_nodes.insert(trait_def.id);
            }
            res::TopLevelItem::TypeDef(type_def) => {
                allowed_nodes.insert(type_def.id);
                if let res::TypeDefKind::Enum { variants } = &type_def.kind {
                    for variant in variants {
                        allowed_nodes.insert(variant.id);
                    }
                }
            }
            res::TopLevelItem::FuncDecl(func_decl) => {
                allowed_nodes.insert(func_decl.id);
            }
            res::TopLevelItem::FuncDef(func_def) => {
                allowed_nodes.insert(func_def.id);
            }
            res::TopLevelItem::MethodBlock(_) | res::TopLevelItem::ClosureDef(_) => {}
        }
    }

    let mut out = HashMap::new();
    for def in resolved.def_table.defs() {
        let Some(kind) = completion_kind_for_def(&def.kind) else {
            continue;
        };
        if matches!(
            def.kind,
            DefKind::LocalVar { .. } | DefKind::Param { .. } | DefKind::TypeParam
        ) {
            continue;
        }
        let node_allowed = resolved
            .def_table
            .lookup_def_node_id(def.id)
            .is_some_and(|id| allowed_nodes.contains(&id))
            || resolved.def_table.lookup_def_node_id(def.id) == Some(NodeId(0));
        if !node_allowed {
            continue;
        }
        out.insert(
            def.name.clone(),
            CompletionItem {
                label: def.name.clone(),
                kind,
                def_id: def.id,
                detail: Some(def.kind.to_string()),
            },
        );
    }
    out
}

enum CallableAtCursor<'a> {
    Func(&'a res::FuncDef),
    Method(&'a res::MethodDef),
    Closure(&'a res::ClosureDef),
}

impl<'a> CallableAtCursor<'a> {
    fn span(&self) -> Span {
        match self {
            Self::Func(def) => def.span,
            Self::Method(def) => def.span,
            Self::Closure(def) => def.span,
        }
    }

    fn body(&self) -> &'a res::Expr {
        match self {
            Self::Func(def) => &def.body,
            Self::Method(def) => &def.body,
            Self::Closure(def) => &def.body,
        }
    }
}

fn containing_callable(
    module: &res::Module,
    cursor: crate::diag::Position,
) -> Option<CallableAtCursor<'_>> {
    let mut best: Option<CallableAtCursor<'_>> = None;
    for item in &module.top_level_items {
        match item {
            res::TopLevelItem::FuncDef(def) => {
                choose_smallest_callable(&mut best, CallableAtCursor::Func(def), cursor);
            }
            res::TopLevelItem::MethodBlock(block) => {
                for method in &block.method_items {
                    if let res::MethodItem::Def(def) = method {
                        choose_smallest_callable(&mut best, CallableAtCursor::Method(def), cursor);
                    }
                }
            }
            res::TopLevelItem::ClosureDef(def) => {
                choose_smallest_callable(&mut best, CallableAtCursor::Closure(def), cursor);
            }
            _ => {}
        }
    }
    best
}

fn choose_smallest_callable<'a>(
    best: &mut Option<CallableAtCursor<'a>>,
    candidate: CallableAtCursor<'a>,
    cursor: crate::diag::Position,
) {
    if !span_contains_pos(candidate.span(), cursor) {
        return;
    }
    let replace = best
        .as_ref()
        .is_none_or(|current| span_width(candidate.span()) <= span_width(current.span()));
    if replace {
        *best = Some(candidate);
    }
}

fn add_callable_params_to_scope(
    callable: &CallableAtCursor<'_>,
    def_table: &crate::resolve::DefTable,
    scope: &mut HashMap<String, CompletionItem>,
) {
    match callable {
        CallableAtCursor::Func(def) => {
            for type_param in &def.sig.type_params {
                insert_def_into_scope(type_param.def_id, def_table, scope);
            }
            for param in &def.sig.params {
                insert_def_into_scope(param.def_id, def_table, scope);
            }
        }
        CallableAtCursor::Method(def) => {
            for type_param in &def.sig.type_params {
                insert_def_into_scope(type_param.def_id, def_table, scope);
            }
            insert_def_into_scope(def.sig.self_param.def_id, def_table, scope);
            for param in &def.sig.params {
                insert_def_into_scope(param.def_id, def_table, scope);
            }
        }
        CallableAtCursor::Closure(def) => {
            for param in &def.sig.params {
                insert_def_into_scope(param.def_id, def_table, scope);
            }
        }
    }
}

fn collect_defs_before_cursor_in_expr(
    expr: &res::Expr,
    cursor: crate::diag::Position,
    def_table: &crate::resolve::DefTable,
    scopes: &mut Vec<HashMap<String, CompletionItem>>,
) {
    if !span_contains_pos(expr.span, cursor) {
        return;
    }
    if let res::ExprKind::Block { items, tail } = &expr.kind {
        scopes.push(HashMap::new());
        for item in items {
            match item {
                BlockItem::Stmt(stmt) => {
                    if position_leq(stmt.span.end, cursor) {
                        collect_stmt_bindings(stmt, def_table, scopes.last_mut().unwrap());
                        continue;
                    }
                    if span_contains_pos(stmt.span, cursor) {
                        collect_defs_at_cursor_in_stmt(stmt, cursor, def_table, scopes);
                        return;
                    }
                }
                BlockItem::Expr(item_expr) => {
                    if span_contains_pos(item_expr.span, cursor) {
                        collect_defs_before_cursor_in_expr(item_expr, cursor, def_table, scopes);
                        return;
                    }
                }
            }
        }
        if let Some(tail) = tail
            && span_contains_pos(tail.span, cursor)
        {
            collect_defs_before_cursor_in_expr(tail, cursor, def_table, scopes);
        }
        return;
    }

    match &expr.kind {
        res::ExprKind::If {
            cond,
            then_body,
            else_body,
        } => {
            if span_contains_pos(cond.span, cursor) {
                collect_defs_before_cursor_in_expr(cond, cursor, def_table, scopes);
            } else if span_contains_pos(then_body.span, cursor) {
                collect_defs_before_cursor_in_expr(then_body, cursor, def_table, scopes);
            } else if span_contains_pos(else_body.span, cursor) {
                collect_defs_before_cursor_in_expr(else_body, cursor, def_table, scopes);
            }
        }
        res::ExprKind::Match { scrutinee, arms } => {
            if span_contains_pos(scrutinee.span, cursor) {
                collect_defs_before_cursor_in_expr(scrutinee, cursor, def_table, scopes);
                return;
            }
            for arm in arms {
                if !span_contains_pos(arm.span, cursor) {
                    continue;
                }
                scopes.push(HashMap::new());
                collect_match_pattern_bindings(&arm.pattern, def_table, scopes.last_mut().unwrap());
                collect_defs_before_cursor_in_expr(&arm.body, cursor, def_table, scopes);
                return;
            }
        }
        res::ExprKind::Closure { params, body, .. } => {
            if span_contains_pos(body.span, cursor) {
                scopes.push(HashMap::new());
                for param in params {
                    insert_def_into_scope(param.def_id, def_table, scopes.last_mut().unwrap());
                }
                collect_defs_before_cursor_in_expr(body, cursor, def_table, scopes);
            }
        }
        _ => {}
    }
}

fn collect_defs_at_cursor_in_stmt(
    stmt: &crate::tree::StmtExpr<DefId>,
    cursor: crate::diag::Position,
    def_table: &crate::resolve::DefTable,
    scopes: &mut Vec<HashMap<String, CompletionItem>>,
) {
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            if span_contains_pos(value.span, cursor) {
                collect_defs_before_cursor_in_expr(value, cursor, def_table, scopes);
            }
        }
        StmtExprKind::Assign {
            assignee, value, ..
        } => {
            if span_contains_pos(assignee.span, cursor) {
                collect_defs_before_cursor_in_expr(assignee, cursor, def_table, scopes);
            } else if span_contains_pos(value.span, cursor) {
                collect_defs_before_cursor_in_expr(value, cursor, def_table, scopes);
            }
        }
        StmtExprKind::While { cond, body } => {
            if span_contains_pos(cond.span, cursor) {
                collect_defs_before_cursor_in_expr(cond, cursor, def_table, scopes);
            } else if span_contains_pos(body.span, cursor) {
                collect_defs_before_cursor_in_expr(body, cursor, def_table, scopes);
            }
        }
        StmtExprKind::For {
            pattern,
            iter,
            body,
        } => {
            if span_contains_pos(iter.span, cursor) {
                collect_defs_before_cursor_in_expr(iter, cursor, def_table, scopes);
                return;
            }
            if span_contains_pos(body.span, cursor) {
                scopes.push(HashMap::new());
                collect_bind_pattern_bindings(pattern, def_table, scopes.last_mut().unwrap());
                collect_defs_before_cursor_in_expr(body, cursor, def_table, scopes);
            }
        }
        StmtExprKind::Return { value } => {
            if let Some(value) = value
                && span_contains_pos(value.span, cursor)
            {
                collect_defs_before_cursor_in_expr(value, cursor, def_table, scopes);
            }
        }
        StmtExprKind::VarDecl { .. } | StmtExprKind::Break | StmtExprKind::Continue => {}
    }
}

fn collect_stmt_bindings(
    stmt: &crate::tree::StmtExpr<DefId>,
    def_table: &crate::resolve::DefTable,
    scope: &mut HashMap<String, CompletionItem>,
) {
    match &stmt.kind {
        StmtExprKind::LetBind { pattern, .. } | StmtExprKind::VarBind { pattern, .. } => {
            collect_bind_pattern_bindings(pattern, def_table, scope);
        }
        StmtExprKind::VarDecl { def_id, .. } => {
            insert_def_into_scope(*def_id, def_table, scope);
        }
        _ => {}
    }
}

fn collect_bind_pattern_bindings(
    pattern: &crate::tree::BindPattern<DefId>,
    def_table: &crate::resolve::DefTable,
    scope: &mut HashMap<String, CompletionItem>,
) {
    match &pattern.kind {
        crate::tree::BindPatternKind::Name { def_id, .. } => {
            insert_def_into_scope(*def_id, def_table, scope);
        }
        crate::tree::BindPatternKind::Array { patterns }
        | crate::tree::BindPatternKind::Tuple { patterns } => {
            for sub in patterns {
                collect_bind_pattern_bindings(sub, def_table, scope);
            }
        }
        crate::tree::BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                collect_bind_pattern_bindings(&field.pattern, def_table, scope);
            }
        }
    }
}

fn collect_match_pattern_bindings(
    pattern: &crate::tree::MatchPattern<DefId>,
    def_table: &crate::resolve::DefTable,
    scope: &mut HashMap<String, CompletionItem>,
) {
    match pattern {
        crate::tree::MatchPattern::Binding { def_id, .. }
        | crate::tree::MatchPattern::TypedBinding { def_id, .. } => {
            insert_def_into_scope(*def_id, def_table, scope);
        }
        crate::tree::MatchPattern::Tuple { patterns, .. } => {
            for sub in patterns {
                collect_match_pattern_bindings(sub, def_table, scope);
            }
        }
        crate::tree::MatchPattern::EnumVariant { bindings, .. } => {
            for binding in bindings {
                if let crate::tree::MatchPatternBinding::Named { def_id, .. } = binding {
                    insert_def_into_scope(*def_id, def_table, scope);
                }
            }
        }
        _ => {}
    }
}

fn insert_def_into_scope(
    def_id: DefId,
    def_table: &crate::resolve::DefTable,
    scope: &mut HashMap<String, CompletionItem>,
) {
    let Some(def) = def_table.lookup_def(def_id) else {
        return;
    };
    let Some(kind) = completion_kind_for_def(&def.kind) else {
        return;
    };
    scope.insert(
        def.name.clone(),
        CompletionItem {
            label: def.name.clone(),
            kind,
            def_id: def.id,
            detail: Some(def.kind.to_string()),
        },
    );
}

fn enclosing_callable_def_id(
    module: &crate::tree::typed::Module,
    cursor: crate::diag::Position,
) -> Option<DefId> {
    let mut best: Option<(DefId, Span)> = None;
    for callable in module.callables() {
        let span = callable.span();
        if !span_contains_pos(span, cursor) {
            continue;
        }
        let replace = best
            .as_ref()
            .is_none_or(|(_, best_span)| span_width(span) <= span_width(*best_span));
        if replace {
            best = Some((callable.def_id(), span));
        }
    }
    best.map(|(def_id, _)| def_id)
}

fn span_contains_pos(span: Span, pos: crate::diag::Position) -> bool {
    position_leq(span.start, pos) && position_leq(pos, span.end)
}

fn span_width(span: Span) -> usize {
    span.end.offset.saturating_sub(span.start.offset)
}

fn is_ident_byte(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphanumeric()
}

fn offset_for_position(source: &str, pos: crate::diag::Position) -> Option<usize> {
    if pos.line == 0 || pos.column == 0 {
        return Some(0);
    }
    let mut line = 1usize;
    let mut col = 1usize;
    for (offset, ch) in source.char_indices() {
        if line == pos.line && col == pos.column {
            return Some(offset);
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line == pos.line && col == pos.column).then_some(source.len())
}

fn position_for_offset(source: &str, target_offset: usize) -> Option<crate::diag::Position> {
    if target_offset > source.len() {
        return None;
    }
    let mut line = 1usize;
    let mut column = 1usize;
    for (offset, ch) in source.char_indices() {
        if offset == target_offset {
            return Some(crate::diag::Position {
                offset: target_offset,
                line,
                column,
            });
        }
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    if target_offset == source.len() {
        return Some(crate::diag::Position {
            offset: target_offset,
            line,
            column,
        });
    }
    None
}

fn single_char_span(source: &str, offset: usize) -> Option<Span> {
    let start = position_for_offset(source, offset)?;
    let mut next_offset = source.len();
    for (idx, _) in source[offset..].char_indices().skip(1) {
        next_offset = offset + idx;
        break;
    }
    if next_offset == source.len() && offset < source.len() {
        next_offset = source.len();
    }
    let end = position_for_offset(source, next_offset)?;
    Some(Span { start, end })
}

fn synthesize_member_completion_source(
    source: &str,
    cursor: crate::diag::Position,
) -> Option<String> {
    let offset = offset_for_position(source, cursor)?;
    if offset == 0 || offset > source.len() {
        return None;
    }

    let bytes = source.as_bytes();
    let mut dot_probe = offset;
    while dot_probe > 0 && bytes[dot_probe - 1].is_ascii_whitespace() {
        dot_probe -= 1;
    }
    if dot_probe == 0 || bytes[dot_probe - 1] != b'.' {
        return None;
    }

    let mut synthesized = String::with_capacity(source.len() + 16);
    synthesized.push_str(&source[..offset]);
    synthesized.push_str("__mc_completion");
    if should_terminate_member_probe(source, offset) {
        synthesized.push(';');
    }
    synthesized.push_str(&source[offset..]);
    Some(synthesized)
}

fn should_terminate_member_probe(source: &str, offset: usize) -> bool {
    let mut i = offset;
    let bytes = source.as_bytes();
    while i < bytes.len() {
        match bytes[i] {
            b' ' | b'\t' => {
                i += 1;
            }
            b'/' if i + 1 < bytes.len() && bytes[i + 1] == b'/' => {
                return true;
            }
            b'\r' | b'\n' | b'}' => return true,
            b';' => return false,
            _ => return false,
        }
    }
    true
}

fn semantic_token_kind_for_def(kind: &DefKind) -> Option<SemanticTokenKind> {
    match kind {
        DefKind::TypeDef { .. } => Some(SemanticTokenKind::Type),
        DefKind::TraitDef { .. } => Some(SemanticTokenKind::Trait),
        DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => Some(SemanticTokenKind::Function),
        DefKind::TypeParam => Some(SemanticTokenKind::TypeParameter),
        DefKind::EnumVariantName => Some(SemanticTokenKind::EnumVariant),
        DefKind::LocalVar { .. } => Some(SemanticTokenKind::Variable),
        DefKind::Param { .. } => Some(SemanticTokenKind::Parameter),
    }
}

fn param_mode_name(mode: &crate::tree::ParamMode) -> &'static str {
    match mode {
        crate::tree::ParamMode::In => "in",
        crate::tree::ParamMode::InOut => "inout",
        crate::tree::ParamMode::Out => "out",
        crate::tree::ParamMode::Sink => "sink",
    }
}

struct SnapshotOverlayLoader {
    snapshot: AnalysisSnapshot,
    fs_loader: frontend::FsModuleLoader,
}

impl SnapshotOverlayLoader {
    fn new(snapshot: AnalysisSnapshot, project_root: PathBuf) -> Self {
        Self {
            snapshot,
            fs_loader: frontend::FsModuleLoader::new(project_root),
        }
    }
}

impl ModuleLoader for SnapshotOverlayLoader {
    fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), FrontendError> {
        let (file_path, disk_source) = self.fs_loader.load(path)?;
        if let Some(overlay) = snapshot_text_for_path(&self.snapshot, &file_path) {
            return Ok((file_path, overlay));
        }
        Ok((file_path, disk_source))
    }
}

fn snapshot_text_for_path(snapshot: &AnalysisSnapshot, path: &Path) -> Option<String> {
    if let Some(file_id) = snapshot.file_id(path) {
        return snapshot.text(file_id).map(|s| s.to_string());
    }
    if let Ok(canon) = path.canonicalize()
        && let Some(file_id) = snapshot.file_id(&canon)
    {
        return snapshot.text(file_id).map(|s| s.to_string());
    }
    None
}

fn infer_project_root(entry_file: &Path) -> PathBuf {
    for ancestor in entry_file.ancestors() {
        if ancestor.join("Cargo.toml").exists() {
            return ancestor.to_path_buf();
        }
    }
    entry_file
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf()
}

fn stable_source_revision(source: &str) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    source.hash(&mut hasher);
    hasher.finish()
}

fn frontend_error_diagnostics(error: FrontendError) -> Vec<Diagnostic> {
    fn frontend_diag(message: String, span: Span) -> Diagnostic {
        Diagnostic {
            phase: crate::analysis::diagnostics::DiagnosticPhase::Resolve,
            code: "MC-FRONTEND".to_string(),
            severity: crate::analysis::diagnostics::DiagnosticSeverity::Error,
            span,
            message,
            metadata: Default::default(),
        }
    }

    match error {
        FrontendError::Lex { error, .. } => vec![Diagnostic::from_lex_error(&error)],
        FrontendError::Parse { error, .. } => vec![Diagnostic::from_parse_error(&error)],
        FrontendError::UnknownRequireAlias { span, .. }
        | FrontendError::RequireMemberUndefined { span, .. }
        | FrontendError::RequireMemberPrivate { span, .. }
        | FrontendError::DuplicateRequireAlias { span, .. }
        | FrontendError::SymbolImportAliasUnsupported { span, .. } => {
            vec![frontend_diag(error.to_string(), span)]
        }
        _ => vec![frontend_diag(error.to_string(), Span::default())],
    }
}

#[cfg(test)]
#[path = "../tests/analysis/t_analysis_db.rs"]
mod tests;
