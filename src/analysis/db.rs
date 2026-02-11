//! Analysis database facade.
//!
//! This composes the foundational query runtime pieces:
//! - revisioned source snapshots,
//! - query memoization/dependency tracking,
//! - module-closure invalidation.

use std::collections::HashSet;
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
    span_intersects_span,
};
use crate::diag::Span;
use crate::frontend::ModuleId;
use crate::resolve::{DefId, DefKind};
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
        Ok(resolved.def_table.lookup_node_def_id(node_id))
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
        let Some(def_id) = resolved.def_table.lookup_node_def_id(use_node_id) else {
            return Ok(None);
        };
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
        Ok(typed.type_map.lookup_node_type(node_id))
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
            resolved, typed, ..
        } = self.lookup_state_for_file(file_id)?;

        if let Some(typed) = typed {
            if let Some(node_id) = node_at_span(&typed.module, query_span) {
                let def_id = typed.def_table.lookup_node_def_id(node_id);
                let def_name =
                    def_id.and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()));
                let ty = typed.type_map.lookup_node_type(node_id);
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
        let def_id = resolved.def_table.lookup_node_def_id(node_id);
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
        _query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        let state = self.lookup_state_for_file(file_id)?;
        let Some(resolved) = state.resolved else {
            return Ok(Vec::new());
        };

        let mut out = Vec::new();
        for def in resolved.def_table.defs() {
            let Some(kind) = completion_kind_for_def(&def.kind) else {
                continue;
            };
            out.push(CompletionItem {
                label: def.name.clone(),
                kind,
                def_id: def.id,
                detail: Some(def.kind.to_string()),
            });
        }
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

#[cfg(test)]
#[path = "../tests/analysis/t_analysis_db.rs"]
mod tests;
