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
use crate::analysis::results::{CompletionItem, CompletionKind, HoverInfo, SignatureHelp};
use crate::analysis::snapshot::{AnalysisSnapshot, FileId, SourceStore};
use crate::diag::{Position, Span};
use crate::frontend::ModuleId;
use crate::lexer::{LexError, Lexer};
use crate::parse::Parser;
use crate::resolve::{DefId, DefKind, resolve};
use crate::tree::visit::{self, Visitor};
use crate::tree::{Expr, MatchPattern, MatchPatternBinding, NodeId, NodeIdGen, StmtExpr};
use crate::typecheck::type_check;
use crate::types::Type;

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
    typed: Option<crate::context::TypeCheckedContext>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, Default)]
struct LookupState {
    resolved: Option<crate::context::ResolvedContext>,
    typed: Option<crate::context::TypeCheckedContext>,
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
        let LookupState { resolved, typed } = self.lookup_state_for_file(file_id)?;

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
        self.execute_query(lookup_key, move |_rt| {
            let lexer = Lexer::new(&source);
            let tokens = match lexer.tokenize().collect::<Result<Vec<_>, LexError>>() {
                Ok(tokens) => tokens,
                Err(_) => return Ok(LookupState::default()),
            };

            let id_gen = NodeIdGen::new();
            let mut parser = Parser::new_with_id_gen(&tokens, id_gen);
            let parsed_module = match parser.parse() {
                Ok(module) => module,
                Err(_) => return Ok(LookupState::default()),
            };
            let parsed = crate::context::ParsedContext::new(parsed_module, parser.into_id_gen());

            let resolved = match resolve(parsed) {
                Ok(resolved) => resolved,
                Err(_) => return Ok(LookupState::default()),
            };
            let typed = type_check(resolved.clone()).ok();

            Ok(LookupState {
                resolved: Some(resolved),
                typed,
            })
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

fn node_at_span<D, T>(module: &crate::tree::Module<D, T>, query_span: Span) -> Option<NodeId> {
    let mut collector = NodeSpanCollector::default();
    collector.visit_module(module);

    let mut best: Option<(NodeId, Span)> = None;
    for (node_id, span) in collector.nodes {
        if !span_contains_span(span, query_span) {
            continue;
        }
        let replace = best.as_ref().is_none_or(|(_, best_span)| {
            let width = span_width(span);
            let best_width = span_width(*best_span);
            width < best_width
                || (width == best_width && span.start.offset >= best_span.start.offset)
        });
        if replace {
            best = Some((node_id, span));
        }
    }
    best.map(|(node_id, _)| node_id)
}

fn span_width(span: Span) -> usize {
    span.end.offset.saturating_sub(span.start.offset)
}

fn span_contains_span(span: Span, query: Span) -> bool {
    position_leq(span.start, query.start) && position_leq(query.end, span.end)
}

fn position_leq(lhs: Position, rhs: Position) -> bool {
    (lhs.line, lhs.column) <= (rhs.line, rhs.column)
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

fn param_mode_name(mode: &crate::tree::ParamMode) -> &'static str {
    match mode {
        crate::tree::ParamMode::In => "in",
        crate::tree::ParamMode::InOut => "inout",
        crate::tree::ParamMode::Out => "out",
        crate::tree::ParamMode::Sink => "sink",
    }
}

#[derive(Clone, Debug)]
struct CallSite {
    node_id: NodeId,
    span: Span,
    arg_spans: Vec<Span>,
}

fn call_site_at_span<D, T>(
    module: &crate::tree::Module<D, T>,
    query_span: Span,
) -> Option<CallSite> {
    let mut collector = CallSiteCollector::default();
    collector.visit_module(module);
    let mut best: Option<CallSite> = None;
    for call in collector.calls {
        if !span_contains_span(call.span, query_span) {
            continue;
        }
        let replace = best
            .as_ref()
            .is_none_or(|best_call| span_width(call.span) <= span_width(best_call.span));
        if replace {
            best = Some(call);
        }
    }
    best
}

fn active_param_index(arg_spans: &[Span], pos: Position) -> usize {
    if arg_spans.is_empty() {
        return 0;
    }
    for (i, span) in arg_spans.iter().enumerate() {
        if position_leq(pos, span.end) {
            return i;
        }
    }
    arg_spans.len().saturating_sub(1)
}

#[derive(Default)]
struct CallSiteCollector {
    calls: Vec<CallSite>,
}

impl<D, T> Visitor<D, T> for CallSiteCollector {
    fn visit_expr(&mut self, expr: &Expr<D, T>) {
        match &expr.kind {
            crate::tree::ExprKind::Call { args, .. }
            | crate::tree::ExprKind::MethodCall { args, .. } => {
                self.calls.push(CallSite {
                    node_id: expr.id,
                    span: expr.span,
                    arg_spans: args.iter().map(|arg| arg.span).collect(),
                });
            }
            _ => {}
        }
        visit::walk_expr(self, expr);
    }
}

#[derive(Default)]
struct NodeSpanCollector {
    nodes: Vec<(NodeId, Span)>,
}

impl NodeSpanCollector {
    fn record(&mut self, id: NodeId, span: Span) {
        self.nodes.push((id, span));
    }
}

impl<D, T> Visitor<D, T> for NodeSpanCollector {
    fn visit_type_def(&mut self, type_def: &crate::tree::TypeDef<D>) {
        self.record(type_def.id, type_def.span);
        visit::walk_type_def::<Self, D, T>(self, type_def);
    }

    fn visit_trait_def(&mut self, trait_def: &crate::tree::TraitDef<D>) {
        self.record(trait_def.id, trait_def.span);
        visit::walk_trait_def::<Self, D, T>(self, trait_def);
    }

    fn visit_trait_method(&mut self, method: &crate::tree::TraitMethod<D>) {
        self.record(method.id, method.span);
        visit::walk_trait_method::<Self, D, T>(self, method);
    }

    fn visit_trait_property(&mut self, property: &crate::tree::TraitProperty<D>) {
        self.record(property.id, property.span);
        visit::walk_trait_property::<Self, D, T>(self, property);
    }

    fn visit_type_expr(&mut self, type_expr: &crate::tree::TypeExpr<D>) {
        self.record(type_expr.id, type_expr.span);
        visit::walk_type_expr::<Self, D, T>(self, type_expr);
    }

    fn visit_func_decl(&mut self, func_decl: &crate::tree::FuncDecl<D>) {
        self.record(func_decl.id, func_decl.span);
        visit::walk_func_decl::<Self, D, T>(self, func_decl);
    }

    fn visit_func_def(&mut self, func_def: &crate::tree::FuncDef<D, T>) {
        self.record(func_def.id, func_def.span);
        visit::walk_func_def(self, func_def);
    }

    fn visit_type_param(&mut self, param: &crate::tree::TypeParam<D>) {
        self.record(param.id, param.span);
        visit::walk_type_param::<Self, D, T>(self, param);
    }

    fn visit_method_block(&mut self, method_block: &crate::tree::MethodBlock<D, T>) {
        self.record(method_block.id, method_block.span);
        visit::walk_method_block(self, method_block);
    }

    fn visit_method_decl(&mut self, method_decl: &crate::tree::MethodDecl<D>) {
        self.record(method_decl.id, method_decl.span);
        visit::walk_method_decl::<Self, D, T>(self, method_decl);
    }

    fn visit_method_def(&mut self, method_def: &crate::tree::MethodDef<D, T>) {
        self.record(method_def.id, method_def.span);
        visit::walk_method_def(self, method_def);
    }

    fn visit_closure_def(&mut self, closure_def: &crate::tree::ClosureDef<D, T>) {
        self.record(closure_def.id, closure_def.span);
        visit::walk_closure_def(self, closure_def);
    }

    fn visit_param(&mut self, param: &crate::tree::Param<D>) {
        self.record(param.id, param.span);
        visit::walk_param::<Self, D, T>(self, param);
    }

    fn visit_bind_pattern(&mut self, pattern: &crate::tree::BindPattern<D>) {
        self.record(pattern.id, pattern.span);
        visit::walk_bind_pattern::<Self, D, T>(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &MatchPattern<D>) {
        match pattern {
            MatchPattern::Binding { id, span, .. }
            | MatchPattern::TypedBinding { id, span, .. }
            | MatchPattern::EnumVariant { id, span, .. } => self.record(*id, *span),
            MatchPattern::Wildcard { .. }
            | MatchPattern::BoolLit { .. }
            | MatchPattern::IntLit { .. }
            | MatchPattern::Tuple { .. } => {}
        }
        visit::walk_match_pattern::<Self, D, T>(self, pattern);
        visit::walk_match_pattern_bindings::<Self, D, T>(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding<D>) {
        if let MatchPatternBinding::Named { id, span, .. } = binding {
            self.record(*id, *span);
        }
        visit::walk_match_pattern_binding::<Self, D, T>(self, binding);
    }

    fn visit_match_arm(&mut self, arm: &crate::tree::MatchArm<D, T>) {
        self.record(arm.id, arm.span);
        visit::walk_match_arm(self, arm);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr<D, T>) {
        self.record(stmt.id, stmt.span);
        visit::walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr<D, T>) {
        self.record(expr.id, expr.span);
        visit::walk_expr(self, expr);
    }
}

#[cfg(test)]
#[path = "../tests/analysis/t_analysis_db.rs"]
mod tests;
