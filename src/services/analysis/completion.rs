//! Completion logic for analysis queries.
//!
//! This module owns completion context detection, scope/member candidate
//! collection, and fallback source synthesis for incomplete member probes.

use std::collections::{HashMap, HashSet};

use crate::core::capsule::ModuleId;
use crate::core::diag::{Position, Span};
use crate::core::resolve::{DefId, DefKind, UNKNOWN_DEF_ID};
use crate::core::tree::resolved as res;
use crate::core::tree::visit::Visitor;
use crate::core::tree::{BlockItem, NodeId, StmtExprKind};
use crate::core::types::Type;
use crate::services::analysis::results::{CompletionItem, CompletionKind};
use crate::services::analysis::syntax_index::{node_at_span, position_leq};

pub(crate) fn collect(
    source: &str,
    query_span: Span,
    resolved: &crate::core::context::ResolvedContext,
    typed: Option<&crate::core::context::TypeCheckedContext>,
) -> Vec<CompletionItem> {
    let site = classify_completion_site(source, query_span, resolved, typed);
    let items = dispatch_site_completions(&site, query_span.start, resolved);
    completion_post_pass(items, site.prefix())
}

pub(crate) fn synthesize_member_completion_source(
    source: &str,
    cursor: Position,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompletionSiteKind {
    Scope,
    Member,
    RequiresPath,
    TypeExpr,
    Pattern,
}

#[derive(Debug, Clone)]
enum CompletionSite {
    Scope {
        prefix: String,
        cursor: Position,
    },
    Member {
        prefix: String,
        receiver_ty: Type,
        caller_def_id: Option<DefId>,
    },
    RequiresPath {
        prefix: String,
    },
    TypeExpr {
        prefix: String,
    },
    Pattern {
        prefix: String,
    },
}

impl CompletionSite {
    fn kind(&self) -> CompletionSiteKind {
        match self {
            Self::Scope { .. } => CompletionSiteKind::Scope,
            Self::Member { .. } => CompletionSiteKind::Member,
            Self::RequiresPath { .. } => CompletionSiteKind::RequiresPath,
            Self::TypeExpr { .. } => CompletionSiteKind::TypeExpr,
            Self::Pattern { .. } => CompletionSiteKind::Pattern,
        }
    }

    fn prefix(&self) -> &str {
        match self {
            Self::Scope { prefix, .. }
            | Self::Member { prefix, .. }
            | Self::RequiresPath { prefix }
            | Self::TypeExpr { prefix }
            | Self::Pattern { prefix } => prefix,
        }
    }
}

struct CompletionProviders {
    scope: fn(&CompletionSite, &crate::core::context::ResolvedContext) -> Vec<CompletionItem>,
    member: fn(&CompletionSite, &crate::core::context::ResolvedContext) -> Vec<CompletionItem>,
    requires_path:
        fn(&CompletionSite, &crate::core::context::ResolvedContext) -> Vec<CompletionItem>,
    type_expr: fn(&CompletionSite, &crate::core::context::ResolvedContext) -> Vec<CompletionItem>,
    pattern: fn(&CompletionSite, &crate::core::context::ResolvedContext) -> Vec<CompletionItem>,
}

impl Default for CompletionProviders {
    fn default() -> Self {
        Self {
            scope: provide_scope_completions,
            member: provide_member_completions,
            requires_path: provide_requires_path_completions,
            type_expr: provide_type_expr_completions,
            pattern: provide_pattern_completions,
        }
    }
}

#[derive(Debug, Clone)]
struct PrefixProbe {
    prefix: String,
    dot_probe: usize,
}

fn classify_completion_site(
    source: &str,
    query_span: Span,
    resolved: &crate::core::context::ResolvedContext,
    typed: Option<&crate::core::context::TypeCheckedContext>,
) -> CompletionSite {
    let cursor = query_span.start;
    let offset = offset_for_position(source, query_span.start).unwrap_or(source.len());
    let probe = prefix_probe(source, offset);

    if let Some(site) = classify_member_site(source, query_span, typed, &probe) {
        return site;
    }
    if requires_site_at_cursor(&resolved.module, cursor) {
        return CompletionSite::RequiresPath {
            prefix: probe.prefix,
        };
    }
    let pattern_classifier = PatternAndTypeClassifier::classify(&resolved.module, cursor);
    if pattern_classifier.in_type_expr {
        return CompletionSite::TypeExpr {
            prefix: probe.prefix,
        };
    }
    if pattern_classifier.in_pattern {
        return CompletionSite::Pattern {
            prefix: probe.prefix,
        };
    }

    CompletionSite::Scope {
        prefix: probe.prefix,
        cursor,
    }
}

fn prefix_probe(source: &str, offset: usize) -> PrefixProbe {
    let bytes = source.as_bytes();
    let mut prefix_start = offset;
    while prefix_start > 0 && is_ident_byte(bytes[prefix_start - 1]) {
        prefix_start -= 1;
    }
    let prefix = source[prefix_start..offset.min(source.len())].to_string();

    let mut dot_probe = prefix_start;
    while dot_probe > 0 && bytes[dot_probe - 1].is_ascii_whitespace() {
        dot_probe -= 1;
    }

    PrefixProbe { prefix, dot_probe }
}

fn classify_member_site(
    source: &str,
    query_span: Span,
    typed: Option<&crate::core::context::TypeCheckedContext>,
    probe: &PrefixProbe,
) -> Option<CompletionSite> {
    let bytes = source.as_bytes();
    if probe.dot_probe == 0 || bytes[probe.dot_probe - 1] != b'.' {
        return None;
    }
    let Some(typed) = typed else {
        return None;
    };

    let mut recv_end = probe.dot_probe - 1;
    while recv_end > 0 && bytes[recv_end - 1].is_ascii_whitespace() {
        recv_end -= 1;
    }
    if recv_end == 0 {
        return None;
    }
    let receiver_char = recv_end - 1;
    let Some(receiver_span) = single_char_span(source, receiver_char) else {
        return None;
    };
    let Some(node_id) = node_at_span(&typed.module, receiver_span) else {
        return None;
    };
    let Some(receiver_ty) = typed.type_map.lookup_node_type(node_id) else {
        return None;
    };

    Some(CompletionSite::Member {
        prefix: probe.prefix.clone(),
        receiver_ty,
        caller_def_id: enclosing_callable_def_id(&typed.module, query_span.start),
    })
}

fn requires_site_at_cursor(module: &res::Module, cursor: Position) -> bool {
    module
        .requires
        .iter()
        .any(|req| span_contains_pos(req.span, cursor))
}

fn dispatch_site_completions(
    site: &CompletionSite,
    fallback_cursor: Position,
    resolved: &crate::core::context::ResolvedContext,
) -> Vec<CompletionItem> {
    let providers = CompletionProviders::default();
    let items = match site.kind() {
        CompletionSiteKind::Scope => (providers.scope)(site, resolved),
        CompletionSiteKind::Member => (providers.member)(site, resolved),
        CompletionSiteKind::RequiresPath => (providers.requires_path)(site, resolved),
        CompletionSiteKind::TypeExpr => (providers.type_expr)(site, resolved),
        CompletionSiteKind::Pattern => (providers.pattern)(site, resolved),
    };
    if items.is_empty()
        && matches!(
            site.kind(),
            CompletionSiteKind::RequiresPath
                | CompletionSiteKind::TypeExpr
                | CompletionSiteKind::Pattern
        )
    {
        scope_completions(resolved, fallback_cursor)
    } else {
        items
    }
}

fn completion_post_pass(mut items: Vec<CompletionItem>, prefix: &str) -> Vec<CompletionItem> {
    items.retain(|item| item.label.starts_with(prefix));
    let mut seen = HashSet::<String>::new();
    items.retain(|item| seen.insert(item.label.clone()));
    items.sort_unstable_by(|a, b| a.label.cmp(&b.label));
    items
}

fn provide_scope_completions(
    site: &CompletionSite,
    resolved: &crate::core::context::ResolvedContext,
) -> Vec<CompletionItem> {
    match site {
        CompletionSite::Scope { cursor, .. } => scope_completions(resolved, *cursor),
        _ => Vec::new(),
    }
}

fn provide_member_completions(
    site: &CompletionSite,
    resolved: &crate::core::context::ResolvedContext,
) -> Vec<CompletionItem> {
    match site {
        CompletionSite::Member {
            receiver_ty,
            caller_def_id,
            ..
        } => member_completions(resolved, receiver_ty, *caller_def_id),
        _ => Vec::new(),
    }
}

fn provide_requires_path_completions(
    _site: &CompletionSite,
    _resolved: &crate::core::context::ResolvedContext,
) -> Vec<CompletionItem> {
    Vec::new()
}

fn provide_type_expr_completions(
    _site: &CompletionSite,
    _resolved: &crate::core::context::ResolvedContext,
) -> Vec<CompletionItem> {
    Vec::new()
}

fn provide_pattern_completions(
    _site: &CompletionSite,
    _resolved: &crate::core::context::ResolvedContext,
) -> Vec<CompletionItem> {
    Vec::new()
}

struct PatternAndTypeClassifier {
    cursor: Position,
    in_type_expr: bool,
    in_pattern: bool,
}

impl PatternAndTypeClassifier {
    fn classify(module: &res::Module, cursor: Position) -> Self {
        let mut classifier = Self {
            cursor,
            in_type_expr: false,
            in_pattern: false,
        };
        classifier.visit_module(module);
        classifier
    }
}

impl Visitor<DefId, ()> for PatternAndTypeClassifier {
    fn visit_type_expr(&mut self, type_expr: &crate::core::tree::TypeExpr<DefId>) {
        if span_contains_pos(type_expr.span, self.cursor) {
            self.in_type_expr = true;
        }
        crate::core::tree::visit::walk_type_expr(self, type_expr);
    }

    fn visit_bind_pattern(&mut self, pattern: &crate::core::tree::BindPattern<DefId>) {
        if span_contains_pos(pattern.span, self.cursor) {
            self.in_pattern = true;
        }
        crate::core::tree::visit::walk_bind_pattern(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &crate::core::tree::MatchPattern<DefId>) {
        let span = match pattern {
            crate::core::tree::MatchPattern::Wildcard { span }
            | crate::core::tree::MatchPattern::BoolLit { span, .. }
            | crate::core::tree::MatchPattern::IntLit { span, .. }
            | crate::core::tree::MatchPattern::Binding { span, .. }
            | crate::core::tree::MatchPattern::TypedBinding { span, .. }
            | crate::core::tree::MatchPattern::Tuple { span, .. }
            | crate::core::tree::MatchPattern::EnumVariant { span, .. } => *span,
        };
        if span_contains_pos(span, self.cursor) {
            self.in_pattern = true;
        }
        crate::core::tree::visit::walk_match_pattern(self, pattern);
    }
}

fn scope_completions(
    resolved: &crate::core::context::ResolvedContext,
    cursor: crate::core::diag::Position,
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
    resolved: &crate::core::context::ResolvedContext,
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
    resolved: &crate::core::context::ResolvedContext,
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
    resolved: &crate::core::context::ResolvedContext,
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
    resolved: &crate::core::context::ResolvedContext,
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

fn global_scope(
    resolved: &crate::core::context::ResolvedContext,
) -> HashMap<String, CompletionItem> {
    let mut allowed_nodes = HashSet::new();
    for item in &resolved.module.top_level_items {
        match item {
            res::TopLevelItem::ProtocolDef(protocol_def) => {
                allowed_nodes.insert(protocol_def.id);
            }
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
            res::TopLevelItem::TypestateDef(typestate_def) => {
                allowed_nodes.insert(typestate_def.id);
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
    cursor: crate::core::diag::Position,
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
    cursor: crate::core::diag::Position,
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
    def_table: &crate::core::resolve::DefTable,
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
    cursor: crate::core::diag::Position,
    def_table: &crate::core::resolve::DefTable,
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
    stmt: &crate::core::tree::StmtExpr<DefId>,
    cursor: crate::core::diag::Position,
    def_table: &crate::core::resolve::DefTable,
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
    stmt: &crate::core::tree::StmtExpr<DefId>,
    def_table: &crate::core::resolve::DefTable,
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
    pattern: &crate::core::tree::BindPattern<DefId>,
    def_table: &crate::core::resolve::DefTable,
    scope: &mut HashMap<String, CompletionItem>,
) {
    match &pattern.kind {
        crate::core::tree::BindPatternKind::Name { def_id, .. } => {
            insert_def_into_scope(*def_id, def_table, scope);
        }
        crate::core::tree::BindPatternKind::Array { patterns }
        | crate::core::tree::BindPatternKind::Tuple { patterns } => {
            for sub in patterns {
                collect_bind_pattern_bindings(sub, def_table, scope);
            }
        }
        crate::core::tree::BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                collect_bind_pattern_bindings(&field.pattern, def_table, scope);
            }
        }
    }
}

fn collect_match_pattern_bindings(
    pattern: &crate::core::tree::MatchPattern<DefId>,
    def_table: &crate::core::resolve::DefTable,
    scope: &mut HashMap<String, CompletionItem>,
) {
    match pattern {
        crate::core::tree::MatchPattern::Binding { def_id, .. }
        | crate::core::tree::MatchPattern::TypedBinding { def_id, .. } => {
            insert_def_into_scope(*def_id, def_table, scope);
        }
        crate::core::tree::MatchPattern::Tuple { patterns, .. } => {
            for sub in patterns {
                collect_match_pattern_bindings(sub, def_table, scope);
            }
        }
        crate::core::tree::MatchPattern::EnumVariant { bindings, .. } => {
            for binding in bindings {
                if let crate::core::tree::MatchPatternBinding::Named { def_id, .. } = binding {
                    insert_def_into_scope(*def_id, def_table, scope);
                }
            }
        }
        _ => {}
    }
}

fn insert_def_into_scope(
    def_id: DefId,
    def_table: &crate::core::resolve::DefTable,
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
    module: &crate::core::tree::typed::Module,
    cursor: crate::core::diag::Position,
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

fn span_contains_pos(span: Span, pos: crate::core::diag::Position) -> bool {
    position_leq(span.start, pos) && position_leq(pos, span.end)
}

fn span_width(span: Span) -> usize {
    span.end.offset.saturating_sub(span.start.offset)
}

fn is_ident_byte(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphanumeric()
}

fn offset_for_position(source: &str, pos: crate::core::diag::Position) -> Option<usize> {
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

fn position_for_offset(source: &str, target_offset: usize) -> Option<crate::core::diag::Position> {
    if target_offset > source.len() {
        return None;
    }
    let mut line = 1usize;
    let mut column = 1usize;
    for (offset, ch) in source.char_indices() {
        if offset == target_offset {
            return Some(crate::core::diag::Position {
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
        return Some(crate::core::diag::Position {
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::core::api::{
        parse_module_with_id_gen, resolve_stage_partial, typecheck_stage_partial,
    };
    use crate::core::context::ParsedContext;
    use crate::core::diag::{Position, Span};
    use crate::core::tree::NodeIdGen;

    use super::{CompletionSite, classify_completion_site, position_for_offset};

    #[test]
    fn classify_site_detects_member_context() {
        let source = r#"
type Point = { x: u64 }

Point :: {
    fn get_x(self) -> u64 { self.x }
}

fn main() {
    let p = Point { x: 1 };
    p.x
}
"#;
        let (resolved, typed) = resolved_and_typed(source);
        let cursor = cursor_at_end_of(source, "p.");
        let site = classify_completion_site(source, cursor, &resolved, Some(&typed));
        assert!(matches!(site, CompletionSite::Member { .. }));
    }

    #[test]
    fn classify_site_detects_requires_path_context() {
        let source = r#"
requires {
    std::io
}

fn main() {}
"#;
        let resolved = resolved_only(source);
        let cursor = cursor_at_end_of(source, "std::i");
        let site = classify_completion_site(source, cursor, &resolved, None);
        assert!(matches!(site, CompletionSite::RequiresPath { .. }));
    }

    #[test]
    fn classify_site_detects_type_expression_context() {
        let source = r#"
fn main() {
    let n: u64 = 1;
}
"#;
        let resolved = resolved_only(source);
        let cursor = cursor_at_end_of(source, "u64");
        let site = classify_completion_site(source, cursor, &resolved, None);
        assert!(matches!(site, CompletionSite::TypeExpr { .. }));
    }

    #[test]
    fn classify_site_detects_pattern_context() {
        let source = r#"
fn f() -> u64 {
    match 1 {
        value => value,
    }
}
"#;
        let resolved = resolved_only(source);
        let cursor = cursor_at_end_of(source, "valu");
        let site = classify_completion_site(source, cursor, &resolved, None);
        assert!(matches!(site, CompletionSite::Pattern { .. }));
    }

    fn resolved_only(source: &str) -> crate::core::context::ResolvedContext {
        let (module, id_gen) =
            parse_module_with_id_gen(source, NodeIdGen::new()).expect("parse should succeed");
        let parsed = ParsedContext::new(module, id_gen);
        resolve_stage_partial(parsed, HashMap::new(), HashMap::new()).context
    }

    fn resolved_and_typed(
        source: &str,
    ) -> (
        crate::core::context::ResolvedContext,
        crate::core::context::TypeCheckedContext,
    ) {
        let (module, id_gen) =
            parse_module_with_id_gen(source, NodeIdGen::new()).expect("parse should succeed");
        let parsed = ParsedContext::new(module, id_gen);
        let resolve_out = resolve_stage_partial(parsed, HashMap::new(), HashMap::new());
        let resolved = resolve_out.context;
        let typed = typecheck_stage_partial(resolved.clone(), resolve_out.imported_facts).context;
        (resolved, typed)
    }

    fn cursor_at_end_of(source: &str, needle: &str) -> Span {
        let idx = source
            .find(needle)
            .unwrap_or_else(|| panic!("needle not found: {needle}"));
        let offset = idx + needle.len();
        let pos = position_for_offset(source, offset).unwrap_or_else(|| Position {
            offset,
            line: 1,
            column: 1,
        });
        Span {
            start: pos,
            end: pos,
        }
    }
}
