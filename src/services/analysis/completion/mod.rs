//! Completion logic for analysis queries.
//!
//! This module owns completion context detection, scope/member candidate
//! collection, and fallback source synthesis for incomplete member probes.

mod site;
mod source_probe;

use std::collections::{HashMap, HashSet};

use crate::core::capsule::ModuleId;
use crate::core::diag::{Position, Span};
use crate::core::resolve::{DefId, DefKind, UNKNOWN_DEF_ID};
use crate::core::tree::resolved as res;
use crate::core::tree::{BlockItem, NodeId, StmtExprKind};
use crate::core::types::Type;
use crate::services::analysis::results::{CompletionItem, CompletionKind};
use crate::services::analysis::syntax_index::position_leq;
use site::{CompletionSite, CompletionSiteKind, classify_completion_site};

pub(crate) fn collect(
    source: &str,
    query_span: Span,
    resolved: &crate::core::context::ResolvedContext,
    typed: Option<&crate::core::context::TypeCheckedContext>,
) -> Vec<CompletionItem> {
    let site = classify_completion_site(
        source,
        query_span,
        resolved,
        typed,
        enclosing_callable_def_id,
    );
    let items = dispatch_site_completions(&site, query_span.start, resolved);
    completion_post_pass(items, site.prefix())
}

pub(crate) fn synthesize_member_completion_source(
    source: &str,
    cursor: Position,
) -> Option<String> {
    source_probe::synthesize_member_completion_source(source, cursor)
}

struct CompletionProviders {
    scope: fn(&CompletionSite, &crate::core::context::ResolvedContext) -> Vec<CompletionItem>,
    member: fn(&CompletionSite, &crate::core::context::ResolvedContext) -> Vec<CompletionItem>,
    qualified_path:
        fn(&CompletionSite, &crate::core::context::ResolvedContext) -> Vec<CompletionItem>,
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
            qualified_path: provide_qualified_path_completions,
            requires_path: provide_requires_path_completions,
            type_expr: provide_type_expr_completions,
            pattern: provide_pattern_completions,
        }
    }
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
        CompletionSiteKind::QualifiedPath => (providers.qualified_path)(site, resolved),
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

fn provide_qualified_path_completions(
    site: &CompletionSite,
    resolved: &crate::core::context::ResolvedContext,
) -> Vec<CompletionItem> {
    match site {
        CompletionSite::QualifiedPath { path_segments, .. } => {
            qualified_path_completions(resolved, path_segments)
        }
        _ => Vec::new(),
    }
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

fn qualified_path_completions(
    resolved: &crate::core::context::ResolvedContext,
    path_segments: &[String],
) -> Vec<CompletionItem> {
    // V1 scope: resolve first-segment nominal paths (`Protocol::Role`,
    // `Enum::Variant`) that appear in typestate/protocol surface syntax.
    if path_segments.len() != 1 {
        return Vec::new();
    }
    let Some(owner_name) = path_segments.first() else {
        return Vec::new();
    };

    for item in &resolved.module.top_level_items {
        match item {
            res::TopLevelItem::ProtocolDef(protocol_def) if protocol_def.name == *owner_name => {
                return protocol_def
                    .roles
                    .iter()
                    .map(|role| CompletionItem {
                        label: role.name.clone(),
                        kind: CompletionKind::EnumVariant,
                        def_id: role.def_id,
                        detail: Some("protocol role".to_string()),
                    })
                    .collect();
            }
            res::TopLevelItem::TypeDef(type_def) if type_def.name == *owner_name => {
                if let res::TypeDefKind::Enum { variants } = &type_def.kind {
                    return variants
                        .iter()
                        .map(|variant| CompletionItem {
                            label: variant.name.clone(),
                            kind: CompletionKind::EnumVariant,
                            def_id: resolved
                                .def_table
                                .lookup_node_def_id(variant.id)
                                .unwrap_or(UNKNOWN_DEF_ID),
                            detail: Some("enum variant".to_string()),
                        })
                        .collect();
                }
                return Vec::new();
            }
            _ => {}
        }
    }
    Vec::new()
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
        }
        | StmtExprKind::CompoundAssign {
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
        DefKind::ProtocolDef => Some(CompletionKind::Type),
        DefKind::ProtocolRole => Some(CompletionKind::EnumVariant),
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::core::api::{
        parse_module_with_id_gen, resolve_stage_partial, typecheck_stage_partial,
    };
    use crate::core::context::ParsedContext;
    use crate::core::diag::{Position, Span};
    use crate::core::tree::NodeIdGen;

    use super::source_probe::position_for_offset;
    use super::{CompletionSite, classify_completion_site, enclosing_callable_def_id};

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
        let site = classify_completion_site(
            source,
            cursor,
            &resolved,
            Some(&typed),
            enclosing_callable_def_id,
        );
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
        let site =
            classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
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
        let site =
            classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
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
        let site =
            classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
        assert!(matches!(site, CompletionSite::Pattern { .. }));
    }

    #[test]
    fn classify_site_detects_qualified_path_context() {
        let source = r#"
type Flag = On | Off

fn main() -> u64 {
    let x = Flag::Of;
    0
}
"#;
        let resolved = resolved_only(source);
        let cursor = cursor_at_end_of(source, "Flag::Of");
        let site =
            classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
        assert!(matches!(site, CompletionSite::QualifiedPath { .. }));
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
