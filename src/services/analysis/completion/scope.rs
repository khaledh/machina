//! Scope-oriented completion collection.
//!
//! This module walks enclosing callables/blocks to surface visible locals,
//! params, and top-level symbols for scope-completion queries.

use std::collections::{HashMap, HashSet};

use crate::core::diag::{Position, Span};
use crate::core::resolve::{DefId, DefKind};
use crate::core::tree::resolved as res;
use crate::core::tree::{BlockItem, NodeId, StmtExprKind};
use crate::services::analysis::results::{CompletionItem, CompletionKind};
use crate::services::analysis::syntax_index::position_leq;

pub(super) fn scope_completions(
    resolved: &crate::core::context::ResolvedContext,
    cursor: Position,
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

pub(super) fn enclosing_callable_def_id(
    module: &crate::core::tree::typed::Module,
    cursor: Position,
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

fn containing_callable(module: &res::Module, cursor: Position) -> Option<CallableAtCursor<'_>> {
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
    cursor: Position,
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
    cursor: Position,
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
    cursor: Position,
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

fn span_contains_pos(span: Span, pos: Position) -> bool {
    position_leq(span.start, pos) && position_leq(pos, span.end)
}

fn span_width(span: Span) -> usize {
    span.end.offset.saturating_sub(span.start.offset)
}
