//! Local-scope completion collection.
//!
//! This module walks callable/block/pattern structure around the cursor and
//! returns scope frames in visibility order (outer -> inner).

use std::collections::HashMap;

use crate::core::diag::{Position, Span};
use crate::core::resolve::DefId;
use crate::core::tree::{
    BlockItem, ClosureDef, Expr, ExprKind, FuncDef, MethodDef, MethodItem, Module, StmtExprKind,
    TopLevelItem,
};
use crate::services::analysis::results::CompletionItem;
use crate::services::analysis::syntax_index::position_leq;

use super::global::completion_kind_for_def;

pub(super) fn collect_local_scopes(
    module: &Module,
    def_table: &crate::core::resolve::DefTable,
    cursor: Position,
) -> Vec<HashMap<String, CompletionItem>> {
    let mut collector = LocalScopeCollector::new(cursor, def_table);
    if let Some(callable) = containing_callable(module, cursor) {
        collector.collect_callable(callable);
    }
    collector.into_scopes()
}

pub(super) fn enclosing_callable_def_id(_module: &Module, _cursor: Position) -> Option<DefId> {
    None
}

struct LocalScopeCollector<'a> {
    cursor: Position,
    def_table: &'a crate::core::resolve::DefTable,
    scopes: Vec<HashMap<String, CompletionItem>>,
}

impl<'a> LocalScopeCollector<'a> {
    fn new(cursor: Position, def_table: &'a crate::core::resolve::DefTable) -> Self {
        Self {
            cursor,
            def_table,
            scopes: Vec::new(),
        }
    }

    fn into_scopes(self) -> Vec<HashMap<String, CompletionItem>> {
        self.scopes
    }

    fn collect_callable(&mut self, callable: CallableAtCursor<'_>) {
        self.push_scope();
        self.add_callable_params(&callable);
        self.collect_expr(callable.body());
    }

    fn collect_expr(&mut self, expr: &Expr) {
        if !self.span_contains_pos(expr.span) {
            return;
        }
        if let ExprKind::Block { items, tail } = &expr.kind {
            self.push_scope();
            for item in items {
                match item {
                    BlockItem::Stmt(stmt) => {
                        if position_leq(stmt.span.end, self.cursor) {
                            self.collect_stmt_bindings(stmt);
                            continue;
                        }
                        if self.span_contains_pos(stmt.span) {
                            self.collect_stmt_at_cursor(stmt);
                            return;
                        }
                    }
                    BlockItem::Expr(item_expr) => {
                        if self.span_contains_pos(item_expr.span) {
                            self.collect_expr(item_expr);
                            return;
                        }
                    }
                }
            }
            if let Some(tail) = tail
                && self.span_contains_pos(tail.span)
            {
                self.collect_expr(tail);
            }
            return;
        }

        match &expr.kind {
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                if self.span_contains_pos(cond.span) {
                    self.collect_expr(cond);
                } else if self.span_contains_pos(then_body.span) {
                    self.collect_expr(then_body);
                } else if self.span_contains_pos(else_body.span) {
                    self.collect_expr(else_body);
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                if self.span_contains_pos(scrutinee.span) {
                    self.collect_expr(scrutinee);
                    return;
                }
                for arm in arms {
                    if !self.span_contains_pos(arm.span) {
                        continue;
                    }
                    self.push_scope();
                    self.collect_match_pattern_bindings(&arm.pattern);
                    self.collect_expr(&arm.body);
                    return;
                }
            }
            ExprKind::Closure { params, body, .. } => {
                if self.span_contains_pos(body.span) {
                    self.push_scope();
                    for param in params {
                        self.insert_def(self.def_table.def_id(param.id));
                    }
                    self.collect_expr(body);
                }
            }
            _ => {}
        }
    }

    fn collect_stmt_at_cursor(&mut self, stmt: &crate::core::tree::StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                if self.span_contains_pos(value.span) {
                    self.collect_expr(value);
                }
            }
            StmtExprKind::Assign {
                assignee, value, ..
            }
            | StmtExprKind::CompoundAssign {
                assignee, value, ..
            } => {
                if self.span_contains_pos(assignee.span) {
                    self.collect_expr(assignee);
                } else if self.span_contains_pos(value.span) {
                    self.collect_expr(value);
                }
            }
            StmtExprKind::While { cond, body } => {
                if self.span_contains_pos(cond.span) {
                    self.collect_expr(cond);
                } else if self.span_contains_pos(body.span) {
                    self.collect_expr(body);
                }
            }
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                if self.span_contains_pos(iter.span) {
                    self.collect_expr(iter);
                    return;
                }
                if self.span_contains_pos(body.span) {
                    self.push_scope();
                    self.collect_bind_pattern_bindings(pattern);
                    self.collect_expr(body);
                }
            }
            StmtExprKind::Defer { value } => {
                if self.span_contains_pos(value.span) {
                    self.collect_expr(value);
                }
            }
            StmtExprKind::Using { ident, value, body } => {
                if self.span_contains_pos(value.span) {
                    self.collect_expr(value);
                    return;
                }
                if self.span_contains_pos(body.span) {
                    self.push_scope();
                    let _ = ident;
                    self.insert_def(self.def_table.def_id(stmt.id));
                    self.collect_expr(body);
                }
            }
            StmtExprKind::Return { value } => {
                if let Some(value) = value
                    && self.span_contains_pos(value.span)
                {
                    self.collect_expr(value);
                }
            }
            StmtExprKind::VarDecl { .. } | StmtExprKind::Break | StmtExprKind::Continue => {}
        }
    }

    fn collect_stmt_bindings(&mut self, stmt: &crate::core::tree::StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, .. } | StmtExprKind::VarBind { pattern, .. } => {
                self.collect_bind_pattern_bindings(pattern);
            }
            StmtExprKind::VarDecl { .. } => {
                self.insert_def(self.def_table.def_id(stmt.id));
            }
            StmtExprKind::Using { .. } => {
                self.insert_def(self.def_table.def_id(stmt.id));
            }
            _ => {}
        }
    }

    fn collect_bind_pattern_bindings(&mut self, pattern: &crate::core::tree::BindPattern) {
        match &pattern.kind {
            crate::core::tree::BindPatternKind::Name { .. } => {
                self.insert_def(self.def_table.def_id(pattern.id));
            }
            crate::core::tree::BindPatternKind::Array { patterns }
            | crate::core::tree::BindPatternKind::Tuple { patterns } => {
                for sub in patterns {
                    self.collect_bind_pattern_bindings(sub);
                }
            }
            crate::core::tree::BindPatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.collect_bind_pattern_bindings(&field.pattern);
                }
            }
        }
    }

    fn collect_match_pattern_bindings(&mut self, pattern: &crate::core::tree::MatchPattern) {
        match pattern {
            crate::core::tree::MatchPattern::Binding { id, .. }
            | crate::core::tree::MatchPattern::TypedBinding { id, .. } => {
                self.insert_def(self.def_table.def_id(*id));
            }
            crate::core::tree::MatchPattern::Tuple { patterns, .. } => {
                for sub in patterns {
                    self.collect_match_pattern_bindings(sub);
                }
            }
            crate::core::tree::MatchPattern::EnumVariant { bindings, .. } => {
                for binding in bindings {
                    if let crate::core::tree::MatchPatternBinding::Named { id, .. } = binding {
                        self.insert_def(self.def_table.def_id(*id));
                    }
                }
            }
            _ => {}
        }
    }

    fn add_callable_params(&mut self, callable: &CallableAtCursor<'_>) {
        match callable {
            CallableAtCursor::Func(def) => {
                for type_param in &def.sig.type_params {
                    self.insert_def(self.def_table.def_id(type_param.id));
                }
                for param in &def.sig.params {
                    self.insert_def(self.def_table.def_id(param.id));
                }
            }
            CallableAtCursor::Method(def) => {
                for type_param in &def.sig.type_params {
                    self.insert_def(self.def_table.def_id(type_param.id));
                }
                self.insert_def(self.def_table.def_id(def.sig.self_param.id));
                for param in &def.sig.params {
                    self.insert_def(self.def_table.def_id(param.id));
                }
            }
            CallableAtCursor::Closure(def) => {
                for param in &def.sig.params {
                    self.insert_def(self.def_table.def_id(param.id));
                }
            }
        }
    }

    fn insert_def(&mut self, def_id: DefId) {
        let Some(def) = self.def_table.lookup_def(def_id) else {
            return;
        };
        let Some(kind) = completion_kind_for_def(&def.kind) else {
            return;
        };
        let Some(scope) = self.scopes.last_mut() else {
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

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn span_contains_pos(&self, span: Span) -> bool {
        span_contains_pos(span, self.cursor)
    }
}

enum CallableAtCursor<'a> {
    Func(&'a FuncDef),
    Method(&'a MethodDef),
    Closure(&'a ClosureDef),
}

impl<'a> CallableAtCursor<'a> {
    fn span(&self) -> Span {
        match self {
            Self::Func(def) => def.span,
            Self::Method(def) => def.span,
            Self::Closure(def) => def.span,
        }
    }

    fn body(&self) -> &'a Expr {
        match self {
            Self::Func(def) => &def.body,
            Self::Method(def) => &def.body,
            Self::Closure(def) => &def.body,
        }
    }
}

fn containing_callable(module: &Module, cursor: Position) -> Option<CallableAtCursor<'_>> {
    let mut best: Option<CallableAtCursor<'_>> = None;
    for item in &module.top_level_items {
        match item {
            TopLevelItem::FuncDef(def) => {
                choose_smallest_callable(&mut best, CallableAtCursor::Func(def), cursor);
            }
            TopLevelItem::MethodBlock(block) => {
                for method in &block.method_items {
                    if let MethodItem::Def(def) = method {
                        choose_smallest_callable(&mut best, CallableAtCursor::Method(def), cursor);
                    }
                }
            }
            TopLevelItem::ClosureDef(def) => {
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

fn span_contains_pos(span: Span, pos: Position) -> bool {
    position_leq(span.start, pos) && position_leq(pos, span.end)
}

fn span_width(span: Span) -> usize {
    span.end.offset.saturating_sub(span.start.offset)
}
