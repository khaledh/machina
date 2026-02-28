//! Validation checks for stmt-specific semantics.

use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::{BlockItem, Expr, ExprKind, StmtExpr, StmtExprKind};
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::types::Type;

pub(super) fn check_stmt_semantics(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let mut checker = StmtSemanticsChecker {
        // Validation runs after solve, so we can inspect fully-resolved node
        // types without adding new constraint machinery.
        node_types: &engine.state().solve.resolved_node_types,
        errors: Vec::new(),
        cleanup_active: false,
    };
    checker.visit_module(&engine.context().module);
    checker.errors
}

struct StmtSemanticsChecker<'a> {
    node_types: &'a std::collections::HashMap<crate::core::tree::NodeId, Type>,
    errors: Vec<TypeCheckError>,
    cleanup_active: bool,
}

impl Visitor for StmtSemanticsChecker<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        if self.cleanup_active && matches!(expr.kind, ExprKind::Try { on_error: None, .. }) {
            self.errors.push(TEK::TryInCleanupScope.at(expr.span));
        }

        if let ExprKind::Block { items, tail } = &expr.kind {
            self.visit_block_with_cleanup(items, tail.as_deref());
            return;
        }

        visit::walk_expr(self, expr);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::Defer { value } => {
                // `defer` has V1-only semantic rules that are easier to express
                // as a post-solve stmt check than during constraint collection.
                self.check_defer(value);
                // Defer-specific rules already cover fallibility and bare `?`,
                // so avoid layering the broader cleanup-scope restriction on
                // top of them.
                let saved = self.cleanup_active;
                self.cleanup_active = false;
                self.visit_expr(value);
                self.cleanup_active = saved;
            }
            StmtExprKind::Using { value, body, .. } => {
                self.visit_expr(value);

                let saved = self.cleanup_active;
                self.cleanup_active = true;
                self.visit_expr(body);
                self.cleanup_active = saved;
            }
            _ => visit::walk_stmt_expr(self, stmt),
        }
    }
}

impl StmtSemanticsChecker<'_> {
    fn visit_block_with_cleanup(&mut self, items: &[BlockItem], tail: Option<&Expr>) {
        let saved = self.cleanup_active;
        let mut block_cleanup_active = saved;

        for item in items {
            self.cleanup_active = block_cleanup_active;
            match item {
                BlockItem::Stmt(stmt) => {
                    self.visit_stmt_expr(stmt);
                    if matches!(stmt.kind, StmtExprKind::Defer { .. }) {
                        // A defer only becomes active for the statements that
                        // follow it in the same lexical block.
                        block_cleanup_active = true;
                    }
                }
                BlockItem::Expr(expr) => self.visit_expr(expr),
            }
        }

        self.cleanup_active = block_cleanup_active;
        if let Some(tail) = tail {
            self.visit_expr(tail);
        }
        self.cleanup_active = saved;
    }

    fn check_defer(&mut self, value: &Expr) {
        // Bare `?` would make deferred cleanup implicitly participate in outer
        // error propagation, which V1 deliberately forbids.
        if bare_try_in_expr(value) {
            self.errors.push(TEK::DeferBareTry.at(value.span));
        }

        let Some(value_ty) = self.node_types.get(&value.id) else {
            return;
        };
        // In partial-analysis paths we may still have unresolved types; avoid
        // piling on noisy follow-up diagnostics until the expression is known.
        if value_ty.contains_unresolved() {
            return;
        }
        // Deferred cleanup must be non-fallible at the stmt boundary.
        if matches!(value_ty, Type::ErrorUnion { .. }) {
            self.errors
                .push(TEK::DeferExprFallible(value_ty.clone()).at(value.span));
        }
    }
}

fn bare_try_in_expr(expr: &Expr) -> bool {
    struct BareTryFinder(bool);

    impl Visitor for BareTryFinder {
        fn visit_expr(&mut self, expr: &Expr) {
            if matches!(expr.kind, ExprKind::Try { on_error: None, .. }) {
                // Stop at the first unhandled `?`; we only need a yes/no answer.
                self.0 = true;
                return;
            }
            visit::walk_expr(self, expr);
        }
    }

    let mut finder = BareTryFinder(false);
    finder.visit_expr(expr);
    finder.0
}
