//! Slice borrow checking: prevents mutation/move of arrays while slices exist.
//!
//! Conservative rule: once a slice is stored in a local, its base (array/string) cannot
//! be mutated or moved for the remainder of that local's scope.
//! 
//! Note: This is a conservative rule (scope‑based, not last‑use).
//!
//! Example violation:
//! ```
//! let s = arr[0..5];  // s borrows arr
//! arr[0] = 42;        // ERROR: arr is borrowed by s
//! ```

use std::collections::{HashMap, HashSet};

use crate::ast::{
    Expr, ExprKind, Function, FunctionParamMode, Pattern, PatternKind, StmtExpr, StmtExprKind,
    Visitor, walk_expr, walk_stmt_expr,
};
use crate::context::TypeCheckedContext;
use crate::resolve::def_map::DefId;
use crate::semck::SemCheckError;
use crate::semck::util::lookup_call_sig;

pub(super) fn check(ctx: &TypeCheckedContext) -> Vec<SemCheckError> {
    let mut checker = SliceBorrowChecker::new(ctx);
    checker.visit_module(&ctx.module);
    checker.errors
}

struct SliceBorrowChecker<'a> {
    ctx: &'a TypeCheckedContext,
    errors: Vec<SemCheckError>,
    /// Stack of scopes, each containing DefIds borrowed in that scope.
    /// A borrow in scope N is active until scope N exits.
    borrowed_scopes: Vec<HashSet<DefId>>,
    /// Maps each local definition to its scope index.
    def_scopes: HashMap<DefId, usize>,
    /// Stack of definitions introduced in each scope (for cleanup on exit).
    defs_in_scope: Vec<Vec<DefId>>,
}

impl<'a> SliceBorrowChecker<'a> {
    fn new(ctx: &'a TypeCheckedContext) -> Self {
        Self {
            ctx,
            errors: Vec::new(),
            borrowed_scopes: Vec::new(),
            def_scopes: HashMap::new(),
            defs_in_scope: Vec::new(),
        }
    }

    /// Push a new scope onto the stack (called on block entry).
    fn enter_scope(&mut self) {
        self.borrowed_scopes.push(HashSet::new());
        self.defs_in_scope.push(Vec::new());
    }

    /// Pop the current scope, releasing its borrows and definitions.
    fn exit_scope(&mut self) {
        if let Some(defs) = self.defs_in_scope.pop() {
            for def in defs {
                self.def_scopes.remove(&def);
            }
        }
        self.borrowed_scopes.pop();
    }

    /// Record a new local definition in the current scope.
    fn record_def(&mut self, def: DefId) {
        let scope_idx = self.borrowed_scopes.len().saturating_sub(1);
        self.def_scopes.insert(def, scope_idx);
        if let Some(defs) = self.defs_in_scope.last_mut() {
            defs.push(def);
        }
    }

    /// Get the scope index where a definition was introduced.
    fn scope_for_def(&self, def: DefId) -> Option<usize> {
        self.def_scopes.get(&def).copied()
    }

    /// Check if a def is currently borrowed (in any active scope).
    fn is_borrowed(&self, def: DefId) -> bool {
        self.borrowed_scopes.iter().any(|set| set.contains(&def))
    }

    /// Mark a def as borrowed in the given scope (borrow lasts until scope exits).
    fn add_borrow(&mut self, def: DefId, scope_idx: usize) {
        if let Some(scope) = self.borrowed_scopes.get_mut(scope_idx) {
            scope.insert(def);
        }
    }

    /// When a slice is bound to a local, mark the slice's base as borrowed.
    /// The borrow scope = the slice holder's scope (so borrow ends when holder goes out of scope).
    fn record_slice_binding_for_def(&mut self, holder_def: DefId, slice_expr: &Expr) {
        let ExprKind::Slice { target, .. } = &slice_expr.kind else {
            return;
        };
        let Some(base_def) = self.base_def_id(target) else {
            return;
        };
        // Borrow lasts for the slice holder's scope.
        let scope_idx = self.scope_for_def(holder_def).unwrap_or_else(|| {
            self.borrowed_scopes.len().saturating_sub(1)
        });
        self.add_borrow(base_def, scope_idx);
    }

    /// Error if assigning to a borrowed base (mutation conflict).
    fn check_write_target(&mut self, expr: &Expr) {
        if let Some(def) = self.base_def_id(expr)
            && self.is_borrowed(def)
        {
            self.errors
                .push(SemCheckError::SliceBorrowConflict(expr.span));
        }
    }

    /// Error if moving a borrowed base.
    fn check_move_expr(&mut self, expr: &Expr) {
        let ExprKind::Move { expr: inner } = &expr.kind else {
            return;
        };
        if let Some(def) = self.base_def_id(inner)
            && self.is_borrowed(def)
        {
            self.errors
                .push(SemCheckError::SliceBorrowConflict(expr.span));
        }
    }

    /// Error if passing a borrowed base to a mutating parameter (inout/out/sink).
    fn check_call(&mut self, call: &Expr, args: &[Expr]) {
        let Some(sig) = lookup_call_sig(call, self.ctx) else {
            return;
        };

        for (param, arg) in sig.params.iter().zip(args) {
            // Explicit move is checked separately by check_move_expr.
            if matches!(arg.kind, ExprKind::Move { .. }) {
                continue;
            }
            // Only mutating modes conflict with borrows.
            if !matches!(
                param.mode,
                FunctionParamMode::Inout | FunctionParamMode::Out | FunctionParamMode::Sink
            ) {
                continue;
            }
            if let Some(def) = self.base_def_id(arg)
                && self.is_borrowed(def)
            {
                self.errors
                    .push(SemCheckError::SliceBorrowConflict(arg.span));
            }
        }
    }

    /// Extract the base variable's DefId from an lvalue expression.
    /// E.g., `base[0].field` -> DefId of `base`.
    fn base_def_id(&self, expr: &Expr) -> Option<DefId> {
        match &expr.kind {
            ExprKind::Var(_) => self.ctx.def_map.lookup_def(expr.id).map(|def| def.id),
            ExprKind::ArrayIndex { target, .. }
            | ExprKind::TupleField { target, .. }
            | ExprKind::StructField { target, .. }
            | ExprKind::Slice { target, .. } => self.base_def_id(target),
            ExprKind::Move { expr } => self.base_def_id(expr),
            _ => None,
        }
    }

    /// Record all definitions introduced by a pattern (for scope tracking).
    fn record_pattern_defs(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Ident { .. } => {
                if let Some(def) = self.ctx.def_map.lookup_def(pattern.id) {
                    self.record_def(def.id);
                }
            }
            PatternKind::Array { patterns } | PatternKind::Tuple { patterns } => {
                for pattern in patterns {
                    self.record_pattern_defs(pattern);
                }
            }
            PatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.record_pattern_defs(&field.pattern);
                }
            }
        }
    }
}

impl Visitor for SliceBorrowChecker<'_> {
    fn visit_func(&mut self, func: &Function) {
        self.enter_scope();
        // Record params so slice holders can be tracked.
        for param in &func.sig.params {
            if let Some(def) = self.ctx.def_map.lookup_def(param.id) {
                self.record_def(def.id);
            }
        }
        self.visit_expr(&func.body);
        self.exit_scope();
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, value, .. }
            | StmtExprKind::VarBind { pattern, value, .. } => {
                self.record_pattern_defs(pattern);
                // If binding a slice, record the borrow.
                if matches!(value.kind, ExprKind::Slice { .. }) {
                    if let Some(def) = self.ctx.def_map.lookup_def(pattern.id) {
                        self.record_slice_binding_for_def(def.id, value);
                    }
                }
            }
            StmtExprKind::VarDecl { .. } => {
                if let Some(def) = self.ctx.def_map.lookup_def(stmt.id) {
                    self.record_def(def.id);
                }
            }
            StmtExprKind::Assign { assignee, value } => {
                // Check mutation of borrowed base.
                self.check_write_target(assignee);
                // If assigning a slice, record the borrow.
                if matches!(value.kind, ExprKind::Slice { .. }) {
                    if let Some(holder_def) = self.base_def_id(assignee) {
                        self.record_slice_binding_for_def(holder_def, value);
                    }
                }
            }
            StmtExprKind::While { .. } => {}
            StmtExprKind::For { pattern, .. } => {
                self.record_pattern_defs(pattern);
            }
        }

        walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            // Blocks introduce a new scope.
            ExprKind::Block { items, tail } => {
                self.enter_scope();
                for item in items {
                    self.visit_block_item(item);
                }
                if let Some(tail) = tail {
                    self.visit_expr(tail);
                }
                self.exit_scope();
            }
            ExprKind::Call { callee: _, args } => {
                self.check_call(expr, args);
                walk_expr(self, expr);
            }
            ExprKind::Move { .. } => {
                self.check_move_expr(expr);
                walk_expr(self, expr);
            }
            _ => walk_expr(self, expr),
        }
    }
}
