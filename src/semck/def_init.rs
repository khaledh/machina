//! Definite initialization analysis for locals.
//!
//! This pass rejects use-before-init for `var x: T;` declarations. It is a
//! forward dataflow analysis over the AST CFG:
//! - State: set of definitely-initialized defs.
//! - Meet: intersection (must be initialized on all paths).
//! - Transfer: update the set as statements initialize variables.

use std::collections::HashSet;

use crate::analysis::dataflow::solve_forward;
use crate::ast::cfg::{AstBlockId, AstCfgBuilder, AstCfgNode, AstItem, AstTerminator};
use crate::ast::{
    Expr, ExprKind, Function, MatchPattern, MatchPatternBinding, NodeId, Pattern, PatternKind,
    StmtExpr, StmtExprKind, Visitor, walk_expr,
};
use crate::context::TypeCheckedContext;
use crate::resolve::def_map::{DefId, DefKind};
use crate::semck::SemCheckError;

pub(super) struct DefInitResult {
    pub errors: Vec<SemCheckError>,
    pub init_assigns: HashSet<NodeId>,
}

pub(super) fn check(ctx: &TypeCheckedContext) -> DefInitResult {
    let mut errors = Vec::new();
    let mut init_assigns = HashSet::new();
    for func in ctx.module.funcs() {
        check_func(func, ctx, &mut errors, &mut init_assigns);
    }
    DefInitResult {
        errors,
        init_assigns,
    }
}

fn check_func(
    func: &Function,
    ctx: &TypeCheckedContext,
    errors: &mut Vec<SemCheckError>,
    init_assigns: &mut HashSet<NodeId>,
) {
    let cfg = AstCfgBuilder::new().build_from_expr(&func.body);

    // Params are initialized at entry; declared locals (`var x: T;`) are not.
    let entry_state = collect_param_defs(func, ctx);

    // Bottom = "all defs" so unreachable blocks don't produce spurious errors.
    // (Intersection with "all" is identity, so unreachable blocks inherit from reachable ones.)
    let bottom = collect_all_defs(func, ctx);

    solve_forward(
        &cfg,
        AstBlockId(0),
        entry_state,
        bottom,
        |states| intersect_sets(states),
        |block_id, in_state| {
            let mut checker = DefInitChecker::new(ctx, in_state.clone(), errors, init_assigns);
            checker.visit_cfg_node(&cfg.nodes[block_id.0]);
            checker.initialized
        },
    );
}

fn collect_param_defs(func: &Function, ctx: &TypeCheckedContext) -> HashSet<DefId> {
    let mut defs = HashSet::new();
    for param in &func.sig.params {
        if let Some(def) = ctx.def_map.lookup_def(param.id) {
            defs.insert(def.id);
        }
    }
    defs
}

fn collect_all_defs(func: &Function, ctx: &TypeCheckedContext) -> HashSet<DefId> {
    let mut defs = collect_param_defs(func, ctx);
    let mut collector = DefCollector {
        ctx,
        defs: &mut defs,
    };
    collector.collect_expr(&func.body);
    defs
}

/// Meet operation: a def is initialized only if initialized on ALL incoming paths.
fn intersect_sets(states: &[HashSet<DefId>]) -> HashSet<DefId> {
    let mut iter = states.iter();
    let Some(first) = iter.next() else {
        return HashSet::new();
    };
    let mut out = first.clone();
    for s in iter {
        out.retain(|d| s.contains(d));
    }
    out
}

/// Collects all local definitions in a function (params, let/var bindings, match bindings).
/// Used to build the "bottom" state for dataflow (all defs = initialized everywhere).
struct DefCollector<'a> {
    ctx: &'a TypeCheckedContext,
    defs: &'a mut HashSet<DefId>,
}

impl<'a> DefCollector<'a> {
    fn collect_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        crate::ast::BlockItem::Stmt(stmt) => self.collect_stmt(stmt),
                        crate::ast::BlockItem::Expr(expr) => self.collect_expr(expr),
                    }
                }
                if let Some(tail) = tail {
                    self.collect_expr(tail);
                }
            }
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.collect_expr(cond);
                self.collect_expr(then_body);
                self.collect_expr(else_body);
            }
            ExprKind::Match { scrutinee, arms } => {
                self.collect_expr(scrutinee);
                for arm in arms {
                    self.collect_match_pattern(&arm.pattern);
                    self.collect_expr(&arm.body);
                }
            }
            _ => {
                // Default recursive walk for other expressions.
                walk_expr(self, expr);
            }
        }
    }

    fn collect_stmt(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, value, .. }
            | StmtExprKind::VarBind { pattern, value, .. } => {
                self.collect_pattern(pattern);
                self.collect_expr(value);
            }
            StmtExprKind::VarDecl { .. } => {
                if let Some(def) = self.ctx.def_map.lookup_def(stmt.id) {
                    self.defs.insert(def.id);
                }
            }
            StmtExprKind::Assign { assignee, value } => {
                self.collect_expr(assignee);
                self.collect_expr(value);
            }
            StmtExprKind::While { cond, body } => {
                self.collect_expr(cond);
                self.collect_expr(body);
            }
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.collect_pattern(pattern);
                self.collect_expr(iter);
                self.collect_expr(body);
            }
        }
    }

    fn collect_pattern(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Ident { .. } => {
                if let Some(def) = self.ctx.def_map.lookup_def(pattern.id) {
                    self.defs.insert(def.id);
                }
            }
            PatternKind::Array { patterns } | PatternKind::Tuple { patterns } => {
                for pat in patterns {
                    self.collect_pattern(pat);
                }
            }
            PatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.collect_pattern(&field.pattern);
                }
            }
        }
    }

    fn collect_match_pattern(&mut self, pattern: &MatchPattern) {
        if let MatchPattern::EnumVariant { bindings, .. } = pattern {
            for MatchPatternBinding { id, .. } in bindings {
                if let Some(def) = self.ctx.def_map.lookup_def(*id) {
                    self.defs.insert(def.id);
                }
            }
        }
    }
}

impl<'a> Visitor for DefCollector<'a> {
    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        self.collect_stmt(stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        self.collect_expr(expr);
    }
}

/// Walks a CFG block, checking for uses of uninitialized variables and
/// updating the initialized set as assignments occur.
struct DefInitChecker<'a> {
    ctx: &'a TypeCheckedContext,
    initialized: HashSet<DefId>,
    errors: &'a mut Vec<SemCheckError>,
    init_assigns: &'a mut HashSet<NodeId>,
}

impl<'a> DefInitChecker<'a> {
    fn new(
        ctx: &'a TypeCheckedContext,
        initialized: HashSet<DefId>,
        errors: &'a mut Vec<SemCheckError>,
        init_assigns: &'a mut HashSet<NodeId>,
    ) -> Self {
        Self {
            ctx,
            initialized,
            errors,
            init_assigns,
        }
    }

    fn visit_cfg_node(&mut self, node: &AstCfgNode<'_>) {
        // Loop bodies pre-initialize their pattern bindings.
        for pattern in &node.loop_inits {
            self.mark_pattern_initialized(pattern);
        }
        for item in &node.items {
            match item {
                AstItem::Stmt(stmt) => self.check_stmt(stmt),
                AstItem::Expr(expr) => self.check_expr(expr),
            }
        }
        if let AstTerminator::If { cond, .. } = &node.term {
            self.check_expr(cond);
        }
    }

    fn check_stmt(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, value, .. }
            | StmtExprKind::VarBind { pattern, value, .. } => {
                // Read the initializer first, then bind the names.
                self.check_expr(value);
                self.mark_pattern_initialized(pattern);
            }
            StmtExprKind::VarDecl { .. } => {
                // Declaration without initializer: ensure it's NOT in the initialized set.
                if let Some(def) = self.ctx.def_map.lookup_def(stmt.id) {
                    self.initialized.remove(&def.id);
                }
            }
            StmtExprKind::Assign { assignee, value } => {
                self.check_expr(value);
                self.check_assignment(assignee);
            }
            StmtExprKind::While { cond, body } => {
                self.check_expr(cond);
                self.check_expr(body);
            }
            StmtExprKind::For { iter, body, .. } => {
                self.check_expr(iter);
                self.check_expr(body);
            }
        }
    }

    /// Handle the left-hand side of an assignment.
    /// - Bare variable: marks it initialized (this is the initializing assignment).
    /// - Projections (field, index): the base must already be initialized.
    fn check_assignment(&mut self, assignee: &Expr) {
        match &assignee.kind {
            ExprKind::Var(_) => {
                // Assigning to a variable initializes it. Track the first
                // assignment so lowering can skip dropping uninitialized memory.
                if let Some(def) = self.ctx.def_map.lookup_def(assignee.id)
                    && !self.initialized.contains(&def.id)
                {
                    self.init_assigns.insert(assignee.id);
                }
                self.mark_var_initialized(assignee);
            }
            ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
                // Assigning to a field requires the base to be initialized.
                self.check_place_base_initialized(target);
            }
            ExprKind::ArrayIndex { target, indices } => {
                self.check_place_base_initialized(target);
                for index in indices {
                    self.check_expr(index);
                }
            }
            ExprKind::Slice { target, start, end } => {
                self.check_place_base_initialized(target);
                if let Some(start) = start {
                    self.check_expr(start);
                }
                if let Some(end) = end {
                    self.check_expr(end);
                }
            }
            _ => {
                // Fallback to expression checking for any other assignee form.
                self.check_expr(assignee);
            }
        }
    }

    /// Recursively descend through projections to find the base variable, then check it.
    /// E.g., for `a.b[i].c`, we check that `a` is initialized.
    fn check_place_base_initialized(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var(_) => self.check_var_use(expr),
            ExprKind::StructField { target, .. }
            | ExprKind::TupleField { target, .. }
            | ExprKind::ArrayIndex { target, .. }
            | ExprKind::Slice { target, .. } => self.check_place_base_initialized(target),
            _ => self.check_expr(expr),
        }
    }

    fn mark_pattern_initialized(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Ident { .. } => {
                if let Some(def) = self.ctx.def_map.lookup_def(pattern.id) {
                    self.initialized.insert(def.id);
                }
            }
            PatternKind::Array { patterns } | PatternKind::Tuple { patterns } => {
                for pat in patterns {
                    self.mark_pattern_initialized(pat);
                }
            }
            PatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.mark_pattern_initialized(&field.pattern);
                }
            }
        }
    }

    fn mark_var_initialized(&mut self, expr: &Expr) {
        if let Some(def) = self.ctx.def_map.lookup_def(expr.id) {
            self.initialized.insert(def.id);
        }
    }

    /// Report an error if a local variable is used before initialization.
    fn check_var_use(&mut self, expr: &Expr) {
        let Some(def) = self.ctx.def_map.lookup_def(expr.id) else {
            return;
        };
        // Only check locals and params (globals, functions, etc. are always "initialized").
        if !matches!(def.kind, DefKind::LocalVar { .. } | DefKind::Param { .. }) {
            return;
        }
        if !self.initialized.contains(&def.id) {
            self.errors
                .push(SemCheckError::UseBeforeInit(def.name.clone(), expr.span));
        }
    }

    fn check_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var(_) => self.check_var_use(expr),
            ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
                self.check_place_base_initialized(target);
            }
            ExprKind::ArrayIndex { target, indices } => {
                self.check_place_base_initialized(target);
                for index in indices {
                    self.check_expr(index);
                }
            }
            ExprKind::Slice { target, start, end } => {
                self.check_place_base_initialized(target);
                if let Some(start) = start {
                    self.check_expr(start);
                }
                if let Some(end) = end {
                    self.check_expr(end);
                }
            }
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.check_expr(cond);
                // Analyze each branch independently, then intersect the results.
                let then_state = self.with_init(self.initialized.clone(), |this| {
                    this.check_expr(then_body);
                });
                let else_state = self.with_init(self.initialized.clone(), |this| {
                    this.check_expr(else_body);
                });
                // After the if: only defs initialized in BOTH branches are safe.
                self.initialized = intersect_sets(&[then_state, else_state]);
            }
            ExprKind::Match { scrutinee, arms } => {
                self.check_expr(scrutinee);
                // Each arm may bind variables (pattern bindings start initialized).
                let mut arm_states = Vec::new();
                for arm in arms {
                    let mut init = self.initialized.clone();
                    self.mark_match_pattern_initialized(&mut init, &arm.pattern);
                    let out = self.with_init(init, |this| {
                        this.check_expr(&arm.body);
                    });
                    arm_states.push(out);
                }
                // After match: intersect all arm results.
                if !arm_states.is_empty() {
                    self.initialized = intersect_sets(&arm_states);
                }
            }
            ExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        crate::ast::BlockItem::Stmt(stmt) => self.check_stmt(stmt),
                        crate::ast::BlockItem::Expr(expr) => self.check_expr(expr),
                    }
                }
                if let Some(tail) = tail {
                    self.check_expr(tail);
                }
            }
            ExprKind::Call { callee, args } => {
                self.check_expr(callee);
                for arg in args {
                    self.check_expr(arg);
                }
            }
            ExprKind::StructLit { fields, .. } => {
                for field in fields {
                    self.check_expr(&field.value);
                }
            }
            ExprKind::StructUpdate { target, fields } => {
                self.check_expr(target);
                for field in fields {
                    self.check_expr(&field.value);
                }
            }
            ExprKind::EnumVariant { payload, .. } => {
                for expr in payload {
                    self.check_expr(expr);
                }
            }
            ExprKind::ArrayLit { init, .. } => match init {
                crate::ast::ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.check_expr(elem);
                    }
                }
                crate::ast::ArrayLitInit::Repeat(expr, _) => self.check_expr(expr),
            },
            ExprKind::TupleLit(fields) => {
                for field in fields {
                    self.check_expr(field);
                }
            }
            ExprKind::StringFmt { segments } => {
                for segment in segments {
                    if let crate::ast::StringFmtSegment::Expr { expr, .. } = segment {
                        self.check_expr(expr);
                    }
                }
            }
            ExprKind::BinOp { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }
            ExprKind::UnaryOp { expr, .. } => self.check_expr(expr),
            ExprKind::HeapAlloc { expr } => self.check_expr(expr),
            ExprKind::Move { expr } => self.check_expr(expr),
            ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::UnitLit
            | ExprKind::Range { .. } => {}
        }
    }

    fn mark_match_pattern_initialized(
        &mut self,
        init: &mut HashSet<DefId>,
        pattern: &MatchPattern,
    ) {
        if let MatchPattern::EnumVariant { bindings, .. } = pattern {
            for MatchPatternBinding { id, .. } in bindings {
                if let Some(def) = self.ctx.def_map.lookup_def(*id) {
                    init.insert(def.id);
                }
            }
        }
    }

    /// Run `f` with a temporary initialized set, then restore the original and return
    /// the resulting state. Used to analyze branches without polluting the outer state.
    fn with_init<F>(&mut self, init: HashSet<DefId>, f: F) -> HashSet<DefId>
    where
        F: FnOnce(&mut Self),
    {
        let saved = std::mem::replace(&mut self.initialized, init);
        f(self);
        std::mem::replace(&mut self.initialized, saved)
    }
}
