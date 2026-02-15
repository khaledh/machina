//! Definite initialization analysis for locals.
//!
//! This pass rejects use-before-init for `var x: T;` declarations. It is a
//! forward dataflow analysis over the AST CFG:
//! - State: fully-initialized defs + initialized projection paths.
//! - Meet: intersection (must be initialized on all paths).
//! - Transfer: update the set as statements initialize variables.

use std::collections::{HashMap, HashSet};

use crate::core::analysis::dataflow::{DataflowGraph, solve_forward};
use crate::core::context::NormalizedContext;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefKind};
use crate::core::semck::SemCheckError;
use crate::core::tree::cfg::{
    AstBlockId, TreeCfgBuilder, TreeCfgItem, TreeCfgNode, TreeCfgTerminator,
};
use crate::core::tree::normalized::{
    ArrayLitInit, BindPattern, BindPatternKind, BlockItem, CallArgMode, EmitKind, Expr, ExprKind,
    FuncDef, MatchPattern, MatchPatternBinding, NodeId, ParamMode, StmtExpr, StmtExprKind,
    StringFmtSegment,
};
use crate::core::tree::visit::{Visitor, walk_expr};
use crate::core::types::{Type, TypeId};

pub(super) struct DefInitResult {
    pub errors: Vec<SemCheckError>,
    pub init_assigns: HashSet<NodeId>,
    pub full_init_assigns: HashSet<NodeId>,
}

/// Tracks which definitions are initialized: either fully (the whole variable)
/// or partially (specific sub-paths like `x.a` or `arr[0]`).
#[derive(Clone, Debug, PartialEq, Eq)]
struct InitState {
    full: HashSet<DefId>,
    partial: HashSet<InitPath>,
    maybe_partial: HashSet<InitPath>,
}

impl InitState {
    fn new(full: HashSet<DefId>) -> Self {
        Self {
            full,
            partial: HashSet::new(),
            maybe_partial: HashSet::new(),
        }
    }

    fn is_full(&self, def_id: DefId) -> bool {
        self.full.contains(&def_id)
    }

    fn mark_full(&mut self, def_id: DefId) {
        self.full.insert(def_id);
        self.partial.retain(|path| path.base != def_id);
        self.maybe_partial.retain(|path| path.base != def_id);
    }

    fn clear_def(&mut self, def_id: DefId) {
        self.full.remove(&def_id);
        self.partial.retain(|path| path.base != def_id);
        self.maybe_partial.retain(|path| path.base != def_id);
    }

    fn add_partial(&mut self, path: InitPath) {
        if self.is_full(path.base) {
            return;
        }
        self.partial.insert(path.clone());
        self.maybe_partial.insert(path);
    }

    /// A path is initialized if: (1) the base is fully init, (2) the exact path
    /// is in partial, or (3) any prefix is in partial (e.g., `x.a` covers `x.a.b`).
    fn is_path_initialized(&self, path: &InitPath) -> bool {
        if self.is_full(path.base) {
            return true;
        }
        if self.partial.contains(path) {
            return true;
        }
        for prefix_len in 1..path.projections.len() {
            let prefix = InitPath {
                base: path.base,
                projections: path.projections[..prefix_len].to_vec(),
            };
            if self.partial.contains(&prefix) {
                return true;
            }
        }
        false
    }

    fn has_maybe_partial_for(&self, def_id: DefId) -> bool {
        self.maybe_partial.iter().any(|path| path.base == def_id)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct InitPath {
    base: DefId,
    projections: Vec<InitProj>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum InitProj {
    Field(String),
    TupleField(usize),
    Index(u64),
}

pub(super) fn check(ctx: &NormalizedContext) -> DefInitResult {
    let mut errors = Vec::new();
    let mut init_assigns = HashSet::new();
    let mut full_init_assigns = HashSet::new();
    for func_def in ctx.module.func_defs() {
        check_func(
            func_def,
            ctx,
            &mut errors,
            &mut init_assigns,
            &mut full_init_assigns,
        );
    }
    DefInitResult {
        errors,
        init_assigns,
        full_init_assigns,
    }
}

fn check_func(
    func_def: &FuncDef,
    ctx: &NormalizedContext,
    errors: &mut Vec<SemCheckError>,
    init_assigns: &mut HashSet<NodeId>,
    full_init_assigns: &mut HashSet<NodeId>,
) {
    let cfg = TreeCfgBuilder::<TypeId>::new().build_from_expr(&func_def.body);

    // Params are initialized at entry, except `out` params which start uninitialized.
    let entry_state = InitState::new(collect_param_defs(func_def, false));
    let out_params = collect_out_param_defs(func_def, ctx);
    let out_param_defs: HashSet<_> = out_params.iter().map(|(def_id, _, _)| *def_id).collect();

    // Bottom = "all defs" so unreachable blocks don't produce spurious errors.
    // (Intersection with "all" is identity, so unreachable blocks inherit from reachable ones.)
    let bottom = InitState::new(collect_all_defs(func_def));
    let def_spans = collect_def_spans(func_def);

    let result = solve_forward(
        &cfg,
        AstBlockId(0),
        entry_state,
        bottom,
        intersect_states,
        |block_id, in_state| {
            let mut checker = DefInitChecker::new(
                ctx,
                in_state.clone(),
                &out_param_defs,
                errors,
                init_assigns,
                full_init_assigns,
            );
            checker.visit_cfg_node(&cfg.nodes[block_id.0]);
            checker.initialized
        },
    );

    // Require that all out params are initialized on every exit path.
    let exit_blocks: Vec<_> = (0..cfg.num_nodes())
        .map(AstBlockId)
        .filter(|&block| cfg.succs(block).is_empty())
        .collect();
    for (def_id, name, span) in out_params {
        let initialized_on_all_exits = exit_blocks
            .iter()
            .all(|block| result.out_map[block.0].is_full(def_id));
        if !initialized_on_all_exits {
            errors.push(SemCheckError::OutParamNotInitialized(name, span));
        }
    }

    let local_defs = collect_local_defs(func_def, ctx);
    let mut reported = HashSet::new();
    for def_id in local_defs {
        let Some(def) = ctx.def_table.lookup_def(def_id) else {
            continue;
        };
        let Some(ty) = ctx.type_map.lookup_def_type(def) else {
            continue;
        };
        if !ty.needs_drop() {
            continue;
        }
        let has_partial_on_any_exit = exit_blocks.iter().any(|block| {
            let state = &result.out_map[block.0];
            !state.is_full(def_id) && state.has_maybe_partial_for(def_id)
        });
        if has_partial_on_any_exit && reported.insert(def_id) {
            let span = def_spans.get(&def_id).cloned().unwrap_or_default();
            errors.push(SemCheckError::PartialInitNotAllowed(def.name.clone(), span));
        }
    }
}

fn collect_param_defs(func_def: &FuncDef, include_out: bool) -> HashSet<DefId> {
    let mut defs = HashSet::new();
    for param in &func_def.sig.params {
        if !include_out && param.mode == ParamMode::Out {
            continue;
        }
        defs.insert(param.def_id);
    }
    defs
}

fn collect_out_param_defs(
    func_def: &FuncDef,
    ctx: &NormalizedContext,
) -> Vec<(DefId, String, Span)> {
    let mut defs = Vec::new();
    for param in &func_def.sig.params {
        if param.mode != ParamMode::Out {
            continue;
        }
        if let Some(def) = ctx.def_table.lookup_def(param.def_id) {
            defs.push((param.def_id, def.name.clone(), param.span));
        }
    }
    defs
}

fn collect_all_defs(func_def: &FuncDef) -> HashSet<DefId> {
    let mut defs = collect_param_defs(func_def, true);
    let mut collector = DefCollector { defs: &mut defs };
    collector.collect_expr(&func_def.body);
    defs
}

fn collect_local_defs(func_def: &FuncDef, ctx: &NormalizedContext) -> HashSet<DefId> {
    collect_all_defs(func_def)
        .into_iter()
        .filter(|def_id| {
            matches!(
                ctx.def_table.lookup_def(*def_id).map(|def| &def.kind),
                Some(DefKind::LocalVar { .. })
            )
        })
        .collect()
}

fn collect_def_spans(func_def: &FuncDef) -> HashMap<DefId, Span> {
    let mut spans = HashMap::new();
    for param in &func_def.sig.params {
        spans.insert(param.def_id, param.span);
    }
    let mut collector = DefSpanCollector { spans: &mut spans };
    collector.collect_expr(&func_def.body);
    spans
}

/// Meet operation: a def (or sub-path) is initialized only if initialized on ALL paths.
fn intersect_states(states: &[InitState]) -> InitState {
    let mut iter = states.iter();
    let Some(first) = iter.next() else {
        return InitState::new(HashSet::new());
    };

    let mut full = first.full.clone();
    for state in iter {
        full.retain(|def| state.full.contains(def));
    }

    // Collect all partial paths from any state, then keep only those initialized
    // on ALL paths (maybe via full init of base, exact partial, or a prefix).
    let mut partial_candidates = HashSet::new();
    let mut maybe_partial = HashSet::new();
    for state in states {
        partial_candidates.extend(state.partial.iter().cloned());
        maybe_partial.extend(state.maybe_partial.iter().cloned());
    }

    let mut partial = HashSet::new();
    for path in partial_candidates {
        if full.contains(&path.base) {
            continue;
        }
        if states.iter().all(|state| state.is_path_initialized(&path)) {
            partial.insert(path);
        }
    }

    InitState {
        full,
        partial,
        maybe_partial,
    }
}

/// Collects all local definitions in a function (params, let/var bindings, match bindings).
/// Used to build the "bottom" state for dataflow (all defs = initialized everywhere).
struct DefCollector<'a> {
    defs: &'a mut HashSet<DefId>,
}

impl<'a> DefCollector<'a> {
    fn collect_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        BlockItem::Stmt(stmt) => self.collect_stmt(stmt),
                        BlockItem::Expr(expr) => self.collect_expr(expr),
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
                self.collect_bind_pattern(pattern);
                self.collect_expr(value);
            }
            StmtExprKind::VarDecl { def_id, .. } => {
                self.defs.insert(*def_id);
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
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
                self.collect_bind_pattern(pattern);
                self.collect_expr(iter);
                self.collect_expr(body);
            }
            StmtExprKind::Break | StmtExprKind::Continue => {}
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.collect_expr(value);
                }
            }
        }
    }

    fn collect_bind_pattern(&mut self, pattern: &BindPattern) {
        match &pattern.kind {
            BindPatternKind::Name { def_id, .. } => {
                self.defs.insert(*def_id);
            }
            BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
                for pat in patterns {
                    self.collect_bind_pattern(pat);
                }
            }
            BindPatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.collect_bind_pattern(&field.pattern);
                }
            }
        }
    }

    fn collect_match_pattern(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Binding { def_id, .. } | MatchPattern::TypedBinding { def_id, .. } => {
                self.defs.insert(*def_id);
            }
            MatchPattern::Tuple { patterns, .. } => {
                for pattern in patterns {
                    self.collect_match_pattern(pattern);
                }
            }
            MatchPattern::EnumVariant { bindings, .. } => {
                for binding in bindings {
                    if let MatchPatternBinding::Named { def_id, .. } = binding {
                        self.defs.insert(*def_id);
                    }
                }
            }
            _ => {}
        }
    }
}

impl<'a> Visitor<DefId, TypeId> for DefCollector<'a> {
    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        self.collect_stmt(stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        self.collect_expr(expr);
    }
}

struct DefSpanCollector<'a> {
    spans: &'a mut std::collections::HashMap<DefId, Span>,
}

impl<'a> DefSpanCollector<'a> {
    fn collect_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        BlockItem::Stmt(stmt) => self.collect_stmt(stmt),
                        BlockItem::Expr(expr) => self.collect_expr(expr),
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
            StmtExprKind::VarDecl { def_id, .. } => {
                self.spans.entry(*def_id).or_insert(stmt.span);
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
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
            StmtExprKind::Break | StmtExprKind::Continue => {}
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.collect_expr(value);
                }
            }
        }
    }

    fn collect_pattern(&mut self, pattern: &BindPattern) {
        match &pattern.kind {
            BindPatternKind::Name { def_id, .. } => {
                self.spans.entry(*def_id).or_insert(pattern.span);
            }
            BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
                for pat in patterns {
                    self.collect_pattern(pat);
                }
            }
            BindPatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.collect_pattern(&field.pattern);
                }
            }
        }
    }

    fn collect_match_pattern(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Binding { def_id, span, .. }
            | MatchPattern::TypedBinding { def_id, span, .. } => {
                self.spans.entry(*def_id).or_insert(*span);
            }
            MatchPattern::Tuple { patterns, .. } => {
                for pattern in patterns {
                    self.collect_match_pattern(pattern);
                }
            }
            MatchPattern::EnumVariant { bindings, .. } => {
                for binding in bindings {
                    if let MatchPatternBinding::Named { def_id, span, .. } = binding {
                        self.spans.entry(*def_id).or_insert(*span);
                    }
                }
            }
            _ => {}
        }
    }
}

impl<'a> Visitor<DefId, TypeId> for DefSpanCollector<'a> {
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
    ctx: &'a NormalizedContext,
    initialized: InitState,
    out_param_defs: &'a HashSet<DefId>,
    errors: &'a mut Vec<SemCheckError>,
    init_assigns: &'a mut HashSet<NodeId>,
    full_init_assigns: &'a mut HashSet<NodeId>,
}

impl<'a> DefInitChecker<'a> {
    fn new(
        ctx: &'a NormalizedContext,
        initialized: InitState,
        out_param_defs: &'a HashSet<DefId>,
        errors: &'a mut Vec<SemCheckError>,
        init_assigns: &'a mut HashSet<NodeId>,
        full_init_assigns: &'a mut HashSet<NodeId>,
    ) -> Self {
        Self {
            ctx,
            initialized,
            out_param_defs,
            errors,
            init_assigns,
            full_init_assigns,
        }
    }

    fn visit_cfg_node(&mut self, node: &TreeCfgNode<'_, TypeId>) {
        // Loop bodies pre-initialize their pattern bindings.
        for pattern in &node.loop_inits {
            self.mark_pattern_initialized(pattern);
        }
        for item in &node.items {
            match item {
                TreeCfgItem::Stmt(stmt) => self.check_stmt(stmt),
                TreeCfgItem::Expr(expr) => self.check_expr(expr),
            }
        }
        if let TreeCfgTerminator::If { cond, .. } = &node.term {
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
            StmtExprKind::VarDecl { def_id, .. } => {
                // Declaration without initializer: clear any inherited init from a
                // dominating path (e.g., the variable might shadow one from an outer
                // scope or be re-declared after a conditional init).
                self.initialized.clear_def(*def_id);
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
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
            StmtExprKind::Break | StmtExprKind::Continue => {}
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.check_expr(value);
                }
            }
        }
    }

    /// Handle the left-hand side of an assignment.
    /// - Bare variable: marks it initialized (this is the initializing assignment).
    /// - Projections (field, index): allow partial init for out params, otherwise
    ///   require a fully-initialized base.
    fn check_assignment(&mut self, assignee: &Expr) {
        match &assignee.kind {
            ExprKind::Var { def_id, .. } => {
                // Assigning to a variable initializes it. Track the first
                // assignment so lowering can skip dropping uninitialized memory.
                if !self.initialized.is_full(*def_id) {
                    self.init_assigns.insert(assignee.id);
                }
                self.mark_var_initialized(assignee);
            }
            ExprKind::ArrayIndex { target, indices } => {
                let allow_partial = self.allow_partial_for_assignment(assignee);
                self.check_projection_assignment(assignee, target, allow_partial);
                for index in indices {
                    self.check_expr(index);
                }
            }
            ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
                let allow_partial = self.allow_partial_for_assignment(assignee);
                self.check_projection_assignment(assignee, target, allow_partial);
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
            ExprKind::Var { .. } => self.check_var_use(expr),
            ExprKind::StructField { target, .. }
            | ExprKind::TupleField { target, .. }
            | ExprKind::ArrayIndex { target, .. }
            | ExprKind::Slice { target, .. } => self.check_place_base_initialized(target),
            _ => self.check_expr(expr),
        }
    }

    fn check_projection_assignment(&mut self, assignee: &Expr, target: &Expr, allow_partial: bool) {
        let Some(_) = self.lookup_tracked_def(assignee) else {
            self.check_expr(assignee);
            return;
        };

        if allow_partial && let Some(path) = self.init_path_from_lvalue(assignee) {
            self.mark_partial_init(path, assignee.id);
            return;
        }

        // For non-out params we still require a fully-initialized base.
        self.check_place_base_initialized(target);
    }

    fn check_lvalue_use(&mut self, expr: &Expr) {
        if matches!(expr.kind, ExprKind::Var { .. }) {
            self.check_var_use(expr);
            return;
        }

        let Some(base_def) = self.lookup_tracked_def(expr) else {
            self.check_untracked_lvalue(expr);
            return;
        };

        if let Some(path) = self.init_path_from_lvalue(expr) {
            if !self.initialized.is_path_initialized(&path) {
                self.report_use_before_init(base_def, expr.span);
            }
            return;
        }

        if !self.initialized.is_full(base_def) {
            self.report_use_before_init(base_def, expr.span);
        }
    }

    /// Out params can be initialized field-by-field (partial init allowed).
    fn allow_partial_for_assignment(&self, assignee: &Expr) -> bool {
        let Some(base_def) = self.lookup_tracked_def(assignee) else {
            return false;
        };
        self.out_param_defs.contains(&base_def)
    }

    fn mark_partial_init(&mut self, path: InitPath, node_id: NodeId) {
        let was_full = self.initialized.is_full(path.base);
        if !self.initialized.is_path_initialized(&path) {
            self.init_assigns.insert(node_id);
        }
        self.initialized.add_partial(path.clone());
        let promoted = self.try_promote_full(path.base);
        if promoted && !was_full && self.is_local_var(path.base) {
            self.full_init_assigns.insert(node_id);
        }
    }

    /// When all fields/elements of an aggregate are individually initialized,
    /// promote it to fully-initialized (e.g., after `x.a = ...; x.b = ...`).
    fn try_promote_full(&mut self, base: DefId) -> bool {
        if self.initialized.is_full(base) {
            return false;
        }
        let Some(ty) = self.def_type(base) else {
            return false;
        };
        if self.is_fully_initialized_via_partials(base, &ty) {
            self.initialized.mark_full(base);
            return true;
        }
        false
    }

    /// Check if every top-level field/element of `base` is in the partial set.
    /// Only checks one level deep (no recursive descent into nested types).
    fn is_fully_initialized_via_partials(&self, base: DefId, ty: &Type) -> bool {
        match ty {
            Type::Struct { fields, .. } => fields.iter().all(|field| {
                self.initialized.partial.contains(&InitPath {
                    base,
                    projections: vec![InitProj::Field(field.name.clone())],
                })
            }),
            Type::Tuple { field_tys } => field_tys.iter().enumerate().all(|(index, _)| {
                self.initialized.partial.contains(&InitPath {
                    base,
                    projections: vec![InitProj::TupleField(index)],
                })
            }),
            Type::Array { dims, .. } => {
                let Some(&len) = dims.first() else {
                    return false;
                };
                (0..len as u64).all(|index| {
                    self.initialized.partial.contains(&InitPath {
                        base,
                        projections: vec![InitProj::Index(index)],
                    })
                })
            }
            _ => false,
        }
    }

    fn init_path_from_lvalue(&self, expr: &Expr) -> Option<InitPath> {
        let mut projections = Vec::new();
        let base = self.collect_lvalue_path(expr, &mut projections)?;
        if projections.is_empty() {
            return None;
        }
        Some(InitPath { base, projections })
    }

    fn collect_lvalue_path(&self, expr: &Expr, projections: &mut Vec<InitProj>) -> Option<DefId> {
        match &expr.kind {
            ExprKind::Var { def_id, .. } => Some(*def_id),
            ExprKind::StructField { target, field } => {
                let base = self.collect_lvalue_path(target, projections)?;
                projections.push(InitProj::Field(field.clone()));
                Some(base)
            }
            ExprKind::TupleField { target, index } => {
                let base = self.collect_lvalue_path(target, projections)?;
                projections.push(InitProj::TupleField(*index));
                Some(base)
            }
            ExprKind::ArrayIndex { target, indices } => {
                let base = self.collect_lvalue_path(target, projections)?;
                for index in indices {
                    let ExprKind::IntLit(value) = index.kind else {
                        return None;
                    };
                    projections.push(InitProj::Index(value));
                }
                Some(base)
            }
            _ => None,
        }
    }

    fn def_type(&self, def_id: DefId) -> Option<Type> {
        let def = self.ctx.def_table.lookup_def(def_id)?;
        self.ctx.type_map.lookup_def_type(def)
    }

    /// Traverse projections to find the base variable, returning its DefId only
    /// if it's a local or param (we don't track init for globals/functions).
    fn lookup_tracked_def(&self, expr: &Expr) -> Option<DefId> {
        let def_id = match &expr.kind {
            ExprKind::Var { def_id, .. } => Some(*def_id),
            ExprKind::StructField { target, .. }
            | ExprKind::TupleField { target, .. }
            | ExprKind::ArrayIndex { target, .. }
            | ExprKind::Slice { target, .. } => self.lookup_tracked_def(target),
            _ => None,
        }?;

        let def = self.ctx.def_table.lookup_def(def_id)?;
        matches!(def.kind, DefKind::LocalVar { .. } | DefKind::Param { .. }).then_some(def_id)
    }

    fn is_local_var(&self, def_id: DefId) -> bool {
        let Some(def) = self.ctx.def_table.lookup_def(def_id) else {
            return false;
        };
        matches!(def.kind, DefKind::LocalVar { .. })
    }

    fn check_untracked_lvalue(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
                self.check_expr(target);
            }
            ExprKind::ArrayIndex { target, indices } => {
                self.check_expr(target);
                for index in indices {
                    self.check_expr(index);
                }
            }
            ExprKind::Slice { target, start, end } => {
                self.check_expr(target);
                if let Some(start) = start {
                    self.check_expr(start);
                }
                if let Some(end) = end {
                    self.check_expr(end);
                }
            }
            _ => {
                self.check_expr(expr);
            }
        }
    }

    fn report_use_before_init(&mut self, def_id: DefId, span: Span) {
        let Some(def) = self.ctx.def_table.lookup_def(def_id) else {
            return;
        };
        self.errors
            .push(SemCheckError::UseBeforeInit(def.name.clone(), span));
    }

    fn mark_pattern_initialized(&mut self, pattern: &BindPattern) {
        match &pattern.kind {
            BindPatternKind::Name { def_id, .. } => {
                self.initialized.mark_full(*def_id);
            }
            BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
                for pat in patterns {
                    self.mark_pattern_initialized(pat);
                }
            }
            BindPatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.mark_pattern_initialized(&field.pattern);
                }
            }
        }
    }

    fn mark_var_initialized(&mut self, expr: &Expr) {
        if let ExprKind::Var { def_id, .. } = expr.kind {
            self.initialized.mark_full(def_id);
        }
    }

    /// Report an error if a local variable is used before initialization.
    fn check_var_use(&mut self, expr: &Expr) {
        let ExprKind::Var { def_id, .. } = expr.kind else {
            return;
        };
        let Some(def) = self.ctx.def_table.lookup_def(def_id) else {
            return;
        };
        // Only check locals and params (globals, functions, etc. are always "initialized").
        if !matches!(def.kind, DefKind::LocalVar { .. } | DefKind::Param { .. }) {
            return;
        }
        if !self.initialized.is_full(def.id) {
            self.errors
                .push(SemCheckError::UseBeforeInit(def.name.clone(), expr.span));
        }
    }

    fn check_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var { .. } => self.check_var_use(expr),
            ExprKind::StructField { .. } | ExprKind::TupleField { .. } => {
                self.check_lvalue_use(expr);
            }
            ExprKind::ArrayIndex { indices, .. } => {
                self.check_lvalue_use(expr);
                for index in indices {
                    self.check_expr(index);
                }
            }
            ExprKind::Slice { start, end, .. } => {
                self.check_lvalue_use(expr);
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
                self.initialized = intersect_states(&[then_state, else_state]);
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
                    self.initialized = intersect_states(&arm_states);
                }
            }
            ExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        BlockItem::Stmt(stmt) => self.check_stmt(stmt),
                        BlockItem::Expr(expr) => self.check_expr(expr),
                    }
                }
                if let Some(tail) = tail {
                    self.check_expr(tail);
                }
            }
            ExprKind::Call { callee, args } => {
                self.check_expr(callee);
                if let Some(sig) = self.ctx.call_sigs.get(&expr.id) {
                    let mut out_defs = Vec::new();
                    for (param, arg) in sig.params.iter().zip(args) {
                        if param.mode == ParamMode::Out && arg.mode == CallArgMode::Out {
                            // Out args are write-only and become initialized after the call.
                            if let Some(def_id) = self.check_out_arg(&arg.expr) {
                                out_defs.push(def_id);
                            }
                        } else {
                            self.check_expr(&arg.expr);
                        }
                    }
                    for def_id in out_defs {
                        self.initialized.mark_full(def_id);
                    }
                } else {
                    for arg in args {
                        self.check_expr(&arg.expr);
                    }
                }
            }

            ExprKind::MethodCall { callee, args, .. } => {
                self.check_expr(callee);
                if let Some(sig) = self.ctx.call_sigs.get(&expr.id) {
                    let mut out_defs = Vec::new();
                    for (param, arg) in sig.params.iter().zip(args) {
                        if param.mode == ParamMode::Out && arg.mode == CallArgMode::Out {
                            if let Some(def_id) = self.check_out_arg(&arg.expr) {
                                out_defs.push(def_id);
                            }
                        } else {
                            self.check_expr(&arg.expr);
                        }
                    }
                    for def_id in out_defs {
                        self.initialized.mark_full(def_id);
                    }
                } else {
                    for arg in args {
                        self.check_expr(&arg.expr);
                    }
                }
            }
            ExprKind::Emit { kind } => match kind {
                EmitKind::Send { to, payload }
                | EmitKind::Request {
                    to,
                    payload,
                    request_site_label: _,
                } => {
                    self.check_expr(to);
                    self.check_expr(payload);
                }
            },
            ExprKind::Reply { cap, value } => {
                self.check_expr(cap);
                self.check_expr(value);
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
                ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.check_expr(elem);
                    }
                }
                ArrayLitInit::Repeat(expr, _) => self.check_expr(expr),
            },
            ExprKind::SetLit { elems, .. } => {
                for elem in elems {
                    self.check_expr(elem);
                }
            }
            ExprKind::MapLit { entries, .. } => {
                for entry in entries {
                    self.check_expr(&entry.key);
                    self.check_expr(&entry.value);
                }
            }
            ExprKind::TupleLit(fields) => {
                for field in fields {
                    self.check_expr(field);
                }
            }
            ExprKind::StringFmt { segments } => {
                for segment in segments {
                    if let StringFmtSegment::Expr { expr, .. } = segment {
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
            ExprKind::Coerce { expr, .. } => self.check_expr(expr),
            ExprKind::ImplicitMove { expr } => self.check_expr(expr),
            ExprKind::AddrOf { expr } => self.check_expr(expr),
            ExprKind::Deref { expr } => self.check_expr(expr),
            ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::UnitLit
            | ExprKind::Range { .. }
            | ExprKind::Closure { .. } => {}
        }
    }

    fn check_out_arg(&mut self, arg: &Expr) -> Option<DefId> {
        match &arg.kind {
            ExprKind::Var { def_id, .. } => {
                let def = self.ctx.def_table.lookup_def(*def_id)?;
                if matches!(def.kind, DefKind::LocalVar { .. } | DefKind::Param { .. }) {
                    if !self.initialized.is_full(*def_id) {
                        // Record the first init through an `out` call so lowering skips drops.
                        self.init_assigns.insert(arg.id);
                    }
                    return Some(*def_id);
                }
                None
            }
            ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
                self.check_projection_assignment(arg, target, true);
                None
            }
            ExprKind::ArrayIndex { target, indices } => {
                self.check_projection_assignment(arg, target, true);
                for index in indices {
                    self.check_expr(index);
                }
                None
            }
            ExprKind::Slice { target, start, end } => {
                self.check_place_base_initialized(target);
                if let Some(start) = start {
                    self.check_expr(start);
                }
                if let Some(end) = end {
                    self.check_expr(end);
                }
                None
            }
            ExprKind::Coerce { expr, .. } => self.check_out_arg(expr),
            ExprKind::ImplicitMove { expr } => self.check_out_arg(expr),
            _ => {
                self.check_expr(arg);
                None
            }
        }
    }

    fn mark_match_pattern_initialized(&mut self, init: &mut InitState, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Binding { def_id, .. } | MatchPattern::TypedBinding { def_id, .. } => {
                init.mark_full(*def_id);
            }
            MatchPattern::Tuple { patterns, .. } => {
                for pattern in patterns {
                    self.mark_match_pattern_initialized(init, pattern);
                }
            }
            MatchPattern::EnumVariant { bindings, .. } => {
                for binding in bindings {
                    if let MatchPatternBinding::Named { def_id, .. } = binding {
                        init.mark_full(*def_id);
                    }
                }
            }
            _ => {}
        }
    }

    /// Run `f` with a temporary initialized set, then restore the original and return
    /// the resulting state. Used to analyze branches without polluting the outer state.
    fn with_init<F>(&mut self, init: InitState, f: F) -> InitState
    where
        F: FnOnce(&mut Self),
    {
        let saved = std::mem::replace(&mut self.initialized, init);
        f(self);
        std::mem::replace(&mut self.initialized, saved)
    }
}
