//! Closure capture analysis and capture-mode classification.
//!
//! This pass finds outer-scope defs referenced inside closures, records their
//! DefIds per closure, and classifies captures as immutable or mutable borrows
//! based on how the closure body uses them. Moving a captured base is rejected.

use std::collections::{HashMap, HashSet};

use crate::core::context::NormalizedContext;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefKind};
use crate::core::semck::SemCheckError;
use crate::core::tree::normalized::{
    BindPattern, BindPatternKind, CallArg, CaptureSpec, Expr, ExprKind, MatchPattern,
    MatchPatternBinding, Param, ParamMode, StmtExpr, StmtExprKind,
};
use crate::core::tree::visit::{
    Visitor, walk_bind_pattern, walk_expr, walk_match_pattern, walk_match_pattern_binding,
    walk_match_pattern_bindings, walk_stmt_expr,
};
use crate::core::types::TypeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureMode {
    ImmBorrow,
    MutBorrow,
    Move,
}

#[derive(Debug, Clone)]
pub struct ClosureCapture {
    pub def_id: DefId,
    pub mode: CaptureMode,
}

pub struct CaptureResult {
    pub errors: Vec<SemCheckError>,
    pub captures: HashMap<DefId, Vec<ClosureCapture>>,
}

pub(crate) fn check(ctx: &NormalizedContext) -> CaptureResult {
    let mut checker = ClosureCaptureChecker::new(ctx);
    checker.visit_module(&ctx.module);
    CaptureResult {
        errors: checker.errors,
        captures: checker.captures,
    }
}

struct ClosureCaptureChecker<'a> {
    ctx: &'a NormalizedContext,
    errors: Vec<SemCheckError>,
    captures: HashMap<DefId, Vec<ClosureCapture>>,
}

impl<'a> Visitor<DefId, TypeId> for ClosureCaptureChecker<'a> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Closure {
                def_id,
                captures,
                params,
                body,
                ..
            } => {
                self.check_closure_expr(*def_id, captures, params, body);
            }
            _ => walk_expr(self, expr),
        }
    }
}

impl<'a> ClosureCaptureChecker<'a> {
    fn new(ctx: &'a NormalizedContext) -> Self {
        Self {
            ctx,
            errors: Vec::new(),
            captures: HashMap::new(),
        }
    }

    fn check_closure_expr(
        &mut self,
        def_id: DefId,
        captures: &[CaptureSpec],
        params: &[Param],
        body: &Expr,
    ) {
        // Start with local defs (params + bindings) so we only capture outer defs.
        let mut locals = HashSet::new();
        for param in params {
            locals.insert(param.def_id);
        }
        let mut collector = LocalCollector::new(&mut locals);
        collector.visit_expr(body);

        // Seed explicit move captures, then scan for inferred borrows.
        let mut capture_modes = HashMap::new();
        for spec in captures {
            let CaptureSpec::Move { def_id, .. } = spec;
            capture_modes.insert(*def_id, CaptureMode::Move);
        }
        let mut used = HashSet::new();
        CaptureScan::new(self, &locals, &mut capture_modes, &mut used).visit_expr(body);

        // Check for unused move captures.
        for spec in captures {
            let (spec_def_id, span) = match spec {
                CaptureSpec::Move { def_id, span, .. } => (*def_id, *span),
            };
            if !used.contains(&spec_def_id) {
                let name = self.def_name(spec_def_id);
                self.errors
                    .push(SemCheckError::ClosureCaptureUnused(name, span));
            }
        }

        if !capture_modes.is_empty() {
            // Sort captures by DefId to ensure deterministic output.
            let mut capture_list: Vec<ClosureCapture> = capture_modes
                .into_iter()
                .map(|(def_id, mode)| ClosureCapture { def_id, mode })
                .collect();
            capture_list.sort_by_key(|capture| capture.def_id.0);
            self.captures.insert(def_id, capture_list);
        }
    }

    fn check_call_mutation(
        &mut self,
        call: &Expr,
        args: &[CallArg],
        receiver: Option<&Expr>,
        locals: &HashSet<DefId>,
        captures: &mut HashMap<DefId, CaptureMode>,
        used: &mut HashSet<DefId>,
    ) {
        // We only know arg modes after type check, via CallSig.
        let Some(sig) = self.ctx.call_sigs.get(&call.id) else {
            panic!(
                "compiler bug: missing call signature for call node {}",
                call.id
            );
        };

        if let (Some(receiver_param), Some(receiver_expr)) = (sig.receiver.as_ref(), receiver) {
            match receiver_param.mode {
                ParamMode::InOut | ParamMode::Out => {
                    // Treat inout/out as a write to the receiver base.
                    if let Some(def_id) =
                        self.check_write(receiver_expr, locals, captures, receiver_expr.span)
                    {
                        used.insert(def_id);
                    }
                }
                ParamMode::Sink => {
                    // Sink means ownership transfer (move) of the receiver base.
                    if let Some(def_id) =
                        self.check_move(receiver_expr, locals, captures, receiver_expr.span)
                    {
                        used.insert(def_id);
                    }
                }
                ParamMode::In => {}
            }
        }

        for (param, arg) in sig.params.iter().zip(args) {
            match param.mode {
                ParamMode::InOut | ParamMode::Out => {
                    if let Some(def_id) = self.check_write(&arg.expr, locals, captures, arg.span) {
                        used.insert(def_id);
                    }
                }
                ParamMode::Sink => {
                    if let Some(def_id) = self.check_move(&arg.expr, locals, captures, arg.span) {
                        used.insert(def_id);
                    }
                }
                ParamMode::In => {}
            }
        }
    }

    fn check_write(
        &mut self,
        expr: &Expr,
        locals: &HashSet<DefId>,
        captures: &mut HashMap<DefId, CaptureMode>,
        span: Span,
    ) -> Option<DefId> {
        let Some(def_id) = Self::lvalue_base_def_id(expr) else {
            panic!("compiler bug: expected lvalue base for write at {}", span);
        };
        self.maybe_capture(def_id, locals, captures, CaptureMode::MutBorrow);
        Some(def_id)
    }

    fn check_move(
        &mut self,
        expr: &Expr,
        locals: &HashSet<DefId>,
        captures: &mut HashMap<DefId, CaptureMode>,
        span: Span,
    ) -> Option<DefId> {
        let Some(def_id) = Self::lvalue_base_def_id(expr) else {
            panic!("compiler bug: expected lvalue base for move at {}", span);
        };
        if self.maybe_capture(def_id, locals, captures, CaptureMode::ImmBorrow) {
            // Moving a captured value would invalidate the outer binding.
            let name = self.def_name(def_id);
            self.errors
                .push(SemCheckError::ClosureCaptureMove(name, span));
        }
        Some(def_id)
    }

    fn maybe_capture(
        &mut self,
        def_id: DefId,
        locals: &HashSet<DefId>,
        captures: &mut HashMap<DefId, CaptureMode>,
        mode: CaptureMode,
    ) -> bool {
        // Only capture defs that are defined outside this closure.
        if locals.contains(&def_id) {
            return false;
        }
        if !self.is_capture_candidate(def_id) {
            return false;
        }
        // Explicit move capture dominates: don't downgrade to a borrow.
        if captures.get(&def_id) == Some(&CaptureMode::Move) {
            return false;
        }
        // Upgrade to MutBorrow if needed, otherwise insert the new mode.
        captures
            .entry(def_id)
            .and_modify(|cap_mode| {
                if mode == CaptureMode::MutBorrow {
                    *cap_mode = CaptureMode::MutBorrow;
                }
            })
            .or_insert(mode);
        true
    }

    fn is_capture_candidate(&self, def_id: DefId) -> bool {
        // Only variables/params can be captured; types/functions aren't.
        let Some(def) = self.ctx.def_table.lookup_def(def_id) else {
            panic!("compiler bug: missing def for id {}", def_id.0);
        };
        matches!(def.kind, DefKind::LocalVar { .. } | DefKind::Param { .. })
    }

    fn def_name(&self, def_id: DefId) -> String {
        self.ctx
            .def_table
            .lookup_def(def_id)
            .map(|def| def.name.clone())
            .unwrap_or_else(|| format!("def#{}", def_id.0))
    }

    fn lvalue_base_def_id(expr: &Expr) -> Option<DefId> {
        // Walk through projections to find the base def for mutation/move checks.
        match &expr.kind {
            ExprKind::Var { def_id, .. } => Some(*def_id),
            ExprKind::ArrayIndex { target, .. }
            | ExprKind::TupleField { target, .. }
            | ExprKind::StructField { target, .. }
            | ExprKind::Slice { target, .. }
            | ExprKind::Coerce { expr: target, .. }
            | ExprKind::Move { expr: target }
            | ExprKind::ImplicitMove { expr: target } => Self::lvalue_base_def_id(target),
            _ => None,
        }
    }
}

// --- Local Collector ---

struct LocalCollector<'a> {
    locals: &'a mut HashSet<DefId>,
}

impl<'a> LocalCollector<'a> {
    fn new(locals: &'a mut HashSet<DefId>) -> Self {
        Self { locals }
    }
}

impl<'a> Visitor<DefId, TypeId> for LocalCollector<'a> {
    fn visit_expr(&mut self, expr: &Expr) {
        // Skip nested closures: they have their own capture sets.
        if matches!(expr.kind, ExprKind::Closure { .. }) {
            return;
        }
        walk_expr(self, expr);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        // VarDecl introduces a local binding even without an initializer.
        if let StmtExprKind::VarDecl { def_id, .. } = &stmt.kind {
            self.locals.insert(*def_id);
        }
        walk_stmt_expr(self, stmt);
    }

    fn visit_bind_pattern(&mut self, pattern: &BindPattern) {
        // Pattern bindings (let/var/for) create local defs.
        if let BindPatternKind::Name { def_id, .. } = &pattern.kind {
            self.locals.insert(*def_id);
        }
        walk_bind_pattern(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        if let MatchPattern::Binding { def_id, .. } | MatchPattern::TypedBinding { def_id, .. } =
            pattern
        {
            self.locals.insert(*def_id);
        }
        walk_match_pattern(self, pattern);
        walk_match_pattern_bindings(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding) {
        // Enum variant bindings are another source of local defs.
        if let MatchPatternBinding::Named { def_id, .. } = binding {
            self.locals.insert(*def_id);
        }
        walk_match_pattern_binding(self, binding);
    }
}

// --- Capture Scan ---

struct CaptureScan<'a, 'b> {
    checker: &'a mut ClosureCaptureChecker<'b>,
    locals: &'a HashSet<DefId>,
    captures: &'a mut HashMap<DefId, CaptureMode>,
    used: &'a mut HashSet<DefId>,
}

impl<'a, 'b> CaptureScan<'a, 'b> {
    fn new(
        checker: &'a mut ClosureCaptureChecker<'b>,
        locals: &'a HashSet<DefId>,
        captures: &'a mut HashMap<DefId, CaptureMode>,
        used: &'a mut HashSet<DefId>,
    ) -> Self {
        Self {
            checker,
            locals,
            captures,
            used,
        }
    }
}

impl<'a, 'b> Visitor<DefId, TypeId> for CaptureScan<'a, 'b> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var { def_id, .. } => {
                self.used.insert(*def_id);
                // Any outer var use becomes a capture.
                self.checker.maybe_capture(
                    *def_id,
                    self.locals,
                    self.captures,
                    CaptureMode::ImmBorrow,
                );
            }
            ExprKind::Move { expr: inner } | ExprKind::ImplicitMove { expr: inner } => {
                // Move is forbidden for captured defs.
                if let Some(def_id) =
                    self.checker
                        .check_move(inner, self.locals, self.captures, expr.span)
                {
                    self.used.insert(def_id);
                }
                self.visit_expr(inner);
                return;
            }
            ExprKind::Closure {
                def_id,
                captures,
                params,
                body,
                ..
            } => {
                // Nested closure: compute captures separately and stop traversal here.
                self.checker
                    .check_closure_expr(*def_id, captures, params, body);
                return;
            }
            ExprKind::Call { args, .. } => {
                // Calls may implicitly write/move via arg modes.
                self.checker.check_call_mutation(
                    expr,
                    args,
                    None,
                    self.locals,
                    self.captures,
                    self.used,
                );
            }
            ExprKind::MethodCall { callee, args, .. } => {
                // Receiver mode also matters for mutation/move.
                self.checker.check_call_mutation(
                    expr,
                    args,
                    Some(callee),
                    self.locals,
                    self.captures,
                    self.used,
                );
            }
            _ => {}
        }
        walk_expr(self, expr);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        // Assignments are explicit writes that mark a mutable capture.
        if let StmtExprKind::Assign { assignee, .. } = &stmt.kind
            && let Some(def_id) =
                self.checker
                    .check_write(assignee, self.locals, self.captures, assignee.span)
        {
            self.used.insert(def_id);
        }
        walk_stmt_expr(self, stmt);
    }
}
