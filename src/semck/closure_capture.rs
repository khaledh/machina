//! Closure capture analysis and read-only enforcement.
//!
//! This pass finds outer-scope defs referenced inside closures, records their
//! DefIds per closure, and rejects any mutation or move of captured vars
//! (including via inout/out/sink arguments) to keep captures immutable for now.

use std::collections::{HashMap, HashSet};

use crate::context::NormalizedContext;
use crate::resolve::{DefId, DefKind};
use crate::semck::SemCheckError;
use crate::tree::normalized::{
    BindPattern, BindPatternKind, CallArg, Expr, ExprKind, MatchPattern, MatchPatternBinding,
    Param, ParamMode, StmtExpr, StmtExprKind,
};
use crate::tree::visit::{
    Visitor, walk_bind_pattern, walk_expr, walk_match_pattern, walk_match_pattern_binding,
    walk_match_pattern_bindings, walk_stmt_expr,
};
use crate::types::TypeId;

pub struct CaptureResult {
    pub errors: Vec<SemCheckError>,
    pub captures: HashMap<DefId, Vec<DefId>>,
}

pub(super) fn check(ctx: &NormalizedContext) -> CaptureResult {
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
    captures: HashMap<DefId, Vec<DefId>>,
}

impl<'a> Visitor<DefId, TypeId> for ClosureCaptureChecker<'a> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Closure {
                def_id,
                params,
                body,
                ..
            } => {
                self.check_closure_expr(*def_id, params, body);
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

    fn check_closure_expr(&mut self, def_id: DefId, params: &[Param], body: &Expr) {
        // Start with local defs (params + bindings) so we only capture outer defs.
        let mut locals = HashSet::new();
        for param in params {
            locals.insert(param.def_id);
        }
        let mut collector = LocalCollector::new(&mut locals);
        collector.visit_expr(body);

        // Scan for reads, but also reject mutation/move of any captured defs.
        let mut captures = HashSet::new();
        CaptureScan::new(self, &locals, &mut captures).visit_expr(body);

        // Sort captures by DefId to ensure deterministic output.
        let mut capture_list: Vec<DefId> = captures.into_iter().collect();
        capture_list.sort_by_key(|id| id.0);
        self.captures.insert(def_id, capture_list);
    }

    fn check_call_mutation(
        &mut self,
        call: &Expr,
        args: &[CallArg],
        receiver: Option<&Expr>,
        locals: &HashSet<DefId>,
        captures: &mut HashSet<DefId>,
    ) {
        // We only know arg modes after type check, via CallSig.
        let Some(sig) = self.ctx.type_map.lookup_call_sig(call.id) else {
            panic!(
                "compiler bug: missing call signature for call node {}",
                call.id
            );
        };

        if let (Some(receiver_param), Some(receiver_expr)) = (sig.receiver.as_ref(), receiver) {
            match receiver_param.mode {
                ParamMode::InOut | ParamMode::Out => {
                    // Treat inout/out as a write to the receiver base.
                    self.check_write(receiver_expr, locals, captures, receiver_expr.span);
                }
                ParamMode::Sink => {
                    // Sink means ownership transfer (move) of the receiver base.
                    self.check_move(receiver_expr, locals, captures, receiver_expr.span);
                }
                ParamMode::In => {}
            }
        }

        for (param, arg) in sig.params.iter().zip(args) {
            match param.mode {
                ParamMode::InOut | ParamMode::Out => {
                    self.check_write(&arg.expr, locals, captures, arg.span);
                }
                ParamMode::Sink => {
                    self.check_move(&arg.expr, locals, captures, arg.span);
                }
                ParamMode::In => {}
            }
        }
    }

    fn check_write(
        &mut self,
        expr: &Expr,
        locals: &HashSet<DefId>,
        captures: &mut HashSet<DefId>,
        span: crate::diag::Span,
    ) {
        let Some(def_id) = Self::lvalue_base_def_id(expr) else {
            panic!("compiler bug: expected lvalue base for write at {}", span);
        };
        if self.maybe_capture(def_id, locals, captures) {
            // Mutating captured vars is rejected for the immutable-capture phase.
            let name = self.def_name(def_id);
            self.errors
                .push(SemCheckError::ClosureCaptureMutate(name, span));
        }
    }

    fn check_move(
        &mut self,
        expr: &Expr,
        locals: &HashSet<DefId>,
        captures: &mut HashSet<DefId>,
        span: crate::diag::Span,
    ) {
        let Some(def_id) = Self::lvalue_base_def_id(expr) else {
            panic!("compiler bug: expected lvalue base for move at {}", span);
        };
        if self.maybe_capture(def_id, locals, captures) {
            // Moving a captured value would invalidate the outer binding.
            let name = self.def_name(def_id);
            self.errors
                .push(SemCheckError::ClosureCaptureMove(name, span));
        }
    }

    fn maybe_capture(
        &mut self,
        def_id: DefId,
        locals: &HashSet<DefId>,
        captures: &mut HashSet<DefId>,
    ) -> bool {
        // Only capture defs that are defined outside this closure.
        if locals.contains(&def_id) {
            return false;
        }
        if !self.is_capture_candidate(def_id) {
            return false;
        }
        captures.insert(def_id);
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
        match pattern {
            MatchPattern::Binding { def_id, .. } => {
                self.locals.insert(*def_id);
            }
            _ => {}
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
    captures: &'a mut HashSet<DefId>,
}

impl<'a, 'b> CaptureScan<'a, 'b> {
    fn new(
        checker: &'a mut ClosureCaptureChecker<'b>,
        locals: &'a HashSet<DefId>,
        captures: &'a mut HashSet<DefId>,
    ) -> Self {
        Self {
            checker,
            locals,
            captures,
        }
    }
}

impl<'a, 'b> Visitor<DefId, TypeId> for CaptureScan<'a, 'b> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var { def_id, .. } => {
                // Any outer var use becomes a capture.
                self.checker
                    .maybe_capture(*def_id, self.locals, self.captures);
            }
            ExprKind::Move { expr: inner } | ExprKind::ImplicitMove { expr: inner } => {
                // Move is forbidden for captured defs.
                self.checker
                    .check_move(inner, self.locals, self.captures, expr.span);
                self.visit_expr(inner);
                return;
            }
            ExprKind::Closure {
                def_id,
                params,
                body,
                ..
            } => {
                // Nested closure: compute captures separately and stop traversal here.
                self.checker.check_closure_expr(*def_id, params, body);
                return;
            }
            ExprKind::Call { args, .. } => {
                // Calls may implicitly write/move via arg modes.
                self.checker
                    .check_call_mutation(expr, args, None, self.locals, self.captures);
            }
            ExprKind::MethodCall { callee, args, .. } => {
                // Receiver mode also matters for mutation/move.
                self.checker.check_call_mutation(
                    expr,
                    args,
                    Some(callee),
                    self.locals,
                    self.captures,
                );
            }
            _ => {}
        }
        walk_expr(self, expr);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        // Assignments are explicit writes that must be rejected on captures.
        if let StmtExprKind::Assign { assignee, .. } = &stmt.kind {
            self.checker
                .check_write(assignee, self.locals, self.captures, assignee.span);
        }
        walk_stmt_expr(self, stmt);
    }
}
