//! Borrow conflict and escape checks for captured closures.
//!
//! This module walks each function CFG, combines closure bindings with
//! liveness, and enforces that captured bases are not accessed in conflicting
//! ways while a closure is live. It also rejects escaping captured closures.
use std::collections::{HashMap, HashSet};

use crate::core::context::NormalizedContext;
use crate::core::diag::Span;
use crate::core::resolve::DefId;
use crate::core::semck::closure::capture::CaptureMode;
use crate::core::semck::{push_error, SemCheckError, SEK};
use crate::core::tree::NodeId;
use crate::core::tree::cfg::{TreeCfgBuilder, TreeCfgItem, TreeCfgTerminator};
use crate::core::tree::normalized::ArrayLitInit;
use crate::core::tree::normalized::{CallArg, Expr, ExprKind, FuncDef, ParamMode, StmtExprKind};
use crate::core::tree::visit::{Visitor, walk_expr};
use crate::core::types::TypeId;

use super::bindings::{analyze_closure_bindings, apply_item_bindings};
use super::collect::{collect_expr_var_uses, collect_item_var_uses, lvalue_base_def_id};
use super::liveness::analyze_closure_liveness;
use super::{CaptureMap, ClosureBindings};

pub(super) fn check_func_def(
    ctx: &NormalizedContext,
    func_def: &FuncDef,
    capture_map: &HashMap<DefId, CaptureMap>,
    errors: &mut Vec<SemCheckError>,
) {
    // Build flow-sensitive closure bindings and liveness for this function.
    let cfg = TreeCfgBuilder::<TypeId>::new().build_from_expr(&func_def.body);
    let bindings = analyze_closure_bindings(&cfg, capture_map);
    let liveness = analyze_closure_liveness(&cfg);
    let ret_expr_id = return_expr_id(func_def);

    for (block_idx, node) in cfg.nodes.iter().enumerate() {
        let mut state = bindings.in_map[block_idx].clone();

        for (item_idx, item) in node.items.iter().enumerate() {
            let mut active_closures = liveness.live_after[block_idx][item_idx].clone();
            collect_item_var_uses(item, &mut active_closures);
            active_closures.retain(|def_id| state.contains_key(def_id));

            if !active_closures.is_empty() {
                let borrowed = borrowed_bases_for_active(&active_closures, &state);
                if !borrowed.is_empty() {
                    // Enforce borrow conflicts for any active captured bases.
                    check_item_for_conflicts(ctx, item, &borrowed, errors);
                }
            }

            // Captured closures cannot escape via return/store/arg.
            check_item_for_escapes(item, &state, capture_map, errors);

            if let TreeCfgItem::Expr(expr) = item
                && Some(expr.id) == ret_expr_id
            {
                check_return_expr(expr, &state, capture_map, errors);
            }

            apply_item_bindings(&mut state, item, capture_map);
        }

        if let TreeCfgTerminator::If { cond, .. } = &node.term {
            let mut active_closures = liveness.live_out[block_idx].clone();
            collect_expr_var_uses(cond, &mut active_closures);
            active_closures.retain(|def_id| state.contains_key(def_id));

            if !active_closures.is_empty() {
                let borrowed = borrowed_bases_for_active(&active_closures, &state);
                if !borrowed.is_empty() {
                    let mut borrowed_mut = HashSet::new();
                    let mut borrowed_imm = HashSet::new();
                    for (def_id, mode) in &borrowed {
                        match mode {
                            CaptureMode::MutBorrow => {
                                borrowed_mut.insert(*def_id);
                            }
                            CaptureMode::ImmBorrow => {
                                borrowed_imm.insert(*def_id);
                            }
                            // Move captures don't contribute to borrow conflicts.
                            CaptureMode::Move => {}
                        }
                    }
                    if !borrowed_mut.is_empty() {
                        let mut visitor = MutBorrowConflictVisitor::new(ctx, &borrowed, errors);
                        visitor.visit_expr(cond);
                    }
                    if !borrowed_imm.is_empty() {
                        let mut visitor = ImmBorrowConflictVisitor::new(ctx, &borrowed_imm, errors);
                        visitor.visit_expr(cond);
                    }
                }
            }
            check_expr_for_escapes(cond, &state, capture_map, errors);
        }
    }
}

fn return_expr_id(func_def: &FuncDef) -> Option<NodeId> {
    match &func_def.body.kind {
        ExprKind::Block { tail, .. } => tail.as_deref().map(|expr| expr.id),
        _ => Some(func_def.body.id),
    }
}

fn borrowed_bases_for_active(
    active_closures: &HashSet<DefId>,
    state: &ClosureBindings,
) -> HashMap<DefId, CaptureMode> {
    let mut borrowed = HashMap::new();
    for closure_def in active_closures {
        if let Some(captures) = state.get(closure_def) {
            for (base_def, mode) in captures {
                // Move captures own the value in the closure env, so they don't
                // create a borrow conflict with the outer binding.
                if *mode == CaptureMode::Move {
                    continue;
                }
                borrowed
                    .entry(*base_def)
                    .and_modify(|current| {
                        if *mode == CaptureMode::MutBorrow {
                            *current = CaptureMode::MutBorrow;
                        }
                    })
                    .or_insert(*mode);
            }
        }
    }
    borrowed
}

fn check_item_for_conflicts(
    ctx: &NormalizedContext,
    item: &TreeCfgItem<'_, TypeId>,
    borrowed_bases: &HashMap<DefId, CaptureMode>,
    errors: &mut Vec<SemCheckError>,
) {
    let mut borrowed_mut = HashSet::new();
    let mut borrowed_imm = HashSet::new();
    for (def_id, mode) in borrowed_bases {
        match mode {
            CaptureMode::MutBorrow => {
                borrowed_mut.insert(*def_id);
            }
            CaptureMode::ImmBorrow => {
                borrowed_imm.insert(*def_id);
            }
            // Move captures don't participate in borrow-conflict sets.
            CaptureMode::Move => {}
        }
    }

    if !borrowed_mut.is_empty() {
        // Mutable borrow: any use of the base is illegal.
        let mut visitor = MutBorrowConflictVisitor::new(ctx, borrowed_bases, errors);
        match item {
            TreeCfgItem::Stmt(stmt) => match &stmt.kind {
                StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                    visitor.visit_expr(value);
                }
                StmtExprKind::VarDecl { .. } => {}
                StmtExprKind::Assign {
                    assignee, value, ..
                } => {
                    visitor.visit_expr(assignee);
                    visitor.visit_expr(value);
                }
                StmtExprKind::While { cond, body } => {
                    visitor.visit_expr(cond);
                    visitor.visit_expr(body);
                }
                StmtExprKind::For { iter, body, .. } => {
                    visitor.visit_expr(iter);
                    visitor.visit_expr(body);
                }
                StmtExprKind::Break | StmtExprKind::Continue => {}
                StmtExprKind::Return { value } => {
                    if let Some(value) = value {
                        visitor.visit_expr(value);
                    }
                }
            },
            TreeCfgItem::Expr(expr) => visitor.visit_expr(expr),
        }
    }

    if !borrowed_imm.is_empty() {
        // Immutable borrow: allow reads, forbid writes/moves.
        match item {
            TreeCfgItem::Stmt(stmt) => match &stmt.kind {
                StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                    let mut visitor = ImmBorrowConflictVisitor::new(ctx, &borrowed_imm, errors);
                    visitor.visit_expr(value);
                }
                StmtExprKind::VarDecl { .. } => {}
                StmtExprKind::Assign {
                    assignee, value, ..
                } => {
                    check_write_target(ctx, assignee, &borrowed_imm, errors);
                    let mut visitor = ImmBorrowConflictVisitor::new(ctx, &borrowed_imm, errors);
                    visitor.visit_expr(value);
                }
                StmtExprKind::While { cond, body } => {
                    let mut visitor = ImmBorrowConflictVisitor::new(ctx, &borrowed_imm, errors);
                    visitor.visit_expr(cond);
                    visitor.visit_expr(body);
                }
                StmtExprKind::For { iter, body, .. } => {
                    let mut visitor = ImmBorrowConflictVisitor::new(ctx, &borrowed_imm, errors);
                    visitor.visit_expr(iter);
                    visitor.visit_expr(body);
                }
                StmtExprKind::Break | StmtExprKind::Continue => {}
                StmtExprKind::Return { value } => {
                    if let Some(value) = value {
                        let mut visitor = ImmBorrowConflictVisitor::new(ctx, &borrowed_imm, errors);
                        visitor.visit_expr(value);
                    }
                }
            },
            TreeCfgItem::Expr(expr) => {
                let mut visitor = ImmBorrowConflictVisitor::new(ctx, &borrowed_imm, errors);
                visitor.visit_expr(expr);
            }
        }
    }
}

fn check_write_target(
    ctx: &NormalizedContext,
    expr: &Expr,
    borrowed_imm: &HashSet<DefId>,
    errors: &mut Vec<SemCheckError>,
) {
    if let Some(def_id) = lvalue_base_def_id(expr)
        && borrowed_imm.contains(&def_id)
    {
        let name = def_name(ctx, def_id);
        push_error(errors, expr.span, SEK::ClosureBorrowConflict(name));
    }
}

// ======================================================================
// Borrow conflict visitors
// ======================================================================

struct MutBorrowConflictVisitor<'a> {
    ctx: &'a NormalizedContext,
    borrowed_bases: &'a HashMap<DefId, CaptureMode>,
    errors: &'a mut Vec<SemCheckError>,
    reported: HashSet<DefId>,
}

impl<'a> MutBorrowConflictVisitor<'a> {
    fn new(
        ctx: &'a NormalizedContext,
        borrowed_bases: &'a HashMap<DefId, CaptureMode>,
        errors: &'a mut Vec<SemCheckError>,
    ) -> Self {
        Self {
            ctx,
            borrowed_bases,
            errors,
            reported: HashSet::new(),
        }
    }

    fn note_use(&mut self, def_id: DefId, span: Span) {
        let Some(mode) = self.borrowed_bases.get(&def_id) else {
            return;
        };
        if *mode != CaptureMode::MutBorrow {
            return;
        }
        if !self.reported.insert(def_id) {
            return;
        }
        // Report once per base to avoid spamming.
        let name = def_name(self.ctx, def_id);
        self.errors
            .push(SEK::ClosureBorrowConflict(name).at(span));
    }
}

impl Visitor<DefId, TypeId> for MutBorrowConflictVisitor<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Var { def_id, .. } = expr.kind {
            self.note_use(def_id, expr.span);
        }
        if matches!(expr.kind, ExprKind::Closure { .. }) {
            return;
        }
        walk_expr(self, expr);
    }
}

struct ImmBorrowConflictVisitor<'a> {
    ctx: &'a NormalizedContext,
    borrowed_bases: &'a HashSet<DefId>,
    errors: &'a mut Vec<SemCheckError>,
}

impl<'a> ImmBorrowConflictVisitor<'a> {
    fn new(
        ctx: &'a NormalizedContext,
        borrowed_bases: &'a HashSet<DefId>,
        errors: &'a mut Vec<SemCheckError>,
    ) -> Self {
        Self {
            ctx,
            borrowed_bases,
            errors,
        }
    }

    fn check_call(&mut self, call: &Expr, args: &[CallArg], receiver: Option<&Expr>) {
        let Some(sig) = self.ctx.call_sigs.get(&call.id) else {
            panic!(
                "compiler bug: missing call signature for call node {}",
                call.id
            );
        };

        // Treat inout/out/sink params as a write/move of the base.
        if let (Some(receiver_param), Some(receiver)) = (sig.receiver.as_ref(), receiver)
            && matches!(
                receiver_param.mode,
                ParamMode::InOut | ParamMode::Out | ParamMode::Sink
            )
        {
            self.check_move_target(receiver, receiver.span);
        }

        for (param, arg) in sig.params.iter().zip(args) {
            if matches!(
                param.mode,
                ParamMode::InOut | ParamMode::Out | ParamMode::Sink
            ) {
                self.check_move_target(&arg.expr, arg.span);
            }
        }
    }

    fn check_move_target(&mut self, expr: &Expr, span: Span) {
        let Some(def_id) = lvalue_base_def_id(expr) else {
            return;
        };
        if !self.borrowed_bases.contains(&def_id) {
            return;
        }
        let name = def_name(self.ctx, def_id);
        self.errors
            .push(SEK::ClosureBorrowConflict(name).at(span));
    }
}

impl Visitor<DefId, TypeId> for ImmBorrowConflictVisitor<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Move { expr: inner } | ExprKind::ImplicitMove { expr: inner } => {
                self.check_move_target(inner, expr.span);
                self.visit_expr(inner);
                return;
            }
            ExprKind::Call { args, .. } => {
                self.check_call(expr, args, None);
            }
            ExprKind::MethodCall { callee, args, .. } => {
                self.check_call(expr, args, Some(callee));
            }
            ExprKind::Closure { .. } => {
                return;
            }
            _ => {}
        }
        walk_expr(self, expr);
    }
}

// ======================================================================
// Closure escape checks (captured closures must not escape or be stored)
// ======================================================================

fn check_item_for_escapes(
    item: &TreeCfgItem<'_, TypeId>,
    state: &ClosureBindings,
    capture_map: &HashMap<DefId, CaptureMap>,
    errors: &mut Vec<SemCheckError>,
) {
    match item {
        TreeCfgItem::Stmt(stmt) => match &stmt.kind {
            StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                check_expr_for_escapes(value, state, capture_map, errors);
            }
            StmtExprKind::VarDecl { .. } => {}
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                if !matches!(assignee.kind, ExprKind::Var { .. })
                    && is_captured_closure_value(value, state, capture_map)
                {
                    push_error(errors, value.span, SEK::ClosureEscapeStore);
                }
                check_expr_for_escapes(value, state, capture_map, errors);
            }
            StmtExprKind::While { cond, body } => {
                check_expr_for_escapes(cond, state, capture_map, errors);
                check_expr_for_escapes(body, state, capture_map, errors);
            }
            StmtExprKind::For { iter, body, .. } => {
                check_expr_for_escapes(iter, state, capture_map, errors);
                check_expr_for_escapes(body, state, capture_map, errors);
            }
            StmtExprKind::Break | StmtExprKind::Continue => {}
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    check_expr_for_escapes(value, state, capture_map, errors);
                }
            }
        },
        TreeCfgItem::Expr(expr) => {
            check_expr_for_escapes(expr, state, capture_map, errors);
        }
    }
}

fn check_expr_for_escapes(
    expr: &Expr,
    state: &ClosureBindings,
    capture_map: &HashMap<DefId, CaptureMap>,
    errors: &mut Vec<SemCheckError>,
) {
    let mut visitor = ClosureEscapeVisitor::new(state, capture_map, errors);
    visitor.visit_expr(expr);
}

fn check_return_expr(
    expr: &Expr,
    state: &ClosureBindings,
    capture_map: &HashMap<DefId, CaptureMap>,
    errors: &mut Vec<SemCheckError>,
) {
    match &expr.kind {
        ExprKind::Block { tail, .. } => {
            if let Some(tail) = tail {
                check_return_expr(tail, state, capture_map, errors);
            }
        }
        ExprKind::If {
            then_body,
            else_body,
            ..
        } => {
            check_return_expr(then_body, state, capture_map, errors);
            check_return_expr(else_body, state, capture_map, errors);
        }
        ExprKind::Match { arms, .. } => {
            for arm in arms {
                check_return_expr(&arm.body, state, capture_map, errors);
            }
        }
        _ => {
            if is_captured_closure_value(expr, state, capture_map) {
                push_error(errors, expr.span, SEK::ClosureEscapeReturn);
            }
        }
    }
}

struct ClosureEscapeVisitor<'a> {
    state: &'a ClosureBindings,
    capture_map: &'a HashMap<DefId, CaptureMap>,
    errors: &'a mut Vec<SemCheckError>,
}

impl<'a> ClosureEscapeVisitor<'a> {
    fn new(
        state: &'a ClosureBindings,
        capture_map: &'a HashMap<DefId, CaptureMap>,
        errors: &'a mut Vec<SemCheckError>,
    ) -> Self {
        Self {
            state,
            capture_map,
            errors,
        }
    }

    fn check_store_value(&mut self, expr: &Expr) {
        if is_captured_closure_value(expr, self.state, self.capture_map) {
            self.errors
                .push(SEK::ClosureEscapeStore.at(expr.span));
        }
    }

    fn check_arg_value(&mut self, expr: &Expr) {
        if is_captured_closure_value(expr, self.state, self.capture_map) {
            self.errors
                .push(SEK::ClosureEscapeArg.at(expr.span));
        }
    }
}

impl Visitor<DefId, TypeId> for ClosureEscapeVisitor<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Call { callee, args } => {
                self.visit_expr(callee);
                for arg in args {
                    self.check_arg_value(&arg.expr);
                    self.visit_expr(&arg.expr);
                }
                return;
            }
            ExprKind::MethodCall { callee, args, .. } => {
                self.visit_expr(callee);
                for arg in args {
                    self.check_arg_value(&arg.expr);
                    self.visit_expr(&arg.expr);
                }
                return;
            }
            ExprKind::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.check_store_value(elem);
                        self.visit_expr(elem);
                    }
                }
                ArrayLitInit::Repeat(elem, _) => {
                    self.check_store_value(elem);
                    self.visit_expr(elem);
                }
            },
            ExprKind::TupleLit(fields) => {
                for field in fields {
                    self.check_store_value(field);
                    self.visit_expr(field);
                }
            }
            ExprKind::StructLit { fields, .. } => {
                for field in fields {
                    self.check_store_value(&field.value);
                    self.visit_expr(&field.value);
                }
            }
            ExprKind::StructUpdate { fields, .. } => {
                for field in fields {
                    self.check_store_value(&field.value);
                    self.visit_expr(&field.value);
                }
            }
            ExprKind::EnumVariant { payload, .. } => {
                for elem in payload {
                    self.check_store_value(elem);
                    self.visit_expr(elem);
                }
            }
            ExprKind::Closure { .. } => {
                // Skip nested closure bodies.
                return;
            }
            _ => {}
        }

        walk_expr(self, expr);
    }
}

fn is_captured_closure_value(
    expr: &Expr,
    state: &ClosureBindings,
    capture_map: &HashMap<DefId, CaptureMap>,
) -> bool {
    match &expr.kind {
        ExprKind::Closure { def_id, .. } => capture_map.contains_key(def_id),
        ExprKind::Var { def_id, .. } => state
            .get(def_id)
            .map(|captures| !captures.is_empty())
            .unwrap_or(false),
        ExprKind::Move { expr }
        | ExprKind::ImplicitMove { expr }
        | ExprKind::Coerce { expr, .. } => is_captured_closure_value(expr, state, capture_map),
        _ => false,
    }
}

fn def_name(ctx: &NormalizedContext, def_id: DefId) -> String {
    ctx.def_table
        .lookup_def(def_id)
        .map(|def| def.name.clone())
        .unwrap_or_else(|| format!("def#{}", def_id.0))
}
