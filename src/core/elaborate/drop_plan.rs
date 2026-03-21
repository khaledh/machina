//! Build drop plans for SSA lowering.
//!
//! The drop plan records scope-based drops and control-flow drop depths so
//! SSA lowering can emit drops without re-deriving semantic intent.

use crate::core::ast::{
    ArrayLitInit, BindPattern, BindPatternKind, BlockItem, EmitKind, Expr, ExprKind, FuncDef,
    MethodDef, MethodItem, Module, NodeId, ParamMode, StmtExpr, StmtExprKind,
};
use crate::core::plans::{DropGuard, DropItem, DropPlanMap, DropScopePlan};
use crate::core::resolve::{DefId, DefTable};
use crate::core::typecheck::type_map::TypeMap;

pub fn build_drop_plans(module: &Module, def_table: &DefTable, type_map: &TypeMap) -> DropPlanMap {
    let mut builder = DropPlanBuilder::new(def_table, type_map);

    for func_def in module.func_defs() {
        builder.visit_func_def(func_def);
    }

    for method_block in module.method_blocks() {
        for method_item in &method_block.method_items {
            let MethodItem::Def(method_def) = method_item else {
                continue;
            };
            builder.visit_method_def(method_def);
        }
    }

    builder.finish()
}

struct DropPlanBuilder<'a> {
    def_table: &'a DefTable,
    type_map: &'a TypeMap,
    plans: DropPlanMap,
    /// Active lexical scopes (function + nested blocks).
    scope_stack: Vec<DropScopePlan>,
    /// Stack of drop depths for active loops (for break/continue).
    loop_stack: Vec<usize>,
}

impl<'a> DropPlanBuilder<'a> {
    fn new(def_table: &'a DefTable, type_map: &'a TypeMap) -> Self {
        Self {
            def_table,
            type_map,
            plans: DropPlanMap::new(),
            scope_stack: Vec::new(),
            loop_stack: Vec::new(),
        }
    }

    fn finish(self) -> DropPlanMap {
        self.plans
    }

    // --- Scope management ---

    fn enter_scope(&mut self) {
        self.scope_stack.push(DropScopePlan::default());
    }

    fn exit_scope(&mut self, id: NodeId) {
        let scope = self
            .scope_stack
            .pop()
            .unwrap_or_else(|| panic!("drop plan scope stack underflow"));
        self.plans.insert_scope(id, scope);
    }

    fn current_scope_mut(&mut self) -> &mut DropScopePlan {
        self.scope_stack
            .last_mut()
            .unwrap_or_else(|| panic!("drop plan missing active scope"))
    }

    // --- Drop registration ---

    fn register_def_drop(&mut self, def_id: DefId, guard: DropGuard) {
        let Some(def) = self.def_table.lookup_def(def_id) else {
            panic!("drop plan missing def {:?}", def_id);
        };
        let Some(ty_id) = self.type_map.lookup_def_type_id(def) else {
            return;
        };
        let ty = self.type_map.type_table().get(ty_id).clone();
        if !ty.needs_drop() {
            return;
        }

        let scope = self.current_scope_mut();
        if scope.drops.iter().any(|item| item.def_id == def_id) {
            return;
        }

        scope.drops.push(DropItem {
            def_id,
            ty: ty_id,
            guard,
        });
    }

    fn register_pattern_drop(&mut self, pattern: &BindPattern, guard: DropGuard) {
        match &pattern.kind {
            BindPatternKind::Name { .. } => {
                let def_id = self.def_table.def_id(pattern.id);
                self.register_def_drop(def_id, guard);
            }
            BindPatternKind::Array { .. } | BindPatternKind::Tuple { .. } => pattern
                .kind
                .for_each_child_pattern(|pat| self.register_pattern_drop(pat, guard)),
            BindPatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.register_pattern_drop(&field.pattern, guard);
                }
            }
        }
    }

    // --- Tree walk ---

    fn visit_func_def(&mut self, func_def: &FuncDef) {
        // Function-level scope for params and locals.
        self.enter_scope();

        for param in &func_def.sig.params {
            if param.mode != ParamMode::Sink {
                continue;
            }
            // Sink params can be moved; guard drops on the liveness flag.
            self.register_def_drop(self.def_table.def_id(param.id), DropGuard::IfInitialized);
        }

        self.visit_value_expr(&func_def.body);

        self.exit_scope(func_def.id);
    }

    fn visit_method_def(&mut self, method_def: &MethodDef) {
        // Method bodies get the same drop planning as functions.
        self.enter_scope();

        let self_param = &method_def.sig.self_param;
        if self_param.mode == ParamMode::Sink {
            // Sink receiver can be moved; guard drops on the liveness flag.
            self.register_def_drop(
                self.def_table.def_id(self_param.id),
                DropGuard::IfInitialized,
            );
        }

        for param in &method_def.sig.params {
            if param.mode != ParamMode::Sink {
                continue;
            }
            // Sink params can be moved; guard drops on the liveness flag.
            self.register_def_drop(self.def_table.def_id(param.id), DropGuard::IfInitialized);
        }

        self.visit_value_expr(&method_def.body);

        self.exit_scope(method_def.id);
    }

    fn visit_block(&mut self, expr: &Expr, items: &[BlockItem], tail: Option<&Expr>) {
        // Each block introduces a new drop scope.
        self.enter_scope();

        for item in items {
            match item {
                BlockItem::Stmt(stmt) => self.visit_stmt_expr(stmt),
                BlockItem::Expr(expr) => self.visit_value_expr(expr),
            }
        }

        if let Some(tail) = tail {
            self.visit_value_expr(tail);
        }

        self.exit_scope(expr.id);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, value, .. }
            | StmtExprKind::VarBind { pattern, value, .. } => {
                self.visit_value_expr(value);
                // Use drop flags for move-sensitive bindings so moved-out values
                // don't get dropped at scope exit.
                self.register_pattern_drop(pattern, DropGuard::IfInitialized);
            }
            StmtExprKind::VarDecl { .. } => {
                let def_id = self.def_table.def_id(stmt.id);
                self.register_def_drop(def_id, DropGuard::IfInitialized);
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                self.visit_place_expr(assignee);
                self.visit_value_expr(value);
            }
            StmtExprKind::While { cond, body } => {
                self.visit_value_expr(cond);

                // Record the drop depth for breaks/continues inside this loop.
                let loop_depth = self.scope_stack.len();
                self.loop_stack.push(loop_depth);
                self.visit_value_expr(body);
                self.loop_stack.pop();
            }
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.visit_value_expr(iter);
                self.register_pattern_drop(pattern, DropGuard::Always);

                // Record the drop depth for breaks/continues inside this loop.
                let loop_depth = self.scope_stack.len();
                self.loop_stack.push(loop_depth);
                self.visit_value_expr(body);
                self.loop_stack.pop();
            }
            StmtExprKind::Break | StmtExprKind::Continue => {
                if let Some(depth) = self.loop_stack.last().copied() {
                    // Lowering drops all scopes deeper than this depth.
                    self.plans.insert_depth(stmt.id, depth);
                }
            }
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.visit_value_expr(value);
                }
                // Return exits all scopes in the function.
                self.plans.insert_depth(stmt.id, 0);
            }
            StmtExprKind::Defer { .. } | StmtExprKind::Using { .. } => {
                unreachable!("syntax desugar must remove defer/using before drop planning");
            }
            StmtExprKind::CompoundAssign { .. } => {
                unreachable!("normalize must desugar compound assignment before drop planning");
            }
        }
    }

    fn visit_place_expr(&mut self, place: &Expr) {
        match &place.kind {
            ExprKind::Var { .. } => {}
            ExprKind::Deref { expr } => self.visit_value_expr(expr),
            ExprKind::ArrayIndex { target, indices } => {
                self.visit_place_expr(target);
                for index in indices {
                    self.visit_value_expr(index);
                }
            }
            ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
                self.visit_place_expr(target);
            }
            _ => {}
        }
    }

    fn visit_value_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                self.visit_block(expr, items, tail.as_deref());
            }
            ExprKind::UnitLit
            | ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::Range { .. }
            | ExprKind::ClosureRef { .. } => {}
            ExprKind::StringFmt { .. } => {
                // StringFmt segments are handled via the plan side table; the
                // AST variant carries raw segments that don't need
                // drop-plan traversal.
            }
            ExprKind::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.visit_value_expr(elem);
                    }
                }
                ArrayLitInit::Repeat(expr, _) => {
                    self.visit_value_expr(expr);
                }
            },
            ExprKind::SetLit { elems, .. } => {
                for elem in elems {
                    self.visit_value_expr(elem);
                }
            }
            ExprKind::MapLit { entries, .. } => {
                for entry in entries {
                    self.visit_value_expr(&entry.key);
                    self.visit_value_expr(&entry.value);
                }
            }
            ExprKind::TupleLit(items) => {
                for item in items {
                    self.visit_value_expr(item);
                }
            }
            ExprKind::StructLit { fields, .. } => {
                for field in fields {
                    self.visit_value_expr(&field.value);
                }
            }
            ExprKind::EnumVariant { payload, .. } => {
                for expr in payload {
                    self.visit_value_expr(expr);
                }
            }
            ExprKind::StructUpdate { target, fields } => {
                self.visit_value_expr(target);
                for field in fields {
                    self.visit_value_expr(&field.value);
                }
            }
            ExprKind::BinOp { left, right, .. } => {
                self.visit_value_expr(left);
                self.visit_value_expr(right);
            }
            ExprKind::UnaryOp { expr, .. } => self.visit_value_expr(expr),
            ExprKind::Try {
                fallible_expr,
                on_error,
            } => {
                self.visit_value_expr(fallible_expr);
                if let Some(handler) = on_error {
                    self.visit_value_expr(handler);
                }
            }
            ExprKind::HeapAlloc { expr } => self.visit_value_expr(expr),
            ExprKind::Move { expr } | ExprKind::ImplicitMove { expr } => {
                self.visit_place_expr(expr)
            }
            ExprKind::Coerce { expr, .. } => self.visit_value_expr(expr),
            ExprKind::AddrOf { expr } | ExprKind::Load { expr } => self.visit_place_expr(expr),
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.visit_value_expr(cond);
                self.visit_value_expr(then_body);
                self.visit_value_expr(else_body);
            }
            ExprKind::Slice { target, start, end } => {
                self.visit_place_expr(target);
                if let Some(start) = start {
                    self.visit_value_expr(start);
                }
                if let Some(end) = end {
                    self.visit_value_expr(end);
                }
            }
            ExprKind::MapGet { target, key } => {
                self.visit_value_expr(target);
                self.visit_value_expr(key);
            }
            ExprKind::Len { expr } => self.visit_place_expr(expr),
            ExprKind::Match { scrutinee, arms } => {
                self.visit_value_expr(scrutinee);
                for arm in arms {
                    self.visit_value_expr(&arm.body);
                }
            }
            ExprKind::Call { callee, args } => {
                self.visit_value_expr(callee);
                for arg in args {
                    self.visit_value_expr(&arg.expr);
                }
            }
            ExprKind::MethodCall { callee, args, .. } => {
                self.visit_value_expr(callee);
                for arg in args {
                    self.visit_value_expr(&arg.expr);
                }
            }
            ExprKind::Emit { kind } => match kind {
                EmitKind::Send { to, payload } => {
                    self.visit_value_expr(to);
                    self.visit_value_expr(payload);
                }
                EmitKind::Request { to, payload, .. } => {
                    self.visit_value_expr(to);
                    self.visit_value_expr(payload);
                }
            },
            ExprKind::Reply { cap, value } => {
                self.visit_value_expr(cap);
                self.visit_value_expr(value);
            }
            ExprKind::Var { .. }
            | ExprKind::RoleProjection { .. }
            | ExprKind::Deref { .. }
            | ExprKind::ArrayIndex { .. }
            | ExprKind::TupleField { .. }
            | ExprKind::StructField { .. }
            | ExprKind::Closure { .. } => {
                // These are place expressions or closures that appear inline.
                // Place expressions at value position would have been wrapped in
                // Load/Move by elaboration. If they still appear raw, they don't
                // require drop-plan traversal beyond what their sub-expressions need.
            }
        }
    }
}
