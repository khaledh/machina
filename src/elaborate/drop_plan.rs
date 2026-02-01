//! Build drop plans for SSA lowering.
//!
//! The drop plan records scope-based drops and control-flow drop depths so
//! SSA lowering can emit drops without re-deriving semantic intent.

use crate::resolve::{DefId, DefTable};
use crate::tree::semantic as sem;
use crate::tree::semantic::{DropGuard, DropItem, DropPlanMap, DropScopePlan};
use crate::typeck::type_map::TypeMap;

pub fn build_drop_plans(
    module: &sem::Module,
    def_table: &DefTable,
    type_map: &TypeMap,
) -> DropPlanMap {
    let mut builder = DropPlanBuilder::new(def_table, type_map);

    for func_def in module.func_defs() {
        builder.visit_func_def(func_def);
    }

    for method_block in module.method_blocks() {
        for method_item in &method_block.method_items {
            let sem::MethodItem::Def(method_def) = method_item else {
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

    fn exit_scope(&mut self, id: crate::tree::NodeId) {
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

    fn register_pattern_drop(&mut self, pattern: &sem::BindPattern, guard: DropGuard) {
        match &pattern.kind {
            sem::BindPatternKind::Name { def_id, .. } => self.register_def_drop(*def_id, guard),
            sem::BindPatternKind::Array { patterns } => {
                for pat in patterns {
                    self.register_pattern_drop(pat, guard);
                }
            }
            sem::BindPatternKind::Tuple { fields } => {
                for field in fields {
                    self.register_pattern_drop(&field.pattern, guard);
                }
            }
            sem::BindPatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.register_pattern_drop(&field.pattern, guard);
                }
            }
        }
    }

    // --- Tree walk ---

    fn visit_func_def(&mut self, func_def: &sem::FuncDef) {
        // Function-level scope for params and locals.
        self.enter_scope();

        for param in &func_def.sig.params {
            if param.mode != crate::tree::ParamMode::Sink {
                continue;
            }
            // Sink params can be moved; guard drops on the liveness flag.
            self.register_def_drop(param.def_id, DropGuard::IfInitialized);
        }

        self.visit_value_expr(&func_def.body);

        self.exit_scope(func_def.id);
    }

    fn visit_method_def(&mut self, method_def: &sem::MethodDef) {
        // Method bodies get the same drop planning as functions.
        self.enter_scope();

        let self_param = &method_def.sig.self_param;
        if self_param.mode == crate::tree::ParamMode::Sink {
            // Sink receiver can be moved; guard drops on the liveness flag.
            self.register_def_drop(self_param.def_id, DropGuard::IfInitialized);
        }

        for param in &method_def.sig.params {
            if param.mode != crate::tree::ParamMode::Sink {
                continue;
            }
            // Sink params can be moved; guard drops on the liveness flag.
            self.register_def_drop(param.def_id, DropGuard::IfInitialized);
        }

        self.visit_value_expr(&method_def.body);

        self.exit_scope(method_def.id);
    }

    fn visit_block(
        &mut self,
        expr: &sem::ValueExpr,
        items: &[sem::BlockItem],
        tail: Option<&sem::ValueExpr>,
    ) {
        // Each block introduces a new drop scope.
        self.enter_scope();

        for item in items {
            match item {
                sem::BlockItem::Stmt(stmt) => self.visit_stmt_expr(stmt),
                sem::BlockItem::Expr(expr) => self.visit_value_expr(expr),
            }
        }

        if let Some(tail) = tail {
            self.visit_value_expr(tail);
        }

        self.exit_scope(expr.id);
    }

    fn visit_stmt_expr(&mut self, stmt: &sem::StmtExpr) {
        match &stmt.kind {
            sem::StmtExprKind::LetBind { pattern, value, .. }
            | sem::StmtExprKind::VarBind { pattern, value, .. } => {
                self.visit_value_expr(value);
                // Use drop flags for move-sensitive bindings so moved-out values
                // don't get dropped at scope exit.
                self.register_pattern_drop(pattern, DropGuard::IfInitialized);
            }
            sem::StmtExprKind::VarDecl { def_id, .. } => {
                self.register_def_drop(*def_id, DropGuard::IfInitialized);
            }
            sem::StmtExprKind::Assign {
                assignee, value, ..
            } => {
                self.visit_place_expr(assignee);
                self.visit_value_expr(value);
            }
            sem::StmtExprKind::While { cond, body } => {
                self.visit_value_expr(cond);

                // Record the drop depth for breaks/continues inside this loop.
                let loop_depth = self.scope_stack.len();
                self.loop_stack.push(loop_depth);
                self.visit_value_expr(body);
                self.loop_stack.pop();
            }
            sem::StmtExprKind::For {
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
            sem::StmtExprKind::Break | sem::StmtExprKind::Continue => {
                if let Some(depth) = self.loop_stack.last().copied() {
                    // Lowering drops all scopes deeper than this depth.
                    self.plans.insert_depth(stmt.id, depth);
                }
            }
            sem::StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.visit_value_expr(value);
                }
                // Return exits all scopes in the function.
                self.plans.insert_depth(stmt.id, 0);
            }
        }
    }

    fn visit_place_expr(&mut self, place: &sem::PlaceExpr) {
        match &place.kind {
            sem::PlaceExprKind::Var { .. } => {}
            sem::PlaceExprKind::Deref { value } => self.visit_value_expr(value),
            sem::PlaceExprKind::ArrayIndex { target, indices } => {
                self.visit_place_expr(target);
                for index in indices {
                    self.visit_value_expr(index);
                }
            }
            sem::PlaceExprKind::StructField { target, .. }
            | sem::PlaceExprKind::TupleField { target, .. } => {
                self.visit_place_expr(target);
            }
        }
    }

    fn visit_value_expr(&mut self, expr: &sem::ValueExpr) {
        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                self.visit_block(expr, items, tail.as_deref());
            }
            sem::ValueExprKind::UnitLit
            | sem::ValueExprKind::IntLit(_)
            | sem::ValueExprKind::BoolLit(_)
            | sem::ValueExprKind::CharLit(_)
            | sem::ValueExprKind::StringLit { .. }
            | sem::ValueExprKind::Range { .. }
            | sem::ValueExprKind::ClosureRef { .. } => {}
            sem::ValueExprKind::StringFmt { plan } => {
                for segment in &plan.segments {
                    match segment {
                        sem::SegmentKind::Int { expr, .. }
                        | sem::SegmentKind::StringValue { expr } => {
                            self.visit_value_expr(expr);
                        }
                        sem::SegmentKind::LiteralBytes(_) => {}
                    }
                }
            }
            sem::ValueExprKind::ArrayLit { init, .. } => match init {
                sem::ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.visit_value_expr(elem);
                    }
                }
                sem::ArrayLitInit::Repeat(expr, _) => {
                    self.visit_value_expr(expr);
                }
            },
            sem::ValueExprKind::TupleLit(items) => {
                for item in items {
                    self.visit_value_expr(item);
                }
            }
            sem::ValueExprKind::StructLit { fields, .. } => {
                for field in fields {
                    self.visit_value_expr(&field.value);
                }
            }
            sem::ValueExprKind::EnumVariant { payload, .. } => {
                for expr in payload {
                    self.visit_value_expr(expr);
                }
            }
            sem::ValueExprKind::StructUpdate { target, fields } => {
                self.visit_value_expr(target);
                for field in fields {
                    self.visit_value_expr(&field.value);
                }
            }
            sem::ValueExprKind::BinOp { left, right, .. } => {
                self.visit_value_expr(left);
                self.visit_value_expr(right);
            }
            sem::ValueExprKind::UnaryOp { expr, .. } => self.visit_value_expr(expr),
            sem::ValueExprKind::HeapAlloc { expr } => self.visit_value_expr(expr),
            sem::ValueExprKind::Move { place } | sem::ValueExprKind::ImplicitMove { place } => {
                self.visit_place_expr(place)
            }
            sem::ValueExprKind::Coerce { expr, .. } => self.visit_value_expr(expr),
            sem::ValueExprKind::AddrOf { place } | sem::ValueExprKind::Load { place } => {
                self.visit_place_expr(place)
            }
            sem::ValueExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.visit_value_expr(cond);
                self.visit_value_expr(then_body);
                self.visit_value_expr(else_body);
            }
            sem::ValueExprKind::Slice { target, start, end } => {
                self.visit_place_expr(target);
                if let Some(start) = start {
                    self.visit_value_expr(start);
                }
                if let Some(end) = end {
                    self.visit_value_expr(end);
                }
            }
            sem::ValueExprKind::Len { place } => self.visit_place_expr(place),
            sem::ValueExprKind::Match { scrutinee, arms } => {
                self.visit_value_expr(scrutinee);
                for arm in arms {
                    self.visit_value_expr(&arm.body);
                }
            }
            sem::ValueExprKind::Call { callee, args } => {
                self.visit_value_expr(callee);
                for arg in args {
                    match arg {
                        sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                            self.visit_value_expr(expr);
                        }
                        sem::CallArg::InOut { place, .. } | sem::CallArg::Out { place, .. } => {
                            self.visit_place_expr(place);
                        }
                    }
                }
            }
            sem::ValueExprKind::MethodCall { receiver, args, .. } => {
                match receiver {
                    sem::MethodReceiver::ValueExpr(expr) => self.visit_value_expr(expr),
                    sem::MethodReceiver::PlaceExpr(place) => self.visit_place_expr(place),
                }
                for arg in args {
                    match arg {
                        sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                            self.visit_value_expr(expr);
                        }
                        sem::CallArg::InOut { place, .. } | sem::CallArg::Out { place, .. } => {
                            self.visit_place_expr(place);
                        }
                    }
                }
            }
        }
    }
}
