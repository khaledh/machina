use crate::core::ast::EmitKind;
use crate::core::ast::model::FuncDef;
use crate::core::ast::model::{
    ArrayLitInit, BlockItem, CallArgMode, Expr, ExprKind as VEK, StmtExpr, StmtExprKind as SEK,
    StringFmtSegment,
};
use crate::core::context::{
    AnalyzedContext, ResolvedTables, SemanticContext, SemanticPayload, TypedTables,
};
use crate::core::resolve::{DefId, DefTable};
use crate::core::typecheck::type_map::TypeMap;

/// NRVO (Named Return Value Optimization) analyzer.
///
/// This analyzer checks if the returned value of a function is eligible for NRVO.
/// If it is, it marks the definition as eligible for NRVO.
pub struct NrvoAnalyzer {
    ctx: SemanticContext,
}

impl NrvoAnalyzer {
    pub fn new(ctx: SemanticContext) -> Self {
        Self { ctx }
    }

    pub fn analyze(self) -> AnalyzedContext {
        let SemanticContext { module, payload } = self.ctx;
        let SemanticPayload {
            typed,
            lowering_plans,
            drop_plans,
        } = payload;
        let TypedTables {
            resolved,
            type_map,
            opaque_bindings,
            exposed_types,
            call_sigs,
            generic_insts,
            for_plans,
        } = typed;
        let ResolvedTables {
            def_table,
            module_path,
            def_owners,
            symbol_ids,
            symbols,
            node_id_gen,
            linear_index,
        } = resolved;

        let mut def_table = def_table;

        for func_def in module.func_defs() {
            Self::analyze_func_def(&mut def_table, &type_map, func_def);
        }

        AnalyzedContext {
            module,
            payload: SemanticPayload {
                typed: TypedTables {
                    resolved: ResolvedTables {
                        def_table,
                        module_path,
                        def_owners,
                        symbol_ids,
                        symbols,
                        node_id_gen,
                        linear_index,
                    },
                    type_map,
                    opaque_bindings,
                    exposed_types,
                    call_sigs,
                    generic_insts,
                    for_plans,
                },
                lowering_plans,
                drop_plans,
            },
        }
    }

    fn analyze_func_def(def_table: &mut DefTable, type_map: &TypeMap, func_def: &FuncDef) {
        // Step 1: Check if function return type is compound
        let ret_ty = type_map
            .lookup_node_type(func_def.id)
            .unwrap_or_else(|| panic!("Function {} not found in type_map", func_def.sig.name));

        if !ret_ty.is_compound() {
            return;
        }

        // Step 2: Find the returned variable def
        let ret_var_def_id = Self::find_ret_var_def_id(def_table, &func_def.body);

        // Step 3: Check if the returned variable is only used as lvalue
        if let Some(var_def_id) = ret_var_def_id
            && Self::is_nrvo_safe(def_table, &func_def.body, var_def_id)
        {
            def_table.mark_nrvo_eligible(var_def_id);
        }
    }

    fn find_ret_var_def_id(def_table: &DefTable, expr: &Expr) -> Option<DefId> {
        match &expr.kind {
            VEK::Load { expr } | VEK::Move { expr } | VEK::ImplicitMove { expr } => {
                Self::expr_var_def_id(def_table, expr)
            }

            VEK::Block { tail, .. } => tail
                .as_deref()
                .and_then(|e| Self::find_ret_var_def_id(def_table, e)),

            VEK::Match { arms, .. } => {
                let mut arm_def_id = None;
                for arm in arms {
                    let this_id = Self::find_ret_var_def_id(def_table, &arm.body);
                    match (arm_def_id, this_id) {
                        (None, Some(id)) => arm_def_id = Some(id),
                        (Some(id), Some(this_id)) if id == this_id => {}
                        _ => return None, // mismatch or missing -> not NRVO safe
                    }
                }
                arm_def_id
            }
            _ => None,
        }
    }

    fn is_nrvo_safe(def_table: &DefTable, body: &Expr, var_def_id: DefId) -> bool {
        let checker = NrvoSafetyChecker::new(def_table, var_def_id);
        checker.check_expr(body, true) // true = at return position
    }

    fn expr_var_def_id(def_table: &DefTable, expr: &Expr) -> Option<DefId> {
        match &expr.kind {
            VEK::Var { .. } => def_table.lookup_node_def_id(expr.id),
            _ => None,
        }
    }
}

struct NrvoSafetyChecker<'a> {
    def_table: &'a DefTable,
    var_def_id: DefId,
}

impl<'a> NrvoSafetyChecker<'a> {
    pub fn new(def_table: &'a DefTable, var_def_id: DefId) -> Self {
        Self {
            def_table,
            var_def_id,
        }
    }

    fn node_def_id(&self, expr: &Expr) -> Option<DefId> {
        self.def_table.lookup_node_def_id(expr.id)
    }

    fn check_stmt_expr(&self, stmt_expr: &StmtExpr) -> bool {
        match &stmt_expr.kind {
            SEK::LetBind { value, .. } | SEK::VarBind { value, .. } => {
                self.check_expr(value, false)
            }
            SEK::VarDecl { .. } => true,

            // Assignment is ok if it's at return position
            SEK::Assign {
                assignee, value, ..
            } => {
                // If this is a write to our variable (lvalue use), that's OK
                if self.is_lvalue_use(assignee) {
                    // Just check RHS doesn't use our variable as rvalue
                    self.check_expr(value, false)
                } else {
                    // Assignment to something else - check both sides don't use our var
                    self.check_place_value(assignee, false) && self.check_expr(value, false)
                }
            }
            SEK::CompoundAssign {
                assignee, value, ..
            } => {
                // Compound assignment reads and writes the assignee, so the var
                // is used as rvalue — not safe unless it's a different variable.
                self.check_place_value(assignee, false) && self.check_expr(value, false)
            }

            SEK::While { cond, body } => {
                let cond_ok = self.check_expr(cond, false);
                let body_ok = self.check_expr(body, false);
                cond_ok && body_ok
            }

            SEK::For { iter, body, .. } => {
                let iter_ok = self.check_expr(iter, false);
                let body_ok = self.check_expr(body, false);
                iter_ok && body_ok
            }
            SEK::Break | SEK::Continue => true,
            SEK::Return { value } => value
                .as_deref()
                .is_none_or(|expr| self.check_expr(expr, true)),
            SEK::Defer { .. } | SEK::Using { .. } => {
                unreachable!("syntax desugar must remove defer/using before nrvo analysis");
            }
        }
    }

    fn check_expr(&self, expr: &Expr, at_return: bool) -> bool {
        match &expr.kind {
            VEK::Unsafe { body } => self.check_expr(body, at_return),
            VEK::Load { expr } | VEK::Move { expr } | VEK::ImplicitMove { expr } => {
                self.check_place_value(expr, at_return)
            }
            VEK::AddrOf { expr } => self.check_place_value(expr, false),

            // Block expression: check all expressions, with last one in return context
            VEK::Block { items, tail } => {
                let items_ok = items.iter().all(|item| match item {
                    BlockItem::Stmt(stmt) => self.check_stmt_expr(stmt),
                    BlockItem::Expr(expr) => self.check_expr(expr, false),
                });

                let tail_ok = tail
                    .as_deref()
                    .is_none_or(|expr| self.check_expr(expr, at_return));

                items_ok && tail_ok
            }

            VEK::If {
                cond,
                then_body,
                else_body,
            } => {
                let cond_ok = self.check_expr(cond, false);
                let then_ok = self.check_expr(then_body, at_return);
                let else_ok = self.check_expr(else_body, at_return);
                cond_ok && then_ok && else_ok
            }

            VEK::Match { scrutinee, arms } => {
                let scrutinee_ok = self.check_expr(scrutinee, false);
                let arms_ok = arms.iter().all(|arm| self.check_expr(&arm.body, at_return));
                scrutinee_ok && arms_ok
            }

            VEK::Call { callee, args } => {
                let callee_ok = self.check_expr(callee, false);
                let args_ok = args.iter().all(|arg| self.check_call_arg(arg));
                callee_ok && args_ok
            }
            VEK::MethodCall { callee, args, .. } => {
                let callee_ok = self.check_expr(callee, false);
                let args_ok = args.iter().all(|arg| self.check_call_arg(arg));
                callee_ok && args_ok
            }
            VEK::Emit { kind } => match kind {
                EmitKind::Send { to, payload } | EmitKind::Request { to, payload, .. } => {
                    self.check_expr(to, false) && self.check_expr(payload, false)
                }
            },
            VEK::Reply { cap, value } => {
                self.check_expr(cap, false) && self.check_expr(value, false)
            }

            VEK::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => elems.iter().all(|e| self.check_expr(e, false)),
                ArrayLitInit::Repeat(expr, _) => self.check_expr(expr, false),
            },
            VEK::SetLit { elems, .. } => elems.iter().all(|e| self.check_expr(e, false)),
            VEK::MapLit { entries, .. } => entries.iter().all(|entry| {
                self.check_expr(&entry.key, false) && self.check_expr(&entry.value, false)
            }),

            VEK::Slice { target, start, end } => {
                let target_ok = self.check_place_lvalue(target);
                let start_ok = start
                    .as_deref()
                    .is_none_or(|expr| self.check_expr(expr, false));
                let end_ok = end
                    .as_deref()
                    .is_none_or(|expr| self.check_expr(expr, false));
                target_ok && start_ok && end_ok
            }
            VEK::MapGet { target, key } => {
                self.check_expr(target, false) && self.check_expr(key, false)
            }

            VEK::Len { expr } => self.check_place_lvalue(expr),

            VEK::TupleLit(fields) => fields.iter().all(|e| self.check_expr(e, false)),

            VEK::StructLit { fields, .. } => fields
                .iter()
                .all(|field| self.check_expr(&field.value, false)),

            VEK::StructUpdate { target, fields } => {
                let target_ok = self.check_expr(target, false);
                let fields_ok = fields
                    .iter()
                    .all(|field| self.check_expr(&field.value, false));
                target_ok && fields_ok
            }

            VEK::StringFmt { segments } => segments.iter().all(|segment| match segment {
                StringFmtSegment::Literal { .. } => true,
                StringFmtSegment::Expr { expr, .. } => self.check_expr(expr, false),
            }),

            VEK::HeapAlloc { expr } => self.check_expr(expr, false),

            VEK::Coerce { expr, .. } => self.check_expr(expr, false),

            VEK::UnaryOp { expr, .. } => self.check_expr(expr, false),
            VEK::Try {
                fallible_expr,
                on_error,
            } => {
                self.check_expr(fallible_expr, false)
                    && on_error
                        .as_deref()
                        .is_none_or(|handler| self.check_expr(handler, false))
            }

            VEK::BinOp { left, right, .. } => {
                let left_ok = self.check_expr(left, false);
                let right_ok = self.check_expr(right, false);
                left_ok && right_ok
            }

            VEK::UnitLit
            | VEK::NoneLit
            | VEK::IntLit(_)
            | VEK::BoolLit(_)
            | VEK::CharLit(_)
            | VEK::StringLit { .. }
            | VEK::EnumVariant { .. }
            | VEK::Range { .. }
            | VEK::ClosureRef { .. }
            | VEK::Var { .. }
            | VEK::ArrayIndex { .. }
            | VEK::TupleField { .. }
            | VEK::StructField { .. }
            | VEK::Deref { .. }
            | VEK::Closure { .. }
            | VEK::RoleProjection { .. } => true,
        }
    }

    fn is_lvalue_use(&self, expr: &Expr) -> bool {
        match &expr.kind {
            VEK::Var { .. } => self.node_def_id(expr) == Some(self.var_def_id),
            VEK::ArrayIndex { target, .. } => self.is_lvalue_use(target),
            VEK::TupleField { target, .. } => self.is_lvalue_use(target),
            VEK::StructField { target, .. } => self.is_lvalue_use(target),
            VEK::Deref { .. } => false,
            _ => false,
        }
    }

    fn check_place_lvalue(&self, place: &Expr) -> bool {
        if self.is_lvalue_use(place) {
            return true;
        }

        match &place.kind {
            VEK::ArrayIndex { target, indices } => {
                self.check_place_lvalue(target)
                    && indices.iter().all(|index| self.check_expr(index, false))
            }
            VEK::TupleField { target, .. } | VEK::StructField { target, .. } => {
                self.check_place_lvalue(target)
            }
            VEK::Deref { expr } => self.check_expr(expr, false),
            VEK::Var { .. } => true,
            _ => true,
        }
    }

    fn check_place_value(&self, place: &Expr, at_return: bool) -> bool {
        if let VEK::Var { .. } = &place.kind
            && self.node_def_id(place) == Some(self.var_def_id)
        {
            return at_return;
        }

        if self.is_lvalue_use(place) {
            return false;
        }

        self.check_place_lvalue(place)
    }

    fn check_call_arg(&self, arg: &crate::core::ast::model::CallArg) -> bool {
        match arg.mode {
            CallArgMode::Default | CallArgMode::Move => self.check_expr(&arg.expr, false),
            CallArgMode::InOut | CallArgMode::Out => self.check_place_lvalue(&arg.expr),
        }
    }
}
