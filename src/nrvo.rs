use crate::ast::{BlockItem, Expr, ExprKind, Function, StmtExpr, StmtExprKind};
use crate::context::{AnalyzedContext, TypeCheckedContext};
use crate::resolve::def_map::DefId;
use crate::resolve::def_map::DefMap;
use crate::typeck::type_map::TypeMap;

/// NRVO (Named Return Value Optimization) analyzer.
///
/// This analyzer checks if the returned value of a function is eligible for NRVO.
/// If it is, it marks the definition as eligible for NRVO.
pub struct NrvoAnalyzer {
    ctx: TypeCheckedContext,
}

impl NrvoAnalyzer {
    pub fn new(ctx: TypeCheckedContext) -> Self {
        Self { ctx }
    }

    pub fn analyze(self) -> AnalyzedContext {
        let TypeCheckedContext {
            module,
            def_map,
            type_map,
            symbols,
        } = self.ctx;

        let mut def_map = def_map;

        for func in module.funcs() {
            Self::analyze_function(&mut def_map, &type_map, func);
        }

        AnalyzedContext {
            module,
            def_map,
            type_map,
            symbols,
        }
    }

    fn analyze_function(def_map: &mut DefMap, type_map: &TypeMap, func: &Function) {
        // Step 1: Check if function return type is compound
        let ret_ty = type_map
            .lookup_node_type(func.id)
            .unwrap_or_else(|| panic!("Function {} not found in type_map", func.name));

        if !ret_ty.is_compound() {
            return;
        }

        // Step 2: Find the returned variable def
        let ret_var_def_id = Self::find_ret_var_def_id(def_map, &func.body);

        // Step 3: Check if the returned variable is only used as lvalue
        if let Some(var_def_id) = ret_var_def_id
            && Self::is_nrvo_safe(def_map, &func.body, var_def_id)
        {
            def_map.mark_nrvo_eligible(var_def_id);
        }
    }

    fn find_ret_var_def_id(def_map: &DefMap, expr: &Expr) -> Option<DefId> {
        match &expr.kind {
            ExprKind::Var(_) => def_map.lookup_def(expr.id).map(|def| def.id),

            ExprKind::Block { tail, .. } => tail
                .as_deref()
                .and_then(|expr| Self::find_ret_var_def_id(def_map, expr)),

            ExprKind::Match { arms, .. } => {
                let mut arm_def_id = None;
                for arm in arms {
                    let this_id = Self::find_ret_var_def_id(def_map, &arm.body);
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

    fn is_nrvo_safe(def_map: &DefMap, body: &Expr, var_def_id: DefId) -> bool {
        let checker = NrvoSafetyChecker::new(var_def_id, def_map);
        checker.check_expr(body, true) // true = at return position
    }
}

struct NrvoSafetyChecker<'a> {
    var_def_id: DefId,
    def_map: &'a DefMap,
}

impl<'a> NrvoSafetyChecker<'a> {
    pub fn new(var_def_id: DefId, def_map: &'a DefMap) -> Self {
        Self {
            var_def_id,
            def_map,
        }
    }

    fn check_stmt_expr(&self, stmt_expr: &StmtExpr) -> bool {
        match &stmt_expr.kind {
            StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                self.check_expr(value, false)
            }

            // Assignment is ok if it's at return position
            StmtExprKind::Assign { assignee, value } => {
                // If this is a write to our variable (lvalue use), that's OK
                if self.is_lvalue_use(assignee) {
                    // Just check RHS doesn't use our variable as rvalue
                    self.check_expr(value, false)
                } else {
                    // Assignment to something else - check both sides don't use our var
                    self.check_expr(assignee, false) && self.check_expr(value, false)
                }
            }

            StmtExprKind::While { cond, body } => {
                let cond_ok = self.check_expr(cond, false);
                let body_ok = self.check_expr(body, false);
                cond_ok && body_ok
            }

            StmtExprKind::For { iter, body, .. } => {
                let iter_ok = self.check_expr(iter, false);
                let body_ok = self.check_expr(body, false);
                iter_ok && body_ok
            }
        }
    }

    fn check_expr(&self, expr: &Expr, at_return: bool) -> bool {
        match &expr.kind {
            ExprKind::Var(_) => {
                if let Some(def) = self.def_map.lookup_def(expr.id)
                    && def.id == self.var_def_id
                {
                    at_return
                } else {
                    // Different variable - always OK
                    true
                }
            }

            // Block expression: check all expressions, with last one in return context
            ExprKind::Block { items, tail } => {
                let items_ok = items.iter().all(|item| match item {
                    BlockItem::Stmt(stmt) => self.check_stmt_expr(stmt),
                    BlockItem::Expr(expr) => self.check_expr(expr, false),
                });

                let tail_ok = tail
                    .as_deref()
                    .is_none_or(|expr| self.check_expr(expr, at_return));

                items_ok && tail_ok
            }

            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let cond_ok = self.check_expr(cond, false);
                let then_ok = self.check_expr(then_body, at_return);
                let else_ok = self.check_expr(else_body, at_return);
                cond_ok && then_ok && else_ok
            }

            ExprKind::Match { scrutinee, arms } => {
                let scrutinee_ok = self.check_expr(scrutinee, false);
                let arms_ok = arms.iter().all(|arm| self.check_expr(&arm.body, at_return));
                scrutinee_ok && arms_ok
            }

            ExprKind::Call { callee, args } => {
                let callee_ok = self.check_expr(callee, false);
                let args_ok = args.iter().all(|arg| match &arg.kind {
                    ExprKind::Var(_) if self.is_target_var(arg) => true,
                    _ => self.check_expr(arg, false),
                });
                callee_ok && args_ok
            }

            ExprKind::ArrayLit(elems) => elems.iter().all(|e| self.check_expr(e, false)),

            ExprKind::ArrayIndex { target, indices } => {
                let target_ok = match &target.kind {
                    ExprKind::Var(_) if self.is_target_var(target) => true,
                    _ => self.check_expr(target, false),
                };
                let index_ok = indices.iter().all(|index| self.check_expr(index, false));
                target_ok && index_ok
            }

            ExprKind::TupleLit(fields) => fields.iter().all(|e| self.check_expr(e, false)),

            ExprKind::TupleField { target, .. } => match &target.kind {
                ExprKind::Var(_) if self.is_target_var(target) => true,
                _ => self.check_expr(target, false),
            },

            ExprKind::StructLit { fields, .. } => fields
                .iter()
                .all(|field| self.check_expr(&field.value, false)),

            ExprKind::StructField { target, .. } => match &target.kind {
                ExprKind::Var(_) if self.is_target_var(target) => true,
                _ => self.check_expr(target, false),
            },

            ExprKind::StructUpdate { target, fields } => {
                let target_ok = match &target.kind {
                    ExprKind::Var(_) if self.is_target_var(target) => true,
                    _ => self.check_expr(target, false),
                };
                let fields_ok = fields
                    .iter()
                    .all(|field| self.check_expr(&field.value, false));
                target_ok && fields_ok
            }

            ExprKind::UnaryOp { expr, .. } => self.check_expr(expr, false),

            ExprKind::BinOp { left, right, .. } => {
                let left_ok = self.check_expr(left, false);
                let right_ok = self.check_expr(right, false);
                left_ok && right_ok
            }

            ExprKind::UnitLit
            | ExprKind::UInt64Lit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::EnumVariant { .. }
            | ExprKind::Range { .. } => true,
        }
    }

    fn is_target_var(&self, expr: &Expr) -> bool {
        match self.def_map.lookup_def(expr.id) {
            Some(def) => def.id == self.var_def_id,
            None => false,
        }
    }

    fn is_lvalue_use(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Var(_) => {
                // Check if this Var refers to our target definition
                if let Some(def) = self.def_map.lookup_def(expr.id) {
                    def.id == self.var_def_id
                } else {
                    false
                }
            }
            ExprKind::ArrayIndex { target, .. } => self.is_lvalue_use(target),
            ExprKind::TupleField { target, .. } => self.is_lvalue_use(target),
            _ => false,
        }
    }
}
