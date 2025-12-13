use crate::analysis::DefMap;
use crate::ast::{Expr, ExprKind, Function};
use crate::context::{AnalyzedContext, TypeCheckedContext};
use crate::ids::DefId;

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
        } = self.ctx;

        let mut def_map = def_map;

        for func in &module.funcs {
            Self::analyze_function(&mut def_map, func);
        }

        AnalyzedContext {
            module,
            def_map,
            type_map,
        }
    }

    fn analyze_function(def_map: &mut DefMap, func: &Function) {
        // Step 1: Check if function return type is compound
        if !func.return_type.is_compound() {
            return;
        }

        // Step 2: Find the returned variable def
        let returned_var_def_id = Self::find_returned_var_def_id(def_map, &func.body);

        // Step 3: Check if the returned variable is only used as lvalue
        if let Some(var_def_id) = returned_var_def_id
            && Self::is_nrvo_safe(def_map, &func.body, var_def_id)
        {
            def_map.mark_nrvo_eligible(var_def_id);
        }
    }

    fn find_returned_var_def_id(def_map: &DefMap, expr: &Expr) -> Option<DefId> {
        match &expr.kind {
            ExprKind::VarRef(_) => def_map.lookup_def(expr.id).map(|def| def.id),
            ExprKind::Block(exprs) => exprs
                .last()
                .and_then(|e| Self::find_returned_var_def_id(def_map, e)),
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

    fn check_expr(&self, expr: &Expr, at_return: bool) -> bool {
        match &expr.kind {
            ExprKind::VarRef(_) => {
                if let Some(def) = self.def_map.lookup_def(expr.id)
                    && def.id == self.var_def_id
                {
                    at_return
                } else {
                    // Different variable - always OK
                    true
                }
            }

            // Assignment is ok if it's at return position
            ExprKind::Assign { assignee, value } => {
                // If this is a write to our variable (lvalue use), that's OK
                if self.is_lvalue_use(assignee) {
                    // Just check RHS doesn't use our variable as rvalue
                    self.check_expr(value, false)
                } else {
                    // Assignment to something else - check both sides don't use our var
                    self.check_expr(assignee, false) && self.check_expr(value, false)
                }
            }

            // Block expression: check all expressions, with last one in return context
            ExprKind::Block(exprs) => {
                if exprs.is_empty() {
                    return true;
                }

                // Check all expressions except last one
                let earlier_ok = exprs[..exprs.len() - 1]
                    .iter()
                    .all(|e| self.check_expr(e, false));
                let last_ok = exprs.last().is_none_or(|e| self.check_expr(e, at_return));
                earlier_ok && last_ok
            }

            ExprKind::Let { value, .. } | ExprKind::Var { value, .. } => {
                self.check_expr(value, false)
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

            ExprKind::While { cond, body } => {
                let cond_ok = self.check_expr(cond, false);
                let body_ok = self.check_expr(body, false);
                cond_ok && body_ok
            }

            ExprKind::Call { callee, args } => {
                let callee_ok = self.check_expr(callee, false);
                let args_ok = args.iter().all(|arg| match &arg.kind {
                    ExprKind::VarRef(_) if self.is_target_var(arg) => true,
                    _ => self.check_expr(arg, false),
                });
                callee_ok && args_ok
            }

            ExprKind::ArrayLit(elems) => elems.iter().all(|e| self.check_expr(e, false)),

            ExprKind::Index { target, indices } => {
                let target_ok = match &target.kind {
                    ExprKind::VarRef(_) if self.is_target_var(target) => true,
                    _ => self.check_expr(target, false),
                };
                let index_ok = indices.iter().all(|index| self.check_expr(index, false));
                target_ok && index_ok
            }

            ExprKind::TupleLit(fields) => fields.iter().all(|e| self.check_expr(e, false)),

            ExprKind::TupleFieldAccess { target, .. } => match &target.kind {
                ExprKind::VarRef(_) if self.is_target_var(target) => true,
                _ => self.check_expr(target, false),
            },

            ExprKind::UnaryOp { expr, .. } => self.check_expr(expr, false),

            ExprKind::BinOp { left, right, .. } => {
                let left_ok = self.check_expr(left, false);
                let right_ok = self.check_expr(right, false);
                left_ok && right_ok
            }

            ExprKind::UnitLit | ExprKind::UInt64Lit(_) | ExprKind::BoolLit(_) => true,
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
            ExprKind::VarRef(_) => {
                // Check if this VarRef refers to our target definition
                if let Some(def) = self.def_map.lookup_def(expr.id) {
                    def.id == self.var_def_id
                } else {
                    false
                }
            }
            ExprKind::Index { target, .. } => self.is_lvalue_use(target),
            _ => false,
        }
    }
}
