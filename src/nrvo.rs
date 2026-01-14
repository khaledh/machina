use crate::context::{AnalyzedContext, ElaboratedContext};
use crate::resolve::{DefId, DefTable};
use crate::tir::model::{
    ArrayLitInit, BlockItem, Expr, ExprKind, FuncDef, StmtExpr, StmtExprKind, StringFmtSegment,
};
use crate::typeck::type_map::TypeMap;

/// NRVO (Named Return Value Optimization) analyzer.
///
/// This analyzer checks if the returned value of a function is eligible for NRVO.
/// If it is, it marks the definition as eligible for NRVO.
pub struct NrvoAnalyzer {
    ctx: ElaboratedContext,
}

impl NrvoAnalyzer {
    pub fn new(ctx: ElaboratedContext) -> Self {
        Self { ctx }
    }

    pub fn analyze(self) -> AnalyzedContext {
        let ElaboratedContext {
            module,
            def_table,
            type_map,
            symbols,
            node_id_gen,
        } = self.ctx;

        let mut def_table = def_table;

        for func_def in module.func_defs() {
            Self::analyze_func_def(&mut def_table, &type_map, func_def);
        }

        AnalyzedContext {
            module,
            def_table,
            type_map,
            symbols,
            node_id_gen,
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
        let ret_var_def_id = Self::find_ret_var_def_id(&func_def.body);

        // Step 3: Check if the returned variable is only used as lvalue
        if let Some(var_def_id) = ret_var_def_id
            && Self::is_nrvo_safe(&func_def.body, var_def_id)
        {
            def_table.mark_nrvo_eligible(var_def_id);
        }
    }

    fn find_ret_var_def_id(expr: &Expr) -> Option<DefId> {
        match &expr.kind {
            ExprKind::Var { def_id, .. } => Some(*def_id),

            ExprKind::Block { tail, .. } => tail
                .as_deref()
                .and_then(|expr| Self::find_ret_var_def_id(expr)),

            ExprKind::Match { arms, .. } => {
                let mut arm_def_id = None;
                for arm in arms {
                    let this_id = Self::find_ret_var_def_id(&arm.body);
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

    fn is_nrvo_safe(body: &Expr, var_def_id: DefId) -> bool {
        let checker = NrvoSafetyChecker::new(var_def_id);
        checker.check_expr(body, true) // true = at return position
    }
}

struct NrvoSafetyChecker {
    var_def_id: DefId,
}

impl NrvoSafetyChecker {
    pub fn new(var_def_id: DefId) -> Self {
        Self { var_def_id }
    }

    fn check_stmt_expr(&self, stmt_expr: &StmtExpr) -> bool {
        match &stmt_expr.kind {
            StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                self.check_expr(value, false)
            }
            StmtExprKind::VarDecl { .. } => true,

            // Assignment is ok if it's at return position
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
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
            ExprKind::Var { def_id, .. } => {
                if *def_id == self.var_def_id {
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
                let args_ok = args.iter().all(|arg| match &arg.expr.kind {
                    ExprKind::Var { def_id, .. } if *def_id == self.var_def_id => true,
                    _ => self.check_expr(&arg.expr, false),
                });
                callee_ok && args_ok
            }
            ExprKind::MethodCall { callee, args, .. } => {
                let target_ok = self.check_expr(callee, false);
                let args_ok = args.iter().all(|arg| match &arg.expr.kind {
                    ExprKind::Var { def_id, .. } if *def_id == self.var_def_id => true,
                    _ => self.check_expr(&arg.expr, false),
                });
                target_ok && args_ok
            }

            ExprKind::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => elems.iter().all(|e| self.check_expr(e, false)),
                ArrayLitInit::Repeat(expr, _) => self.check_expr(expr, false),
            },

            ExprKind::ArrayIndex { target, indices } => {
                let target_ok = match &target.kind {
                    ExprKind::Var { def_id, .. } if *def_id == self.var_def_id => true,
                    _ => self.check_expr(target, false),
                };
                let index_ok = indices.iter().all(|index| self.check_expr(index, false));
                target_ok && index_ok
            }

            ExprKind::Slice { target, start, end } => {
                let target_ok = match &target.kind {
                    ExprKind::Var { def_id, .. } if *def_id == self.var_def_id => true,
                    _ => self.check_expr(target, false),
                };
                let start_ok = start
                    .as_deref()
                    .is_none_or(|expr| self.check_expr(expr, false));
                let end_ok = end
                    .as_deref()
                    .is_none_or(|expr| self.check_expr(expr, false));
                target_ok && start_ok && end_ok
            }

            ExprKind::TupleLit(fields) => fields.iter().all(|e| self.check_expr(e, false)),

            ExprKind::TupleField { target, .. } => match &target.kind {
                ExprKind::Var { def_id, .. } if *def_id == self.var_def_id => true,
                _ => self.check_expr(target, false),
            },

            ExprKind::StructLit { fields, .. } => fields
                .iter()
                .all(|field| self.check_expr(&field.value, false)),

            ExprKind::StructField { target, .. } => match &target.kind {
                ExprKind::Var { def_id, .. } if *def_id == self.var_def_id => true,
                _ => self.check_expr(target, false),
            },

            ExprKind::StructUpdate { target, fields } => {
                let target_ok = match &target.kind {
                    ExprKind::Var { def_id, .. } if *def_id == self.var_def_id => true,
                    _ => self.check_expr(target, false),
                };
                let fields_ok = fields
                    .iter()
                    .all(|field| self.check_expr(&field.value, false));
                target_ok && fields_ok
            }

            ExprKind::StringFmt { segments } => segments.iter().all(|segment| match segment {
                StringFmtSegment::Literal { .. } => true,
                StringFmtSegment::Expr { expr, .. } => self.check_expr(expr, false),
            }),

            ExprKind::HeapAlloc { expr } => self.check_expr(expr, false),

            ExprKind::Move { expr } => self.check_expr(expr, false),

            ExprKind::Coerce { expr, .. } => self.check_expr(expr, false),

            ExprKind::ImplicitMove { expr } => self.check_expr(expr, false),

            ExprKind::UnaryOp { expr, .. } => self.check_expr(expr, false),

            ExprKind::BinOp { left, right, .. } => {
                let left_ok = self.check_expr(left, false);
                let right_ok = self.check_expr(right, false);
                left_ok && right_ok
            }

            ExprKind::UnitLit
            | ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::EnumVariant { .. }
            | ExprKind::Range { .. }
            | ExprKind::Closure { .. } => true,
        }
    }

    fn is_lvalue_use(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Var { def_id, .. } => *def_id == self.var_def_id,
            ExprKind::ArrayIndex { target, .. } => self.is_lvalue_use(target),
            ExprKind::TupleField { target, .. } => self.is_lvalue_use(target),
            ExprKind::Coerce { expr, .. } => self.is_lvalue_use(expr),
            ExprKind::ImplicitMove { expr } => self.is_lvalue_use(expr),
            _ => false,
        }
    }
}
