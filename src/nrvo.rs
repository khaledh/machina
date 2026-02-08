use crate::context::{AnalyzedContext, SemanticContext};
use crate::resolve::{DefId, DefTable};
use crate::tree::semantic::{
    ArrayLitInit, BlockItem, CallArg, FuncDef, MethodReceiver, PlaceExpr, PlaceExprKind as PEK,
    SegmentKind, StmtExpr, StmtExprKind as SEK, ValueExpr, ValueExprKind as VEK,
};
use crate::typecheck::type_map::TypeMap;

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
        let SemanticContext {
            module,
            def_table,
            def_owners,
            type_map,
            lowering_plans,
            drop_plans,
            symbols,
            node_id_gen,
            generic_insts,
        } = self.ctx;

        let mut def_table = def_table;

        for func_def in module.func_defs() {
            Self::analyze_func_def(&mut def_table, &type_map, func_def);
        }

        AnalyzedContext {
            module,
            def_table,
            def_owners,
            type_map,
            lowering_plans,
            drop_plans,
            symbols,
            node_id_gen,
            generic_insts,
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

    fn find_ret_var_def_id(expr: &ValueExpr) -> Option<DefId> {
        match &expr.kind {
            VEK::Load { place } | VEK::Move { place } | VEK::ImplicitMove { place } => {
                Self::place_var_def_id(place)
            }

            VEK::Block { tail, .. } => tail.as_deref().and_then(Self::find_ret_var_def_id),

            VEK::Match { arms, .. } => {
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

    fn is_nrvo_safe(body: &ValueExpr, var_def_id: DefId) -> bool {
        let checker = NrvoSafetyChecker::new(var_def_id);
        checker.check_expr(body, true) // true = at return position
    }

    fn place_var_def_id(place: &PlaceExpr) -> Option<DefId> {
        match &place.kind {
            PEK::Var { def_id, .. } => Some(*def_id),
            _ => None,
        }
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
        }
    }

    fn check_expr(&self, expr: &ValueExpr, at_return: bool) -> bool {
        match &expr.kind {
            VEK::Load { place } | VEK::Move { place } | VEK::ImplicitMove { place } => {
                self.check_place_value(place, at_return)
            }
            VEK::AddrOf { place } => self.check_place_value(place, false),

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
            VEK::MethodCall { receiver, args, .. } => {
                let receiver_ok = match receiver {
                    MethodReceiver::ValueExpr(expr) => self.check_expr(expr, false),
                    MethodReceiver::PlaceExpr(place) => self.check_place_lvalue(place),
                };
                let args_ok = args.iter().all(|arg| self.check_call_arg(arg));
                receiver_ok && args_ok
            }

            VEK::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => elems.iter().all(|e| self.check_expr(e, false)),
                ArrayLitInit::Repeat(expr, _) => self.check_expr(expr, false),
            },
            VEK::SetLit { elems, .. } => elems.iter().all(|e| self.check_expr(e, false)),

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

            VEK::Len { place } => self.check_place_lvalue(place),

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

            VEK::StringFmt { plan } => plan.segments.iter().all(|segment| match segment {
                SegmentKind::LiteralBytes(_) => true,
                SegmentKind::Int { expr, .. } | SegmentKind::StringValue { expr } => {
                    self.check_expr(expr, false)
                }
            }),

            VEK::HeapAlloc { expr } => self.check_expr(expr, false),

            VEK::Coerce { expr, .. } => self.check_expr(expr, false),

            VEK::UnaryOp { expr, .. } => self.check_expr(expr, false),

            VEK::BinOp { left, right, .. } => {
                let left_ok = self.check_expr(left, false);
                let right_ok = self.check_expr(right, false);
                left_ok && right_ok
            }

            VEK::UnitLit
            | VEK::IntLit(_)
            | VEK::BoolLit(_)
            | VEK::CharLit(_)
            | VEK::StringLit { .. }
            | VEK::EnumVariant { .. }
            | VEK::Range { .. }
            | VEK::ClosureRef { .. } => true,
        }
    }

    fn is_lvalue_use(&self, expr: &PlaceExpr) -> bool {
        match &expr.kind {
            PEK::Var { def_id, .. } => *def_id == self.var_def_id,
            PEK::ArrayIndex { target, .. } => self.is_lvalue_use(target),
            PEK::TupleField { target, .. } => self.is_lvalue_use(target),
            PEK::StructField { target, .. } => self.is_lvalue_use(target),
            PEK::Deref { .. } => false,
        }
    }

    fn check_place_lvalue(&self, place: &PlaceExpr) -> bool {
        if self.is_lvalue_use(place) {
            return true;
        }

        match &place.kind {
            PEK::ArrayIndex { target, indices } => {
                self.check_place_lvalue(target)
                    && indices.iter().all(|index| self.check_expr(index, false))
            }
            PEK::TupleField { target, .. } | PEK::StructField { target, .. } => {
                self.check_place_lvalue(target)
            }
            PEK::Deref { value } => self.check_expr(value, false),
            PEK::Var { .. } => true,
        }
    }

    fn check_place_value(&self, place: &PlaceExpr, at_return: bool) -> bool {
        if let PEK::Var { def_id, .. } = &place.kind
            && *def_id == self.var_def_id
        {
            return at_return;
        }

        if self.is_lvalue_use(place) {
            return false;
        }

        self.check_place_lvalue(place)
    }

    fn check_call_arg(&self, arg: &CallArg) -> bool {
        match arg {
            CallArg::In { expr, .. } | CallArg::Sink { expr, .. } => self.check_expr(expr, false),
            CallArg::InOut { place, .. } | CallArg::Out { place, .. } => {
                self.check_place_lvalue(place)
            }
        }
    }
}
