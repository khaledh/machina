//! Lowering plan construction for SSA.
//!
//! Builds a plan map for all value expressions, classifying them as linear
//! (single-block) or branching (multi-block) to avoid try/fail lowering.

use std::collections::HashMap;

use crate::core::ast::{
    ArrayLitInit, BinaryOp, BlockItem, EmitKind, Expr, ExprKind, MethodItem, Module, NodeId,
    StmtExpr, StmtExprKind, StringFmtSegment, TopLevelItem,
};
use crate::core::plans::{
    ArgLowering, CallPlanMap, IndexPlanMap, LoweringPlan, LoweringPlanMap, MatchPlanMap,
    SegmentKind, SlicePlanMap, StringFmtPlan,
};

pub fn build_lowering_plans(
    module: &Module,
    call_plans: &CallPlanMap,
    index_plans: &IndexPlanMap,
    match_plans: &MatchPlanMap,
    slice_plans: &SlicePlanMap,
    try_cleanup_plans: &HashMap<NodeId, Vec<Expr>>,
    string_fmt_plans: &HashMap<NodeId, StringFmtPlan>,
) -> LoweringPlanMap {
    let mut builder = LoweringPlanBuilder {
        call_plans,
        string_fmt_plans,
        plans: HashMap::new(),
    };

    builder.visit_module(module);
    LoweringPlanMap {
        value_plans: builder.plans,
        call_plans: call_plans.clone(),
        index_plans: index_plans.clone(),
        match_plans: match_plans.clone(),
        slice_plans: slice_plans.clone(),
        try_cleanup_plans: try_cleanup_plans.clone(),
        string_fmt_plans: string_fmt_plans.clone(),
    }
}

struct LoweringPlanBuilder<'a> {
    call_plans: &'a CallPlanMap,
    string_fmt_plans: &'a HashMap<NodeId, StringFmtPlan>,
    plans: HashMap<NodeId, LoweringPlan>,
}

impl<'a> LoweringPlanBuilder<'a> {
    fn visit_module(&mut self, module: &Module) {
        for item in &module.top_level_items {
            match item {
                TopLevelItem::FuncDef(def) => self.visit_value_expr(&def.body),
                TopLevelItem::MethodBlock(block) => {
                    for method_item in &block.method_items {
                        if let MethodItem::Def(def) = method_item {
                            self.visit_value_expr(&def.body);
                        }
                    }
                }
                TopLevelItem::TypeDef(_)
                | TopLevelItem::TraitDef(_)
                | TopLevelItem::FuncDecl(_)
                | TopLevelItem::TypestateDef(_)
                | TopLevelItem::MachineDef(_)
                | TopLevelItem::ClosureDef(_) => {}
            }
        }
    }

    fn visit_value_expr(&mut self, expr: &Expr) {
        let plan = if self.is_linear_value_expr(expr) {
            LoweringPlan::Linear
        } else {
            LoweringPlan::Branching
        };
        self.plans.insert(expr.id, plan);

        match &expr.kind {
            ExprKind::Block { items, tail } => {
                for item in items {
                    self.visit_block_item(item);
                }
                if let Some(tail) = tail {
                    self.visit_value_expr(tail);
                }
            }

            ExprKind::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.visit_value_expr(elem);
                    }
                }
                ArrayLitInit::Repeat(expr, _) => self.visit_value_expr(expr),
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
                for value in payload {
                    self.visit_value_expr(value);
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
            ExprKind::Try {
                fallible_expr,
                on_error,
            } => {
                self.visit_value_expr(fallible_expr);
                if let Some(handler) = on_error {
                    self.visit_value_expr(handler);
                }
            }
            ExprKind::UnaryOp { expr, .. }
            | ExprKind::HeapAlloc { expr }
            | ExprKind::Coerce { expr, .. } => {
                self.visit_value_expr(expr);
            }

            ExprKind::Move { expr }
            | ExprKind::ImplicitMove { expr }
            | ExprKind::AddrOf { expr }
            | ExprKind::Load { expr }
            | ExprKind::Len { expr } => {
                self.visit_place_expr(expr);
            }

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
                self.visit_value_expr(target);
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
                EmitKind::Send { to, payload } | EmitKind::Request { to, payload, .. } => {
                    self.visit_value_expr(to);
                    self.visit_value_expr(payload);
                }
            },
            ExprKind::Reply { cap, value } => {
                self.visit_value_expr(cap);
                self.visit_value_expr(value);
            }

            ExprKind::StringFmt { segments } => {
                for segment in segments {
                    match segment {
                        StringFmtSegment::Literal { .. } => {}
                        StringFmtSegment::Expr { expr, .. } => self.visit_value_expr(expr),
                    }
                }
                // Also visit sub-expressions stored in the string_fmt_plan side table,
                // since the fstring lowerer lowers those plan expressions directly.
                if let Some(plan) = self.string_fmt_plans.get(&expr.id) {
                    self.visit_string_fmt_plan(plan);
                }
            }

            ExprKind::UnitLit
            | ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::Range { .. }
            | ExprKind::ClosureRef { .. }
            | ExprKind::RoleProjection { .. } => {}

            // Place-only variants that won't appear at value position
            ExprKind::Var { .. }
            | ExprKind::ArrayIndex { .. }
            | ExprKind::TupleField { .. }
            | ExprKind::StructField { .. }
            | ExprKind::Deref { .. }
            | ExprKind::Closure { .. } => {}
        }
    }

    fn visit_block_item(&mut self, item: &BlockItem) {
        match item {
            BlockItem::Stmt(stmt) => self.visit_stmt_expr(stmt),
            BlockItem::Expr(expr) => self.visit_value_expr(expr),
        }
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                self.visit_value_expr(value)
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                self.visit_place_expr(assignee);
                self.visit_value_expr(value);
            }
            StmtExprKind::While { cond, body } => {
                self.visit_value_expr(cond);
                self.visit_value_expr(body);
            }
            StmtExprKind::For { iter, body, .. } => {
                self.visit_value_expr(iter);
                self.visit_value_expr(body);
            }
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.visit_value_expr(value);
                }
            }
            StmtExprKind::Defer { .. } | StmtExprKind::Using { .. } => {
                unreachable!("syntax desugar must remove defer/using before lowering planning");
            }
            StmtExprKind::VarDecl { .. }
            | StmtExprKind::Break
            | StmtExprKind::Continue
            | StmtExprKind::CompoundAssign { .. } => {}
        }
    }

    fn visit_place_expr(&mut self, place: &Expr) {
        match &place.kind {
            ExprKind::Deref { expr } => self.visit_value_expr(expr),
            ExprKind::ArrayIndex { target, indices } => {
                self.visit_place_expr(target);
                for index in indices {
                    self.visit_value_expr(index);
                }
            }
            ExprKind::TupleField { target, .. } | ExprKind::StructField { target, .. } => {
                self.visit_place_expr(target)
            }
            ExprKind::Var { .. } => {}
            _ => {}
        }
    }

    fn visit_string_fmt_plan(&mut self, plan: &StringFmtPlan) {
        for segment in &plan.segments {
            match segment {
                SegmentKind::LiteralBytes(_) => {}
                SegmentKind::Bool { expr }
                | SegmentKind::Int { expr, .. }
                | SegmentKind::StringValue { expr } => self.visit_value_expr(expr),
            }
        }
    }

    fn is_linear_value_expr(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                items.iter().all(|item| self.is_linear_block_item(item))
                    && tail
                        .as_deref()
                        .map(|tail| self.is_linear_value_expr(tail))
                        .unwrap_or(true)
            }

            ExprKind::UnitLit
            | ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::Range { .. } => true,

            ExprKind::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => {
                    elems.iter().all(|elem| self.is_linear_value_expr(elem))
                }
                ArrayLitInit::Repeat(expr, _) => self.is_linear_value_expr(expr),
            },
            ExprKind::MapLit { entries, .. } => entries.iter().all(|entry| {
                self.is_linear_value_expr(&entry.key) && self.is_linear_value_expr(&entry.value)
            }),

            ExprKind::TupleLit(items) => items.iter().all(|item| self.is_linear_value_expr(item)),

            ExprKind::StructLit { fields, .. } => fields
                .iter()
                .all(|field| self.is_linear_value_expr(&field.value)),

            ExprKind::StructUpdate { target, fields } => {
                self.is_linear_value_expr(target)
                    && fields
                        .iter()
                        .all(|field| self.is_linear_value_expr(&field.value))
            }

            ExprKind::EnumVariant { payload, .. } => {
                payload.iter().all(|value| self.is_linear_value_expr(value))
            }

            ExprKind::UnaryOp { expr, .. } => self.is_linear_value_expr(expr),
            ExprKind::Try { .. } => false,
            ExprKind::HeapAlloc { expr } => self.is_linear_value_expr(expr),

            ExprKind::BinOp { left, op, right } => match op {
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => false,
                _ => self.is_linear_value_expr(left) && self.is_linear_value_expr(right),
            },

            ExprKind::Load { expr } => self.is_linear_place_expr(expr),

            ExprKind::Move { expr } | ExprKind::ImplicitMove { expr } => {
                self.is_linear_place_expr(expr)
            }

            ExprKind::Coerce { expr, .. } => self.is_linear_value_expr(expr),

            ExprKind::AddrOf { expr } => self.is_linear_place_expr(expr),

            ExprKind::Len { expr } => self.is_linear_place_expr(expr),

            ExprKind::Slice { target, start, end } => {
                self.is_linear_value_expr(target)
                    && start
                        .as_deref()
                        .map(|expr| self.is_linear_value_expr(expr))
                        .unwrap_or(true)
                    && end
                        .as_deref()
                        .map(|expr| self.is_linear_value_expr(expr))
                        .unwrap_or(true)
            }
            ExprKind::MapGet { target, key } => {
                self.is_linear_value_expr(target) && self.is_linear_value_expr(key)
            }

            ExprKind::StringFmt { segments } => {
                let ast_linear = segments.iter().all(|segment| match segment {
                    StringFmtSegment::Literal { .. } => true,
                    StringFmtSegment::Expr { expr, .. } => self.is_linear_value_expr(expr),
                });
                let plan_linear = self
                    .string_fmt_plans
                    .get(&expr.id)
                    .map(|plan| {
                        plan.segments.iter().all(|segment| match segment {
                            SegmentKind::LiteralBytes(_) => true,
                            SegmentKind::Bool { expr }
                            | SegmentKind::Int { expr, .. }
                            | SegmentKind::StringValue { expr } => self.is_linear_value_expr(expr),
                        })
                    })
                    .unwrap_or(true);
                ast_linear && plan_linear
            }

            ExprKind::Call { callee, args } => {
                self.is_linear_value_expr(callee)
                    && args.iter().all(|arg| self.is_linear_value_expr(&arg.expr))
                    && self.is_linear_call_plan(expr.id, false)
            }

            ExprKind::MethodCall { callee, args, .. } => {
                self.is_linear_value_expr(callee)
                    && args.iter().all(|arg| self.is_linear_value_expr(&arg.expr))
                    && self.is_linear_call_plan(expr.id, true)
            }
            ExprKind::Emit { kind } => match kind {
                EmitKind::Send { to, payload } | EmitKind::Request { to, payload, .. } => {
                    self.is_linear_value_expr(to) && self.is_linear_value_expr(payload)
                }
            },
            ExprKind::Reply { cap, value } => {
                self.is_linear_value_expr(cap) && self.is_linear_value_expr(value)
            }

            ExprKind::ClosureRef { .. } => true,

            _ => false,
        }
    }

    fn is_linear_place_expr(&self, place: &Expr) -> bool {
        match &place.kind {
            ExprKind::Var { .. } => true,
            ExprKind::Deref { expr } => self.is_linear_value_expr(expr),
            ExprKind::TupleField { target, .. } | ExprKind::StructField { target, .. } => {
                self.is_linear_place_expr(target)
            }
            ExprKind::ArrayIndex { target, indices } => {
                self.is_linear_place_expr(target)
                    && indices.iter().all(|index| self.is_linear_value_expr(index))
            }
            _ => false,
        }
    }

    fn is_linear_block_item(&self, item: &BlockItem) -> bool {
        match item {
            BlockItem::Stmt(stmt) => Self::is_linear_stmt(stmt),
            BlockItem::Expr(expr) => self.is_linear_value_expr(expr),
        }
    }

    fn is_linear_stmt(stmt: &StmtExpr) -> bool {
        match &stmt.kind {
            StmtExprKind::LetBind { .. }
            | StmtExprKind::VarBind { .. }
            | StmtExprKind::VarDecl { .. }
            | StmtExprKind::Assign { .. }
            | StmtExprKind::Return { .. } => true,
            StmtExprKind::While { .. }
            | StmtExprKind::For { .. }
            | StmtExprKind::Break
            | StmtExprKind::Continue
            | StmtExprKind::CompoundAssign { .. } => false,
            StmtExprKind::Defer { .. } | StmtExprKind::Using { .. } => {
                unreachable!("syntax desugar must remove defer/using before linearity checks");
            }
        }
    }

    fn is_linear_call_plan(&self, call_id: NodeId, has_receiver: bool) -> bool {
        let Some(plan) = self.call_plans.get(&call_id) else {
            return false;
        };

        if plan.has_receiver != has_receiver {
            return false;
        }

        plan.args
            .iter()
            .all(|arg| matches!(arg, ArgLowering::Direct(_) | ArgLowering::PtrLen { .. }))
    }
}
