//! Lowering plan construction for SSA.
//!
//! Builds a plan map for all value expressions, classifying them as linear
//! (single-block) or branching (multi-block) to avoid try/fail lowering.

use std::collections::HashMap;

use crate::tree::semantic as sem;
use crate::tree::{BinaryOp, NodeId};
use crate::typecheck::type_map::TypeMap;

pub fn build_lowering_plans(module: &sem::Module, type_map: &TypeMap) -> sem::LoweringPlanMap {
    let mut builder = LoweringPlanBuilder {
        type_map,
        plans: HashMap::new(),
    };

    builder.visit_module(module);
    builder.plans
}

struct LoweringPlanBuilder<'a> {
    type_map: &'a TypeMap,
    plans: sem::LoweringPlanMap,
}

impl<'a> LoweringPlanBuilder<'a> {
    fn visit_module(&mut self, module: &sem::Module) {
        for item in &module.top_level_items {
            match item {
                sem::TopLevelItem::FuncDef(def) => self.visit_value_expr(&def.body),
                sem::TopLevelItem::MethodBlock(block) => {
                    for method_item in &block.method_items {
                        if let sem::MethodItem::Def(def) = method_item {
                            self.visit_value_expr(&def.body);
                        }
                    }
                }
                sem::TopLevelItem::TypeDef(_)
                | sem::TopLevelItem::TraitDef(_)
                | sem::TopLevelItem::FuncDecl(_) => {}
            }
        }
    }

    fn visit_value_expr(&mut self, expr: &sem::ValueExpr) {
        let plan = if self.is_linear_value_expr(expr) {
            sem::LoweringPlan::Linear
        } else {
            sem::LoweringPlan::Branching
        };
        self.plans.insert(expr.id, plan);

        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                for item in items {
                    self.visit_block_item(item);
                }
                if let Some(tail) = tail {
                    self.visit_value_expr(tail);
                }
            }

            sem::ValueExprKind::ArrayLit { init, .. } => match init {
                sem::ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.visit_value_expr(elem);
                    }
                }
                sem::ArrayLitInit::Repeat(expr, _) => self.visit_value_expr(expr),
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
                for value in payload {
                    self.visit_value_expr(value);
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
            sem::ValueExprKind::UnaryOp { expr, .. }
            | sem::ValueExprKind::HeapAlloc { expr }
            | sem::ValueExprKind::Coerce { expr, .. } => {
                self.visit_value_expr(expr);
            }

            sem::ValueExprKind::Move { place }
            | sem::ValueExprKind::ImplicitMove { place }
            | sem::ValueExprKind::AddrOf { place }
            | sem::ValueExprKind::Load { place }
            | sem::ValueExprKind::Len { place } => {
                self.visit_place_expr(place);
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

            sem::ValueExprKind::Match { scrutinee, arms } => {
                self.visit_value_expr(scrutinee);
                for arm in arms {
                    self.visit_value_expr(&arm.body);
                }
            }

            sem::ValueExprKind::Call { callee, args } => {
                self.visit_value_expr(callee);
                for arg in args {
                    self.visit_call_arg(arg);
                }
            }
            sem::ValueExprKind::MethodCall { receiver, args, .. } => {
                self.visit_method_receiver(receiver);
                for arg in args {
                    self.visit_call_arg(arg);
                }
            }

            sem::ValueExprKind::StringFmt { plan } => self.visit_string_fmt_plan(plan),

            sem::ValueExprKind::UnitLit
            | sem::ValueExprKind::IntLit(_)
            | sem::ValueExprKind::BoolLit(_)
            | sem::ValueExprKind::CharLit(_)
            | sem::ValueExprKind::StringLit { .. }
            | sem::ValueExprKind::Range { .. }
            | sem::ValueExprKind::ClosureRef { .. } => {}
        }
    }

    fn visit_block_item(&mut self, item: &sem::BlockItem) {
        match item {
            sem::BlockItem::Stmt(stmt) => self.visit_stmt_expr(stmt),
            sem::BlockItem::Expr(expr) => self.visit_value_expr(expr),
        }
    }

    fn visit_stmt_expr(&mut self, stmt: &sem::StmtExpr) {
        match &stmt.kind {
            sem::StmtExprKind::LetBind { value, .. } | sem::StmtExprKind::VarBind { value, .. } => {
                self.visit_value_expr(value)
            }
            sem::StmtExprKind::Assign {
                assignee, value, ..
            } => {
                self.visit_place_expr(assignee);
                self.visit_value_expr(value);
            }
            sem::StmtExprKind::While { cond, body } => {
                self.visit_value_expr(cond);
                self.visit_value_expr(body);
            }
            sem::StmtExprKind::For { iter, body, .. } => {
                self.visit_value_expr(iter);
                self.visit_value_expr(body);
            }
            sem::StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.visit_value_expr(value);
                }
            }
            sem::StmtExprKind::VarDecl { .. }
            | sem::StmtExprKind::Break
            | sem::StmtExprKind::Continue => {}
        }
    }

    fn visit_place_expr(&mut self, place: &sem::PlaceExpr) {
        match &place.kind {
            sem::PlaceExprKind::Deref { value } => self.visit_value_expr(value),
            sem::PlaceExprKind::ArrayIndex { target, indices } => {
                self.visit_place_expr(target);
                for index in indices {
                    self.visit_value_expr(index);
                }
            }
            sem::PlaceExprKind::TupleField { target, .. }
            | sem::PlaceExprKind::StructField { target, .. } => self.visit_place_expr(target),
            sem::PlaceExprKind::Var { .. } => {}
        }
    }

    fn visit_call_arg(&mut self, arg: &sem::CallArg) {
        match arg {
            sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                self.visit_value_expr(expr);
            }
            sem::CallArg::InOut { place, .. } | sem::CallArg::Out { place, .. } => {
                self.visit_place_expr(place);
            }
        }
    }

    fn visit_method_receiver(&mut self, receiver: &sem::MethodReceiver) {
        match receiver {
            sem::MethodReceiver::ValueExpr(expr) => self.visit_value_expr(expr),
            sem::MethodReceiver::PlaceExpr(place) => self.visit_place_expr(place),
        }
    }

    fn visit_string_fmt_plan(&mut self, plan: &sem::StringFmtPlan) {
        for segment in &plan.segments {
            match segment {
                sem::SegmentKind::LiteralBytes(_) => {}
                sem::SegmentKind::Int { expr, .. } | sem::SegmentKind::StringValue { expr } => {
                    self.visit_value_expr(expr)
                }
            }
        }
    }

    fn is_linear_value_expr(&self, expr: &sem::ValueExpr) -> bool {
        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                items.iter().all(|item| self.is_linear_block_item(item))
                    && tail
                        .as_deref()
                        .map(|tail| self.is_linear_value_expr(tail))
                        .unwrap_or(true)
            }

            sem::ValueExprKind::UnitLit
            | sem::ValueExprKind::IntLit(_)
            | sem::ValueExprKind::BoolLit(_)
            | sem::ValueExprKind::CharLit(_)
            | sem::ValueExprKind::StringLit { .. }
            | sem::ValueExprKind::Range { .. } => true,

            sem::ValueExprKind::ArrayLit { init, .. } => match init {
                sem::ArrayLitInit::Elems(elems) => {
                    elems.iter().all(|elem| self.is_linear_value_expr(elem))
                }
                sem::ArrayLitInit::Repeat(expr, _) => self.is_linear_value_expr(expr),
            },

            sem::ValueExprKind::TupleLit(items) => {
                items.iter().all(|item| self.is_linear_value_expr(item))
            }

            sem::ValueExprKind::StructLit { fields, .. } => fields
                .iter()
                .all(|field| self.is_linear_value_expr(&field.value)),

            sem::ValueExprKind::StructUpdate { target, fields } => {
                self.is_linear_value_expr(target)
                    && fields
                        .iter()
                        .all(|field| self.is_linear_value_expr(&field.value))
            }

            sem::ValueExprKind::EnumVariant { payload, .. } => {
                payload.iter().all(|value| self.is_linear_value_expr(value))
            }

            sem::ValueExprKind::UnaryOp { expr, .. } | sem::ValueExprKind::HeapAlloc { expr } => {
                self.is_linear_value_expr(expr)
            }

            sem::ValueExprKind::BinOp { left, op, right } => match op {
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => false,
                _ => self.is_linear_value_expr(left) && self.is_linear_value_expr(right),
            },

            sem::ValueExprKind::Load { place } => self.is_linear_place_expr(place),

            sem::ValueExprKind::Move { place } | sem::ValueExprKind::ImplicitMove { place } => {
                self.is_linear_place_expr(place)
            }

            sem::ValueExprKind::Coerce { expr, .. } => self.is_linear_value_expr(expr),

            sem::ValueExprKind::AddrOf { place } => self.is_linear_place_expr(place),

            sem::ValueExprKind::Len { place } => self.is_linear_place_expr(place),

            sem::ValueExprKind::Slice { target, start, end } => {
                self.is_linear_place_expr(target)
                    && start
                        .as_deref()
                        .map(|expr| self.is_linear_value_expr(expr))
                        .unwrap_or(true)
                    && end
                        .as_deref()
                        .map(|expr| self.is_linear_value_expr(expr))
                        .unwrap_or(true)
            }

            sem::ValueExprKind::StringFmt { plan } => {
                plan.segments.iter().all(|segment| match segment {
                    sem::SegmentKind::LiteralBytes(_) => true,
                    sem::SegmentKind::Int { expr, .. } | sem::SegmentKind::StringValue { expr } => {
                        self.is_linear_value_expr(expr)
                    }
                })
            }

            sem::ValueExprKind::Call { callee, args } => {
                self.is_linear_value_expr(callee)
                    && args.iter().all(|arg| self.is_linear_call_arg(arg))
                    && self.is_linear_call_plan(expr.id, false)
            }

            sem::ValueExprKind::MethodCall { receiver, args, .. } => {
                self.is_linear_method_receiver(receiver)
                    && args.iter().all(|arg| self.is_linear_call_arg(arg))
                    && self.is_linear_call_plan(expr.id, true)
            }

            sem::ValueExprKind::ClosureRef { .. } => true,

            _ => false,
        }
    }

    fn is_linear_call_arg(&self, arg: &sem::CallArg) -> bool {
        match arg {
            sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                self.is_linear_value_expr(expr)
            }
            sem::CallArg::InOut { .. } | sem::CallArg::Out { .. } => false,
        }
    }

    fn is_linear_method_receiver(&self, receiver: &sem::MethodReceiver) -> bool {
        match receiver {
            sem::MethodReceiver::ValueExpr(expr) => self.is_linear_value_expr(expr),
            sem::MethodReceiver::PlaceExpr(_) => false,
        }
    }

    fn is_linear_place_expr(&self, place: &sem::PlaceExpr) -> bool {
        match &place.kind {
            sem::PlaceExprKind::Var { .. } => true,
            sem::PlaceExprKind::Deref { value } => self.is_linear_value_expr(value),
            sem::PlaceExprKind::TupleField { target, .. }
            | sem::PlaceExprKind::StructField { target, .. } => self.is_linear_place_expr(target),
            sem::PlaceExprKind::ArrayIndex { target, indices } => {
                self.is_linear_place_expr(target)
                    && indices.iter().all(|index| self.is_linear_value_expr(index))
            }
        }
    }

    fn is_linear_block_item(&self, item: &sem::BlockItem) -> bool {
        match item {
            sem::BlockItem::Stmt(stmt) => Self::is_linear_stmt(stmt),
            sem::BlockItem::Expr(expr) => self.is_linear_value_expr(expr),
        }
    }

    fn is_linear_stmt(stmt: &sem::StmtExpr) -> bool {
        match &stmt.kind {
            sem::StmtExprKind::LetBind { .. }
            | sem::StmtExprKind::VarBind { .. }
            | sem::StmtExprKind::VarDecl { .. }
            | sem::StmtExprKind::Assign { .. }
            | sem::StmtExprKind::Return { .. } => true,
            sem::StmtExprKind::While { .. }
            | sem::StmtExprKind::For { .. }
            | sem::StmtExprKind::Break
            | sem::StmtExprKind::Continue => false,
        }
    }

    fn is_linear_call_plan(&self, call_id: NodeId, has_receiver: bool) -> bool {
        let Some(plan) = self.type_map.lookup_call_plan(call_id) else {
            return false;
        };

        if plan.has_receiver != has_receiver {
            return false;
        }

        plan.args.iter().all(|arg| {
            matches!(
                arg,
                sem::ArgLowering::Direct(_) | sem::ArgLowering::PtrLen { .. }
            )
        })
    }
}
