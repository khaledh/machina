//! Main value expression elaboration.
//!
//! Transforms normalized expressions into semantic value expressions.
//! The elaboration handles several special cases in priority order:
//!
//! 1. **Lvalue expressions** (vars, field access, indexing, deref): Convert
//!    to place expressions and wrap in Load or Move based on semck results
//!
//! 2. **Closure expressions**: Captureless closures become `ClosureRef`s,
//!    while captured closures are lifted to struct types and struct literals
//!
//! 3. **Call expressions**: Build call plans and elaborate arguments
//!
//! 4. **Simple expressions**: Direct translation of literals, operators,
//!    control flow, etc.

use crate::elaborate::elaborator::Elaborator;
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::tree::{CoerceKind, ParamMode};
use crate::typeck::type_map::{CallParam, CallSig};

impl<'a> Elaborator<'a> {
    /// Main entry point for elaborating a value expression.
    ///
    /// Dispatches to specialized handlers based on expression kind,
    /// with lvalue and closure cases taking priority.
    pub(in crate::elaborate) fn elab_value(&mut self, expr: &norm::Expr) -> sem::ValueExpr {
        if let Some(value) = self.elab_lvalue_expr(expr) {
            return value;
        }

        if let Some(value) = self.elab_closure_expr(expr) {
            return value;
        }

        let kind = if let Some(kind) = self.elab_call_kind(expr) {
            kind
        } else {
            self.elab_simple_value_kind(expr)
        };

        sem::ValueExpr {
            id: expr.id,
            kind,
            ty: expr.ty,
            span: expr.span,
        }
    }

    /// Handle expressions that denote memory locations (lvalues).
    ///
    /// Wraps the place in either a Load (for copies) or ImplicitMove
    /// (when semck determined the value should be moved).
    fn elab_lvalue_expr(&mut self, expr: &norm::Expr) -> Option<sem::ValueExpr> {
        match &expr.kind {
            norm::ExprKind::Var { .. }
            | norm::ExprKind::ArrayIndex { .. }
            | norm::ExprKind::TupleField { .. }
            | norm::ExprKind::StructField { .. }
            | norm::ExprKind::Deref { .. } => {
                let place = self.elab_place(expr);
                let place_ty = place.ty;
                if self.implicit_moves.contains(&expr.id) {
                    return Some(self.new_value(
                        sem::ValueExprKind::ImplicitMove {
                            place: Box::new(place),
                        },
                        place_ty,
                        expr.span,
                    ));
                }
                Some(self.new_value(
                    sem::ValueExprKind::Load {
                        place: Box::new(place),
                    },
                    place_ty,
                    expr.span,
                ))
            }
            norm::ExprKind::Move { expr: inner } => {
                let place = self.elab_place(inner);
                let place_ty = place.ty;
                Some(sem::ValueExpr {
                    id: expr.id,
                    kind: sem::ValueExprKind::Move {
                        place: Box::new(place),
                    },
                    ty: place_ty,
                    span: expr.span,
                })
            }
            norm::ExprKind::ImplicitMove { expr: inner } => {
                let place = self.elab_place(inner);
                let place_ty = place.ty;
                Some(sem::ValueExpr {
                    id: expr.id,
                    kind: sem::ValueExprKind::ImplicitMove {
                        place: Box::new(place),
                    },
                    ty: place_ty,
                    span: expr.span,
                })
            }
            _ => None,
        }
    }

    /// Transform a closure expression into a struct literal.
    ///
    /// The closure's struct type and invoke method are created lazily via
    /// `ensure_closure_info`. The closure literal becomes a struct literal
    /// that initializes each capture field.
    fn elab_closure_expr(&mut self, expr: &norm::Expr) -> Option<sem::ValueExpr> {
        let norm::ExprKind::Closure {
            ident,
            def_id,
            params,
            return_ty,
            body,
            captures: _,
        } = &expr.kind
        else {
            return None;
        };

        if self.is_captureless_closure(*def_id) {
            // Captureless closures become top-level functions referenced by def id.
            self.ensure_closure_func(ident, *def_id, params, return_ty, body, expr.span, expr.id);
            return Some(sem::ValueExpr {
                id: expr.id,
                kind: sem::ValueExprKind::ClosureRef { def_id: *def_id },
                ty: expr.ty,
                span: expr.span,
            });
        }

        let info =
            self.ensure_closure_info(ident, *def_id, params, return_ty, body, expr.span, expr.id);
        let ty_id = self.type_map.insert_node_type(expr.id, info.ty.clone());
        let fields = info
            .captures
            .iter()
            .map(|capture| sem::StructLitField {
                name: capture.name.clone(),
                value: self.capture_value_for_def(capture, expr.span),
                span: expr.span,
            })
            .collect();

        Some(sem::ValueExpr {
            id: expr.id,
            kind: sem::ValueExprKind::StructLit {
                name: info.type_name,
                fields,
            },
            ty: ty_id,
            span: expr.span,
        })
    }

    fn elab_call_kind(&mut self, expr: &norm::Expr) -> Option<sem::ValueExprKind> {
        match &expr.kind {
            norm::ExprKind::Call { callee, args } => Some(self.elab_call_expr(expr, callee, args)),
            norm::ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => Some(self.elab_method_call_expr(expr, callee, method_name, args)),
            _ => None,
        }
    }

    fn elab_call_expr(
        &mut self,
        expr: &norm::Expr,
        callee: &norm::Expr,
        args: &[norm::CallArg],
    ) -> sem::ValueExprKind {
        let call_sig = self.get_call_sig(expr.id);

        if let Some((closure_def_id, info)) = self.closure_call_info(callee) {
            // Rewrite closure calls into method calls on the generated closure struct.
            if info.param_modes.len() != args.len() {
                panic!(
                    "compiler bug: closure call {call_id:?} expects {} args, got {}",
                    info.param_modes.len(),
                    args.len(),
                    call_id = expr.id
                );
            }

            // The semantic call plan uses the canonical receiver+args order.
            let plan_sig = CallSig {
                def_id: Some(closure_def_id),
                receiver: Some(CallParam {
                    mode: ParamMode::In,
                    ty: info.ty.clone(),
                }),
                params: call_sig.params.clone(),
            };

            let plan = self.build_call_plan(expr.id, &plan_sig);
            self.type_map.insert_call_plan(expr.id, plan);

            let receiver = sem::MethodReceiver::ValueExpr(Box::new(self.elab_value(callee)));
            let args = info
                .param_modes
                .iter()
                .zip(args.iter())
                .map(|(mode, arg)| self.elab_call_arg_mode(mode.clone(), arg))
                .collect();

            sem::ValueExprKind::MethodCall {
                receiver,
                method_name: "invoke".to_string(),
                args,
            }
        } else {
            let plan = self.build_call_plan(expr.id, &call_sig);
            self.type_map.insert_call_plan(expr.id, plan);

            let args = call_sig
                .params
                .iter()
                .zip(args.iter())
                .map(|(param, arg)| self.elab_call_arg(param, arg))
                .collect();

            sem::ValueExprKind::Call {
                callee: Box::new(self.elab_value(callee)),
                args,
            }
        }
    }

    fn elab_method_call_expr(
        &mut self,
        expr: &norm::Expr,
        callee: &norm::Expr,
        method_name: &str,
        args: &[norm::CallArg],
    ) -> sem::ValueExprKind {
        let call_sig = self.get_call_sig(expr.id);
        let receiver = call_sig
            .receiver
            .as_ref()
            .map(|receiver| self.elab_method_receiver(receiver, callee))
            .unwrap_or_else(|| {
                panic!(
                    "compiler bug: missing receiver in method call {call_id:?}",
                    call_id = expr.id
                )
            });
        let args = call_sig
            .params
            .iter()
            .zip(args.iter())
            .map(|(param, arg)| self.elab_call_arg(param, arg))
            .collect();
        let plan = self.build_call_plan(expr.id, &call_sig);
        self.type_map.insert_call_plan(expr.id, plan);
        sem::ValueExprKind::MethodCall {
            receiver,
            method_name: method_name.to_string(),
            args,
        }
    }

    fn elab_simple_value_kind(&mut self, expr: &norm::Expr) -> sem::ValueExprKind {
        match &expr.kind {
            norm::ExprKind::Block { items, tail } => sem::ValueExprKind::Block {
                items: items
                    .iter()
                    .map(|item| self.elab_block_item(item))
                    .collect(),
                tail: tail.as_ref().map(|value| Box::new(self.elab_value(value))),
            },
            norm::ExprKind::UnitLit => sem::ValueExprKind::UnitLit,
            norm::ExprKind::IntLit(value) => sem::ValueExprKind::IntLit(*value),
            norm::ExprKind::BoolLit(value) => sem::ValueExprKind::BoolLit(*value),
            norm::ExprKind::CharLit(value) => sem::ValueExprKind::CharLit(*value),
            norm::ExprKind::StringLit { value } => sem::ValueExprKind::StringLit {
                value: value.clone(),
            },
            norm::ExprKind::StringFmt { segments } => sem::ValueExprKind::StringFmt {
                plan: self.elab_string_fmt_plan(segments),
            },
            norm::ExprKind::ArrayLit { elem_ty, init } => sem::ValueExprKind::ArrayLit {
                elem_ty: elem_ty.clone(),
                init: self.elab_array_lit_init(init),
            },
            norm::ExprKind::TupleLit(items) => sem::ValueExprKind::TupleLit(
                items.iter().map(|item| self.elab_value(item)).collect(),
            ),
            norm::ExprKind::StructLit { name, fields } => sem::ValueExprKind::StructLit {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|field| self.elab_struct_lit_field(field))
                    .collect(),
            },
            norm::ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            } => sem::ValueExprKind::EnumVariant {
                enum_name: enum_name.clone(),
                variant: variant.clone(),
                payload: payload.iter().map(|value| self.elab_value(value)).collect(),
            },
            norm::ExprKind::StructUpdate { target, fields } => sem::ValueExprKind::StructUpdate {
                target: Box::new(self.elab_value(target)),
                fields: fields
                    .iter()
                    .map(|field| self.elab_struct_update_field(field))
                    .collect(),
            },
            norm::ExprKind::BinOp { left, op, right } => sem::ValueExprKind::BinOp {
                left: Box::new(self.elab_value(left)),
                op: *op,
                right: Box::new(self.elab_value(right)),
            },
            norm::ExprKind::UnaryOp { op, expr } => sem::ValueExprKind::UnaryOp {
                op: *op,
                expr: Box::new(self.elab_value(expr)),
            },
            norm::ExprKind::HeapAlloc { expr } => sem::ValueExprKind::HeapAlloc {
                expr: Box::new(self.elab_value(expr)),
            },
            norm::ExprKind::If {
                cond,
                then_body,
                else_body,
            } => sem::ValueExprKind::If {
                cond: Box::new(self.elab_value(cond)),
                then_body: Box::new(self.elab_value(then_body)),
                else_body: Box::new(self.elab_value(else_body)),
            },
            norm::ExprKind::Range { start, end } => sem::ValueExprKind::Range {
                start: *start,
                end: *end,
            },
            norm::ExprKind::Slice { target, start, end } => {
                let target_place = self.elab_place(target);
                let target_ty = self.type_map.type_table().get(target_place.ty).clone();
                let slice_ty = self.type_map.type_table().get(expr.ty).clone();
                let plan = self.build_slice_plan(&target_ty, &slice_ty);
                self.type_map.insert_slice_plan(expr.id, plan);

                sem::ValueExprKind::Slice {
                    target: Box::new(target_place),
                    start: start.as_ref().map(|expr| Box::new(self.elab_value(expr))),
                    end: end.as_ref().map(|expr| Box::new(self.elab_value(expr))),
                }
            }
            norm::ExprKind::Match { scrutinee, arms } => {
                // Pre-compute match tests + bindings so lowering only emits the plan.
                let plan = self.build_match_plan(expr.id, scrutinee, arms);
                self.type_map.insert_match_plan(expr.id, plan);
                sem::ValueExprKind::Match {
                    scrutinee: Box::new(self.elab_value(scrutinee)),
                    arms: arms.iter().map(|arm| self.elab_match_arm(arm)).collect(),
                }
            }
            norm::ExprKind::Coerce { kind, expr: inner } => {
                if matches!(kind, CoerceKind::ArrayToSlice) {
                    let target_ty = self.type_map.type_table().get(inner.ty).clone();
                    let slice_ty = self.type_map.type_table().get(expr.ty).clone();
                    let plan = self.build_slice_plan(&target_ty, &slice_ty);
                    self.type_map.insert_slice_plan(expr.id, plan);
                }
                sem::ValueExprKind::Coerce {
                    kind: *kind,
                    expr: Box::new(self.elab_value(inner)),
                }
            }
            norm::ExprKind::AddrOf { expr } => sem::ValueExprKind::AddrOf {
                place: Box::new(self.elab_place(expr)),
            },
            norm::ExprKind::Call { .. }
            | norm::ExprKind::MethodCall { .. }
            | norm::ExprKind::Closure { .. }
            | norm::ExprKind::Move { .. }
            | norm::ExprKind::ImplicitMove { .. }
            | norm::ExprKind::Var { .. }
            | norm::ExprKind::ArrayIndex { .. }
            | norm::ExprKind::TupleField { .. }
            | norm::ExprKind::StructField { .. }
            | norm::ExprKind::Deref { .. } => unreachable!("handled earlier"),
        }
    }

    fn elab_match_arm(&mut self, arm: &norm::MatchArm) -> sem::MatchArm {
        sem::MatchArm {
            id: arm.id,
            pattern: arm.pattern.clone(),
            body: self.elab_value(&arm.body),
            span: arm.span,
        }
    }

    fn elab_array_lit_init(&mut self, init: &norm::ArrayLitInit) -> sem::ArrayLitInit {
        match init {
            norm::ArrayLitInit::Elems(elems) => {
                sem::ArrayLitInit::Elems(elems.iter().map(|elem| self.elab_value(elem)).collect())
            }
            norm::ArrayLitInit::Repeat(elem, count) => {
                sem::ArrayLitInit::Repeat(Box::new(self.elab_value(elem)), *count)
            }
        }
    }

    fn elab_struct_lit_field(&mut self, field: &norm::StructLitField) -> sem::StructLitField {
        sem::StructLitField {
            name: field.name.clone(),
            value: self.elab_value(&field.value),
            span: field.span,
        }
    }

    fn elab_struct_update_field(
        &mut self,
        field: &norm::StructUpdateField,
    ) -> sem::StructUpdateField {
        sem::StructUpdateField {
            name: field.name.clone(),
            value: self.elab_value(&field.value),
            span: field.span,
        }
    }
}
