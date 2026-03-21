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

use crate::core::ast::{
    ArrayLitInit, CallArg, CoerceKind, EmitKind, Expr, ExprKind, MapLitEntry, MatchArm, ParamMode,
    StructLitField, StructUpdateField,
};
use crate::core::elaborate::elaborator::Elaborator;
use crate::core::machine::request_site::labeled_request_site_key;
use crate::core::typecheck::type_map::{CallParam, CallSig};
use crate::core::types::Type;

impl<'a> Elaborator<'a> {
    /// Main entry point for elaborating a value expression.
    ///
    /// Dispatches to specialized handlers based on expression kind,
    /// with lvalue and closure cases taking priority.
    pub(in crate::core::elaborate) fn elab_value(&mut self, expr: &Expr) -> Expr {
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

        Expr {
            id: expr.id,
            kind,
            span: expr.span,
        }
    }

    /// Handle expressions that denote memory locations (lvalues).
    ///
    /// Wraps the place in either a Load (for copies) or ImplicitMove
    /// (when semck determined the value should be moved).
    fn elab_lvalue_expr(&mut self, expr: &Expr) -> Option<Expr> {
        match &expr.kind {
            ExprKind::Var { .. }
            | ExprKind::TupleField { .. }
            | ExprKind::StructField { .. }
            | ExprKind::Deref { .. } => {
                let place = self.elab_place(expr);
                let place_ty = self.type_id_for(place.id);
                if self.implicit_moves.contains(&expr.id) {
                    return Some(self.new_value(
                        ExprKind::ImplicitMove {
                            expr: Box::new(place),
                        },
                        place_ty,
                        expr.span,
                    ));
                }
                Some(self.new_value(
                    ExprKind::Load {
                        expr: Box::new(place),
                    },
                    place_ty,
                    expr.span,
                ))
            }
            ExprKind::ArrayIndex { target, .. }
                if !matches!(
                    self.type_map
                        .type_table()
                        .get(self.type_id_for(target.id))
                        .peel_heap(),
                    Type::Map { .. }
                ) =>
            {
                let place = self.elab_place(expr);
                let place_ty = self.type_id_for(place.id);
                if self.implicit_moves.contains(&expr.id) {
                    return Some(self.new_value(
                        ExprKind::ImplicitMove {
                            expr: Box::new(place),
                        },
                        place_ty,
                        expr.span,
                    ));
                }
                Some(self.new_value(
                    ExprKind::Load {
                        expr: Box::new(place),
                    },
                    place_ty,
                    expr.span,
                ))
            }
            ExprKind::Move { expr: inner } => {
                let place = self.elab_place(inner);
                let place_ty = self.type_id_for(place.id);
                self.record_expr_type(expr.id, place_ty);
                Some(Expr {
                    id: expr.id,
                    kind: ExprKind::Move {
                        expr: Box::new(place),
                    },
                    span: expr.span,
                })
            }
            ExprKind::ImplicitMove { expr: inner } => {
                let place = self.elab_place(inner);
                let place_ty = self.type_id_for(place.id);
                self.record_expr_type(expr.id, place_ty);
                Some(Expr {
                    id: expr.id,
                    kind: ExprKind::ImplicitMove {
                        expr: Box::new(place),
                    },
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
    fn elab_closure_expr(&mut self, expr: &Expr) -> Option<Expr> {
        let ExprKind::Closure {
            ident,
            params,
            return_ty,
            body,
            captures: _,
        } = &expr.kind
        else {
            return None;
        };

        let def_id = self.def_id_for(expr.id);
        if self.is_captureless_closure(def_id) {
            // Captureless closures become top-level functions referenced by def id.
            self.ensure_closure_func(ident, def_id, params, return_ty, body, expr.span, expr.id);
            return Some(Expr {
                id: expr.id,
                kind: ExprKind::ClosureRef {
                    ident: ident.clone(),
                },
                span: expr.span,
            });
        }

        let info =
            self.ensure_closure_info(ident, def_id, params, return_ty, body, expr.span, expr.id);
        let _ty_id = self.insert_synth_node_type(expr.id, info.ty.clone());
        let fields = info
            .captures
            .iter()
            .map(|capture| StructLitField {
                id: self.node_id_gen.new_id(),
                name: capture.name.clone(),
                value: self.capture_value_for_def(capture, expr.span),
                span: expr.span,
            })
            .collect();

        Some(Expr {
            id: expr.id,
            kind: ExprKind::StructLit {
                name: info.type_name,
                type_args: vec![],
                fields,
            },
            span: expr.span,
        })
    }

    fn elab_call_kind(&mut self, expr: &Expr) -> Option<ExprKind> {
        match &expr.kind {
            ExprKind::Call { callee, args } => Some(self.elab_call_expr(expr, callee, args)),
            ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => Some(self.elab_method_call_expr(expr, callee, method_name, args)),
            _ => None,
        }
    }

    fn elab_call_expr(&mut self, expr: &Expr, callee: &Expr, args: &[CallArg]) -> ExprKind {
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
                selected: None,
                receiver: Some(CallParam {
                    mode: ParamMode::In,
                    ty: info.ty.clone(),
                }),
                params: call_sig.params.clone(),
            };

            let plan = self.build_call_plan(expr.id, Some("invoke"), &plan_sig);
            self.record_call_plan(expr.id, plan);

            let elab_callee = Box::new(self.elab_value(callee));
            // Re-record the callee's type as the closure struct type so the lowerer
            // sees a non-scalar struct and materializes it properly.
            self.record_expr_type(elab_callee.id, info.type_id);
            let args = info
                .param_modes
                .iter()
                .zip(args.iter())
                .map(|(mode, arg)| self.elab_call_arg_mode(mode.clone(), arg))
                .collect();

            ExprKind::MethodCall {
                callee: elab_callee,
                method_name: "invoke".to_string(),
                args,
            }
        } else {
            let plan = self.build_call_plan(expr.id, None, &call_sig);
            self.record_call_plan(expr.id, plan);

            let args = call_sig
                .params
                .iter()
                .zip(args.iter())
                .map(|(param, arg)| self.elab_call_arg(param, arg))
                .collect();

            ExprKind::Call {
                callee: Box::new(self.elab_value(callee)),
                args,
            }
        }
    }

    fn elab_method_call_expr(
        &mut self,
        expr: &Expr,
        callee: &Expr,
        method_name: &str,
        args: &[CallArg],
    ) -> ExprKind {
        if method_name == "len" && args.is_empty() {
            let place = self.elab_place(callee);
            return ExprKind::Len {
                expr: Box::new(place),
            };
        }
        let call_sig = self.get_call_sig(expr.id);
        let elab_callee = call_sig
            .receiver
            .as_ref()
            .map(|receiver| self.elab_method_receiver(receiver, callee))
            .unwrap_or_else(|| {
                panic!(
                    "compiler bug: missing receiver in method call {call_id:?}",
                    call_id = expr.id
                )
            });
        // Hosted `create`/`resume` calls carry a source-level role projection
        // argument (`PullRequest as Author`) that is consumed during type
        // checking and does not become a runtime call argument.
        let runtime_args = if matches!(method_name, "create" | "resume")
            && matches!(
                args.first().map(|arg| &arg.expr.kind),
                Some(ExprKind::RoleProjection { .. })
            ) {
            &args[1..]
        } else {
            args
        };
        let args = call_sig
            .params
            .iter()
            .zip(runtime_args.iter())
            .map(|(param, arg)| self.elab_call_arg(param, arg))
            .collect();
        let plan = self.build_call_plan(expr.id, Some(method_name), &call_sig);
        self.record_call_plan(expr.id, plan);
        ExprKind::MethodCall {
            callee: elab_callee,
            method_name: method_name.to_string(),
            args,
        }
    }

    fn elab_simple_value_kind(&mut self, expr: &Expr) -> ExprKind {
        match &expr.kind {
            ExprKind::Block { items, tail } => ExprKind::Block {
                items: items
                    .iter()
                    .map(|item| self.elab_block_item(item))
                    .collect(),
                tail: tail.as_ref().map(|value| Box::new(self.elab_value(value))),
            },
            ExprKind::UnitLit => ExprKind::UnitLit,
            ExprKind::IntLit(value) => ExprKind::IntLit(*value),
            ExprKind::BoolLit(value) => ExprKind::BoolLit(*value),
            ExprKind::CharLit(value) => ExprKind::CharLit(*value),
            ExprKind::StringLit { value } => ExprKind::StringLit {
                value: value.clone(),
            },
            ExprKind::StringFmt { segments } => {
                let plan = self.elab_string_fmt_plan(segments);
                // Store the plan in a side table for lowering; AST StringFmt has `segments`.
                self.record_string_fmt_plan(expr.id, plan);
                ExprKind::StringFmt {
                    segments: segments.clone(),
                }
            }
            ExprKind::ArrayLit { elem_ty, init } => ExprKind::ArrayLit {
                elem_ty: elem_ty.clone(),
                init: self.elab_array_lit_init(init),
            },
            ExprKind::SetLit { elem_ty, elems } => ExprKind::SetLit {
                elem_ty: elem_ty.clone(),
                elems: elems.iter().map(|elem| self.elab_value(elem)).collect(),
            },
            ExprKind::MapLit {
                key_ty,
                value_ty,
                entries,
            } => ExprKind::MapLit {
                key_ty: key_ty.clone(),
                value_ty: value_ty.clone(),
                entries: entries
                    .iter()
                    .map(|entry| MapLitEntry {
                        id: entry.id,
                        key: self.elab_value(&entry.key),
                        value: self.elab_value(&entry.value),
                        span: entry.span,
                    })
                    .collect(),
            },
            ExprKind::TupleLit(items) => {
                ExprKind::TupleLit(items.iter().map(|item| self.elab_value(item)).collect())
            }
            ExprKind::StructLit { name, fields, .. } => ExprKind::StructLit {
                name: name.clone(),
                type_args: vec![],
                fields: fields
                    .iter()
                    .map(|field| self.elab_struct_lit_field(field))
                    .collect(),
            },
            ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
                ..
            } => ExprKind::EnumVariant {
                enum_name: enum_name.clone(),
                type_args: vec![],
                variant: variant.clone(),
                payload: payload.iter().map(|value| self.elab_value(value)).collect(),
            },
            ExprKind::StructUpdate { target, fields } => ExprKind::StructUpdate {
                target: Box::new(self.elab_value(target)),
                fields: fields
                    .iter()
                    .map(|field| self.elab_struct_update_field(field))
                    .collect(),
            },
            ExprKind::BinOp { left, op, right } => ExprKind::BinOp {
                left: Box::new(self.elab_value(left)),
                op: *op,
                right: Box::new(self.elab_value(right)),
            },
            ExprKind::UnaryOp { op, expr } => ExprKind::UnaryOp {
                op: *op,
                expr: Box::new(self.elab_value(expr)),
            },
            ExprKind::Try {
                fallible_expr,
                on_error,
            } => {
                if on_error.is_some() {
                    // `try-or` error handlers lower as callable values and still
                    // need normal call-ABI planning for payload passing.
                    let call_sig = self.get_call_sig(expr.id);
                    let plan = self.build_call_plan(expr.id, None, &call_sig);
                    self.record_call_plan(expr.id, plan);
                }
                ExprKind::Try {
                    fallible_expr: Box::new(self.elab_value(fallible_expr)),
                    on_error: on_error
                        .as_ref()
                        .map(|expr| Box::new(self.elab_value(expr))),
                }
            }
            ExprKind::HeapAlloc { expr } => ExprKind::HeapAlloc {
                expr: Box::new(self.elab_value(expr)),
            },
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => ExprKind::If {
                cond: Box::new(self.elab_value(cond)),
                then_body: Box::new(self.elab_value(then_body)),
                else_body: Box::new(self.elab_value(else_body)),
            },
            ExprKind::Range { start, end } => ExprKind::Range {
                start: Box::new(self.elab_value(start)),
                end: Box::new(self.elab_value(end)),
            },
            ExprKind::Slice { target, start, end } => {
                let target_place = self.elab_place(target);
                let target_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_id_for(target_place.id))
                    .clone();
                let slice_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_id_for(expr.id))
                    .clone();
                let plan = self.build_slice_plan(&target_ty, &slice_ty);
                self.record_slice_plan(expr.id, plan);

                ExprKind::Slice {
                    target: Box::new(target_place),
                    start: start.as_ref().map(|expr| Box::new(self.elab_value(expr))),
                    end: end.as_ref().map(|expr| Box::new(self.elab_value(expr))),
                }
            }
            ExprKind::ArrayIndex { target, indices }
                if matches!(
                    self.type_map
                        .type_table()
                        .get(self.type_id_for(target.id))
                        .peel_heap(),
                    Type::Map { .. }
                ) =>
            {
                let key_expr = indices
                    .first()
                    .unwrap_or_else(|| panic!("backend map index expects a single key expression"));
                ExprKind::MapGet {
                    target: Box::new(self.elab_value(target)),
                    key: Box::new(self.elab_value(key_expr)),
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                // Elaborate the scrutinee first so match planning can use the
                // semantically-correct scrutinee type (including place-based deref behavior).
                let sem_scrutinee = self.elab_value(scrutinee);
                // Pre-compute match tests + bindings so lowering only emits the plan.
                let scrutinee_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_id_for(sem_scrutinee.id))
                    .clone();
                let plan = self.build_match_plan(expr.id, scrutinee_ty, arms);
                self.record_match_plan(expr.id, plan);
                ExprKind::Match {
                    scrutinee: Box::new(sem_scrutinee),
                    arms: arms.iter().map(|arm| self.elab_match_arm(arm)).collect(),
                }
            }
            ExprKind::Coerce { kind, expr: inner } => {
                if matches!(kind, CoerceKind::ArrayToSlice | CoerceKind::DynArrayToSlice) {
                    let target_ty = self
                        .type_map
                        .type_table()
                        .get(self.type_id_for(inner.id))
                        .clone();
                    let slice_ty = self
                        .type_map
                        .type_table()
                        .get(self.type_id_for(expr.id))
                        .clone();
                    let plan = self.build_slice_plan(&target_ty, &slice_ty);
                    self.record_slice_plan(expr.id, plan);
                }
                ExprKind::Coerce {
                    kind: *kind,
                    expr: Box::new(self.elab_value(inner)),
                }
            }
            ExprKind::Emit { kind } => match kind {
                EmitKind::Send { to, payload } => ExprKind::Emit {
                    kind: EmitKind::Send {
                        to: Box::new(self.elab_value(to)),
                        payload: Box::new(self.elab_value(payload)),
                    },
                },
                EmitKind::Request {
                    to,
                    payload,
                    request_site_label,
                    ..
                } => {
                    // Labeled request sites use deterministic symbolic keys;
                    // unlabeled sites keep per-expression identity.
                    let request_site_key = Some(
                        request_site_label
                            .as_deref()
                            .map(labeled_request_site_key)
                            .unwrap_or(expr.id.0 as u64),
                    );
                    ExprKind::Emit {
                        kind: EmitKind::Request {
                            to: Box::new(self.elab_value(to)),
                            payload: Box::new(self.elab_value(payload)),
                            request_site_label: request_site_label.clone(),
                            request_site_key,
                        },
                    }
                }
            },
            ExprKind::Reply { cap, value } => ExprKind::Reply {
                cap: Box::new(self.elab_value(cap)),
                value: Box::new(self.elab_value(value)),
            },
            ExprKind::AddrOf { expr } => ExprKind::AddrOf {
                expr: Box::new(self.elab_place(expr)),
            },
            ExprKind::Call { .. }
            | ExprKind::MethodCall { .. }
            | ExprKind::Closure { .. }
            | ExprKind::Move { .. }
            | ExprKind::ImplicitMove { .. }
            | ExprKind::RoleProjection { .. }
            | ExprKind::Var { .. }
            | ExprKind::TupleField { .. }
            | ExprKind::StructField { .. }
            | ExprKind::Deref { .. } => unreachable!("handled earlier"),
            ExprKind::ArrayIndex { .. } => unreachable!("handled earlier"),
            ExprKind::Load { .. }
            | ExprKind::MapGet { .. }
            | ExprKind::Len { .. }
            | ExprKind::ClosureRef { .. } => {
                unreachable!(
                    "ExprKind::{:?} should not appear during value elaboration",
                    expr.kind
                )
            }
        }
    }

    fn elab_match_arm(&mut self, arm: &MatchArm) -> MatchArm {
        MatchArm {
            id: arm.id,
            patterns: arm.patterns.clone(),
            body: self.elab_value(&arm.body),
            span: arm.span,
        }
    }

    fn elab_array_lit_init(&mut self, init: &ArrayLitInit) -> ArrayLitInit {
        match init {
            ArrayLitInit::Elems(elems) => {
                ArrayLitInit::Elems(elems.iter().map(|elem| self.elab_value(elem)).collect())
            }
            ArrayLitInit::Repeat(elem, count) => {
                ArrayLitInit::Repeat(Box::new(self.elab_value(elem)), *count)
            }
        }
    }

    fn elab_struct_lit_field(&mut self, field: &StructLitField) -> StructLitField {
        StructLitField {
            id: field.id,
            name: field.name.clone(),
            value: self.elab_value(&field.value),
            span: field.span,
        }
    }

    fn elab_struct_update_field(&mut self, field: &StructUpdateField) -> StructUpdateField {
        StructUpdateField {
            id: field.id,
            name: field.name.clone(),
            value: self.elab_value(&field.value),
            span: field.span,
        }
    }
}
