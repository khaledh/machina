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

use crate::core::elaborate::elaborator::Elaborator;
use crate::core::machine::request_site::labeled_request_site_key;
use crate::core::tree as ast;
use crate::core::tree::semantic as sem;
use crate::core::typecheck::type_map::{CallParam, CallSig};

impl<'a> Elaborator<'a> {
    /// Main entry point for elaborating a value expression.
    ///
    /// Dispatches to specialized handlers based on expression kind,
    /// with lvalue and closure cases taking priority.
    pub(in crate::core::elaborate) fn elab_value(&mut self, expr: &ast::Expr) -> sem::ValueExpr {
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
            ty: self.type_id_for(expr.id),
            span: expr.span,
        }
    }

    /// Handle expressions that denote memory locations (lvalues).
    ///
    /// Wraps the place in either a Load (for copies) or ImplicitMove
    /// (when semck determined the value should be moved).
    fn elab_lvalue_expr(&mut self, expr: &ast::Expr) -> Option<sem::ValueExpr> {
        match &expr.kind {
            ast::ExprKind::Var { .. }
            | ast::ExprKind::TupleField { .. }
            | ast::ExprKind::StructField { .. }
            | ast::ExprKind::Deref { .. } => {
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
            ast::ExprKind::ArrayIndex { target, .. }
                if !matches!(
                    self.type_map
                        .type_table()
                        .get(self.type_id_for(target.id))
                        .peel_heap(),
                    crate::core::types::Type::Map { .. }
                ) =>
            {
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
            ast::ExprKind::Move { expr: inner } => {
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
            ast::ExprKind::ImplicitMove { expr: inner } => {
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
    fn elab_closure_expr(&mut self, expr: &ast::Expr) -> Option<sem::ValueExpr> {
        let ast::ExprKind::Closure {
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
            return Some(sem::ValueExpr {
                id: expr.id,
                kind: sem::ValueExprKind::ClosureRef { def_id },
                ty: self.type_id_for(expr.id),
                span: expr.span,
            });
        }

        let info =
            self.ensure_closure_info(ident, def_id, params, return_ty, body, expr.span, expr.id);
        let ty_id = self.insert_synth_node_type(expr.id, info.ty.clone());
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

    fn elab_call_kind(&mut self, expr: &ast::Expr) -> Option<sem::ValueExprKind> {
        match &expr.kind {
            ast::ExprKind::Call { callee, args } => Some(self.elab_call_expr(expr, callee, args)),
            ast::ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => Some(self.elab_method_call_expr(expr, callee, method_name, args)),
            _ => None,
        }
    }

    fn elab_call_expr(
        &mut self,
        expr: &ast::Expr,
        callee: &ast::Expr,
        args: &[ast::CallArg],
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
                    mode: ast::ParamMode::In,
                    ty: info.ty.clone(),
                }),
                params: call_sig.params.clone(),
            };

            let plan = self.build_call_plan(expr.id, Some("invoke"), &plan_sig);
            self.record_call_plan(expr.id, plan);

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
            let plan = self.build_call_plan(expr.id, None, &call_sig);
            self.record_call_plan(expr.id, plan);

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
        expr: &ast::Expr,
        callee: &ast::Expr,
        method_name: &str,
        args: &[ast::CallArg],
    ) -> sem::ValueExprKind {
        if method_name == "len" && args.is_empty() {
            let place = self.elab_place(callee);
            return sem::ValueExprKind::Len {
                place: Box::new(place),
            };
        }
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
        let plan = self.build_call_plan(expr.id, Some(method_name), &call_sig);
        self.record_call_plan(expr.id, plan);
        sem::ValueExprKind::MethodCall {
            receiver,
            method_name: method_name.to_string(),
            args,
        }
    }

    fn elab_simple_value_kind(&mut self, expr: &ast::Expr) -> sem::ValueExprKind {
        match &expr.kind {
            ast::ExprKind::Block { items, tail } => sem::ValueExprKind::Block {
                items: items
                    .iter()
                    .map(|item| self.elab_block_item(item))
                    .collect(),
                tail: tail.as_ref().map(|value| Box::new(self.elab_value(value))),
            },
            ast::ExprKind::UnitLit => sem::ValueExprKind::UnitLit,
            ast::ExprKind::IntLit(value) => sem::ValueExprKind::IntLit(*value),
            ast::ExprKind::BoolLit(value) => sem::ValueExprKind::BoolLit(*value),
            ast::ExprKind::CharLit(value) => sem::ValueExprKind::CharLit(*value),
            ast::ExprKind::StringLit { value } => sem::ValueExprKind::StringLit {
                value: value.clone(),
            },
            ast::ExprKind::StringFmt { segments } => sem::ValueExprKind::StringFmt {
                plan: self.elab_string_fmt_plan(segments),
            },
            ast::ExprKind::ArrayLit { elem_ty, init } => sem::ValueExprKind::ArrayLit {
                elem_ty: elem_ty.clone(),
                init: self.elab_array_lit_init(init),
            },
            ast::ExprKind::SetLit { elem_ty, elems } => sem::ValueExprKind::SetLit {
                elem_ty: elem_ty.clone(),
                elems: elems.iter().map(|elem| self.elab_value(elem)).collect(),
            },
            ast::ExprKind::MapLit {
                key_ty,
                value_ty,
                entries,
            } => sem::ValueExprKind::MapLit {
                key_ty: key_ty.clone(),
                value_ty: value_ty.clone(),
                entries: entries
                    .iter()
                    .map(|entry| sem::MapLitEntry {
                        key: self.elab_value(&entry.key),
                        value: self.elab_value(&entry.value),
                        span: entry.span,
                    })
                    .collect(),
            },
            ast::ExprKind::TupleLit(items) => sem::ValueExprKind::TupleLit(
                items.iter().map(|item| self.elab_value(item)).collect(),
            ),
            ast::ExprKind::StructLit { name, fields, .. } => sem::ValueExprKind::StructLit {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|field| self.elab_struct_lit_field(field))
                    .collect(),
            },
            ast::ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
                ..
            } => sem::ValueExprKind::EnumVariant {
                enum_name: enum_name.clone(),
                variant: variant.clone(),
                payload: payload.iter().map(|value| self.elab_value(value)).collect(),
            },
            ast::ExprKind::StructUpdate { target, fields } => sem::ValueExprKind::StructUpdate {
                target: Box::new(self.elab_value(target)),
                fields: fields
                    .iter()
                    .map(|field| self.elab_struct_update_field(field))
                    .collect(),
            },
            ast::ExprKind::BinOp { left, op, right } => sem::ValueExprKind::BinOp {
                left: Box::new(self.elab_value(left)),
                op: *op,
                right: Box::new(self.elab_value(right)),
            },
            ast::ExprKind::UnaryOp { op, expr } => sem::ValueExprKind::UnaryOp {
                op: *op,
                expr: Box::new(self.elab_value(expr)),
            },
            ast::ExprKind::Try {
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
                sem::ValueExprKind::Try {
                    fallible_expr: Box::new(self.elab_value(fallible_expr)),
                    on_error: on_error
                        .as_ref()
                        .map(|expr| Box::new(self.elab_value(expr))),
                }
            }
            ast::ExprKind::HeapAlloc { expr } => sem::ValueExprKind::HeapAlloc {
                expr: Box::new(self.elab_value(expr)),
            },
            ast::ExprKind::If {
                cond,
                then_body,
                else_body,
            } => sem::ValueExprKind::If {
                cond: Box::new(self.elab_value(cond)),
                then_body: Box::new(self.elab_value(then_body)),
                else_body: Box::new(self.elab_value(else_body)),
            },
            ast::ExprKind::Range { start, end } => sem::ValueExprKind::Range {
                start: Box::new(self.elab_value(start)),
                end: Box::new(self.elab_value(end)),
            },
            ast::ExprKind::Slice { target, start, end } => {
                let target_place = self.elab_place(target);
                let target_ty = self.type_map.type_table().get(target_place.ty).clone();
                let slice_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_id_for(expr.id))
                    .clone();
                let plan = self.build_slice_plan(&target_ty, &slice_ty);
                self.record_slice_plan(expr.id, plan);

                sem::ValueExprKind::Slice {
                    target: Box::new(target_place),
                    start: start.as_ref().map(|expr| Box::new(self.elab_value(expr))),
                    end: end.as_ref().map(|expr| Box::new(self.elab_value(expr))),
                }
            }
            ast::ExprKind::ArrayIndex { target, indices }
                if matches!(
                    self.type_map
                        .type_table()
                        .get(self.type_id_for(target.id))
                        .peel_heap(),
                    crate::core::types::Type::Map { .. }
                ) =>
            {
                let key_expr = indices
                    .first()
                    .unwrap_or_else(|| panic!("backend map index expects a single key expression"));
                sem::ValueExprKind::MapGet {
                    target: Box::new(self.elab_value(target)),
                    key: Box::new(self.elab_value(key_expr)),
                }
            }
            ast::ExprKind::Match { scrutinee, arms } => {
                // Elaborate the scrutinee first so match planning can use the
                // semantically-correct scrutinee type (including place-based deref behavior).
                let sem_scrutinee = self.elab_value(scrutinee);
                // Pre-compute match tests + bindings so lowering only emits the plan.
                let scrutinee_ty = self.type_map.type_table().get(sem_scrutinee.ty).clone();
                let plan = self.build_match_plan(expr.id, scrutinee_ty, arms);
                self.record_match_plan(expr.id, plan);
                sem::ValueExprKind::Match {
                    scrutinee: Box::new(sem_scrutinee),
                    arms: arms.iter().map(|arm| self.elab_match_arm(arm)).collect(),
                }
            }
            ast::ExprKind::Coerce { kind, expr: inner } => {
                if matches!(
                    kind,
                    ast::CoerceKind::ArrayToSlice | ast::CoerceKind::DynArrayToSlice
                ) {
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
                sem::ValueExprKind::Coerce {
                    kind: *kind,
                    expr: Box::new(self.elab_value(inner)),
                }
            }
            ast::ExprKind::Emit { kind } => match kind {
                ast::EmitKind::Send { to, payload } => sem::ValueExprKind::EmitSend {
                    to: Box::new(self.elab_value(to)),
                    payload: Box::new(self.elab_value(payload)),
                },
                ast::EmitKind::Request {
                    to,
                    payload,
                    request_site_label,
                } => sem::ValueExprKind::EmitRequest {
                    to: Box::new(self.elab_value(to)),
                    payload: Box::new(self.elab_value(payload)),
                    // Labeled request sites use deterministic symbolic keys;
                    // unlabeled sites keep per-expression identity.
                    request_site_key: request_site_label
                        .as_deref()
                        .map(labeled_request_site_key)
                        .unwrap_or(expr.id.0 as u64),
                },
            },
            ast::ExprKind::Reply { cap, value } => sem::ValueExprKind::Reply {
                cap: Box::new(self.elab_value(cap)),
                value: Box::new(self.elab_value(value)),
            },
            ast::ExprKind::AddrOf { expr } => sem::ValueExprKind::AddrOf {
                place: Box::new(self.elab_place(expr)),
            },
            ast::ExprKind::Call { .. }
            | ast::ExprKind::MethodCall { .. }
            | ast::ExprKind::Closure { .. }
            | ast::ExprKind::Move { .. }
            | ast::ExprKind::ImplicitMove { .. }
            | ast::ExprKind::Var { .. }
            | ast::ExprKind::TupleField { .. }
            | ast::ExprKind::StructField { .. }
            | ast::ExprKind::Deref { .. } => unreachable!("handled earlier"),
            ast::ExprKind::ArrayIndex { .. } => unreachable!("handled earlier"),
        }
    }

    fn elab_match_arm(&mut self, arm: &ast::MatchArm) -> sem::MatchArm {
        sem::MatchArm {
            id: arm.id,
            pattern: arm.pattern.clone(),
            body: self.elab_value(&arm.body),
            span: arm.span,
        }
    }

    fn elab_array_lit_init(&mut self, init: &ast::ArrayLitInit) -> sem::ArrayLitInit {
        match init {
            ast::ArrayLitInit::Elems(elems) => {
                sem::ArrayLitInit::Elems(elems.iter().map(|elem| self.elab_value(elem)).collect())
            }
            ast::ArrayLitInit::Repeat(elem, count) => {
                sem::ArrayLitInit::Repeat(Box::new(self.elab_value(elem)), *count)
            }
        }
    }

    fn elab_struct_lit_field(&mut self, field: &ast::StructLitField) -> sem::StructLitField {
        sem::StructLitField {
            name: field.name.clone(),
            value: self.elab_value(&field.value),
            span: field.span,
        }
    }

    fn elab_struct_update_field(
        &mut self,
        field: &ast::StructUpdateField,
    ) -> sem::StructUpdateField {
        sem::StructUpdateField {
            name: field.name.clone(),
            value: self.elab_value(&field.value),
            span: field.span,
        }
    }
}
