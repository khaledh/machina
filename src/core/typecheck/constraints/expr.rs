//! Expression constraint collection for typecheck constraints pass.

use super::*;

impl<'a> ConstraintCollector<'a> {
    pub(super) fn collect_expr(&mut self, expr: &Expr, expected: Option<Type>) -> Type {
        let expr_ty = self.node_term(expr.id);

        // Push contextual expected-type information into the expression graph
        // for non-place expressions. Place-like forms (`var`, `block`) are
        // handled by their own structural equalities below.
        if let Some(expected) = expected.clone()
            && !matches!(expr.kind, ExprKind::Var { .. } | ExprKind::Block { .. })
        {
            self.push_assignable(
                expr_ty.clone(),
                expected,
                ConstraintReason::Expr(expr.id, expr.span),
            );
        }

        match &expr.kind {
            ExprKind::UnitLit => self.push_eq(
                expr_ty.clone(),
                Type::Unit,
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::IntLit(_) => {
                // Keep integer literals polymorphic during solving. If they
                // remain unconstrained, solve defaults this var to i32.
                let lit_ty = self.fresh_int_var_term();
                self.push_eq(
                    expr_ty.clone(),
                    lit_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::BoolLit(_) => self.push_eq(
                expr_ty.clone(),
                Type::Bool,
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::CharLit(_) => self.push_eq(
                expr_ty.clone(),
                Type::Char,
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::StringLit { .. } => self.push_eq(
                expr_ty.clone(),
                Type::String,
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::StringFmt { segments } => {
                // Collect interpolated segment expression terms so solver can
                // validate supported formatting operand types later.
                for segment in segments {
                    if let StringFmtSegment::Expr { expr, .. } = segment {
                        self.collect_expr(expr, None);
                    }
                }
                self.push_eq(
                    expr_ty.clone(),
                    Type::String,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::HeapAlloc { expr: inner } => {
                let inner_ty = self.collect_expr(inner, None);
                self.push_eq(
                    expr_ty.clone(),
                    Type::Heap {
                        elem_ty: Box::new(inner_ty),
                    },
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::Move { expr: inner }
            | ExprKind::Coerce { expr: inner, .. }
            | ExprKind::ImplicitMove { expr: inner }
            | ExprKind::AddrOf { expr: inner }
            | ExprKind::Deref { expr: inner } => {
                let inner_ty = self.collect_expr(inner, None);
                self.push_eq(
                    expr_ty.clone(),
                    inner_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::Var { def_id, .. } => {
                let def_ty = self.def_term(*def_id);
                self.push_eq(
                    expr_ty.clone(),
                    def_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::Block { items, tail } => {
                for item in items {
                    self.collect_block_item(item);
                }
                if let Some(tail) = tail {
                    let tail_ty = self.collect_expr(tail, expected.clone());
                    self.push_eq(
                        expr_ty.clone(),
                        tail_ty,
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                } else {
                    if let Some(expected) = expected.clone() {
                        if self.block_has_explicit_return(items) {
                            self.push_eq(
                                expr_ty.clone(),
                                expected,
                                ConstraintReason::Expr(expr.id, expr.span),
                            );
                        } else {
                            self.push_eq(
                                expr_ty.clone(),
                                Type::Unit,
                                ConstraintReason::Expr(expr.id, expr.span),
                            );
                        }
                    } else {
                        self.push_eq(
                            expr_ty.clone(),
                            Type::Unit,
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                    }
                }
            }
            ExprKind::TupleLit(fields) => {
                let mut field_terms = Vec::with_capacity(fields.len());
                for field in fields {
                    field_terms.push(self.collect_expr(field, None));
                }
                self.push_eq(
                    expr_ty.clone(),
                    Type::Tuple {
                        field_tys: field_terms,
                    },
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::ArrayLit { init, .. } => match init {
                crate::core::tree::resolved::ArrayLitInit::Elems(elems) => {
                    let elem_term = if let ExprKind::ArrayLit {
                        elem_ty: Some(explicit_elem_ty),
                        ..
                    } = &expr.kind
                    {
                        self.resolve_type_in_scope(explicit_elem_ty)
                            .unwrap_or_else(|_| self.fresh_var_term())
                    } else {
                        self.fresh_var_term()
                    };
                    for elem in elems {
                        let value_ty = self.collect_expr(elem, Some(elem_term.clone()));
                        self.push_assignable(
                            value_ty,
                            elem_term.clone(),
                            ConstraintReason::Expr(elem.id, elem.span),
                        );
                    }
                    self.push_eq(
                        expr_ty.clone(),
                        array_type_from_elem(&elem_term, elems.len()),
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
                crate::core::tree::resolved::ArrayLitInit::Repeat(value, count) => {
                    let elem_term = if let ExprKind::ArrayLit {
                        elem_ty: Some(explicit_elem_ty),
                        ..
                    } = &expr.kind
                    {
                        self.resolve_type_in_scope(explicit_elem_ty)
                            .unwrap_or_else(|_| self.fresh_var_term())
                    } else {
                        self.fresh_var_term()
                    };
                    let value_ty = self.collect_expr(value, Some(elem_term.clone()));
                    self.push_assignable(
                        value_ty,
                        elem_term.clone(),
                        ConstraintReason::Expr(value.id, value.span),
                    );
                    self.push_eq(
                        expr_ty.clone(),
                        array_type_from_elem(&elem_term, *count as usize),
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
            },
            ExprKind::SetLit { elem_ty, elems } => {
                let elem_term = if let Some(explicit_elem_ty) = elem_ty {
                    self.resolve_type_in_scope(explicit_elem_ty)
                        .unwrap_or_else(|_| self.fresh_var_term())
                } else {
                    self.fresh_var_term()
                };
                for elem in elems {
                    let value_ty = self.collect_expr(elem, Some(elem_term.clone()));
                    self.push_assignable(
                        value_ty,
                        elem_term.clone(),
                        ConstraintReason::Expr(elem.id, elem.span),
                    );
                }
                self.push_eq(
                    expr_ty.clone(),
                    Type::Set {
                        elem_ty: Box::new(elem_term.clone()),
                    },
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.out.expr_obligations.push(ExprObligation::SetElemType {
                    expr_id: expr.id,
                    elem_ty: elem_term.clone(),
                    span: expr.span,
                });
            }
            ExprKind::MapLit {
                key_ty,
                value_ty,
                entries,
            } => {
                let key_term = if let Some(explicit_key_ty) = key_ty {
                    self.resolve_type_in_scope(explicit_key_ty)
                        .unwrap_or_else(|_| self.fresh_var_term())
                } else {
                    self.fresh_var_term()
                };
                let value_term = if let Some(explicit_value_ty) = value_ty {
                    self.resolve_type_in_scope(explicit_value_ty)
                        .unwrap_or_else(|_| self.fresh_var_term())
                } else {
                    self.fresh_var_term()
                };

                for entry in entries {
                    let key_expr_ty = self.collect_expr(&entry.key, Some(key_term.clone()));
                    self.push_assignable(
                        key_expr_ty,
                        key_term.clone(),
                        ConstraintReason::Expr(entry.key.id, entry.key.span),
                    );

                    let value_expr_ty = self.collect_expr(&entry.value, Some(value_term.clone()));
                    self.push_assignable(
                        value_expr_ty,
                        value_term.clone(),
                        ConstraintReason::Expr(entry.value.id, entry.value.span),
                    );
                }

                self.push_eq(
                    expr_ty.clone(),
                    Type::Map {
                        key_ty: Box::new(key_term.clone()),
                        value_ty: Box::new(value_term.clone()),
                    },
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.out.expr_obligations.push(ExprObligation::MapKeyType {
                    expr_id: expr.id,
                    key_ty: key_term,
                    span: expr.span,
                });
            }
            ExprKind::StructLit {
                name,
                type_args,
                fields,
            } => {
                // Resolve nominal struct type (including generic instantiation)
                // when possible, then type fields against known field types.
                let known_struct = self
                    .resolve_type_instance_for_name(name, type_args, expected.as_ref())
                    .or_else(|| {
                        self.resolve_type_instance_for_expr(expr, type_args, expected.as_ref())
                    })
                    .or_else(|| self.type_defs.get(name).cloned());
                if let Some(Type::Struct {
                    fields: struct_fields,
                    ..
                }) = known_struct.as_ref()
                {
                    self.push_eq(
                        expr_ty.clone(),
                        known_struct.clone().expect("known struct type"),
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                    for field in fields {
                        let expected = struct_fields
                            .iter()
                            .find(|f| f.name == field.name)
                            .map(|f| f.ty.clone());
                        self.collect_expr(&field.value, expected);
                    }
                } else {
                    for field in fields {
                        self.collect_expr(&field.value, None);
                    }
                }
                let type_name = known_struct
                    .as_ref()
                    .and_then(|ty| match ty {
                        Type::Struct { name, .. } => Some(name.clone()),
                        _ => None,
                    })
                    .unwrap_or_else(|| name.clone());
                self.out
                    .expr_obligations
                    .push(ExprObligation::StructConstruct {
                        expr_id: expr.id,
                        type_name,
                        caller_def_id: self.current_callable_def_id(),
                        span: expr.span,
                    });
            }
            ExprKind::EnumVariant {
                enum_name,
                type_args,
                variant,
                payload,
            } => {
                // Resolve nominal enum type (including generic instantiation),
                // and collect payload terms with per-position expected types.
                let known_enum = self
                    .resolve_type_instance_for_name(enum_name, type_args, expected.as_ref())
                    .or_else(|| {
                        self.resolve_type_instance_for_expr(expr, type_args, expected.as_ref())
                    })
                    .or_else(|| self.type_defs.get(enum_name).cloned());
                let variant_payload_tys = if let Some(Type::Enum { variants, .. }) = &known_enum {
                    variants
                        .iter()
                        .find(|v| v.name == *variant)
                        .map(|v| v.payload.clone())
                } else {
                    None
                };
                let mut payload_terms = Vec::with_capacity(payload.len());
                let mut payload_nodes = Vec::with_capacity(payload.len());
                let mut payload_spans = Vec::with_capacity(payload.len());
                for (index, item) in payload.iter().enumerate() {
                    payload_nodes.push(item.id);
                    payload_spans.push(item.span);
                    let expected = variant_payload_tys
                        .as_ref()
                        .and_then(|tys| tys.get(index))
                        .cloned();
                    payload_terms.push(self.collect_expr(item, expected));
                }
                if let Some(enum_ty) = known_enum {
                    self.push_eq(
                        expr_ty.clone(),
                        enum_ty,
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
                self.out
                    .expr_obligations
                    .push(ExprObligation::EnumVariantPayload {
                        expr_id: expr.id,
                        enum_name: enum_name.clone(),
                        variant: variant.clone(),
                        payload: payload_terms,
                        payload_nodes,
                        payload_spans,
                        span: expr.span,
                    });
            }
            ExprKind::StructUpdate { target, fields } => {
                let target_ty = self.collect_expr(target, None);
                let mut field_terms = Vec::with_capacity(fields.len());
                for field in fields {
                    let value_ty = self.collect_expr(&field.value, None);
                    field_terms.push((field.name.clone(), value_ty));
                }
                self.out
                    .expr_obligations
                    .push(ExprObligation::StructUpdate {
                        expr_id: expr.id,
                        target: target_ty,
                        fields: field_terms,
                        result: expr_ty.clone(),
                        caller_def_id: self.current_callable_def_id(),
                        span: expr.span,
                    });
            }
            ExprKind::ArrayIndex { target, indices } => {
                let target_ty = self.collect_expr(target, None);
                let mut index_terms = Vec::with_capacity(indices.len());
                let mut index_nodes = Vec::with_capacity(indices.len());
                let mut index_spans = Vec::with_capacity(indices.len());
                for index in indices {
                    index_nodes.push(index.id);
                    index_spans.push(index.span);
                    index_terms.push(self.collect_expr(index, None));
                }
                self.out.expr_obligations.push(ExprObligation::ArrayIndex {
                    expr_id: expr.id,
                    target: target_ty,
                    indices: index_terms,
                    index_nodes,
                    index_spans,
                    result: expr_ty.clone(),
                    span: expr.span,
                });
            }
            ExprKind::TupleField { target, index } => {
                let target_ty = self.collect_expr(target, None);
                self.out.expr_obligations.push(ExprObligation::TupleField {
                    expr_id: expr.id,
                    target: target_ty,
                    index: *index,
                    result: expr_ty.clone(),
                    span: expr.span,
                });
            }
            ExprKind::StructField { target, field } => {
                let target_ty = self.collect_expr(target, None);
                self.out.expr_obligations.push(ExprObligation::StructField {
                    expr_id: expr.id,
                    target: target_ty,
                    field: field.clone(),
                    result: expr_ty.clone(),
                    caller_def_id: self.current_callable_def_id(),
                    span: expr.span,
                });
            }
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.collect_expr(cond, Some(Type::Bool));
                if is_synthesized_missing_else(then_body, else_body) {
                    let then_ty = self.collect_expr(then_body, Some(Type::Unit));
                    let else_ty = self.collect_expr(else_body, Some(Type::Unit));
                    self.push_assignable(
                        then_ty,
                        Type::Unit,
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                    self.push_assignable(
                        else_ty,
                        Type::Unit,
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                    self.push_eq(
                        expr_ty.clone(),
                        Type::Unit,
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                } else {
                    let then_ty = self.collect_expr(then_body, expected.clone());
                    let else_ty = self.collect_expr(else_body, expected.clone());
                    if expected.is_some() {
                        self.push_assignable(
                            then_ty.clone(),
                            expr_ty.clone(),
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                        self.push_assignable(
                            else_ty,
                            expr_ty.clone(),
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                    } else {
                        self.out.expr_obligations.push(ExprObligation::Join {
                            expr_id: expr.id,
                            arms: vec![then_ty, else_ty],
                            result: expr_ty.clone(),
                            span: expr.span,
                        });
                    }
                }
            }
            ExprKind::Range { start, end } => {
                let start_ty = self.collect_expr(start, Some(Type::uint(64)));
                let end_ty = self.collect_expr(end, Some(Type::uint(64)));
                self.push_eq(
                    expr_ty.clone(),
                    Type::Range {
                        elem_ty: Box::new(Type::uint(64)),
                    },
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.out.expr_obligations.push(ExprObligation::Range {
                    expr_id: expr.id,
                    start: start_ty,
                    end: end_ty,
                    result: expr_ty.clone(),
                    span: expr.span,
                });
            }
            ExprKind::Slice { target, start, end } => {
                let target_ty = self.collect_expr(target, None);
                let start_ty = start
                    .as_ref()
                    .map(|start| self.collect_expr(start, Some(Type::uint(64))));
                let end_ty = end
                    .as_ref()
                    .map(|end| self.collect_expr(end, Some(Type::uint(64))));
                self.out.expr_obligations.push(ExprObligation::Slice {
                    expr_id: expr.id,
                    target: target_ty,
                    start: start_ty,
                    end: end_ty,
                    result: expr_ty.clone(),
                    span: expr.span,
                });
            }
            ExprKind::Match { scrutinee, arms } => {
                let scrutinee_ty = self.collect_expr(scrutinee, None);
                let mut arm_terms = Vec::with_capacity(arms.len());
                for arm in arms {
                    self.out
                        .pattern_obligations
                        .push(PatternObligation::MatchArm {
                            arm_id: arm.id,
                            pattern: arm.pattern.clone(),
                            scrutinee_ty: scrutinee_ty.clone(),
                            caller_def_id: self.current_callable_def_id(),
                            span: arm.span,
                        });
                    let arm_ty = self.collect_match_arm(arm, expected.clone());
                    if expected.is_some() {
                        self.push_assignable(
                            arm_ty.clone(),
                            expr_ty.clone(),
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                    }
                    arm_terms.push(arm_ty);
                }
                if expected.is_none() {
                    self.out.expr_obligations.push(ExprObligation::Join {
                        expr_id: expr.id,
                        arms: arm_terms,
                        result: expr_ty.clone(),
                        span: expr.span,
                    });
                }
            }
            ExprKind::Call { callee, args } => {
                // Calls are represented as obligations because overload/generic
                // resolution needs solver-time type information.
                self.collect_call(expr, callee, args);
            }
            ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => {
                let receiver_ty = self.collect_expr(callee, None);
                let arg_terms = args
                    .iter()
                    .map(|arg| self.collect_expr(&arg.expr, None))
                    .collect::<Vec<_>>();
                self.out.call_obligations.push(CallObligation {
                    call_node: expr.id,
                    span: expr.span,
                    caller_def_id: self.current_callable_def_id(),
                    callee: CallCallee::Method {
                        name: method_name.clone(),
                    },
                    callee_ty: None,
                    receiver: Some(receiver_ty),
                    arg_terms,
                    ret_ty: expr_ty.clone(),
                });
            }
            ExprKind::BinOp { left, right, .. } => {
                let left_ty = self.collect_expr(left, None);
                let right_ty = self.collect_expr(right, None);
                self.collect_binop_constraints(expr, left_ty, right_ty);
            }
            ExprKind::UnaryOp { op, expr: inner } => {
                let inner_expected = match op {
                    UnaryOp::Neg | UnaryOp::BitNot => expected.clone(),
                    UnaryOp::LogicalNot => None,
                    UnaryOp::Try => None,
                };
                let inner_ty = self.collect_expr(inner, inner_expected);
                match op {
                    UnaryOp::Neg | UnaryOp::BitNot => {
                        self.out.expr_obligations.push(ExprObligation::UnaryOp {
                            expr_id: expr.id,
                            op: *op,
                            operand: inner_ty.clone(),
                            result: expr_ty.clone(),
                            span: expr.span,
                        });
                        self.push_eq(
                            expr_ty.clone(),
                            inner_ty,
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                    }
                    UnaryOp::LogicalNot => {
                        self.out.expr_obligations.push(ExprObligation::UnaryOp {
                            expr_id: expr.id,
                            op: *op,
                            operand: inner_ty.clone(),
                            result: expr_ty.clone(),
                            span: expr.span,
                        });
                        self.push_eq(
                            inner_ty,
                            Type::Bool,
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                        self.push_eq(
                            expr_ty.clone(),
                            Type::Bool,
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                    }
                    UnaryOp::Try => {
                        self.out.expr_obligations.push(ExprObligation::Try {
                            expr_id: expr.id,
                            operand: inner_ty,
                            result: expr_ty.clone(),
                            expected_return_ty: self.current_return_ty(),
                            callable_def_id: self.current_callable_def_id(),
                            span: expr.span,
                        });
                    }
                }
            }
            ExprKind::Closure {
                def_id,
                params,
                return_ty,
                body,
                ..
            } => {
                let closure_sig = self.collect_closure_signature(params, return_ty);
                if let Some(sig) = closure_sig.as_ref() {
                    self.push_eq(
                        expr_ty.clone(),
                        sig.fn_ty.clone(),
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
                let closure_def_ty = self.def_term(*def_id);
                self.push_eq(
                    closure_def_ty,
                    expr_ty.clone(),
                    ConstraintReason::Decl(*def_id, expr.span),
                );

                let return_term = closure_sig
                    .as_ref()
                    .map(|sig| sig.ret_ty.clone())
                    .unwrap_or_else(|| self.fresh_var_term());

                self.enter_callable(*def_id, return_term.clone(), expr.span);
                for (index, param) in params.iter().enumerate() {
                    let param_term = self.def_term(param.def_id);
                    let sig_param_ty = closure_sig
                        .as_ref()
                        .and_then(|sig| sig.param_tys.get(index))
                        .cloned();
                    if let Some(param_ty) = sig_param_ty {
                        self.push_eq(
                            param_term.clone(),
                            param_ty,
                            ConstraintReason::Decl(param.def_id, param.span),
                        );
                    } else if let Ok(param_ty) = self.resolve_type_in_scope(&param.typ) {
                        self.push_eq(
                            param_term.clone(),
                            param_ty,
                            ConstraintReason::Decl(param.def_id, param.span),
                        );
                    }
                    let node_term = self.node_term(param.id);
                    self.push_eq(
                        node_term,
                        param_term,
                        ConstraintReason::Decl(param.def_id, param.span),
                    );
                }
                let body_ty = self.collect_expr(body, Some(return_term.clone()));
                self.push_assignable(
                    body_ty,
                    return_term,
                    ConstraintReason::Expr(body.id, body.span),
                );
                self.exit_callable(*def_id, expr.span);
            }
        }

        expr_ty
    }
}
