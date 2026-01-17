use super::elaborator::Elaborator;
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;

impl<'a> Elaborator<'a> {
    fn elab_block_item(&mut self, item: &norm::BlockItem) -> sem::BlockItem {
        match item {
            norm::BlockItem::Stmt(stmt) => sem::BlockItem::Stmt(self.elab_stmt_expr(stmt)),
            norm::BlockItem::Expr(expr) => sem::BlockItem::Expr(self.elab_value(expr)),
        }
    }

    fn elab_stmt_expr(&mut self, stmt: &norm::StmtExpr) -> sem::StmtExpr {
        let is_let = matches!(stmt.kind, norm::StmtExprKind::LetBind { .. });
        let kind = match &stmt.kind {
            norm::StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            }
            | norm::StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                let value = self.elab_bind_value(pattern, value);
                if is_let {
                    sem::StmtExprKind::LetBind {
                        pattern: pattern.clone(),
                        decl_ty: decl_ty.clone(),
                        value,
                    }
                } else {
                    sem::StmtExprKind::VarBind {
                        pattern: pattern.clone(),
                        decl_ty: decl_ty.clone(),
                        value,
                    }
                }
            }
            norm::StmtExprKind::VarDecl {
                ident,
                def_id,
                decl_ty,
            } => sem::StmtExprKind::VarDecl {
                ident: ident.clone(),
                def_id: *def_id,
                decl_ty: decl_ty.clone(),
            },
            norm::StmtExprKind::Assign {
                assignee, value, ..
            } => {
                let place = self.elab_place(assignee);
                sem::StmtExprKind::Assign {
                    assignee: Box::new(place.clone()),
                    value: Box::new(self.elab_value(value)),
                    init: self.init_info_for_id(place.id),
                }
            }
            norm::StmtExprKind::While { cond, body } => sem::StmtExprKind::While {
                cond: Box::new(self.elab_value(cond)),
                body: Box::new(self.elab_value(body)),
            },
            norm::StmtExprKind::For {
                pattern,
                iter,
                body,
            } => sem::StmtExprKind::For {
                pattern: pattern.clone(),
                iter: Box::new(self.elab_value(iter)),
                body: Box::new(self.elab_value(body)),
            },
        };

        sem::StmtExpr {
            id: stmt.id,
            kind,
            span: stmt.span,
        }
    }

    fn elab_bind_value(
        &mut self,
        pattern: &norm::BindPattern,
        value: &norm::Expr,
    ) -> Box<sem::ValueExpr> {
        if let norm::ExprKind::Closure {
            ident,
            def_id,
            params,
            return_ty,
            body,
            captures: _,
        } = &value.kind
        {
            let info = self.ensure_closure_info(
                ident, *def_id, params, return_ty, body, value.span, value.id,
            );
            self.record_closure_binding(pattern, *def_id, &info);
        }
        Box::new(self.elab_value(value))
    }

    fn elab_match_arm(&mut self, arm: &norm::MatchArm) -> sem::MatchArm {
        sem::MatchArm {
            id: arm.id,
            pattern: arm.pattern.clone(),
            body: self.elab_value(&arm.body),
            span: arm.span,
        }
    }

    fn elab_string_fmt_segment(&mut self, seg: &norm::StringFmtSegment) -> sem::StringFmtSegment {
        match seg {
            norm::StringFmtSegment::Literal { value, span } => sem::StringFmtSegment::Literal {
                value: value.clone(),
                span: *span,
            },
            norm::StringFmtSegment::Expr { expr, span } => sem::StringFmtSegment::Expr {
                expr: Box::new(self.elab_value(expr)),
                span: *span,
            },
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

    pub(super) fn elab_value(&mut self, expr: &norm::Expr) -> sem::ValueExpr {
        // First handle lvalue forms so we can wrap them as load/move/implicit-move.
        match &expr.kind {
            norm::ExprKind::Var { .. }
            | norm::ExprKind::ArrayIndex { .. }
            | norm::ExprKind::TupleField { .. }
            | norm::ExprKind::StructField { .. }
            | norm::ExprKind::Deref { .. } => {
                let place = self.elab_place(expr);
                let place_ty = place.ty;
                if self.implicit_moves.contains(&expr.id) {
                    return self.new_value(
                        sem::ValueExprKind::ImplicitMove {
                            place: Box::new(place),
                        },
                        place_ty,
                        expr.span,
                    );
                }
                return self.new_value(
                    sem::ValueExprKind::Load {
                        place: Box::new(place),
                    },
                    place_ty,
                    expr.span,
                );
            }
            norm::ExprKind::Move { expr: inner } => {
                let place = self.elab_place(inner);
                let place_ty = place.ty;
                return sem::ValueExpr {
                    id: expr.id,
                    kind: sem::ValueExprKind::Move {
                        place: Box::new(place),
                    },
                    ty: place_ty,
                    span: expr.span,
                };
            }
            norm::ExprKind::ImplicitMove { expr: inner } => {
                let place = self.elab_place(inner);
                let place_ty = place.ty;
                return sem::ValueExpr {
                    id: expr.id,
                    kind: sem::ValueExprKind::ImplicitMove {
                        place: Box::new(place),
                    },
                    ty: place_ty,
                    span: expr.span,
                };
            }
            _ => {}
        }

        let kind = match &expr.kind {
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
                segments: segments
                    .iter()
                    .map(|seg| self.elab_string_fmt_segment(seg))
                    .collect(),
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
            norm::ExprKind::Slice { target, start, end } => sem::ValueExprKind::Slice {
                target: Box::new(self.elab_place(target)),
                start: start.as_ref().map(|expr| Box::new(self.elab_value(expr))),
                end: end.as_ref().map(|expr| Box::new(self.elab_value(expr))),
            },
            norm::ExprKind::Match { scrutinee, arms } => sem::ValueExprKind::Match {
                scrutinee: Box::new(self.elab_value(scrutinee)),
                arms: arms.iter().map(|arm| self.elab_match_arm(arm)).collect(),
            },
            norm::ExprKind::Call { callee, args } => {
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
                    let receiver =
                        sem::MethodReceiver::ValueExpr(Box::new(self.elab_value(callee)));
                    let args = info
                        .param_modes
                        .iter()
                        .zip(args.iter())
                        .map(|(mode, arg)| self.elab_call_arg_mode(mode.clone(), arg))
                        .collect();
                    self.type_map.insert_call_def(expr.id, closure_def_id);
                    sem::ValueExprKind::MethodCall {
                        receiver,
                        method_name: "invoke".to_string(),
                        args,
                    }
                } else {
                    let call_sig = self.call_sig(expr.id);
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
            norm::ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => {
                let call_sig = self.call_sig(expr.id);
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
                sem::ValueExprKind::MethodCall {
                    receiver,
                    method_name: method_name.clone(),
                    args,
                }
            }
            norm::ExprKind::Closure {
                ident,
                def_id,
                params,
                return_ty,
                body,
                captures: _,
            } => {
                let info = self.ensure_closure_info(
                    ident, *def_id, params, return_ty, body, expr.span, expr.id,
                );
                let ty_id = self.type_map.insert_node_type(expr.id, info.ty.clone());
                // Closure literals become struct literals with capture fields.
                let fields = info
                    .captures
                    .iter()
                    .map(|capture| sem::StructLitField {
                        name: capture.name.clone(),
                        value: self.capture_value_for_def(capture, expr.span),
                        span: expr.span,
                    })
                    .collect();
                return sem::ValueExpr {
                    id: expr.id,
                    kind: sem::ValueExprKind::StructLit {
                        name: info.type_name,
                        fields,
                    },
                    ty: ty_id,
                    span: expr.span,
                };
            }
            norm::ExprKind::Coerce { kind, expr } => sem::ValueExprKind::Coerce {
                kind: *kind,
                expr: Box::new(self.elab_value(expr)),
            },
            norm::ExprKind::AddrOf { expr } => sem::ValueExprKind::AddrOf {
                place: Box::new(self.elab_place(expr)),
            },
            norm::ExprKind::Move { .. }
            | norm::ExprKind::ImplicitMove { .. }
            | norm::ExprKind::Var { .. }
            | norm::ExprKind::ArrayIndex { .. }
            | norm::ExprKind::TupleField { .. }
            | norm::ExprKind::StructField { .. }
            | norm::ExprKind::Deref { .. } => {
                unreachable!("handled earlier")
            }
        };

        sem::ValueExpr {
            id: expr.id,
            kind,
            ty: expr.ty,
            span: expr.span,
        }
    }
}
