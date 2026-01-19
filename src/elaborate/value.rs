use super::elaborator::Elaborator;
use crate::diag::Span;
use crate::resolve::{DefId, DefKind};
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::tree::{BinaryOp, ParamMode};
use crate::typeck::type_map::{CallParam, CallSig};
use crate::types::Type;

const MAX_U64_DEC_LEN: usize = 20;

struct ForLocal {
    def_id: DefId,
    name: String,
    ty: Type,
    pattern: sem::BindPattern,
}

impl<'a> Elaborator<'a> {
    fn elab_block_item(&mut self, item: &norm::BlockItem) -> sem::BlockItem {
        match item {
            norm::BlockItem::Stmt(stmt) => match &stmt.kind {
                norm::StmtExprKind::For {
                    pattern,
                    iter,
                    body,
                } => sem::BlockItem::Expr(self.elab_for_expr(stmt, pattern, iter, body)),
                _ => sem::BlockItem::Stmt(self.elab_stmt_expr(stmt)),
            },
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
            norm::StmtExprKind::For { .. } => {
                unreachable!("for loops should be desugared before statement elaboration")
            }
            norm::StmtExprKind::Break => sem::StmtExprKind::Break,
            norm::StmtExprKind::Continue => sem::StmtExprKind::Continue,
            norm::StmtExprKind::Return { value } => sem::StmtExprKind::Return {
                value: value.as_ref().map(|expr| Box::new(self.elab_value(expr))),
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

    fn elab_string_fmt_plan(&mut self, segments: &[norm::StringFmtSegment]) -> sem::StringFmtPlan {
        let mut plan_segments = Vec::with_capacity(segments.len());
        let mut reserve_terms = Vec::new();

        for segment in segments {
            match segment {
                norm::StringFmtSegment::Literal { value, .. } => {
                    // Literal bytes contribute directly to both plan segments and reserve length.
                    plan_segments.push(sem::SegmentKind::LiteralBytes(value.clone()));
                    reserve_terms.push(sem::LenTerm::Literal(value.len()));
                }
                norm::StringFmtSegment::Expr { expr, .. } => {
                    if let norm::ExprKind::StringLit { value } = &expr.kind {
                        // Inline string literals as literal bytes for cheaper view formatting.
                        plan_segments.push(sem::SegmentKind::LiteralBytes(value.clone()));
                        reserve_terms.push(sem::LenTerm::Literal(value.len()));
                        continue;
                    }

                    let ty = self.type_map.type_table().get(expr.ty).clone();
                    match ty {
                        Type::String => {
                            // String values require owned formatting and a dynamic reserve term.
                            let expr = self.elab_value(expr);
                            let segment_index = plan_segments.len();
                            plan_segments.push(sem::SegmentKind::StringValue {
                                expr: Box::new(expr),
                            });
                            reserve_terms.push(sem::LenTerm::StringValue { segment_index });
                        }
                        Type::Int { signed, bits } => {
                            // Integers contribute a conservative literal reserve bound.
                            plan_segments.push(sem::SegmentKind::Int {
                                expr: Box::new(self.elab_value(expr)),
                                signed,
                                bits,
                            });
                            reserve_terms.push(sem::LenTerm::Literal(MAX_U64_DEC_LEN));
                        }
                        _ => {
                            panic!("compiler bug: unsupported f-string expr type");
                        }
                    }
                }
            }
        }

        // Any dynamic string segment forces the owned formatter path.
        let kind = if plan_segments
            .iter()
            .any(|segment| matches!(segment, sem::SegmentKind::StringValue { .. }))
        {
            sem::FmtKind::Owned
        } else {
            sem::FmtKind::View
        };

        sem::StringFmtPlan {
            kind,
            segments: plan_segments,
            reserve_terms,
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

    fn elab_for_expr(
        &mut self,
        stmt: &norm::StmtExpr,
        pattern: &norm::BindPattern,
        iter: &norm::Expr,
        body: &norm::Expr,
    ) -> sem::ValueExpr {
        let span = stmt.span;
        let u64_ty = Type::uint(64);
        let bool_ty = Type::Bool;

        let mut items = Vec::new();

        let (iter_place, idx_place, len_value, elem_ty, is_range) = match &iter.kind {
            norm::ExprKind::Range { start, end } => {
                let len_expr = self.make_u64_lit(*end, span);
                let start_expr = self.make_u64_lit(*start, span);

                let idx_info = self.new_for_local("idx", u64_ty.clone(), true, span);
                let idx_stmt = self.make_var_bind_stmt(idx_info.pattern.clone(), start_expr, span);
                items.push(sem::BlockItem::Stmt(idx_stmt));

                let len_info = self.new_for_local("len", u64_ty.clone(), false, span);
                let len_stmt = self.make_let_bind_stmt(len_info.pattern.clone(), len_expr, span);
                items.push(sem::BlockItem::Stmt(len_stmt));

                let idx_place = self.make_var_place(&idx_info, span);
                let len_place = self.make_var_place(&len_info, span);
                (
                    None,
                    idx_place,
                    self.make_len_load(len_place, span),
                    u64_ty.clone(),
                    true,
                )
            }
            _ => {
                let iter_ty = self.type_map.type_table().get(iter.ty).clone();
                let elem_ty = match &iter_ty {
                    Type::Array { .. } => iter_ty
                        .array_item_type()
                        .unwrap_or_else(|| panic!("compiler bug: empty array dims")),
                    Type::Slice { elem_ty } => (**elem_ty).clone(),
                    _ => panic!("compiler bug: invalid for-iter type"),
                };

                let iter_info = self.new_for_local("iter", iter_ty.clone(), false, span);
                let iter_value = self.elab_value(iter);
                let iter_stmt =
                    self.make_let_bind_stmt(iter_info.pattern.clone(), iter_value, span);
                items.push(sem::BlockItem::Stmt(iter_stmt));

                let len_expr = match iter_ty {
                    Type::Array { dims, .. } => {
                        let len = dims
                            .first()
                            .copied()
                            .unwrap_or_else(|| panic!("compiler bug: empty array dims"));
                        self.make_u64_lit(len as u64, span)
                    }
                    Type::Slice { .. } => {
                        let iter_place = self.make_var_place(&iter_info, span);
                        self.make_len_expr(iter_place, span)
                    }
                    _ => unreachable!("checked above"),
                };

                let len_info = self.new_for_local("len", u64_ty.clone(), false, span);
                let len_stmt = self.make_let_bind_stmt(len_info.pattern.clone(), len_expr, span);
                items.push(sem::BlockItem::Stmt(len_stmt));

                let idx_info = self.new_for_local("idx", u64_ty.clone(), true, span);
                let idx_init = self.make_u64_lit(0, span);
                let idx_stmt = self.make_var_bind_stmt(idx_info.pattern.clone(), idx_init, span);
                items.push(sem::BlockItem::Stmt(idx_stmt));

                let idx_place = self.make_var_place(&idx_info, span);
                let len_place = self.make_var_place(&len_info, span);
                let iter_place = self.make_var_place(&iter_info, span);
                (
                    Some(iter_place),
                    idx_place,
                    self.make_len_load(len_place, span),
                    elem_ty,
                    false,
                )
            }
        };

        let idx_load = self.make_load_expr(idx_place.clone(), u64_ty.clone(), span);
        let cond_expr = self.make_binop_expr(BinaryOp::Lt, idx_load, len_value, bool_ty, span);

        let mut loop_items = Vec::new();

        let cur_info = self.new_for_local("cur", u64_ty.clone(), false, span);
        let cur_load = self.make_load_expr(idx_place.clone(), u64_ty.clone(), span);
        let cur_stmt = self.make_let_bind_stmt(cur_info.pattern.clone(), cur_load, span);
        loop_items.push(sem::BlockItem::Stmt(cur_stmt));

        // Increment before the body so `continue` doesn't skip the step.
        let idx_next = {
            let idx_val = self.make_load_expr(idx_place.clone(), u64_ty.clone(), span);
            let one = self.make_u64_lit(1, span);
            self.make_binop_expr(BinaryOp::Add, idx_val, one, u64_ty.clone(), span)
        };
        let idx_assign = self.make_assign_stmt(idx_place.clone(), idx_next, span);
        loop_items.push(sem::BlockItem::Stmt(idx_assign));

        let elem_expr = if is_range {
            let cur_place = self.make_var_place(&cur_info, span);
            self.make_load_expr(cur_place, u64_ty.clone(), span)
        } else {
            let iter_place = iter_place
                .unwrap_or_else(|| panic!("compiler bug: missing iter place for array/slice loop"));
            let cur_place = self.make_var_place(&cur_info, span);
            let cur_value = self.make_load_expr(cur_place, u64_ty.clone(), span);
            let index_place = self.make_index_place(iter_place, cur_value, elem_ty.clone(), span);
            self.make_load_expr(index_place, elem_ty.clone(), span)
        };

        let pattern_stmt = self.make_let_bind_stmt(pattern.clone(), elem_expr, span);
        loop_items.push(sem::BlockItem::Stmt(pattern_stmt));

        loop_items.push(sem::BlockItem::Expr(self.elab_value(body)));

        let loop_body = self.make_block_expr(loop_items, span);
        let while_stmt = self.make_stmt(
            sem::StmtExprKind::While {
                cond: Box::new(cond_expr),
                body: Box::new(loop_body),
            },
            span,
        );
        items.push(sem::BlockItem::Stmt(while_stmt));

        self.make_block_expr(items, span)
    }

    fn new_for_local(&mut self, suffix: &str, ty: Type, is_mutable: bool, span: Span) -> ForLocal {
        let def_id = self.def_table.next_def_id();
        let name = format!("__for_{}_{}", suffix, def_id.0);
        let def_id = self.def_table.add_def(
            name.clone(),
            DefKind::LocalVar {
                nrvo_eligible: false,
                is_mutable,
            },
        );
        if let Some(def) = self.def_table.lookup_def(def_id) {
            let _ = self.type_map.insert_def_type(def.clone(), ty.clone());
        }
        let pattern = sem::BindPattern {
            id: self.node_id_gen.new_id(),
            kind: sem::BindPatternKind::Name {
                ident: name.clone(),
                def_id,
            },
            span,
        };
        ForLocal {
            def_id,
            name,
            ty,
            pattern,
        }
    }

    fn make_var_place(&mut self, info: &ForLocal, span: Span) -> sem::PlaceExpr {
        self.make_place_expr(
            sem::PlaceExprKind::Var {
                ident: info.name.clone(),
                def_id: info.def_id,
            },
            info.ty.clone(),
            span,
        )
    }

    fn make_len_expr(&mut self, place: sem::PlaceExpr, span: Span) -> sem::ValueExpr {
        self.make_value_expr(
            sem::ValueExprKind::Len {
                place: Box::new(place),
            },
            Type::uint(64),
            span,
        )
    }

    fn make_len_load(&mut self, place: sem::PlaceExpr, span: Span) -> sem::ValueExpr {
        self.make_load_expr(place, Type::uint(64), span)
    }

    fn make_u64_lit(&mut self, value: u64, span: Span) -> sem::ValueExpr {
        self.make_value_expr(sem::ValueExprKind::IntLit(value), Type::uint(64), span)
    }

    fn make_binop_expr(
        &mut self,
        op: BinaryOp,
        left: sem::ValueExpr,
        right: sem::ValueExpr,
        result_ty: Type,
        span: Span,
    ) -> sem::ValueExpr {
        self.make_value_expr(
            sem::ValueExprKind::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            result_ty,
            span,
        )
    }

    fn make_index_place(
        &mut self,
        target: sem::PlaceExpr,
        index: sem::ValueExpr,
        elem_ty: Type,
        span: Span,
    ) -> sem::PlaceExpr {
        self.make_place_expr(
            sem::PlaceExprKind::ArrayIndex {
                target: Box::new(target),
                indices: vec![index],
            },
            elem_ty,
            span,
        )
    }

    fn make_load_expr(&mut self, place: sem::PlaceExpr, ty: Type, span: Span) -> sem::ValueExpr {
        self.make_value_expr(
            sem::ValueExprKind::Load {
                place: Box::new(place),
            },
            ty,
            span,
        )
    }

    fn make_let_bind_stmt(
        &mut self,
        pattern: sem::BindPattern,
        value: sem::ValueExpr,
        span: Span,
    ) -> sem::StmtExpr {
        self.make_stmt(
            sem::StmtExprKind::LetBind {
                pattern,
                decl_ty: None,
                value: Box::new(value),
            },
            span,
        )
    }

    fn make_var_bind_stmt(
        &mut self,
        pattern: sem::BindPattern,
        value: sem::ValueExpr,
        span: Span,
    ) -> sem::StmtExpr {
        self.make_stmt(
            sem::StmtExprKind::VarBind {
                pattern,
                decl_ty: None,
                value: Box::new(value),
            },
            span,
        )
    }

    fn make_assign_stmt(
        &mut self,
        assignee: sem::PlaceExpr,
        value: sem::ValueExpr,
        span: Span,
    ) -> sem::StmtExpr {
        self.make_stmt(
            sem::StmtExprKind::Assign {
                assignee: Box::new(assignee.clone()),
                value: Box::new(value),
                init: self.init_info_for_id(assignee.id),
            },
            span,
        )
    }

    fn make_block_expr(&mut self, items: Vec<sem::BlockItem>, span: Span) -> sem::ValueExpr {
        self.make_value_expr(
            sem::ValueExprKind::Block { items, tail: None },
            Type::Unit,
            span,
        )
    }

    fn make_stmt(&mut self, kind: sem::StmtExprKind, span: Span) -> sem::StmtExpr {
        sem::StmtExpr {
            id: self.node_id_gen.new_id(),
            kind,
            span,
        }
    }

    fn make_value_expr(
        &mut self,
        kind: sem::ValueExprKind,
        ty: Type,
        span: Span,
    ) -> sem::ValueExpr {
        let id = self.node_id_gen.new_id();
        let ty_id = self.type_map.insert_node_type(id, ty);
        sem::ValueExpr {
            id,
            kind,
            ty: ty_id,
            span,
        }
    }

    fn make_place_expr(
        &mut self,
        kind: sem::PlaceExprKind,
        ty: Type,
        span: Span,
    ) -> sem::PlaceExpr {
        let id = self.node_id_gen.new_id();
        let ty_id = self.type_map.insert_node_type(id, ty);
        sem::PlaceExpr {
            id,
            kind,
            ty: ty_id,
            span,
        }
    }

    pub(super) fn elab_value(&mut self, expr: &norm::Expr) -> sem::ValueExpr {
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

        let info =
            self.ensure_closure_info(ident, *def_id, params, return_ty, body, expr.span, expr.id);
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
            norm::ExprKind::Slice { target, start, end } => sem::ValueExprKind::Slice {
                target: Box::new(self.elab_place(target)),
                start: start.as_ref().map(|expr| Box::new(self.elab_value(expr))),
                end: end.as_ref().map(|expr| Box::new(self.elab_value(expr))),
            },
            norm::ExprKind::Match { scrutinee, arms } => sem::ValueExprKind::Match {
                scrutinee: Box::new(self.elab_value(scrutinee)),
                arms: arms.iter().map(|arm| self.elab_match_arm(arm)).collect(),
            },
            norm::ExprKind::Coerce { kind, expr } => sem::ValueExprKind::Coerce {
                kind: *kind,
                expr: Box::new(self.elab_value(expr)),
            },
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
}
