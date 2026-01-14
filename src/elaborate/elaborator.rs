use std::collections::HashSet;

use crate::ast::NodeIdGen;
use crate::ast::{CallArgMode, InitInfo, NodeId, ParamMode};
use crate::diag::Span;
use crate::nir::model as nsir;
use crate::sir::model as sir;
use crate::typeck::type_map::{CallParam, CallSig, TypeMap};
use crate::types::TypeId;

pub struct Elaborator<'a> {
    type_map: &'a TypeMap,
    node_id_gen: &'a mut NodeIdGen,
    implicit_moves: &'a HashSet<NodeId>,
    init_assigns: &'a HashSet<NodeId>,
    full_init_assigns: &'a HashSet<NodeId>,
}

impl<'a> Elaborator<'a> {
    pub fn new(
        type_map: &'a TypeMap,
        node_id_gen: &'a mut NodeIdGen,
        implicit_moves: &'a HashSet<NodeId>,
        init_assigns: &'a HashSet<NodeId>,
        full_init_assigns: &'a HashSet<NodeId>,
    ) -> Self {
        Self {
            type_map,
            node_id_gen,
            implicit_moves,
            init_assigns,
            full_init_assigns,
        }
    }

    pub fn elaborate_module(&mut self, module: &nsir::Module) -> sir::Module {
        let top_level_items = module
            .top_level_items
            .iter()
            .map(|item| self.elab_top_level_item(item))
            .collect();
        sir::Module { top_level_items }
    }

    fn elab_top_level_item(&mut self, item: &nsir::TopLevelItem) -> sir::TopLevelItem {
        match item {
            nsir::TopLevelItem::TypeDef(def) => sir::TopLevelItem::TypeDef(def.clone()),
            nsir::TopLevelItem::FuncDecl(decl) => sir::TopLevelItem::FuncDecl(sir::FuncDecl {
                id: decl.id,
                def_id: decl.def_id,
                sig: decl.sig.clone(),
                span: decl.span,
            }),
            nsir::TopLevelItem::FuncDef(def) => sir::TopLevelItem::FuncDef(sir::FuncDef {
                id: def.id,
                def_id: def.def_id,
                sig: def.sig.clone(),
                body: self.elab_value(&def.body),
                span: def.span,
            }),
            nsir::TopLevelItem::MethodBlock(block) => {
                sir::TopLevelItem::MethodBlock(sir::MethodBlock {
                    id: block.id,
                    type_name: block.type_name.clone(),
                    method_defs: block
                        .method_defs
                        .iter()
                        .map(|method| self.elab_method_def(method))
                        .collect(),
                    span: block.span,
                })
            }
            nsir::TopLevelItem::ClosureDecl(decl) => {
                sir::TopLevelItem::ClosureDecl(sir::ClosureDecl {
                    id: decl.id,
                    def_id: decl.def_id,
                    sig: decl.sig.clone(),
                    body: self.elab_value(&decl.body),
                    span: decl.span,
                })
            }
        }
    }

    fn elab_method_def(&mut self, def: &nsir::MethodDef) -> sir::MethodDef {
        sir::MethodDef {
            id: def.id,
            def_id: def.def_id,
            sig: def.sig.clone(),
            body: self.elab_value(&def.body),
            span: def.span,
        }
    }

    fn init_info_for_id(&self, id: NodeId) -> InitInfo {
        InitInfo {
            is_init: self.init_assigns.contains(&id),
            promotes_full: self.full_init_assigns.contains(&id),
        }
    }

    fn new_value(&mut self, kind: sir::ValueExprKind, ty: TypeId, span: Span) -> sir::ValueExpr {
        let id = self.node_id_gen.new_id();
        sir::ValueExpr { id, kind, ty, span }
    }

    fn elab_block_item(&mut self, item: &nsir::BlockItem) -> sir::BlockItem {
        match item {
            nsir::BlockItem::Stmt(stmt) => sir::BlockItem::Stmt(self.elab_stmt_expr(stmt)),
            nsir::BlockItem::Expr(expr) => sir::BlockItem::Expr(self.elab_value(expr)),
        }
    }

    fn elab_stmt_expr(&mut self, stmt: &nsir::StmtExpr) -> sir::StmtExpr {
        let kind = match &stmt.kind {
            nsir::StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => sir::StmtExprKind::LetBind {
                pattern: pattern.clone(),
                decl_ty: decl_ty.clone(),
                value: Box::new(self.elab_value(value)),
            },
            nsir::StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => sir::StmtExprKind::VarBind {
                pattern: pattern.clone(),
                decl_ty: decl_ty.clone(),
                value: Box::new(self.elab_value(value)),
            },
            nsir::StmtExprKind::VarDecl {
                ident,
                def_id,
                decl_ty,
            } => sir::StmtExprKind::VarDecl {
                ident: ident.clone(),
                def_id: *def_id,
                decl_ty: decl_ty.clone(),
            },
            nsir::StmtExprKind::Assign {
                assignee, value, ..
            } => {
                let place = self.elab_place(assignee);
                sir::StmtExprKind::Assign {
                    assignee: Box::new(place.clone()),
                    value: Box::new(self.elab_value(value)),
                    init: self.init_info_for_id(place.id),
                }
            }
            nsir::StmtExprKind::While { cond, body } => sir::StmtExprKind::While {
                cond: Box::new(self.elab_value(cond)),
                body: Box::new(self.elab_value(body)),
            },
            nsir::StmtExprKind::For {
                pattern,
                iter,
                body,
            } => sir::StmtExprKind::For {
                pattern: pattern.clone(),
                iter: Box::new(self.elab_value(iter)),
                body: Box::new(self.elab_value(body)),
            },
        };

        sir::StmtExpr {
            id: stmt.id,
            kind,
            span: stmt.span,
        }
    }

    fn elab_match_arm(&mut self, arm: &nsir::MatchArm) -> sir::MatchArm {
        sir::MatchArm {
            id: arm.id,
            pattern: arm.pattern.clone(),
            body: self.elab_value(&arm.body),
            span: arm.span,
        }
    }

    fn elab_string_fmt_segment(&mut self, seg: &nsir::StringFmtSegment) -> sir::StringFmtSegment {
        match seg {
            nsir::StringFmtSegment::Literal { value, span } => sir::StringFmtSegment::Literal {
                value: value.clone(),
                span: *span,
            },
            nsir::StringFmtSegment::Expr { expr, span } => sir::StringFmtSegment::Expr {
                expr: Box::new(self.elab_value(expr)),
                span: *span,
            },
        }
    }

    fn elab_array_lit_init(&mut self, init: &nsir::ArrayLitInit) -> sir::ArrayLitInit {
        match init {
            nsir::ArrayLitInit::Elems(elems) => {
                sir::ArrayLitInit::Elems(elems.iter().map(|elem| self.elab_value(elem)).collect())
            }
            nsir::ArrayLitInit::Repeat(elem, count) => {
                sir::ArrayLitInit::Repeat(Box::new(self.elab_value(elem)), *count)
            }
        }
    }

    fn elab_struct_lit_field(&mut self, field: &nsir::StructLitField) -> sir::StructLitField {
        sir::StructLitField {
            name: field.name.clone(),
            value: self.elab_value(&field.value),
            span: field.span,
        }
    }

    fn elab_struct_update_field(
        &mut self,
        field: &nsir::StructUpdateField,
    ) -> sir::StructUpdateField {
        sir::StructUpdateField {
            name: field.name.clone(),
            value: self.elab_value(&field.value),
            span: field.span,
        }
    }

    fn call_sig(&self, call_id: NodeId) -> CallSig {
        self.type_map
            .lookup_call_sig(call_id)
            .unwrap_or_else(|| panic!("compiler bug: missing call signature for {call_id:?}"))
    }

    fn elab_call_arg(&mut self, param: &CallParam, arg: &nsir::CallArg) -> sir::CallArg {
        match param.mode {
            ParamMode::In => sir::CallArg::In {
                expr: self.elab_value(&arg.expr),
                span: arg.span,
            },
            ParamMode::InOut => {
                if matches!(arg.expr.kind, nsir::ExprKind::Slice { .. }) {
                    return sir::CallArg::In {
                        expr: self.elab_value(&arg.expr),
                        span: arg.span,
                    };
                }
                sir::CallArg::InOut {
                    place: self.elab_place(&arg.expr),
                    span: arg.span,
                }
            }
            ParamMode::Out => {
                let place = self.elab_place(&arg.expr);
                sir::CallArg::Out {
                    init: self.init_info_for_id(place.id),
                    place,
                    span: arg.span,
                }
            }
            ParamMode::Sink => {
                let expr = if arg.mode == CallArgMode::Move {
                    let place = self.elab_place(&arg.expr);
                    self.new_value(
                        sir::ValueExprKind::Move {
                            place: Box::new(place),
                        },
                        arg.expr.ty,
                        arg.expr.span,
                    )
                } else {
                    self.elab_value(&arg.expr)
                };
                sir::CallArg::Sink {
                    expr,
                    span: arg.span,
                }
            }
        }
    }

    fn elab_method_receiver(
        &mut self,
        receiver: &CallParam,
        callee: &nsir::Expr,
    ) -> sir::MethodReceiver {
        match receiver.mode {
            ParamMode::InOut => sir::MethodReceiver::PlaceExpr(self.elab_place(callee)),
            ParamMode::In | ParamMode::Sink => {
                sir::MethodReceiver::ValueExpr(Box::new(self.elab_value(callee)))
            }
            ParamMode::Out => panic!("compiler bug: out self is not allowed"),
        }
    }

    fn elab_place(&mut self, expr: &nsir::Expr) -> sir::PlaceExpr {
        let kind = match &expr.kind {
            nsir::ExprKind::Var { ident, def_id } => sir::PlaceExprKind::Var {
                ident: ident.clone(),
                def_id: *def_id,
            },
            nsir::ExprKind::ArrayIndex { target, indices } => sir::PlaceExprKind::ArrayIndex {
                target: Box::new(self.elab_place(target)),
                indices: indices.iter().map(|idx| self.elab_value(idx)).collect(),
            },
            nsir::ExprKind::TupleField { target, index } => sir::PlaceExprKind::TupleField {
                target: Box::new(self.elab_place(target)),
                index: *index,
            },
            nsir::ExprKind::StructField { target, field } => sir::PlaceExprKind::StructField {
                target: Box::new(self.elab_place(target)),
                field: field.clone(),
            },
            _ => panic!(
                "compiler bug: invalid place expression in SIR: {:?}",
                expr.kind
            ),
        };

        sir::PlaceExpr {
            id: expr.id,
            kind,
            ty: expr.ty,
            span: expr.span,
        }
    }

    fn elab_value(&mut self, expr: &nsir::Expr) -> sir::ValueExpr {
        match &expr.kind {
            nsir::ExprKind::Var { .. }
            | nsir::ExprKind::ArrayIndex { .. }
            | nsir::ExprKind::TupleField { .. }
            | nsir::ExprKind::StructField { .. } => {
                let place = self.elab_place(expr);
                if self.implicit_moves.contains(&expr.id) {
                    return self.new_value(
                        sir::ValueExprKind::ImplicitMove {
                            place: Box::new(place),
                        },
                        expr.ty,
                        expr.span,
                    );
                }
                return self.new_value(
                    sir::ValueExprKind::Load {
                        place: Box::new(place),
                    },
                    expr.ty,
                    expr.span,
                );
            }
            nsir::ExprKind::Move { expr: inner } => {
                let place = self.elab_place(inner);
                return sir::ValueExpr {
                    id: expr.id,
                    kind: sir::ValueExprKind::Move {
                        place: Box::new(place),
                    },
                    ty: expr.ty,
                    span: expr.span,
                };
            }
            nsir::ExprKind::ImplicitMove { expr: inner } => {
                let place = self.elab_place(inner);
                return sir::ValueExpr {
                    id: expr.id,
                    kind: sir::ValueExprKind::ImplicitMove {
                        place: Box::new(place),
                    },
                    ty: expr.ty,
                    span: expr.span,
                };
            }
            _ => {}
        }

        let kind = match &expr.kind {
            nsir::ExprKind::Block { items, tail } => sir::ValueExprKind::Block {
                items: items
                    .iter()
                    .map(|item| self.elab_block_item(item))
                    .collect(),
                tail: tail.as_ref().map(|value| Box::new(self.elab_value(value))),
            },
            nsir::ExprKind::UnitLit => sir::ValueExprKind::UnitLit,
            nsir::ExprKind::IntLit(value) => sir::ValueExprKind::IntLit(*value),
            nsir::ExprKind::BoolLit(value) => sir::ValueExprKind::BoolLit(*value),
            nsir::ExprKind::CharLit(value) => sir::ValueExprKind::CharLit(*value),
            nsir::ExprKind::StringLit { value } => sir::ValueExprKind::StringLit {
                value: value.clone(),
            },
            nsir::ExprKind::StringFmt { segments } => sir::ValueExprKind::StringFmt {
                segments: segments
                    .iter()
                    .map(|seg| self.elab_string_fmt_segment(seg))
                    .collect(),
            },
            nsir::ExprKind::ArrayLit { elem_ty, init } => sir::ValueExprKind::ArrayLit {
                elem_ty: elem_ty.clone(),
                init: self.elab_array_lit_init(init),
            },
            nsir::ExprKind::TupleLit(items) => sir::ValueExprKind::TupleLit(
                items.iter().map(|item| self.elab_value(item)).collect(),
            ),
            nsir::ExprKind::StructLit { name, fields } => sir::ValueExprKind::StructLit {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|field| self.elab_struct_lit_field(field))
                    .collect(),
            },
            nsir::ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            } => sir::ValueExprKind::EnumVariant {
                enum_name: enum_name.clone(),
                variant: variant.clone(),
                payload: payload.iter().map(|value| self.elab_value(value)).collect(),
            },
            nsir::ExprKind::StructUpdate { target, fields } => sir::ValueExprKind::StructUpdate {
                target: Box::new(self.elab_value(target)),
                fields: fields
                    .iter()
                    .map(|field| self.elab_struct_update_field(field))
                    .collect(),
            },
            nsir::ExprKind::BinOp { left, op, right } => sir::ValueExprKind::BinOp {
                left: Box::new(self.elab_value(left)),
                op: *op,
                right: Box::new(self.elab_value(right)),
            },
            nsir::ExprKind::UnaryOp { op, expr } => sir::ValueExprKind::UnaryOp {
                op: *op,
                expr: Box::new(self.elab_value(expr)),
            },
            nsir::ExprKind::HeapAlloc { expr } => sir::ValueExprKind::HeapAlloc {
                expr: Box::new(self.elab_value(expr)),
            },
            nsir::ExprKind::If {
                cond,
                then_body,
                else_body,
            } => sir::ValueExprKind::If {
                cond: Box::new(self.elab_value(cond)),
                then_body: Box::new(self.elab_value(then_body)),
                else_body: Box::new(self.elab_value(else_body)),
            },
            nsir::ExprKind::Range { start, end } => sir::ValueExprKind::Range {
                start: *start,
                end: *end,
            },
            nsir::ExprKind::Slice { target, start, end } => sir::ValueExprKind::Slice {
                target: Box::new(self.elab_place(target)),
                start: start.as_ref().map(|expr| Box::new(self.elab_value(expr))),
                end: end.as_ref().map(|expr| Box::new(self.elab_value(expr))),
            },
            nsir::ExprKind::Match { scrutinee, arms } => sir::ValueExprKind::Match {
                scrutinee: Box::new(self.elab_value(scrutinee)),
                arms: arms.iter().map(|arm| self.elab_match_arm(arm)).collect(),
            },
            nsir::ExprKind::Call { callee, args } => {
                let call_sig = self.call_sig(expr.id);
                let args = call_sig
                    .params
                    .iter()
                    .zip(args.iter())
                    .map(|(param, arg)| self.elab_call_arg(param, arg))
                    .collect();
                sir::ValueExprKind::Call {
                    callee: Box::new(self.elab_value(callee)),
                    args,
                }
            }
            nsir::ExprKind::MethodCall {
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
                sir::ValueExprKind::MethodCall {
                    receiver,
                    method_name: method_name.clone(),
                    args,
                }
            }
            nsir::ExprKind::Closure {
                ident,
                def_id,
                params,
                return_ty,
                body,
            } => sir::ValueExprKind::Closure {
                ident: ident.clone(),
                def_id: *def_id,
                params: params.clone(),
                return_ty: return_ty.clone(),
                body: Box::new(self.elab_value(body)),
            },
            nsir::ExprKind::Coerce { kind, expr } => sir::ValueExprKind::Coerce {
                kind: *kind,
                expr: Box::new(self.elab_value(expr)),
            },
            nsir::ExprKind::Move { .. }
            | nsir::ExprKind::ImplicitMove { .. }
            | nsir::ExprKind::Var { .. }
            | nsir::ExprKind::ArrayIndex { .. }
            | nsir::ExprKind::TupleField { .. }
            | nsir::ExprKind::StructField { .. } => {
                unreachable!("handled earlier")
            }
        };

        sir::ValueExpr {
            id: expr.id,
            kind,
            ty: expr.ty,
            span: expr.span,
        }
    }
}
