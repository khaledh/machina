use std::collections::HashSet;

use crate::diag::Span;
use crate::tree::NodeIdGen;
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::tree::{CallArgMode, InitInfo, NodeId, ParamMode};
use crate::typeck::type_map::{CallParam, CallSig, TypeMap};
use crate::types::TypeId;

pub struct Elaborator<'a> {
    type_map: &'a TypeMap,
    node_id_gen: &'a mut NodeIdGen,
    implicit_moves: &'a HashSet<NodeId>,
    init_assigns: &'a HashSet<NodeId>,
    full_init_assigns: &'a HashSet<NodeId>,
    closure_defs: Vec<sem::ClosureDef>,
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
            closure_defs: Vec::new(),
        }
    }

    pub fn elaborate_module(&mut self, module: &norm::Module) -> sem::Module {
        // Lift closures to top level
        self.closure_defs.clear();
        let mut top_level_items: Vec<_> = module
            .top_level_items
            .iter()
            .map(|item| self.elab_top_level_item(item))
            .collect();
        top_level_items.extend(
            self.closure_defs
                .drain(..)
                .map(sem::TopLevelItem::ClosureDef),
        );
        sem::Module { top_level_items }
    }

    fn elab_top_level_item(&mut self, item: &norm::TopLevelItem) -> sem::TopLevelItem {
        match item {
            norm::TopLevelItem::TypeDef(def) => sem::TopLevelItem::TypeDef(def.clone()),
            norm::TopLevelItem::FuncDecl(decl) => sem::TopLevelItem::FuncDecl(sem::FuncDecl {
                id: decl.id,
                def_id: decl.def_id,
                sig: decl.sig.clone(),
                span: decl.span,
            }),
            norm::TopLevelItem::FuncDef(def) => sem::TopLevelItem::FuncDef(sem::FuncDef {
                id: def.id,
                def_id: def.def_id,
                sig: def.sig.clone(),
                body: self.elab_value(&def.body),
                span: def.span,
            }),
            norm::TopLevelItem::MethodBlock(block) => {
                sem::TopLevelItem::MethodBlock(sem::MethodBlock {
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
            norm::TopLevelItem::ClosureDef(_) => {
                panic!("compiler bug: closure definitions should be synthesized in elaborate")
            }
        }
    }

    fn elab_method_def(&mut self, def: &norm::MethodDef) -> sem::MethodDef {
        sem::MethodDef {
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

    fn new_value(&mut self, kind: sem::ValueExprKind, ty: TypeId, span: Span) -> sem::ValueExpr {
        let id = self.node_id_gen.new_id();
        sem::ValueExpr { id, kind, ty, span }
    }

    fn elab_block_item(&mut self, item: &norm::BlockItem) -> sem::BlockItem {
        match item {
            norm::BlockItem::Stmt(stmt) => sem::BlockItem::Stmt(self.elab_stmt_expr(stmt)),
            norm::BlockItem::Expr(expr) => sem::BlockItem::Expr(self.elab_value(expr)),
        }
    }

    fn elab_stmt_expr(&mut self, stmt: &norm::StmtExpr) -> sem::StmtExpr {
        let kind = match &stmt.kind {
            norm::StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => sem::StmtExprKind::LetBind {
                pattern: pattern.clone(),
                decl_ty: decl_ty.clone(),
                value: Box::new(self.elab_value(value)),
            },
            norm::StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => sem::StmtExprKind::VarBind {
                pattern: pattern.clone(),
                decl_ty: decl_ty.clone(),
                value: Box::new(self.elab_value(value)),
            },
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

    fn call_sig(&self, call_id: NodeId) -> CallSig {
        self.type_map
            .lookup_call_sig(call_id)
            .unwrap_or_else(|| panic!("compiler bug: missing call signature for {call_id:?}"))
    }

    fn elab_call_arg(&mut self, param: &CallParam, arg: &norm::CallArg) -> sem::CallArg {
        match param.mode {
            ParamMode::In => sem::CallArg::In {
                expr: self.elab_value(&arg.expr),
                span: arg.span,
            },
            ParamMode::InOut => {
                if matches!(arg.expr.kind, norm::ExprKind::Slice { .. }) {
                    return sem::CallArg::In {
                        expr: self.elab_value(&arg.expr),
                        span: arg.span,
                    };
                }
                sem::CallArg::InOut {
                    place: self.elab_place(&arg.expr),
                    span: arg.span,
                }
            }
            ParamMode::Out => {
                let place = self.elab_place(&arg.expr);
                sem::CallArg::Out {
                    init: self.init_info_for_id(place.id),
                    place,
                    span: arg.span,
                }
            }
            ParamMode::Sink => {
                let expr = if arg.mode == CallArgMode::Move {
                    let place = self.elab_place(&arg.expr);
                    self.new_value(
                        sem::ValueExprKind::Move {
                            place: Box::new(place),
                        },
                        arg.expr.ty,
                        arg.expr.span,
                    )
                } else {
                    self.elab_value(&arg.expr)
                };
                sem::CallArg::Sink {
                    expr,
                    span: arg.span,
                }
            }
        }
    }

    fn elab_method_receiver(
        &mut self,
        receiver: &CallParam,
        callee: &norm::Expr,
    ) -> sem::MethodReceiver {
        match receiver.mode {
            ParamMode::InOut => sem::MethodReceiver::PlaceExpr(self.elab_place(callee)),
            ParamMode::In | ParamMode::Sink => {
                sem::MethodReceiver::ValueExpr(Box::new(self.elab_value(callee)))
            }
            ParamMode::Out => panic!("compiler bug: out self is not allowed"),
        }
    }

    fn elab_place(&mut self, expr: &norm::Expr) -> sem::PlaceExpr {
        let kind = match &expr.kind {
            norm::ExprKind::Var { ident, def_id } => sem::PlaceExprKind::Var {
                ident: ident.clone(),
                def_id: *def_id,
            },
            norm::ExprKind::ArrayIndex { target, indices } => sem::PlaceExprKind::ArrayIndex {
                target: Box::new(self.elab_place(target)),
                indices: indices.iter().map(|idx| self.elab_value(idx)).collect(),
            },
            norm::ExprKind::TupleField { target, index } => sem::PlaceExprKind::TupleField {
                target: Box::new(self.elab_place(target)),
                index: *index,
            },
            norm::ExprKind::StructField { target, field } => sem::PlaceExprKind::StructField {
                target: Box::new(self.elab_place(target)),
                field: field.clone(),
            },
            _ => panic!(
                "compiler bug: invalid place expression in semantic tree: {:?}",
                expr.kind
            ),
        };

        sem::PlaceExpr {
            id: expr.id,
            kind,
            ty: expr.ty,
            span: expr.span,
        }
    }

    fn elab_value(&mut self, expr: &norm::Expr) -> sem::ValueExpr {
        match &expr.kind {
            norm::ExprKind::Var { .. }
            | norm::ExprKind::ArrayIndex { .. }
            | norm::ExprKind::TupleField { .. }
            | norm::ExprKind::StructField { .. } => {
                let place = self.elab_place(expr);
                if self.implicit_moves.contains(&expr.id) {
                    return self.new_value(
                        sem::ValueExprKind::ImplicitMove {
                            place: Box::new(place),
                        },
                        expr.ty,
                        expr.span,
                    );
                }
                return self.new_value(
                    sem::ValueExprKind::Load {
                        place: Box::new(place),
                    },
                    expr.ty,
                    expr.span,
                );
            }
            norm::ExprKind::Move { expr: inner } => {
                let place = self.elab_place(inner);
                return sem::ValueExpr {
                    id: expr.id,
                    kind: sem::ValueExprKind::Move {
                        place: Box::new(place),
                    },
                    ty: expr.ty,
                    span: expr.span,
                };
            }
            norm::ExprKind::ImplicitMove { expr: inner } => {
                let place = self.elab_place(inner);
                return sem::ValueExpr {
                    id: expr.id,
                    kind: sem::ValueExprKind::ImplicitMove {
                        place: Box::new(place),
                    },
                    ty: expr.ty,
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
            } => {
                let closure_body = self.elab_value(body);
                let closure_def = sem::ClosureDef {
                    id: self.node_id_gen.new_id(),
                    def_id: *def_id,
                    sig: sem::ClosureSig {
                        name: ident.clone(),
                        params: params.clone(),
                        return_ty: return_ty.clone(),
                        span: expr.span,
                    },
                    body: closure_body,
                    span: expr.span,
                };
                self.closure_defs.push(closure_def);
                sem::ValueExprKind::ClosureRef { def_id: *def_id }
            }
            norm::ExprKind::Coerce { kind, expr } => sem::ValueExprKind::Coerce {
                kind: *kind,
                expr: Box::new(self.elab_value(expr)),
            },
            norm::ExprKind::Move { .. }
            | norm::ExprKind::ImplicitMove { .. }
            | norm::ExprKind::Var { .. }
            | norm::ExprKind::ArrayIndex { .. }
            | norm::ExprKind::TupleField { .. }
            | norm::ExprKind::StructField { .. } => {
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
