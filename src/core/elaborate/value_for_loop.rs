//! For loop desugaring.
//!
//! Transforms `for` loops into `while` loops with explicit index management.
//! This simplifies lowering by eliminating the need for special iterator
//! handling—lowering only needs to handle `while` loops.
//!
//! ## Desugaring pattern
//!
//! A for loop over an array or slice:
//! ```text
//! for elem in arr { body }
//! ```
//!
//! Becomes:
//! ```text
//! {
//!     let iter = arr;
//!     let len = len(iter);    // or constant for fixed-size arrays
//!     var idx: u64 = 0;
//!     while idx < len {
//!         let cur = idx;
//!         idx = idx + 1;      // increment before body (so `continue` works)
//!         let elem = iter[cur];
//!         body
//!     }
//! }
//! ```
//!
//! For range loops (`for i in 0..10`), the iterator binding is omitted
//! and the element is just the current index value.

use crate::core::analysis::facts::SyntheticReason;
use crate::core::diag::Span;
use crate::core::elaborate::elaborator::Elaborator;
use crate::core::resolve::{DefId, DefKind};
use crate::core::tree::BinaryOp;
use crate::core::tree::normalized as norm;
use crate::core::tree::semantic as sem;
use crate::core::types::Type;

/// Metadata for a synthesized local variable in the desugared loop.
struct ForLocal {
    def_id: DefId,
    name: String,
    ty: Type,
    pattern: sem::BindPattern,
}

impl<'a> Elaborator<'a> {
    /// Desugar a for loop into a while loop with explicit index management.
    pub(in crate::core::elaborate::value) fn elab_for_expr(
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
                let start_expr = self.elab_value(start);
                let len_expr = self.elab_value(end);

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
                    Type::DynArray { elem_ty } => (**elem_ty).clone(),
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
                    Type::DynArray { .. } => {
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
                .unwrap_or_else(|| panic!("compiler bug: missing iter place for iterable loop"));
            let cur_place = self.make_var_place(&cur_info, span);
            let cur_value = self.make_load_expr(cur_place, u64_ty.clone(), span);
            let index_place = self.make_index_place(iter_place, cur_value, elem_ty.clone(), span);
            self.make_load_expr(index_place, elem_ty.clone(), span)
        };

        let pattern = self.elab_bind_pattern(pattern, &elem_ty);
        let pattern_stmt = self.make_let_bind_stmt(pattern, elem_expr, span);
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
        let def_id = self.add_synthetic_def(
            name.clone(),
            DefKind::LocalVar {
                nrvo_eligible: false,
                is_mutable,
            },
            SyntheticReason::ElaborateSyntheticNode,
        );
        if let Some(def) = self.def_table.lookup_def(def_id) {
            let _ = self.type_map.insert_def_type(
                def.clone(),
                ty.clone(),
                "elaborate",
                SyntheticReason::ElaborateSyntheticNode,
            );
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
        let id = self.node_id_gen.new_id();
        let target_ty = self.type_map.type_table().get(target.ty).clone();
        let plan = self.build_index_plan(&target_ty);
        self.index_plans.insert(id, plan);
        let ty_id = self.insert_synth_node_type(id, elem_ty);

        sem::PlaceExpr {
            id,
            kind: sem::PlaceExprKind::ArrayIndex {
                target: Box::new(target),
                indices: vec![index],
            },
            ty: ty_id,
            span,
        }
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
        let ty_id = self.insert_synth_node_type(id, ty);
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
        let ty_id = self.insert_synth_node_type(id, ty);
        sem::PlaceExpr {
            id,
            kind,
            ty: ty_id,
            span,
        }
    }
}
