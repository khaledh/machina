use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{ExprValue, FuncLowerer};
use crate::mcir::types::*;
use crate::tir::model::{BindPattern, BindPatternKind as PK, Expr, ExprKind as EK};
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    // --- Control Flow ---

    /// Lower an if expression in value position.
    pub(super) fn lower_if_expr(
        &mut self,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<ExprValue, LowerError> {
        let cond_op = self.lower_scalar_expr(cond)?;
        let result_ty = self.ty_for_node(then_body.id)?;
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty.is_scalar() {
            // Scalar if-expr: write both branches into the same temp.
            let temp_place = self.new_temp_scalar(result_ty_id);

            self.lower_if_join(
                cond_op,
                |this| {
                    if let ExprValue::Scalar(op) = this.lower_expr_value(then_body)? {
                        this.emit_copy_scalar(temp_place.clone(), Rvalue::Use(op));
                    }
                    Ok(())
                },
                |this| {
                    if let ExprValue::Scalar(op) = this.lower_expr_value(else_body)? {
                        this.emit_copy_scalar(temp_place.clone(), Rvalue::Use(op));
                    }
                    Ok(())
                },
            )?;

            Ok(ExprValue::Scalar(Operand::Copy(temp_place)))
        } else {
            // Aggregate if-expr: lower both branches into a temp place.
            let temp_place = self.new_temp_aggregate(result_ty_id);

            self.lower_if_join(
                cond_op,
                |this| this.lower_agg_value_into(temp_place.clone(), then_body),
                |this| this.lower_agg_value_into(temp_place.clone(), else_body),
            )?;

            Ok(ExprValue::Aggregate(temp_place))
        }
    }

    /// Lower an if-expression into a destination aggregate place.
    pub(super) fn lower_if_expr_into(
        &mut self,
        dst: Place<Aggregate>,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<(), LowerError> {
        let cond_op = self.lower_scalar_expr(cond)?;

        self.lower_if_join(
            cond_op,
            |this| this.lower_agg_value_into(dst.clone(), then_body),
            |this| this.lower_agg_value_into(dst.clone(), else_body),
        )?;

        Ok(())
    }

    /// Helper for lowering an if-expression.
    pub(super) fn lower_if_join<FThen, FElse>(
        &mut self,
        cond: Operand,
        then_fn: FThen,
        else_fn: FElse,
    ) -> Result<(), LowerError>
    where
        FThen: FnOnce(&mut Self) -> Result<(), LowerError>,
        FElse: FnOnce(&mut Self) -> Result<(), LowerError>,
    {
        let then_bb = self.fb.new_block();
        let else_bb = self.fb.new_block();
        let join_bb = self.fb.new_block();

        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond,
                then_bb,
                else_bb,
            },
        );

        self.curr_block = then_bb;
        then_fn(self)?;
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(join_bb));

        self.curr_block = else_bb;
        else_fn(self)?;
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(join_bb));

        self.curr_block = join_bb;

        Ok(())
    }

    /// Lower a while expression (returns unit).
    pub(super) fn lower_while_expr(
        &mut self,
        cond: &Expr,
        body: &Expr,
    ) -> Result<ExprValue, LowerError> {
        let loop_cond_bb = self.fb.new_block();
        let loop_body_bb = self.fb.new_block();
        let loop_exit_bb = self.fb.new_block();

        // Jump to loop condition
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(loop_cond_bb));

        // Loop condition
        self.curr_block = loop_cond_bb;
        let cond_op = self.lower_scalar_expr(cond)?;
        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond: cond_op,
                then_bb: loop_body_bb,
                else_bb: loop_exit_bb,
            },
        );

        // Lower body
        self.curr_block = loop_body_bb;
        let EK::Block { items, tail } = &body.kind else {
            return Err(LowerError::ExpectedBlock(body.id));
        };
        self.lower_block_expr(items, tail.as_deref())?;
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(loop_cond_bb));

        // After loop
        self.curr_block = loop_exit_bb;

        // while loops return unit
        let c = Const::Unit;
        Ok(ExprValue::Scalar(Operand::Const(c)))
    }

    /// Lower a for expression (returns unit).
    pub(super) fn lower_for_expr(
        &mut self,
        pattern: &BindPattern,
        iter: &Expr,
        body: &Expr,
    ) -> Result<ExprValue, LowerError> {
        let iter_ty = self.ty_for_node(iter.id)?;

        match (&iter.kind, &iter_ty) {
            (EK::Range { start, end }, _) => self.lower_for_range_expr(*start, *end, pattern, body),
            (_, Type::Array { .. }) => self.lower_for_array_expr(pattern, iter, &iter_ty, body),
            (_, Type::Slice { .. }) => self.lower_for_slice_expr(pattern, iter, &iter_ty, body),
            _ => Err(LowerError::UnsupportedOperandExpr(iter.id)),
        }
    }

    pub(super) fn lower_for_range_expr(
        &mut self,
        start: u64,
        end: u64,
        pattern: &BindPattern,
        body: &Expr,
    ) -> Result<ExprValue, LowerError> {
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let start_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: start as i128,
        });
        let end_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: end as i128,
        });

        self.lower_for_indexed_loop(start_op, end_op, body, |this, idx_place| {
            // Bind pattern to current index (only name pattern expected).
            let PK::Name { def_id, .. } = &pattern.kind else {
                return Err(LowerError::PatternMismatch(pattern.id));
            };
            let name = this.def_name(*def_id, pattern.id)?;
            this.bind_ident(
                *def_id,
                name,
                u64_ty_id,
                PlaceAny::Scalar(idx_place.clone()),
            )
        })
    }

    pub(super) fn lower_for_array_expr(
        &mut self,
        pattern: &BindPattern,
        iter: &Expr,
        iter_ty: &Type,
        body: &Expr,
    ) -> Result<ExprValue, LowerError> {
        // Evaluate the iterable once
        let iter_place = match self.lower_expr_value(iter)? {
            ExprValue::Scalar(_) => return Err(LowerError::ExprIsNotAggregate(iter.id)),
            ExprValue::Aggregate(place) => place,
        };

        // Extract len + item type from array type
        let Type::Array { dims, .. } = iter_ty else {
            return Err(LowerError::UnsupportedOperandExpr(iter.id));
        };
        if dims.is_empty() {
            return Err(LowerError::UnsupportedOperandExpr(iter.id));
        }

        let len = dims[0];
        let item_ty = iter_ty
            .array_item_type()
            .unwrap_or_else(|| panic!("compiler bug: empty array dims"));

        let elem_ty_id = self.ty_lowerer.lower_ty(&item_ty);
        let start_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: 0_i128,
        });
        let len_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: len as i128,
        });

        self.lower_for_indexed_loop(start_op, len_op, body, |this, idx_place| {
            // element place = iter_place[idx]
            let elem_place = this.project_place(
                &iter_place,
                Projection::Index {
                    index: Operand::Copy(idx_place.clone()),
                },
                elem_ty_id,
            );
            this.bind_pattern_with_type(pattern, elem_place, &item_ty)
        })
    }

    pub(super) fn lower_for_slice_expr(
        &mut self,
        pattern: &BindPattern,
        iter: &Expr,
        iter_ty: &Type,
        body: &Expr,
    ) -> Result<ExprValue, LowerError> {
        // Evaluate the iterable once.
        let iter_place = match self.lower_expr_value(iter)? {
            ExprValue::Scalar(_) => return Err(LowerError::ExprIsNotAggregate(iter.id)),
            ExprValue::Aggregate(place) => place,
        };

        let Type::Slice { elem_ty } = iter_ty else {
            return Err(LowerError::UnsupportedOperandExpr(iter.id));
        };
        let elem_ty = (**elem_ty).clone();

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let ptr_ty_id = self.ty_lowerer.lower_ty(&Type::Heap {
            elem_ty: Box::new(elem_ty.clone()),
        });

        // ptr = slice.ptr (stash in a temp pointer local)
        let mut ptr_proj = iter_place.projections().to_vec();
        ptr_proj.push(Projection::Field { index: 0 });
        let ptr_place = Place::new(iter_place.base(), u64_ty_id, ptr_proj);
        let ptr_temp = self.new_temp_scalar(ptr_ty_id);
        self.emit_copy_scalar(ptr_temp.clone(), Rvalue::Use(Operand::Copy(ptr_place)));

        // len = slice.len
        let mut len_proj = iter_place.projections().to_vec();
        len_proj.push(Projection::Field { index: 1 });
        let len_place = Place::new(iter_place.base(), u64_ty_id, len_proj);
        let len_op = Operand::Copy(len_place);

        let elem_ty_id = self.ty_lowerer.lower_ty(&elem_ty);
        let start_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: 0_i128,
        });

        self.lower_for_indexed_loop(start_op, len_op, body, |this, idx_place| {
            let elem_place = this.place_from_ty_id(
                ptr_temp.base(),
                elem_ty_id,
                vec![Projection::Index {
                    index: Operand::Copy(idx_place.clone()),
                }],
            );
            this.bind_pattern_with_type(pattern, elem_place, &elem_ty)
        })
    }

    fn lower_for_indexed_loop<F>(
        &mut self,
        start_op: Operand,
        len_op: Operand,
        body: &Expr,
        mut bind_elem: F,
    ) -> Result<ExprValue, LowerError>
    where
        F: FnMut(&mut Self, &Place<Scalar>) -> Result<(), LowerError>,
    {
        // Lowered form:
        // var i = start;
        // while i < len {
        //     <bind pattern to element/idx>
        //     body
        //     i = i + 1;
        // }
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let idx_place = self.new_temp_scalar(u64_ty_id);

        // i = start
        self.emit_copy_scalar(idx_place.clone(), Rvalue::Use(start_op));

        // blocks
        let loop_cond_bb = self.fb.new_block();
        let loop_body_bb = self.fb.new_block();
        let loop_exit_bb = self.fb.new_block();

        // Jump to loop condition
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(loop_cond_bb));

        // condition: i < len
        self.curr_block = loop_cond_bb;
        let cond_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);
        let cond_op = self.emit_scalar_rvalue(
            cond_ty_id,
            Rvalue::BinOp {
                op: BinOp::Lt,
                lhs: Operand::Copy(idx_place.clone()),
                rhs: len_op,
            },
        );
        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond: cond_op,
                then_bb: loop_body_bb,
                else_bb: loop_exit_bb,
            },
        );

        // loop body
        self.curr_block = loop_body_bb;
        bind_elem(self, &idx_place)?;

        let EK::Block { items, tail } = &body.kind else {
            return Err(LowerError::ExpectedBlock(body.id));
        };
        self.lower_block_expr(items, tail.as_deref())?;

        // i = i + 1
        let one_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: 1_i128,
        });
        self.emit_copy_scalar(
            idx_place.clone(),
            Rvalue::BinOp {
                op: BinOp::Add,
                lhs: Operand::Copy(idx_place.clone()),
                rhs: one_op,
            },
        );

        // Jump to loop condition
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(loop_cond_bb));

        // After loop
        self.curr_block = loop_exit_bb;

        // for loops return unit
        Ok(ExprValue::Scalar(Operand::Const(Const::Unit)))
    }
}
