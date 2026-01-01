use crate::ast::{Expr, ExprKind as EK, StringFmtSegment};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::FuncLowerer;
use crate::lower::lower_util::u64_const;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::*;
use crate::types::Type;

const MAX_U64_DEC_LEN: usize = 20;

impl<'a> FuncLowerer<'a> {
    pub(super) fn lower_string_fmt_into(
        &mut self,
        dst: Place<Aggregate>,
        segments: &[StringFmtSegment],
    ) -> Result<(), LowerError> {
        let total_len = self.string_fmt_total_len(segments)?;
        let buf_len = total_len.max(1);

        let buf_ty = Type::Array {
            elem_ty: Box::new(Type::uint(8)),
            dims: vec![buf_len],
        };
        let buf_ty_id = self.ty_lowerer.lower_ty(&buf_ty);
        let buf = self.new_temp_aggregate(buf_ty_id);

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let pos = self.new_temp_scalar(u64_ty_id);
        self.emit_copy_scalar(pos.clone(), Rvalue::Use(u64_const(0)));

        // Base pointer for slices into the buffer.
        let base_ptr_place = self.new_temp_scalar(u64_ty_id);
        self.emit_copy_scalar(
            base_ptr_place.clone(),
            Rvalue::AddrOf(PlaceAny::Aggregate(buf.clone())),
        );
        let base_ptr = Operand::Copy(base_ptr_place);

        for segment in segments {
            match segment {
                StringFmtSegment::Literal { value, .. } => {
                    self.append_literal_segment(base_ptr.clone(), &pos, value)?;
                }
                StringFmtSegment::Expr { expr, .. } => {
                    self.append_expr_segment(base_ptr.clone(), &pos, expr)?;
                }
            }
        }

        let final_slice = self.build_u8_slice(base_ptr.clone(), Operand::Copy(pos.clone()))?;
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::StringFromBytes),
                args: vec![PlaceAny::Aggregate(dst), PlaceAny::Aggregate(final_slice)],
            },
        );

        Ok(())
    }

    fn string_fmt_total_len(&mut self, segments: &[StringFmtSegment]) -> Result<usize, LowerError> {
        let mut total = 0usize;
        for segment in segments {
            match segment {
                StringFmtSegment::Literal { value, .. } => {
                    total = total.saturating_add(value.len());
                }
                StringFmtSegment::Expr { expr, .. } => match &expr.kind {
                    EK::StringLit { value, .. } => {
                        total = total.saturating_add(value.len());
                    }
                    _ => {
                        let ty = self.ty_for_node(expr.id)?;
                        match ty {
                            Type::Int { .. } => {
                                total = total.saturating_add(MAX_U64_DEC_LEN);
                            }
                            _ => return Err(LowerError::UnsupportedStringFmtSegment(expr.id)),
                        }
                    }
                },
            }
        }
        Ok(total)
    }

    fn append_expr_segment(
        &mut self,
        base_ptr: Operand,
        pos: &Place<Scalar>,
        expr: &Expr,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            EK::StringLit { value, .. } => self.append_literal_segment(base_ptr, pos, value),
            _ => {
                let ty = self.ty_for_node(expr.id)?;
                if ty.is_int() {
                    self.append_int_segment(base_ptr, pos, expr)
                } else {
                    Err(LowerError::UnsupportedStringFmtSegment(expr.id))
                }
            }
        }
    }

    fn append_literal_segment(
        &mut self,
        base_ptr: Operand,
        pos: &Place<Scalar>,
        value: &str,
    ) -> Result<(), LowerError> {
        if value.is_empty() {
            return Ok(());
        }

        let len = value.len() as u64;
        let len_op = u64_const(len);

        let dst_slice =
            self.build_u8_slice_at(base_ptr.clone(), Operand::Copy(pos.clone()), len_op.clone())?;

        let gid = self.global_interner.intern(
            GlobalPayload::String(value.to_string()),
            GlobalSection::RoData,
        );
        let src_ptr = Operand::Const(Const::GlobalAddr { id: gid });
        let src_slice = self.build_u8_slice(src_ptr, len_op.clone())?;

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::MemCopy),
                args: vec![
                    PlaceAny::Aggregate(dst_slice),
                    PlaceAny::Aggregate(src_slice),
                ],
            },
        );

        self.bump_pos(pos, len_op);
        Ok(())
    }

    fn append_int_segment(
        &mut self,
        base_ptr: Operand,
        pos: &Place<Scalar>,
        expr: &Expr,
    ) -> Result<(), LowerError> {
        let ty = self.ty_for_node(expr.id)?;
        let Type::Int { signed, bits } = ty else {
            return Err(LowerError::UnsupportedStringFmtSegment(expr.id));
        };

        let value_op = self.lower_scalar_expr(expr)?;
        if !signed {
            let value_op = self.coerce_int_to_u64(expr, value_op)?;
            return self.append_u64_dec_segment(base_ptr, pos, value_op);
        }

        let value_op = self.coerce_int_to_i64(expr, value_op, bits)?;
        self.append_i64_dec_segment(base_ptr, pos, value_op)
    }

    fn append_u64_dec_segment(
        &mut self,
        base_ptr: Operand,
        pos: &Place<Scalar>,
        value_op: Operand,
    ) -> Result<(), LowerError> {
        let max_len_op = u64_const(MAX_U64_DEC_LEN as u64);
        let dst_slice =
            self.build_u8_slice_at(base_ptr, Operand::Copy(pos.clone()), max_len_op.clone())?;

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let len_place = self.new_temp_scalar(u64_ty_id);
        let value_arg = self.runtime_arg_place(value_op);
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: Some(PlaceAny::Scalar(len_place.clone())),
                callee: Callee::Runtime(RuntimeFn::U64ToDec),
                args: vec![PlaceAny::Aggregate(dst_slice), value_arg],
            },
        );

        self.bump_pos(pos, Operand::Copy(len_place));
        Ok(())
    }

    fn append_i64_dec_segment(
        &mut self,
        base_ptr: Operand,
        pos: &Place<Scalar>,
        value_op: Operand,
    ) -> Result<(), LowerError> {
        let max_len_op = u64_const(MAX_U64_DEC_LEN as u64);
        let dst_slice =
            self.build_u8_slice_at(base_ptr, Operand::Copy(pos.clone()), max_len_op.clone())?;

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let len_place = self.new_temp_scalar(u64_ty_id);
        let value_arg = self.runtime_arg_place(value_op);
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: Some(PlaceAny::Scalar(len_place.clone())),
                callee: Callee::Runtime(RuntimeFn::I64ToDec),
                args: vec![PlaceAny::Aggregate(dst_slice), value_arg],
            },
        );

        self.bump_pos(pos, Operand::Copy(len_place));
        Ok(())
    }

    fn build_u8_slice_at(
        &mut self,
        base_ptr: Operand,
        offset: Operand,
        len: Operand,
    ) -> Result<Place<Aggregate>, LowerError> {
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let ptr = self.emit_scalar_rvalue(
            u64_ty_id,
            Rvalue::BinOp {
                op: BinOp::Add,
                lhs: base_ptr,
                rhs: offset,
            },
        );
        self.build_u8_slice(ptr, len)
    }

    fn build_u8_slice(
        &mut self,
        ptr: Operand,
        len: Operand,
    ) -> Result<Place<Aggregate>, LowerError> {
        let slice_ty = Type::Slice {
            elem_ty: Box::new(Type::uint(8)),
        };
        let slice_ty_id = self.ty_lowerer.lower_ty(&slice_ty);
        let slice = self.new_temp_aggregate(slice_ty_id);

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let ptr_field = Place::new(
            slice.base(),
            u64_ty_id,
            vec![Projection::Field { index: 0 }],
        );
        let len_field = Place::new(
            slice.base(),
            u64_ty_id,
            vec![Projection::Field { index: 1 }],
        );

        self.emit_copy_scalar(ptr_field, Rvalue::Use(ptr));
        self.emit_copy_scalar(len_field, Rvalue::Use(len));
        Ok(slice)
    }

    fn bump_pos(&mut self, pos: &Place<Scalar>, len: Operand) {
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let next = self.emit_scalar_rvalue(
            u64_ty_id,
            Rvalue::BinOp {
                op: BinOp::Add,
                lhs: Operand::Copy(pos.clone()),
                rhs: len,
            },
        );
        self.emit_copy_scalar(pos.clone(), Rvalue::Use(next));
    }

    fn coerce_int_to_u64(&mut self, expr: &Expr, op: Operand) -> Result<Operand, LowerError> {
        let ty = self.ty_for_node(expr.id)?;
        let Type::Int { signed, bits } = ty else {
            return Err(LowerError::UnsupportedStringFmtSegment(expr.id));
        };

        if bits == 64 && !signed {
            return Ok(op);
        }

        if let Operand::Const(Const::Int { value, .. }) = op {
            return Ok(Operand::Const(Const::Int {
                signed: false,
                bits: 64,
                value,
            }));
        }

        // Note: this zero-extends smaller integers into u64.
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let temp = self.new_temp_scalar(u64_ty_id);
        self.emit_copy_scalar(temp.clone(), Rvalue::Use(op));
        Ok(Operand::Copy(temp))
    }

    fn coerce_int_to_i64(
        &mut self,
        _expr: &Expr,
        op: Operand,
        bits: u8,
    ) -> Result<Operand, LowerError> {
        if bits == 64 {
            return Ok(op);
        }

        if let Operand::Const(Const::Int { value, .. }) = op {
            return Ok(Operand::Const(Const::Int {
                signed: true,
                bits: 64,
                value,
            }));
        }

        let i64_ty_id = self.ty_lowerer.lower_ty(&Type::sint(64));
        let temp = self.new_temp_scalar(i64_ty_id);
        self.emit_copy_scalar(temp.clone(), Rvalue::Use(op));
        Ok(Operand::Copy(temp))
    }
}
