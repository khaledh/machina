use crate::hir::model::{Expr, ExprKind as EK, StringFmtSegment};
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
        // Lower f-strings by allocating a fixed-size buffer on the stack and
        // driving a runtime formatter that appends each segment into it.
        let total_len = self.string_fmt_total_len(segments)?;
        let buf_len = total_len.max(1);

        let buf_ty = Type::Array {
            elem_ty: Box::new(Type::uint(8)),
            dims: vec![buf_len],
        };
        let buf_ty_id = self.ty_lowerer.lower_ty(&buf_ty);
        let buf = self.new_temp_aggregate(buf_ty_id);

        // Base pointer for the formatter buffer, passed through slice builders.
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let base_ptr_place = self.new_temp_scalar(u64_ty_id);
        self.emit_copy_scalar(
            base_ptr_place.clone(),
            Rvalue::AddrOf(PlaceAny::Aggregate(buf.clone())),
        );
        let base_ptr = Operand::Copy(base_ptr_place);

        // Initialize the formatter with the backing buffer slice.
        let init_slice = self.build_u8_slice(base_ptr.clone(), u64_const(total_len as u64))?;
        let fmt_ty_id = self.fmt_ty_id();
        let fmt = self.new_temp_aggregate(fmt_ty_id);
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::FmtInit),
                args: vec![
                    PlaceAny::Aggregate(fmt.clone()),
                    PlaceAny::Aggregate(init_slice),
                ],
            },
        );

        // Append each segment: literals become byte appends, expressions become
        // integer appends (string variables are not supported yet).
        for segment in segments {
            match segment {
                StringFmtSegment::Literal { value, .. } => {
                    self.append_literal_segment(&fmt, value)?;
                }
                StringFmtSegment::Expr { expr, .. } => {
                    self.append_expr_segment(&fmt, expr)?;
                }
            }
        }

        // Finalize into a string view.
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::FmtFinish),
                args: vec![PlaceAny::Aggregate(dst), PlaceAny::Aggregate(fmt)],
            },
        );

        Ok(())
    }

    fn string_fmt_total_len(&mut self, segments: &[StringFmtSegment]) -> Result<usize, LowerError> {
        // Compute a conservative upper bound for the output buffer.
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
                        // Non-literal expressions are restricted to integers.
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

    fn fmt_ty_id(&mut self) -> TyId {
        // Local struct type for mc_fmt_t { ptr, len, cap }.
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        self.ty_lowerer.table.add(TyKind::Struct {
            fields: vec![
                StructField {
                    name: "ptr".to_string(),
                    ty: u64_ty_id,
                },
                StructField {
                    name: "len".to_string(),
                    ty: u64_ty_id,
                },
                StructField {
                    name: "cap".to_string(),
                    ty: u64_ty_id,
                },
            ],
        })
    }

    fn append_expr_segment(
        &mut self,
        fmt: &Place<Aggregate>,
        expr: &Expr,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            EK::StringLit { value, .. } => self.append_literal_segment(fmt, value),
            _ => {
                // Only integer expressions are permitted in f-strings today.
                let ty = self.ty_for_node(expr.id)?;
                if ty.is_int() {
                    self.append_int_segment(fmt, expr)
                } else {
                    Err(LowerError::UnsupportedStringFmtSegment(expr.id))
                }
            }
        }
    }

    fn append_literal_segment(
        &mut self,
        fmt: &Place<Aggregate>,
        value: &str,
    ) -> Result<(), LowerError> {
        if value.is_empty() {
            return Ok(());
        }

        // Append a literal by referencing rodata and letting the runtime copy it.
        let len_op = u64_const(value.len() as u64);
        let gid = self.global_interner.intern(
            GlobalPayload::String(value.to_string()),
            GlobalSection::RoData,
        );
        let src_ptr = Operand::Const(Const::GlobalAddr { id: gid });
        let src_ptr_arg = self.runtime_arg_place(src_ptr);
        let len_arg = self.runtime_arg_place(len_op);

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::FmtAppendBytes),
                args: vec![PlaceAny::Aggregate(fmt.clone()), src_ptr_arg, len_arg],
            },
        );

        Ok(())
    }

    fn append_int_segment(
        &mut self,
        fmt: &Place<Aggregate>,
        expr: &Expr,
    ) -> Result<(), LowerError> {
        // Normalize to 64-bit so the runtime conversion helpers have a fixed ABI.
        let ty = self.ty_for_node(expr.id)?;
        let Type::Int { signed, bits } = ty else {
            return Err(LowerError::UnsupportedStringFmtSegment(expr.id));
        };

        let value_op = self.lower_scalar_expr(expr)?;
        if !signed {
            let value_op = self.coerce_int_to_u64(expr, value_op)?;
            return self.append_u64_segment(fmt, value_op);
        }

        let value_op = self.coerce_int_to_i64(expr, value_op, bits)?;
        self.append_i64_segment(fmt, value_op)
    }

    fn append_u64_segment(
        &mut self,
        fmt: &Place<Aggregate>,
        value_op: Operand,
    ) -> Result<(), LowerError> {
        // Delegate integer formatting to the runtime builder.
        let value_arg = self.runtime_arg_place(value_op);
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::FmtAppendU64),
                args: vec![PlaceAny::Aggregate(fmt.clone()), value_arg],
            },
        );
        Ok(())
    }

    fn append_i64_segment(
        &mut self,
        fmt: &Place<Aggregate>,
        value_op: Operand,
    ) -> Result<(), LowerError> {
        // Delegate signed integer formatting to the runtime builder.
        let value_arg = self.runtime_arg_place(value_op);
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::FmtAppendI64),
                args: vec![PlaceAny::Aggregate(fmt.clone()), value_arg],
            },
        );
        Ok(())
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
        // Note: this does not sign-extend in MCIR; it relies on upstream semantics.
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
