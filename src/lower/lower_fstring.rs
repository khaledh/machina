use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, Value};
use crate::lower::lower_util::u64_const;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::*;
use crate::tree::semantic::{StringFmtSegment, ValueExpr, ValueExprKind as VEK};
use crate::types::Type;

const MAX_U64_DEC_LEN: usize = 20;

enum OwnedFmtSegment {
    Literal(String),
    StringValue(Place<Aggregate>),
    IntValue { op: Operand, signed: bool, bits: u8 },
}

impl<'a> FuncLowerer<'a> {
    pub(super) fn lower_string_fmt_into(
        &mut self,
        dst: Place<Aggregate>,
        segments: &[StringFmtSegment],
    ) -> Result<(), LowerError> {
        if self.fstring_requires_owned_builder(segments) {
            return self.lower_string_fmt_owned_into(dst, segments);
        }

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

    fn fstring_requires_owned_builder(&mut self, segments: &[StringFmtSegment]) -> bool {
        for segment in segments {
            let StringFmtSegment::Expr { expr, .. } = segment else {
                continue;
            };
            if matches!(expr.kind, VEK::StringLit { .. }) {
                continue;
            }
            if matches!(self.ty_from_id(expr.ty), Type::String) {
                return true;
            }
        }
        false
    }

    // --- Compile-time formatter ---

    fn string_fmt_total_len(&mut self, segments: &[StringFmtSegment]) -> Result<usize, LowerError> {
        // Compute a conservative upper bound for the output buffer.
        let mut total = 0usize;
        for segment in segments {
            match segment {
                StringFmtSegment::Literal { value, .. } => {
                    total = total.saturating_add(value.len());
                }
                StringFmtSegment::Expr { expr, .. } => match &expr.kind {
                    VEK::StringLit { value } => {
                        total = total.saturating_add(value.len());
                    }
                    _ => {
                        // Non-literal expressions are restricted to integers.
                        let ty = self.ty_from_id(expr.ty);
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
        expr: &ValueExpr,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            VEK::StringLit { value } => self.append_literal_segment(fmt, value),
            _ => {
                // Only integer expressions are permitted in f-strings today.
                let ty = self.ty_from_id(expr.ty);
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
        expr: &ValueExpr,
    ) -> Result<(), LowerError> {
        // Normalize to 64-bit so the runtime conversion helpers have a fixed ABI.
        let ty = self.ty_from_id(expr.ty);
        let Type::Int { signed, bits } = ty else {
            return Err(LowerError::UnsupportedStringFmtSegment(expr.id));
        };

        let value_op = self.lower_scalar_expr(expr)?;
        if !signed {
            let value_op = self.coerce_int_to_u64(expr, value_op)?;
            return self.append_u64_segment(fmt, value_op);
        }

        let value_op = self.coerce_int_to_i64(value_op, bits)?;
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

    // --- Run-time formatter ---

    fn lower_string_fmt_owned_into(
        &mut self,
        dst: Place<Aggregate>,
        segments: &[StringFmtSegment],
    ) -> Result<(), LowerError> {
        let owned_segments = self.collect_owned_segments(segments)?;
        self.init_empty_string(&dst)?;
        self.reserve_owned_string_capacity(&dst, &owned_segments)?;

        for segment in owned_segments {
            match segment {
                OwnedFmtSegment::Literal(value) => {
                    self.append_literal_bytes_to_string(&dst, &value)?;
                }
                OwnedFmtSegment::StringValue(place) => {
                    self.append_string_place_to_string(&dst, &place)?;
                }
                OwnedFmtSegment::IntValue { op, signed, bits } => {
                    self.append_int_value_to_string(&dst, op, signed, bits)?;
                }
            }
        }

        Ok(())
    }

    fn collect_owned_segments(
        &mut self,
        segments: &[StringFmtSegment],
    ) -> Result<Vec<OwnedFmtSegment>, LowerError> {
        let mut owned_segments = Vec::with_capacity(segments.len());

        for segment in segments {
            match segment {
                StringFmtSegment::Literal { value, .. } => {
                    owned_segments.push(OwnedFmtSegment::Literal(value.clone()));
                }
                StringFmtSegment::Expr { expr, .. } => match &expr.kind {
                    VEK::StringLit { value } => {
                        owned_segments.push(OwnedFmtSegment::Literal(value.clone()));
                    }
                    _ => {
                        let ty = self.ty_from_id(expr.ty);
                        match ty {
                            Type::String => {
                                let value = self.lower_expr_value(expr)?;
                                let Value::Aggregate(place) = value else {
                                    return Err(LowerError::UnsupportedStringFmtSegment(expr.id));
                                };
                                owned_segments.push(OwnedFmtSegment::StringValue(place));
                            }
                            Type::Int { signed, bits } => {
                                let op = self.lower_scalar_expr(expr)?;
                                owned_segments.push(OwnedFmtSegment::IntValue { op, signed, bits });
                            }
                            _ => return Err(LowerError::UnsupportedStringFmtSegment(expr.id)),
                        }
                    }
                },
            }
        }

        Ok(owned_segments)
    }

    fn reserve_owned_string_capacity(
        &mut self,
        dst: &Place<Aggregate>,
        segments: &[OwnedFmtSegment],
    ) -> Result<(), LowerError> {
        let mut base_total = 0u64;
        for segment in segments {
            match segment {
                OwnedFmtSegment::Literal(value) => {
                    base_total = base_total.saturating_add(value.len() as u64);
                }
                OwnedFmtSegment::IntValue { .. } => {
                    base_total = base_total.saturating_add(MAX_U64_DEC_LEN as u64);
                }
                OwnedFmtSegment::StringValue(_) => {}
            }
        }

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let mut total_op = u64_const(base_total);

        for segment in segments {
            let OwnedFmtSegment::StringValue(place) = segment else {
                continue;
            };
            let (_, len_op) = self.string_ptr_len_ops(place);
            total_op = self.emit_scalar_rvalue(
                u64_ty_id,
                Rvalue::BinOp {
                    op: BinOp::Add,
                    lhs: total_op,
                    rhs: len_op,
                },
            );
        }

        self.emit_range_conversion_check(&total_op, 0, 0x7fff_ffff);

        let total_arg = self.runtime_arg_place(total_op);
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::StringEnsure),
                args: vec![PlaceAny::Aggregate(dst.clone()), total_arg],
            },
        );

        Ok(())
    }

    fn init_empty_string(&mut self, dst: &Place<Aggregate>) -> Result<(), LowerError> {
        let ptr_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let u32_ty_id = self.ty_lowerer.lower_ty(&Type::uint(32));

        self.emit_operand_into_agg_projection(
            dst,
            Projection::Field { index: 0 },
            Operand::Const(Const::Int {
                signed: false,
                bits: 64,
                value: 0,
            }),
            ptr_ty_id,
        )?;
        self.emit_operand_into_agg_projection(
            dst,
            Projection::Field { index: 1 },
            Operand::Const(Const::Int {
                signed: false,
                bits: 32,
                value: 0,
            }),
            u32_ty_id,
        )?;
        self.emit_operand_into_agg_projection(
            dst,
            Projection::Field { index: 2 },
            Operand::Const(Const::Int {
                signed: false,
                bits: 32,
                value: 0,
            }),
            u32_ty_id,
        )?;

        Ok(())
    }

    fn append_literal_bytes_to_string(
        &mut self,
        dst: &Place<Aggregate>,
        value: &str,
    ) -> Result<(), LowerError> {
        if value.is_empty() {
            return Ok(());
        }

        let len_op = u64_const(value.len() as u64);
        let gid = self.global_interner.intern(
            GlobalPayload::String(value.to_string()),
            GlobalSection::RoData,
        );
        let src_ptr = Operand::Const(Const::GlobalAddr { id: gid });
        self.append_bytes_to_string(dst, src_ptr, len_op)
    }

    fn append_string_place_to_string(
        &mut self,
        dst: &Place<Aggregate>,
        place: &Place<Aggregate>,
    ) -> Result<(), LowerError> {
        let (ptr_op, len_op) = self.string_ptr_len_ops(place);
        self.append_bytes_to_string(dst, ptr_op, len_op)
    }

    fn append_int_value_to_string(
        &mut self,
        dst: &Place<Aggregate>,
        value_op: Operand,
        signed: bool,
        bits: u8,
    ) -> Result<(), LowerError> {
        let buf_ty = Type::Array {
            elem_ty: Box::new(Type::uint(8)),
            dims: vec![MAX_U64_DEC_LEN],
        };
        let buf_ty_id = self.ty_lowerer.lower_ty(&buf_ty);
        let buf = self.new_temp_aggregate(buf_ty_id);

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let buf_ptr_place = self.new_temp_scalar(u64_ty_id);
        self.emit_copy_scalar(
            buf_ptr_place.clone(),
            Rvalue::AddrOf(PlaceAny::Aggregate(buf.clone())),
        );
        let buf_ptr_op = Operand::Copy(buf_ptr_place);

        let slice = self.build_u8_slice(buf_ptr_op.clone(), u64_const(MAX_U64_DEC_LEN as u64))?;
        let len_place = self.new_temp_scalar(u64_ty_id);
        let len_dst = PlaceAny::Scalar(len_place.clone());

        let value_op = if signed {
            self.coerce_int_to_i64(value_op, bits)?
        } else {
            self.coerce_uint_op_to_u64(value_op, bits)
        };

        let value_arg = self.runtime_arg_place(value_op);
        let callee = if signed {
            RuntimeFn::I64ToDec
        } else {
            RuntimeFn::U64ToDec
        };

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: Some(len_dst),
                callee: Callee::Runtime(callee),
                args: vec![PlaceAny::Aggregate(slice), value_arg],
            },
        );

        let len_op = Operand::Copy(len_place);
        self.append_bytes_to_string(dst, buf_ptr_op, len_op)
    }

    fn append_bytes_to_string(
        &mut self,
        dst: &Place<Aggregate>,
        ptr: Operand,
        len: Operand,
    ) -> Result<(), LowerError> {
        let ptr_arg = self.runtime_arg_place(ptr);
        let len_arg = self.runtime_arg_place(len);

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::StringAppendBytes),
                args: vec![PlaceAny::Aggregate(dst.clone()), ptr_arg, len_arg],
            },
        );

        Ok(())
    }

    // --- Common utilities ---

    fn coerce_int_to_u64(&mut self, expr: &ValueExpr, op: Operand) -> Result<Operand, LowerError> {
        let ty = self.ty_from_id(expr.ty);
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

    fn coerce_int_to_i64(&mut self, op: Operand, bits: u8) -> Result<Operand, LowerError> {
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

    fn coerce_uint_op_to_u64(&mut self, op: Operand, bits: u8) -> Operand {
        if bits == 64 {
            return op;
        }

        if let Operand::Const(Const::Int { value, .. }) = op {
            return Operand::Const(Const::Int {
                signed: false,
                bits: 64,
                value,
            });
        }

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let temp = self.new_temp_scalar(u64_ty_id);
        self.emit_copy_scalar(temp.clone(), Rvalue::Use(op));
        Operand::Copy(temp)
    }
}
