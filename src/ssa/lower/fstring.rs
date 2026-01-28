//! F-string lowering helpers.

use crate::ssa::lower::LoweringError;
use crate::ssa::lower::lowerer::{FuncLowerer, LinearValue, ValueSlot};
use crate::ssa::model::ir::{BinOp, Callee, CastKind, RuntimeFn, ValueId};
use crate::tree::semantic as sem;
use crate::types::Type;

const MAX_U64_DEC_LEN: usize = 20;

enum LoweredFmtSegment {
    Literal { ptr: ValueId, len: ValueId },
    StringValue { ptr: ValueId, len: ValueId },
    Int { value: ValueId, signed: bool },
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    // --- Fixed-length f-string formatting (view string) ---

    /// Lowers a view f-string, returning `None` if a segment returns early.
    pub(super) fn lower_string_fmt_view(
        &mut self,
        plan: &sem::StringFmtPlan,
        string_ty: crate::ssa::IrTypeId,
    ) -> Result<Option<LinearValue>, LoweringError> {
        let total_len = self.string_fmt_plan_len(plan);
        let buf_len = total_len.max(1);

        // Allocate a fixed-size buffer for the formatter to write into.
        let buf_ty = Type::Array {
            elem_ty: Box::new(Type::uint(8)),
            dims: vec![buf_len],
        };
        let buf_ty_id = self.type_lowerer.lower_type(&buf_ty);
        let buf_addr = self.alloc_local_addr(buf_ty_id);

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let zero = self.builder.const_int(0, false, 64, u64_ty);
        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
        let buf_ptr = self.builder.index_addr(buf_addr, zero, u8_ptr_ty);

        // Initialize the formatter over the backing buffer.
        let fmt_ty = self.type_lowerer.fmt_type();
        let fmt_addr = self.alloc_local_addr(fmt_ty);
        let buf_len_val = self.builder.const_int(total_len as i128, false, 64, u64_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::FmtInit),
            vec![fmt_addr, buf_ptr, buf_len_val],
            unit_ty,
        );

        let (segments, _) = match self.lower_fmt_segments(plan, u8_ptr_ty, u64_ty)? {
            Some(value) => value,
            None => return Ok(None),
        };

        // Append each segment using the runtime formatter.
        for segment in segments {
            match segment {
                LoweredFmtSegment::Literal { ptr, len } => {
                    let _ = self.builder.call(
                        Callee::Runtime(RuntimeFn::FmtAppendBytes),
                        vec![fmt_addr, ptr, len],
                        unit_ty,
                    );
                }
                LoweredFmtSegment::StringValue { .. } => {
                    panic!("ssa view f-string received string segment");
                }
                LoweredFmtSegment::Int { value, signed } => {
                    let callee = if signed {
                        RuntimeFn::FmtAppendI64
                    } else {
                        RuntimeFn::FmtAppendU64
                    };
                    let _ =
                        self.builder
                            .call(Callee::Runtime(callee), vec![fmt_addr, value], unit_ty);
                }
            }
        }

        // Finalize into a string view.
        let slot = self.alloc_value_slot(string_ty);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::FmtFinish),
            vec![slot.addr, fmt_addr],
            unit_ty,
        );
        Ok(Some(self.load_slot(&slot)))
    }

    fn string_fmt_plan_len(&self, plan: &sem::StringFmtPlan) -> usize {
        let mut total = 0usize;
        for term in &plan.reserve_terms {
            match term {
                sem::LenTerm::Literal(value) => {
                    total = total.saturating_add(*value);
                }
                sem::LenTerm::StringValue { .. } => {
                    panic!("ssa view f-string received string reserve term");
                }
            }
        }
        total
    }

    // --- Variable length f-string formatting (owned string) ---

    /// Lowers an owned f-string, returning `None` if a segment returns early.
    pub(super) fn lower_string_fmt_owned(
        &mut self,
        plan: &sem::StringFmtPlan,
        string_ty: crate::ssa::IrTypeId,
    ) -> Result<Option<LinearValue>, LoweringError> {
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
        let (segments, string_lens) = match self.lower_fmt_segments(plan, u8_ptr_ty, u64_ty)? {
            Some(value) => value,
            None => return Ok(None),
        };

        let slot = self.alloc_value_slot(string_ty);
        self.init_empty_string_slot(&slot);

        // Pre-reserve capacity by summing literal and string lengths.
        let mut base_total = 0u64;
        for term in &plan.reserve_terms {
            if let sem::LenTerm::Literal(value) = term {
                base_total = base_total.saturating_add(*value as u64);
            }
        }
        let mut total = self
            .builder
            .const_int(base_total as i128, false, 64, u64_ty);

        for term in &plan.reserve_terms {
            let sem::LenTerm::StringValue { segment_index } = term else {
                continue;
            };
            let len = string_lens
                .get(*segment_index)
                .and_then(|value| *value)
                .unwrap_or_else(|| {
                    panic!("ssa string fmt missing string segment {segment_index}");
                });
            total = self.builder.binop(BinOp::Add, total, len, u64_ty);
        }

        let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let max_excl = self
            .builder
            .const_int((u32::MAX as u64 + 1) as i128, false, 64, u64_ty);
        let zero = self.builder.const_int(0, false, 64, u64_ty);
        self.emit_range_check(total, zero, max_excl);

        let total_u32 = self.builder.int_trunc(total, u32_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::StringEnsure),
            vec![slot.addr, total_u32],
            unit_ty,
        );

        // Append the computed segments to the owned string.
        for segment in segments {
            match segment {
                LoweredFmtSegment::Literal { ptr, len } => {
                    self.append_bytes_to_string(slot.addr, ptr, len);
                }
                LoweredFmtSegment::StringValue { ptr, len } => {
                    self.append_bytes_to_string(slot.addr, ptr, len);
                }
                LoweredFmtSegment::Int { value, signed } => {
                    self.append_int64_to_string(slot.addr, value, signed);
                }
            }
        }

        Ok(Some(self.load_slot(&slot)))
    }

    fn init_empty_string_slot(&mut self, slot: &ValueSlot) {
        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
        let len_ty = self.type_lowerer.lower_type(&Type::uint(32));

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let zero_u64 = self.builder.const_int(0, false, 64, u64_ty);
        let zero_ptr = self.builder.cast(CastKind::IntToPtr, zero_u64, u8_ptr_ty);

        let zero_len = self.builder.const_int(0, false, 32, len_ty);
        self.store_field(slot.addr, 0, u8_ptr_ty, zero_ptr);
        self.store_field(slot.addr, 1, len_ty, zero_len);
        self.store_field(slot.addr, 2, len_ty, zero_len);
    }

    fn append_bytes_to_string(&mut self, string_addr: ValueId, ptr: ValueId, len: ValueId) {
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::StringAppendBytes),
            vec![string_addr, ptr, len],
            unit_ty,
        );
    }

    fn append_int64_to_string(&mut self, string_addr: ValueId, value: ValueId, signed: bool) {
        // Format into a fixed-size buffer, then append the produced bytes.
        let buf_ty = Type::Array {
            elem_ty: Box::new(Type::uint(8)),
            dims: vec![MAX_U64_DEC_LEN],
        };
        let buf_ty_id = self.type_lowerer.lower_type(&buf_ty);
        let buf_addr = self.alloc_local_addr(buf_ty_id);

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let zero = self.builder.const_int(0, false, 64, u64_ty);
        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
        let buf_ptr = self.builder.index_addr(buf_addr, zero, u8_ptr_ty);
        let buf_len = self
            .builder
            .const_int(MAX_U64_DEC_LEN as i128, false, 64, u64_ty);

        let callee = if signed {
            RuntimeFn::I64ToDec
        } else {
            RuntimeFn::U64ToDec
        };
        let len_val = self.builder.call(
            Callee::Runtime(callee),
            vec![buf_ptr, buf_len, value],
            u64_ty,
        );

        self.append_bytes_to_string(string_addr, buf_ptr, len_val);
    }

    // --- Shared between view and owned f-string lowering ---

    /// Lowers format segments, propagating early returns from segment expressions.
    fn lower_fmt_segments(
        &mut self,
        plan: &sem::StringFmtPlan,
        u8_ptr_ty: crate::ssa::IrTypeId,
        u64_ty: crate::ssa::IrTypeId,
    ) -> Result<Option<(Vec<LoweredFmtSegment>, Vec<Option<ValueId>>)>, LoweringError> {
        let mut segments = Vec::with_capacity(plan.segments.len());
        let mut string_lens = vec![None; plan.segments.len()];

        for (index, segment) in plan.segments.iter().enumerate() {
            match segment {
                sem::SegmentKind::LiteralBytes(value) => {
                    if value.is_empty() {
                        continue;
                    }

                    let global_id = self.add_global_bytes(value.as_bytes().to_vec());
                    let ptr_val = self.builder.const_global_addr(global_id, u8_ptr_ty);
                    let len_val = self
                        .builder
                        .const_int(value.len() as i128, false, 64, u64_ty);
                    segments.push(LoweredFmtSegment::Literal {
                        ptr: ptr_val,
                        len: len_val,
                    });
                }
                sem::SegmentKind::StringValue { expr } => {
                    let Some(value) = self.lower_value_expr_opt(expr)? else {
                        return Ok(None);
                    };
                    let (ptr, len) =
                        self.lower_ptr_len_from_value(expr.span, value, &Type::String, 32)?;
                    string_lens[index] = Some(len);
                    segments.push(LoweredFmtSegment::StringValue { ptr, len });
                }
                sem::SegmentKind::Int { expr, signed, bits } => {
                    let Some(value) = self.lower_value_expr_opt(expr)? else {
                        return Ok(None);
                    };
                    let target_ty = self.type_lowerer.lower_type(&Type::Int {
                        signed: *signed,
                        bits: 64,
                    });
                    let value_64 = self.cast_int_value(value, *signed, *bits, target_ty);
                    segments.push(LoweredFmtSegment::Int {
                        value: value_64,
                        signed: *signed,
                    });
                }
            }
        }

        Ok(Some((segments, string_lens)))
    }

    fn cast_int_value(
        &mut self,
        value: ValueId,
        signed: bool,
        bits: u8,
        target_ty: crate::ssa::IrTypeId,
    ) -> ValueId {
        if bits == 64 {
            return value;
        }
        if bits < 64 {
            return self.builder.int_extend(value, target_ty, signed);
        }
        self.builder.int_trunc(value, target_ty)
    }
}
