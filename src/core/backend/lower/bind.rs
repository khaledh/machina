//! Bind pattern lowering for let/var bindings.

use crate::core::ast::{ArrayRestBindPattern, BindPattern, BindPatternKind};
use crate::core::backend::lower::LowerToIrError;
use crate::core::backend::lower::locals::{LocalStorage, LocalValue};
use crate::core::backend::lower::lowerer::FuncLowerer;
use crate::core::types::Type;

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Binds a pattern to a value, updating the locals map.
    pub(super) fn bind_pattern(
        &mut self,
        pattern: &BindPattern,
        value: LocalValue,
        value_ty: &Type,
    ) -> Result<(), LowerToIrError> {
        match &pattern.kind {
            BindPatternKind::Wildcard => {
                self.emit_drop_for_value(
                    value.storage_value(),
                    value_ty,
                    matches!(value.storage, LocalStorage::Addr(_)),
                )?;
                Ok(())
            }
            BindPatternKind::Name { .. } => {
                let def_id = self.def_table.def_id(pattern.id);
                let dest_ty = self.def_type(def_id);
                if let LocalStorage::Value(value) = value.storage {
                    self.emit_conversion_check(value_ty, &dest_ty, value);
                }
                if value_ty.is_scalar() {
                    self.locals.insert(def_id, value);
                } else {
                    let slot = self.slot_for_value_typed(value, value_ty);
                    self.locals
                        .insert(def_id, LocalValue::addr(slot.addr, slot.ty));
                }
                // Track drop liveness for bindings that own drop-tracked values.
                self.set_drop_flag_for_def(def_id, true);
                Ok(())
            }
            BindPatternKind::Tuple { patterns } => {
                let Type::Tuple { .. } = value_ty else {
                    panic!("backend bind tuple on {:?}", value_ty);
                };
                let slot = self.slot_for_value_typed(value, value_ty);

                // Destructure each tuple field into a local binding.
                for (index, pat) in patterns.iter().enumerate() {
                    let (field_ty, field_ir_ty) = self.tuple_field_from_type(value_ty, index);
                    let field_val = self.load_field(slot.addr, index, field_ir_ty);
                    self.bind_pattern(pat, LocalValue::value(field_val, field_ir_ty), &field_ty)?;
                }
                Ok(())
            }
            BindPatternKind::Struct { fields, .. } => {
                let Type::Struct { .. } = value_ty else {
                    panic!("backend bind struct on {:?}", value_ty);
                };
                let slot = self.slot_for_value_typed(value, value_ty);

                // Destructure each struct field into a local binding.
                for field in fields {
                    let field_index = value_ty.struct_field_index(&field.name);
                    let (field_ty, field_ir_ty) =
                        self.struct_field_from_index(value_ty, field_index);
                    let field_val = self.load_field(slot.addr, field_index, field_ir_ty);
                    self.bind_pattern(
                        &field.pattern,
                        LocalValue::value(field_val, field_ir_ty),
                        &field_ty,
                    )?;
                }
                Ok(())
            }
            BindPatternKind::Array {
                prefix,
                rest,
                suffix,
            } => self.bind_array_pattern(prefix, rest.as_ref(), suffix, value, value_ty),
        }
    }

    fn bind_array_pattern(
        &mut self,
        prefix: &[BindPattern],
        rest: Option<&ArrayRestBindPattern>,
        suffix: &[BindPattern],
        value: LocalValue,
        value_ty: &Type,
    ) -> Result<(), LowerToIrError> {
        let slot = self.slot_for_value_typed(value, value_ty);
        let fixed_count = prefix.len() + suffix.len();

        let elem_ty = match value_ty {
            Type::Array { .. } => value_ty
                .array_item_type()
                .unwrap_or_else(|| panic!("backend bind array missing element type")),
            Type::DynArray { elem_ty } => (**elem_ty).clone(),
            other => panic!("backend bind array on {:?}", other),
        };

        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));

        let view = match value_ty {
            Type::Array { dims, .. } => {
                let len = dims.first().copied().unwrap_or(0) as u64;
                self.load_array_view(slot.addr, elem_ptr_ty, len)
            }
            Type::DynArray { .. } => {
                let view = self.load_dyn_array_view(slot.addr, elem_ptr_ty);
                if let Some(rest) = rest {
                    self.emit_array_pattern_min_len_check(view.len, fixed_count);
                    if rest.pattern.is_none() && fixed_count == 0 {
                        // Nothing else to bind; the minimum-length check is a no-op.
                    }
                } else {
                    self.emit_array_pattern_exact_len_check(view.len, fixed_count);
                }
                view
            }
            other => panic!("backend bind array on {:?}", other),
        };

        for (index, pattern) in prefix.iter().enumerate() {
            let index_val = self.builder.const_int(index as i128, false, 64, u64_ty);
            let elem_addr = self.builder.index_addr(view.ptr, index_val, elem_ptr_ty);
            let elem_val = self.builder.load(elem_addr, elem_ir_ty);
            self.bind_pattern(pattern, LocalValue::value(elem_val, elem_ir_ty), &elem_ty)?;
        }

        for (suffix_index, pattern) in suffix.iter().enumerate() {
            let from_end = suffix.len() - suffix_index;
            let offset_val = self.builder.const_int(from_end as i128, false, 64, u64_ty);
            let index_val =
                self.builder
                    .binop(crate::core::ir::BinOp::Sub, view.len, offset_val, u64_ty);
            let elem_addr = self.builder.index_addr(view.ptr, index_val, elem_ptr_ty);
            let elem_val = self.builder.load(elem_addr, elem_ir_ty);
            self.bind_pattern(pattern, LocalValue::value(elem_val, elem_ir_ty), &elem_ty)?;
        }

        if let Some(rest) = rest
            && let Some(rest_pattern) = &rest.pattern
        {
            let start_val = self
                .builder
                .const_int(prefix.len() as i128, false, 64, u64_ty);
            let suffix_len = self
                .builder
                .const_int(suffix.len() as i128, false, 64, u64_ty);
            let end_val =
                self.builder
                    .binop(crate::core::ir::BinOp::Sub, view.len, suffix_len, u64_ty);
            let slice_ty = Type::Slice {
                elem_ty: Box::new(elem_ty.clone()),
            };
            let slice_ir_ty = self.type_lowerer.lower_type(&slice_ty);
            let slice_val = self.emit_slice_value(
                slice_ir_ty,
                elem_ptr_ty,
                u64_ty,
                view.ptr,
                view.len,
                start_val,
                end_val,
            );
            self.bind_pattern(
                rest_pattern,
                LocalValue::value(slice_val, slice_ir_ty),
                &slice_ty,
            )?;
        }

        Ok(())
    }

    fn emit_array_pattern_min_len_check(&mut self, len: crate::core::ir::ValueId, min_len: usize) {
        if min_len == 0 {
            return;
        }
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let required = self
            .builder
            .const_int((min_len - 1) as i128, false, 64, u64_ty);
        self.emit_bounds_check(required, len);
    }

    fn emit_array_pattern_exact_len_check(
        &mut self,
        len: crate::core::ir::ValueId,
        exact_len: usize,
    ) {
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let min = self.builder.const_int(exact_len as i128, false, 64, u64_ty);
        let max = self
            .builder
            .const_int((exact_len + 1) as i128, false, 64, u64_ty);
        self.emit_range_check(len, min, max, false);
    }
}
