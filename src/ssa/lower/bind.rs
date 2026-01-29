//! Bind pattern lowering for let/var bindings.

use crate::ssa::lower::LoweringError;
use crate::ssa::lower::locals::LocalValue;
use crate::ssa::lower::lowerer::FuncLowerer;
use crate::tree::semantic as sem;
use crate::types::Type;

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Binds a pattern to a value, updating the locals map.
    pub(super) fn bind_pattern(
        &mut self,
        pattern: &sem::BindPattern,
        value: LocalValue,
        value_ty: &Type,
    ) -> Result<(), LoweringError> {
        match &pattern.kind {
            sem::BindPatternKind::Name { def_id, .. } => {
                let def = self
                    .def_table
                    .lookup_def(*def_id)
                    .unwrap_or_else(|| panic!("ssa bind missing def {:?}", def_id));
                let dest_ty = self
                    .type_map
                    .lookup_def_type(def)
                    .unwrap_or_else(|| panic!("ssa bind missing def type {:?}", def_id));
                if let crate::ssa::lower::locals::LocalStorage::Value(value) = value.storage {
                    self.emit_conversion_check(value_ty, &dest_ty, value);
                }
                if value_ty.is_scalar() {
                    self.locals.insert(*def_id, value);
                } else {
                    let slot = self.slot_for_value_typed(value, value_ty);
                    self.locals
                        .insert(*def_id, LocalValue::addr(slot.addr, slot.ty));
                }
                // Track drop liveness for bindings that own drop-tracked values.
                self.set_drop_flag_for_def(*def_id, true);
                Ok(())
            }
            sem::BindPatternKind::Tuple { fields } => {
                let Type::Tuple { .. } = value_ty else {
                    panic!("ssa bind tuple on {:?}", value_ty);
                };
                let slot = self.slot_for_value_typed(value, value_ty);

                // Destructure each tuple field into a local binding.
                for field in fields {
                    let (field_ty, field_ir_ty) = self.tuple_field_from_type(value_ty, field.index);
                    let field_val = self.load_field(slot.addr, field.index, field_ir_ty);
                    self.bind_pattern(
                        &field.pattern,
                        LocalValue::value(field_val, field_ir_ty),
                        &field_ty,
                    )?;
                }
                Ok(())
            }
            sem::BindPatternKind::Struct { fields, .. } => {
                let Type::Struct { .. } = value_ty else {
                    panic!("ssa bind struct on {:?}", value_ty);
                };
                let slot = self.slot_for_value_typed(value, value_ty);

                // Destructure each struct field into a local binding.
                for field in fields {
                    let (field_ty, field_ir_ty) =
                        self.struct_field_from_index(value_ty, field.field_index);
                    let field_val = self.load_field(slot.addr, field.field_index, field_ir_ty);
                    self.bind_pattern(
                        &field.pattern,
                        LocalValue::value(field_val, field_ir_ty),
                        &field_ty,
                    )?;
                }
                Ok(())
            }
            sem::BindPatternKind::Array { patterns } => {
                let Type::Array { .. } = value_ty else {
                    panic!("ssa bind array on {:?}", value_ty);
                };
                let elem_ty = value_ty
                    .array_item_type()
                    .unwrap_or_else(|| panic!("ssa bind array missing element type"));
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                let slot = self.slot_for_value_typed(value, value_ty);

                // Load each array element and bind it recursively.
                for (index, pattern) in patterns.iter().enumerate() {
                    let index_val = self.builder.const_int(index as i128, false, 64, u64_ty);
                    let elem_addr = self.builder.index_addr(slot.addr, index_val, elem_ptr_ty);
                    let elem_val = self.builder.load(elem_addr, elem_ir_ty);
                    self.bind_pattern(pattern, LocalValue::value(elem_val, elem_ir_ty), &elem_ty)?;
                }
                Ok(())
            }
        }
    }
}
