//! Shared projection helpers for SSA lowering.

use crate::core::backend::lower::lowerer::FuncLowerer;
use crate::core::ir::IrTypeId;
use crate::core::types::{Type, TypeId};

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Resolves a tuple field type from the semantic type table.
    pub(super) fn lower_tuple_field_ty(&mut self, ty_id: TypeId, index: usize) -> IrTypeId {
        match self.type_map.type_table().get(ty_id) {
            Type::Tuple { field_tys } => {
                let field_ty = field_tys
                    .get(index)
                    .unwrap_or_else(|| panic!("backend tuple field out of range {index}"));
                self.type_lowerer.lower_type(field_ty)
            }
            other => panic!("backend tuple field on non-tuple type {:?}", other),
        }
    }

    /// Resolves a struct field index and type from the semantic type table.
    pub(super) fn lower_struct_field_ty(
        &mut self,
        ty_id: TypeId,
        field: &str,
    ) -> (usize, IrTypeId) {
        match self.type_map.type_table().get(ty_id) {
            Type::Struct { fields, .. } => fields
                .iter()
                .enumerate()
                .find(|(_, f)| f.name == field)
                .map(|(idx, f)| (idx, self.type_lowerer.lower_type(&f.ty)))
                .unwrap_or_else(|| panic!("backend struct field not found: {}", field)),
            other => panic!("backend struct field on non-struct type {:?}", other),
        }
    }

    /// Resolves a tuple field type from a semantic type instance.
    pub(super) fn tuple_field_from_type(&mut self, ty: &Type, index: usize) -> (Type, IrTypeId) {
        match ty {
            Type::Tuple { field_tys } => {
                let field_ty = field_tys
                    .get(index)
                    .unwrap_or_else(|| panic!("backend tuple field out of range {index}"))
                    .clone();
                let field_ir_ty = self.type_lowerer.lower_type(&field_ty);
                (field_ty, field_ir_ty)
            }
            other => panic!("backend tuple field on non-tuple type {:?}", other),
        }
    }

    /// Resolves a struct field by name from a semantic type instance.
    pub(super) fn struct_field_from_type(
        &mut self,
        ty: &Type,
        field: &str,
    ) -> (usize, Type, IrTypeId) {
        match ty {
            Type::Struct { fields, .. } => fields
                .iter()
                .enumerate()
                .find(|(_, f)| f.name == field)
                .map(|(idx, f)| {
                    let field_ty = f.ty.clone();
                    let field_ir_ty = self.type_lowerer.lower_type(&field_ty);
                    (idx, field_ty, field_ir_ty)
                })
                .unwrap_or_else(|| panic!("backend struct field not found: {}", field)),
            other => panic!("backend struct field on non-struct type {:?}", other),
        }
    }

    /// Resolves a struct field by index from a semantic type instance.
    pub(super) fn struct_field_from_index(&mut self, ty: &Type, index: usize) -> (Type, IrTypeId) {
        match ty {
            Type::Struct { fields, .. } => {
                let field = fields
                    .get(index)
                    .unwrap_or_else(|| panic!("backend struct field out of range {index}"));
                let field_ty = field.ty.clone();
                let field_ir_ty = self.type_lowerer.lower_type(&field_ty);
                (field_ty, field_ir_ty)
            }
            other => panic!("backend struct field on non-struct type {:?}", other),
        }
    }

    /// Resolves a shared field carried by every variant of a lowered linear
    /// enum. Shared fields are prepended to each variant payload in declaration
    /// order, so every variant must expose the same field type and byte offset.
    pub(super) fn linear_shared_field_from_type(
        &mut self,
        ty: &Type,
        field: &str,
    ) -> Option<(Type, IrTypeId, u64)> {
        let Type::Enum { name, variants } = ty else {
            return None;
        };
        let info = self.linear_index.types.get(name)?;
        let shared_index = info
            .shared_fields
            .iter()
            .position(|shared| shared.name == field)?;
        let first_variant = variants.first()?;
        let field_ty = first_variant.payload.get(shared_index)?.clone();
        let field_ir_ty = self.type_lowerer.lower_type(&field_ty);

        let layout = self.type_lowerer.enum_layout_for_type(ty);
        let first_layout = layout.variants.first()?;
        let field_offset = *first_layout.field_offsets.get(shared_index)?;

        debug_assert!(layout.variants.iter().all(|variant| {
            variant.field_tys.get(shared_index) == Some(&field_ir_ty)
                && variant.field_offsets.get(shared_index) == Some(&field_offset)
        }));

        Some((field_ty, field_ir_ty, field_offset))
    }
}
