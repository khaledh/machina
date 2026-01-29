//! Shared projection helpers for SSA lowering.

use crate::ssa::IrTypeId;
use crate::ssa::lower::lowerer::FuncLowerer;
use crate::types::{Type, TypeId};

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Resolves a tuple field type from the semantic type table.
    pub(super) fn lower_tuple_field_ty(&mut self, ty_id: TypeId, index: usize) -> IrTypeId {
        match self.type_map.type_table().get(ty_id) {
            Type::Tuple { field_tys } => {
                let field_ty = field_tys
                    .get(index)
                    .unwrap_or_else(|| panic!("ssa tuple field out of range {index}"));
                self.type_lowerer.lower_type(field_ty)
            }
            other => panic!("ssa tuple field on non-tuple type {:?}", other),
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
                .unwrap_or_else(|| panic!("ssa struct field not found: {}", field)),
            other => panic!("ssa struct field on non-struct type {:?}", other),
        }
    }

    /// Resolves a tuple field type from a semantic type instance.
    pub(super) fn tuple_field_from_type(&mut self, ty: &Type, index: usize) -> (Type, IrTypeId) {
        match ty {
            Type::Tuple { field_tys } => {
                let field_ty = field_tys
                    .get(index)
                    .unwrap_or_else(|| panic!("ssa tuple field out of range {index}"))
                    .clone();
                let field_ir_ty = self.type_lowerer.lower_type(&field_ty);
                (field_ty, field_ir_ty)
            }
            other => panic!("ssa tuple field on non-tuple type {:?}", other),
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
                .unwrap_or_else(|| panic!("ssa struct field not found: {}", field)),
            other => panic!("ssa struct field on non-struct type {:?}", other),
        }
    }

    /// Resolves a struct field by index from a semantic type instance.
    pub(super) fn struct_field_from_index(&mut self, ty: &Type, index: usize) -> (Type, IrTypeId) {
        match ty {
            Type::Struct { fields, .. } => {
                let field = fields
                    .get(index)
                    .unwrap_or_else(|| panic!("ssa struct field out of range {index}"));
                let field_ty = field.ty.clone();
                let field_ir_ty = self.type_lowerer.lower_type(&field_ty);
                (field_ty, field_ir_ty)
            }
            other => panic!("ssa struct field on non-struct type {:?}", other),
        }
    }
}
