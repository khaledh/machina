//! Place lowering helpers for SSA explicit-memory ops.

use crate::ssa::lower::{LoweringError, LoweringErrorKind};
use crate::ssa::model::ir::ValueId;
use crate::tree::semantic as sem;
use crate::types::Type;

/// Address + value type for a lowered place.
pub(super) struct PlaceAddr {
    pub(super) addr: ValueId,
    pub(super) value_ty: crate::ssa::IrTypeId,
}

impl<'a> crate::ssa::lower::lowerer::FuncLowerer<'a> {
    /// Lowers a place expression into an address and its value type.
    pub(super) fn lower_place_addr(
        &mut self,
        place: &sem::PlaceExpr,
    ) -> Result<PlaceAddr, LoweringError> {
        match &place.kind {
            sem::PlaceExprKind::Var { def_id, .. } => {
                let value_ty = self.type_lowerer.lower_type_id(place.ty);
                let addr = self.ensure_local_addr(*def_id, value_ty);
                Ok(PlaceAddr { addr, value_ty })
            }
            sem::PlaceExprKind::Deref { value } => {
                let addr = self.lower_value_expr_linear(value)?;
                let value_ty = self.type_lowerer.lower_type_id(place.ty);
                Ok(PlaceAddr { addr, value_ty })
            }
            sem::PlaceExprKind::TupleField { target, index } => {
                let base = self.lower_place_addr(target)?;
                let field_ty = self.lower_tuple_field_ty(target.ty, *index);
                let ptr_ty = self.type_lowerer.ptr_to(field_ty);
                let addr = self.builder.field_addr(base.addr, *index, ptr_ty);
                Ok(PlaceAddr {
                    addr,
                    value_ty: field_ty,
                })
            }
            sem::PlaceExprKind::StructField { target, field } => {
                let base = self.lower_place_addr(target)?;
                let (field_index, field_ty) = self.lower_struct_field_ty(target.ty, field);
                let ptr_ty = self.type_lowerer.ptr_to(field_ty);
                let addr = self.builder.field_addr(base.addr, field_index, ptr_ty);
                Ok(PlaceAddr {
                    addr,
                    value_ty: field_ty,
                })
            }
            sem::PlaceExprKind::ArrayIndex { .. } => {
                Err(self.err_span(place.span, LoweringErrorKind::UnsupportedExpr))
            }
        }
    }

    /// Resolves a tuple field type from the semantic type table.
    fn lower_tuple_field_ty(
        &mut self,
        ty_id: crate::types::TypeId,
        index: usize,
    ) -> crate::ssa::IrTypeId {
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
    fn lower_struct_field_ty(
        &mut self,
        ty_id: crate::types::TypeId,
        field: &str,
    ) -> (usize, crate::ssa::IrTypeId) {
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
}
