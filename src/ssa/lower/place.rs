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
                let addr = self.field_addr_typed(base.addr, *index, field_ty);
                Ok(PlaceAddr {
                    addr,
                    value_ty: field_ty,
                })
            }
            sem::PlaceExprKind::StructField { target, field } => {
                let base = self.lower_place_addr(target)?;
                let (field_index, field_ty) = self.lower_struct_field_ty(target.ty, field);
                let addr = self.field_addr_typed(base.addr, field_index, field_ty);
                Ok(PlaceAddr {
                    addr,
                    value_ty: field_ty,
                })
            }
            sem::PlaceExprKind::ArrayIndex { target, indices } => {
                let plan = self
                    .type_map
                    .lookup_index_plan(place.id)
                    .unwrap_or_else(|| panic!("ssa array index missing index plan {:?}", place.id));

                match plan.base {
                    sem::IndexBaseKind::Array { deref_count, .. } => {
                        let (mut base_addr, mut curr_ty) =
                            self.resolve_deref_base(target, deref_count)?;

                        // Walk indices and compute element/sub-array type in each step.
                        for index_expr in indices {
                            let index_val = self.lower_value_expr_linear(index_expr)?;
                            let next_ty = curr_ty.array_item_type().unwrap_or_else(|| {
                                panic!("ssa array index too many indices for {:?}", curr_ty);
                            });

                            let elem_ir_ty = self.type_lowerer.lower_type(&next_ty);
                            let ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                            base_addr = self.builder.index_addr(base_addr, index_val, ptr_ty);

                            curr_ty = next_ty;
                        }

                        Ok(PlaceAddr {
                            addr: base_addr,
                            value_ty: self.type_lowerer.lower_type_id(place.ty),
                        })
                    }
                    sem::IndexBaseKind::Slice { deref_count } => {
                        if indices.len() != 1 {
                            panic!("ssa slice index expects exactly one index");
                        }

                        let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                        let Type::Slice { elem_ty } = base_ty else {
                            panic!("ssa slice index on non-slice base {:?}", base_ty);
                        };

                        // Load the slice data pointer, then index into it.
                        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                        let ptr_addr = self.field_addr_typed(base_addr, 0, elem_ptr_ty);
                        let base_ptr = self.builder.load(ptr_addr, elem_ptr_ty);

                        let index_val = self.lower_value_expr_linear(&indices[0])?;
                        let addr = self.builder.index_addr(base_ptr, index_val, elem_ptr_ty);
                        Ok(PlaceAddr {
                            addr,
                            value_ty: elem_ir_ty,
                        })
                    }
                    sem::IndexBaseKind::String { .. } => {
                        Err(self.err_span(target.span, LoweringErrorKind::UnsupportedExpr))
                    }
                }
            }
        }
    }

    /// Resolves a tuple field type from the semantic type table.
    pub(super) fn lower_tuple_field_ty(
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
    pub(super) fn lower_struct_field_ty(
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
