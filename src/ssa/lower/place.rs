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
            sem::PlaceExprKind::ArrayIndex { target, indices } => {
                let plan = self
                    .type_map
                    .lookup_index_plan(place.id)
                    .unwrap_or_else(|| panic!("ssa array index missing index plan {:?}", place.id));

                let (dims, deref_count) = match plan.base {
                    sem::IndexBaseKind::Array { dims, deref_count } => (dims, deref_count),
                    sem::IndexBaseKind::Slice { .. } | sem::IndexBaseKind::String { .. } => {
                        return Err(self.err_span(target.span, LoweringErrorKind::UnsupportedExpr));
                    }
                };

                let mut base = self.lower_place_addr(target)?;

                // Peel heap/ref indirections using the plan's deref count.
                let mut curr_ty = self.type_map.type_table().get(target.ty).clone();

                for _ in 0..deref_count {
                    let elem_ty = match curr_ty {
                        Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } => elem_ty,
                        other => panic!("ssa array index on non-heap/ref type {:?}", other),
                    };
                    let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                    let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                    base.addr = self.builder.load(base.addr, ptr_ir_ty);
                    curr_ty = (*elem_ty).clone();
                }

                if indices.len() > dims.len() {
                    panic!("ssa array index has too many indices for dims {:?}", dims);
                }

                // Walk indices and compute element/sub-array type in each step.
                for (i, index_expr) in indices.iter().enumerate() {
                    let index_val = self.lower_value_expr_linear(index_expr)?;

                    let remaining_dims = &dims[(i + 1)..];
                    let elem_ty = if remaining_dims.is_empty() {
                        match curr_ty {
                            Type::Array { elem_ty, .. } => (*elem_ty).clone(),
                            other => panic!("ssa array index on non-array type {:?}", other),
                        }
                    } else {
                        let elem_ty = match curr_ty {
                            Type::Array { elem_ty, .. } => (*elem_ty).clone(),
                            other => panic!("ssa array index on non-array type {:?}", other),
                        };
                        Type::Array {
                            elem_ty: Box::new(elem_ty.clone()),
                            dims: remaining_dims.iter().map(|d| *d as usize).collect(),
                        }
                    };

                    let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                    let ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                    let elem_addr = self.builder.index_addr(base.addr, index_val, ptr_ty);
                    base.addr = elem_addr;

                    curr_ty = elem_ty; // for multi-dimensional arrays
                }

                Ok(PlaceAddr {
                    addr: base.addr,
                    value_ty: self.type_lowerer.lower_type_id(place.ty),
                })
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
