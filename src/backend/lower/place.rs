//! Place lowering helpers for SSA explicit-memory ops.

use crate::backend::lower::LowerToIrError;
use crate::ir::{IrTypeId, IrTypeKind, ValueId};
use crate::tree::semantic as sem;
use crate::types::Type;

use super::FuncLowerer;

/// Address + value type for a lowered place.
pub(super) struct PlaceAddr {
    pub(super) addr: ValueId,
    pub(super) value_ty: IrTypeId,
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Lowers a place expression into an address and its value type.
    pub(super) fn lower_place_addr(
        &mut self,
        place: &sem::PlaceExpr,
    ) -> Result<PlaceAddr, LowerToIrError> {
        match &place.kind {
            sem::PlaceExprKind::Var { def_id, .. } => {
                let value_ty = self.type_lowerer.lower_type_id(place.ty);
                let addr = self.ensure_local_addr(*def_id, value_ty);
                Ok(PlaceAddr { addr, value_ty })
            }
            sem::PlaceExprKind::Deref { value } => {
                let addr = self.lower_linear_value_expr(value)?;
                let value_ty = self.type_lowerer.lower_type_id(place.ty);
                Ok(PlaceAddr { addr, value_ty })
            }
            sem::PlaceExprKind::TupleField { target, index } => {
                let (base_addr, base_ty) = self.lower_place_deref_base(target)?;
                let (_field_ty, field_ir_ty) = self.tuple_field_from_type(&base_ty, *index);
                let addr = self.field_addr_typed(base_addr, *index, field_ir_ty);
                Ok(PlaceAddr {
                    addr,
                    value_ty: field_ir_ty,
                })
            }
            sem::PlaceExprKind::StructField { target, field } => {
                let (base_addr, base_ty) = self.lower_place_deref_base(target)?;
                if let Some(place_addr) =
                    self.lower_builtin_property_place(base_addr, &base_ty, field, place.ty)
                {
                    return Ok(place_addr);
                }
                let (field_index, _field_ty, field_ir_ty) =
                    self.struct_field_from_type(&base_ty, field);
                let addr = self.field_addr_typed(base_addr, field_index, field_ir_ty);
                Ok(PlaceAddr {
                    addr,
                    value_ty: field_ir_ty,
                })
            }
            sem::PlaceExprKind::ArrayIndex { target, indices } => {
                let plan = self.index_plan(place.id);

                match plan.base {
                    sem::IndexBaseKind::Array { deref_count, .. } => {
                        let (mut base_addr, mut curr_ty) =
                            self.resolve_deref_base(target, deref_count)?;

                        // Walk indices and compute element/sub-array type in each step.
                        for index_expr in indices {
                            let index_val = self.lower_linear_value_expr(index_expr)?;
                            let Type::Array { dims, .. } = &curr_ty else {
                                panic!("backend array index too many indices for {:?}", curr_ty);
                            };
                            let len = dims.first().copied().unwrap_or_else(|| {
                                panic!("backend array index missing dims {:?}", curr_ty)
                            });
                            let next_ty = curr_ty.array_item_type().unwrap_or_else(|| {
                                panic!("backend array index too many indices for {:?}", curr_ty);
                            });

                            let elem_ir_ty = self.type_lowerer.lower_type(&next_ty);
                            let ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                            let view = self.load_array_view(base_addr, ptr_ty, len as u64);
                            base_addr = self.index_with_bounds(view, index_val, ptr_ty);

                            curr_ty = next_ty;
                        }

                        Ok(PlaceAddr {
                            addr: base_addr,
                            value_ty: self.type_lowerer.lower_type_id(place.ty),
                        })
                    }
                    sem::IndexBaseKind::Slice { deref_count } => {
                        if indices.len() != 1 {
                            panic!("backend slice index expects exactly one index");
                        }

                        let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                        let Type::Slice { elem_ty } = base_ty else {
                            panic!("backend slice index on non-slice base {:?}", base_ty);
                        };

                        // Load the slice data pointer, then index into it.
                        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                        let view = self.load_slice_view(base_addr, elem_ptr_ty);
                        let index_val = self.lower_linear_value_expr(&indices[0])?;
                        let addr = self.index_with_bounds(view, index_val, elem_ptr_ty);
                        Ok(PlaceAddr {
                            addr,
                            value_ty: elem_ir_ty,
                        })
                    }
                    sem::IndexBaseKind::String { deref_count } => {
                        if indices.len() != 1 {
                            panic!("backend string index expects exactly one index");
                        }

                        let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                        let Type::String = base_ty else {
                            panic!("backend string index on non-string base {:?}", base_ty);
                        };

                        // Load the string data pointer, then index into it.
                        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
                        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
                        let view = self.load_string_view(base_addr);
                        let index_val = self.lower_linear_value_expr(&indices[0])?;
                        let addr = self.index_with_bounds(view, index_val, u8_ptr_ty);
                        Ok(PlaceAddr {
                            addr,
                            value_ty: u8_ty,
                        })
                    }
                    sem::IndexBaseKind::DynArray { deref_count } => {
                        if indices.len() != 1 {
                            panic!("backend dyn-array index expects exactly one index");
                        }

                        let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                        let Type::DynArray { elem_ty } = base_ty else {
                            panic!(
                                "backend dyn-array index on non-dyn-array base {:?}",
                                base_ty
                            );
                        };

                        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                        let view = self.load_dyn_array_view(base_addr, elem_ptr_ty);
                        let index_val = self.lower_linear_value_expr(&indices[0])?;
                        let addr = self.index_with_bounds(view, index_val, elem_ptr_ty);
                        Ok(PlaceAddr {
                            addr,
                            value_ty: elem_ir_ty,
                        })
                    }
                }
            }
        }
    }

    fn lower_place_deref_base(
        &mut self,
        target: &sem::PlaceExpr,
    ) -> Result<(ValueId, Type), LowerToIrError> {
        let mut base = self.lower_place_addr(target)?;
        let mut curr_ty = self.type_map.type_table().get(target.ty).clone();

        while let Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } = curr_ty {
            let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
            let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
            base.addr = self.builder.load(base.addr, ptr_ir_ty);
            curr_ty = (*elem_ty).clone();
        }

        Ok((base.addr, curr_ty))
    }

    fn lower_builtin_property_place(
        &mut self,
        base_addr: ValueId,
        base_ty: &Type,
        field: &str,
        place_ty: crate::types::TypeId,
    ) -> Option<PlaceAddr> {
        let target_ir_ty = self.type_lowerer.lower_type_id(place_ty);
        let value = match field {
            "len" => match base_ty {
                Type::Array { dims, .. } => {
                    let len = dims.first().copied().unwrap_or(0) as i128;
                    self.build_int_const_for_ty(len, target_ir_ty)
                }
                Type::Slice { .. } => {
                    let src_ty = self.type_lowerer.lower_type(&Type::uint(64));
                    let raw = self.load_field(base_addr, 1, src_ty);
                    self.cast_int_if_needed(raw, src_ty, target_ir_ty)
                }
                Type::DynArray { .. } => {
                    let src_ty = self.type_lowerer.lower_type(&Type::uint(32));
                    let raw = self.load_field(base_addr, 1, src_ty);
                    self.cast_int_if_needed(raw, src_ty, target_ir_ty)
                }
                Type::Set { .. } => {
                    let src_ty = self.type_lowerer.lower_type(&Type::uint(32));
                    let raw = self.load_field(base_addr, 1, src_ty);
                    self.cast_int_if_needed(raw, src_ty, target_ir_ty)
                }
                Type::String => {
                    let src_ty = self.type_lowerer.lower_type(&Type::uint(32));
                    let raw = self.load_field(base_addr, 1, src_ty);
                    self.cast_int_if_needed(raw, src_ty, target_ir_ty)
                }
                _ => return None,
            },
            "capacity" => match base_ty {
                Type::DynArray { .. } => {
                    let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
                    let cap_raw = self.load_field(base_addr, 2, u32_ty);
                    let cap_mask = self.builder.const_int(0x7fff_ffff, false, 32, u32_ty);
                    let cap = self
                        .builder
                        .binop(crate::ir::BinOp::And, cap_raw, cap_mask, u32_ty);
                    self.cast_int_if_needed(cap, u32_ty, target_ir_ty)
                }
                Type::Set { .. } => {
                    let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
                    let cap_raw = self.load_field(base_addr, 2, u32_ty);
                    let cap_mask = self.builder.const_int(0x7fff_ffff, false, 32, u32_ty);
                    let cap = self
                        .builder
                        .binop(crate::ir::BinOp::And, cap_raw, cap_mask, u32_ty);
                    self.cast_int_if_needed(cap, u32_ty, target_ir_ty)
                }
                _ => return None,
            },
            "is_empty" => match base_ty {
                Type::DynArray { .. } => {
                    let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
                    let len = self.load_field(base_addr, 1, u32_ty);
                    let zero = self.builder.const_int(0, false, 32, u32_ty);
                    self.builder
                        .cmp(crate::ir::CmpOp::Eq, len, zero, target_ir_ty)
                }
                Type::Set { .. } => {
                    let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
                    let len = self.load_field(base_addr, 1, u32_ty);
                    let zero = self.builder.const_int(0, false, 32, u32_ty);
                    self.builder
                        .cmp(crate::ir::CmpOp::Eq, len, zero, target_ir_ty)
                }
                _ => return None,
            },
            _ => return None,
        };

        let addr = self.alloc_local_addr(target_ir_ty);
        self.builder.store(addr, value);
        Some(PlaceAddr {
            addr,
            value_ty: target_ir_ty,
        })
    }

    fn build_int_const_for_ty(&mut self, value: i128, ty: IrTypeId) -> ValueId {
        match self.type_lowerer.ir_type_cache.kind(ty) {
            IrTypeKind::Int { signed, bits } => self.builder.const_int(value, *signed, *bits, ty),
            other => panic!(
                "backend len constant requires integer type, found {:?}",
                other
            ),
        }
    }

    pub(super) fn cast_int_if_needed(
        &mut self,
        value: ValueId,
        from_ty: IrTypeId,
        to_ty: IrTypeId,
    ) -> ValueId {
        if from_ty == to_ty {
            return value;
        }

        let (from_bits, to_bits) = match (
            self.type_lowerer.ir_type_cache.kind(from_ty),
            self.type_lowerer.ir_type_cache.kind(to_ty),
        ) {
            (
                IrTypeKind::Int {
                    bits: from_bits, ..
                },
                IrTypeKind::Int { bits: to_bits, .. },
            ) => (*from_bits, *to_bits),
            (from_kind, to_kind) => {
                panic!(
                    "backend len cast expects integer types, got {:?} -> {:?}",
                    from_kind, to_kind
                )
            }
        };

        if to_bits > from_bits {
            self.builder.int_extend(value, to_ty, false)
        } else if to_bits < from_bits {
            self.builder.int_trunc(value, to_ty)
        } else {
            value
        }
    }
}
