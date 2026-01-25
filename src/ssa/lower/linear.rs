//! Straight-line (single-block) lowering routines.

use crate::ssa::lower::locals::LocalValue;
use crate::ssa::lower::lowerer::{BranchResult, FuncLowerer, LinearValue, StmtOutcome};
use crate::ssa::lower::mapping::{map_binop, map_cmp};
use crate::ssa::lower::{LoweringError, LoweringErrorKind};
use crate::ssa::model::ir::{BinOp, Terminator, UnOp, ValueId};
use crate::tree::UnaryOp;
use crate::tree::semantic as sem;
use crate::types::Type;

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Lowers a linear value expression directly from the semantic tree.
    ///
    /// This avoids constructing a parallel linear AST for the common cases.
    pub(super) fn lower_linear_value_expr(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, LoweringError> {
        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                self.with_drop_scope(expr.id, |lowerer| {
                    for item in items {
                        match item {
                            sem::BlockItem::Stmt(stmt) => {
                                match lowerer.lower_stmt_expr_linear(stmt)? {
                                    StmtOutcome::Continue => {}
                                    StmtOutcome::Return => {
                                        panic!(
                                            "ssa lower_linear_value_expr hit return in linear block at {:?}",
                                            stmt.span
                                        );
                                    }
                                }
                            }
                            sem::BlockItem::Expr(expr) => {
                                let _ = lowerer.lower_linear_value_expr(expr)?;
                            }
                        }
                    }

                    if let Some(tail) = tail {
                        return lowerer.lower_linear_value_expr(tail);
                    }

                    let ty = lowerer.type_lowerer.lower_type_id(expr.ty);
                    Ok(lowerer.builder.const_unit(ty))
                })
            }

            sem::ValueExprKind::UnitLit => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_unit(ty))
            }
            sem::ValueExprKind::IntLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let (signed, bits) = self.type_lowerer.int_info(expr.ty);
                Ok(self.builder.const_int(*value as i128, signed, bits, ty))
            }
            sem::ValueExprKind::BoolLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_bool(*value, ty))
            }
            sem::ValueExprKind::CharLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let signed = false;
                let bits = 32;
                Ok(self
                    .builder
                    .const_int(*value as u32 as i128, signed, bits, ty))
            }
            sem::ValueExprKind::StringLit { value } => {
                let string_ty = self.type_lowerer.lower_type_id(expr.ty);
                let slot = self.alloc_value_slot(string_ty);

                // pointer to global bytes
                let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
                let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
                let global_id = self.add_global_bytes(value.as_bytes().to_vec());
                let ptr_val = self.builder.const_global_addr(global_id, u8_ptr_ty);

                // length and capacity (same for string literals)
                let len_ty = self.type_lowerer.lower_type(&Type::uint(32));
                let len_val = self
                    .builder
                    .const_int(value.len() as i128, false, 32, len_ty);
                let cap_val = self.builder.const_int(0, false, 32, len_ty);

                // store fields in string struct
                self.store_field(slot.addr, 0, u8_ptr_ty, ptr_val);
                self.store_field(slot.addr, 1, len_ty, len_val);
                self.store_field(slot.addr, 2, len_ty, cap_val);

                Ok(self.load_slot(&slot))
            }

            sem::ValueExprKind::Range { start, .. } => {
                // Range values are represented as u64 in SSA (bounds live in the type).
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_int(*start as i128, false, 64, ty))
            }

            sem::ValueExprKind::StringFmt { plan } => {
                let string_ty = self.type_lowerer.lower_type_id(expr.ty);
                match plan.kind {
                    sem::FmtKind::View => self.lower_string_fmt_view(plan, string_ty),
                    sem::FmtKind::Owned => self.lower_string_fmt_owned(plan, string_ty),
                }
            }

            sem::ValueExprKind::ArrayLit { init, .. } => {
                // Allocate a local for the array and get its address
                let array_ty = self.type_lowerer.lower_type_id(expr.ty);
                let addr = self.alloc_local_addr(array_ty);

                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));

                // Store each element
                match init {
                    sem::ArrayLitInit::Elems(elems) => {
                        for (i, elem_expr) in elems.iter().enumerate() {
                            let value = self.lower_linear_value_expr(elem_expr)?;
                            let index_val = self.builder.const_int(i as i128, false, 64, u64_ty);
                            let elem_ty = self.type_lowerer.lower_type_id(elem_expr.ty);
                            let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ty);
                            let elem_addr = self.builder.index_addr(addr, index_val, elem_ptr_ty);
                            self.builder.store(elem_addr, value);
                        }
                    }
                    sem::ArrayLitInit::Repeat(expr, count) => {
                        let value = self.lower_linear_value_expr(expr)?;
                        let elem_ty = self.type_lowerer.lower_type_id(expr.ty);
                        for i in 0..*count {
                            let index_val = self.builder.const_int(i as i128, false, 64, u64_ty);
                            let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ty);
                            let elem_addr = self.builder.index_addr(addr, index_val, elem_ptr_ty);
                            self.builder.store(elem_addr, value);
                        }
                    }
                }

                // Load the array value
                Ok(self.builder.load(addr, array_ty))
            }

            sem::ValueExprKind::TupleLit(items) => {
                // Allocate a local for the tuple and get its address
                let tuple_ty = self.type_lowerer.lower_type_id(expr.ty);
                let slot = self.alloc_value_slot(tuple_ty);

                // Store each field
                for (i, elem_expr) in items.iter().enumerate() {
                    let value = self.lower_linear_value_expr(elem_expr)?;
                    let field_ty = self.lower_tuple_field_ty(expr.ty, i);
                    self.store_field(slot.addr, i, field_ty, value);
                }

                // Load the tuple value
                Ok(self.load_slot(&slot))
            }

            sem::ValueExprKind::StructLit { fields, .. } => {
                // Allocate a local for the struct and get its address
                let struct_ty = self.type_lowerer.lower_type_id(expr.ty);
                let slot = self.alloc_value_slot(struct_ty);

                // Store each field
                for field in fields.iter() {
                    let value = self.lower_linear_value_expr(&field.value)?;
                    let (field_index, field_ty) = self.lower_struct_field_ty(expr.ty, &field.name);
                    self.store_field(slot.addr, field_index, field_ty, value);
                }

                // Load the struct value
                Ok(self.load_slot(&slot))
            }

            sem::ValueExprKind::StructUpdate { target, fields } => {
                // Allocate a local for the updated struct and get its address
                let struct_ty = self.type_lowerer.lower_type_id(expr.ty);
                let slot = self.alloc_value_slot(struct_ty);

                // Copy the base struct
                let base_value = self.lower_linear_value_expr(target)?;
                self.builder.store(slot.addr, base_value);

                // Overwrite the updated fields
                for field in fields.iter() {
                    let value = self.lower_linear_value_expr(&field.value)?;
                    let (field_index, field_ty) = self.lower_struct_field_ty(expr.ty, &field.name);
                    self.store_field(slot.addr, field_index, field_ty, value);
                }

                // Load the updated struct value
                Ok(self.load_slot(&slot))
            }

            sem::ValueExprKind::EnumVariant {
                enum_name: _,
                variant,
                payload,
            } => {
                let (tag_ty, blob_ty, variant_tag, field_offsets, field_tys) = {
                    let layout = self.type_lowerer.enum_layout(expr.ty);
                    let variant_layout = layout.variant_by_name(variant);
                    (
                        layout.tag_ty,
                        layout.blob_ty,
                        variant_layout.tag,
                        variant_layout.field_offsets.clone(),
                        variant_layout.field_tys.clone(),
                    )
                };

                // Allocate a local for the enum and get its address.
                let enum_ty = self.type_lowerer.lower_type_id(expr.ty);
                let slot = self.alloc_value_slot(enum_ty);

                // Store the tag in field 0.
                let tag_val = self
                    .builder
                    .const_int(variant_tag as i128, false, 32, tag_ty);
                self.store_field(slot.addr, 0, tag_ty, tag_val);

                // Store each payload field into the blob (field 1) at its offset.
                let payload_ptr = self.field_addr_typed(slot.addr, 1, blob_ty);

                if field_offsets.len() != payload.len() || field_tys.len() != payload.len() {
                    panic!(
                        "ssa enum variant payload mismatch for {}: {} offsets, {} tys, {} values",
                        variant,
                        field_offsets.len(),
                        field_tys.len(),
                        payload.len()
                    );
                }

                for ((value_expr, offset), value_ty) in payload
                    .iter()
                    .zip(field_offsets.iter())
                    .zip(field_tys.iter().copied())
                {
                    let value = self.lower_linear_value_expr(value_expr)?;
                    self.store_into_blob(payload_ptr, *offset, value, value_ty);
                }

                Ok(self.load_slot(&slot))
            }

            sem::ValueExprKind::UnaryOp { op, expr: inner } => {
                let value = self.lower_linear_value_expr(inner)?;
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let result = match op {
                    UnaryOp::Neg => self.builder.unop(UnOp::Neg, value, ty),
                    UnaryOp::LogicalNot => self.builder.unop(UnOp::Not, value, ty),
                    UnaryOp::BitNot => self.builder.unop(UnOp::BitNot, value, ty),
                };
                Ok(result)
            }

            sem::ValueExprKind::BinOp { left, op, right } => {
                let lhs = self.lower_linear_value_expr(left)?;
                let rhs = self.lower_linear_value_expr(right)?;
                let ty = self.type_lowerer.lower_type_id(expr.ty);

                if let Some(binop) = map_binop(*op) {
                    return Ok(self.builder.binop(binop, lhs, rhs, ty));
                }
                if let Some(cmp) = map_cmp(*op) {
                    return Ok(self.builder.cmp(cmp, lhs, rhs, ty));
                }
                Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr))
            }

            sem::ValueExprKind::Slice { target, start, end } => {
                // Build a slice value { ptr, len } from a place and optional bounds.
                let plan = self.type_map.lookup_slice_plan(expr.id).unwrap_or_else(|| {
                    panic!("ssa slice missing plan for expr {:?}", expr.id);
                });

                let Type::Slice { elem_ty } = self.type_map.type_table().get(expr.ty).clone()
                else {
                    panic!("ssa slice expr has non-slice type");
                };

                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);

                // Resolve the base pointer and length from the slice plan.
                let (base_ptr, base_len) = match plan.base {
                    sem::SliceBaseKind::Array { len, deref_count } => {
                        let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                        let Type::Array { .. } = base_ty else {
                            panic!("ssa slice on non-array base {:?}", base_ty);
                        };
                        let view = self.load_array_view(base_addr, elem_ptr_ty, len);
                        (view.ptr, view.len)
                    }
                    sem::SliceBaseKind::Slice { deref_count } => {
                        let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                        let Type::Slice { .. } = base_ty else {
                            panic!("ssa slice on non-slice base {:?}", base_ty);
                        };
                        let view = self.load_slice_view(base_addr, elem_ptr_ty);
                        (view.ptr, view.len)
                    }
                    sem::SliceBaseKind::String { deref_count } => {
                        let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                        let Type::String = base_ty else {
                            panic!("ssa slice on non-string base {:?}", base_ty);
                        };
                        let view = self.load_string_view(base_addr);
                        (view.ptr, view.len)
                    }
                };

                // Evaluate bounds (default start=0, end=base_len).
                let start_val = match start {
                    Some(expr) => self.lower_linear_value_expr(expr)?,
                    None => self.builder.const_int(0, false, 64, u64_ty),
                };
                let end_val = match end {
                    Some(expr) => self.lower_linear_value_expr(expr)?,
                    None => base_len,
                };

                let start_check = start
                    .as_deref()
                    .is_some_and(|expr| !matches!(expr.kind, sem::ValueExprKind::IntLit(0)));
                let end_check = end.is_some();

                if start_check || end_check {
                    let zero = self.builder.const_int(0, false, 64, u64_ty);
                    let one = self.builder.const_int(1, false, 64, u64_ty);
                    let max_excl = self.builder.binop(BinOp::Add, base_len, one, u64_ty);

                    // Enforce start <= base_len when it is not trivially zero.
                    if start_check {
                        self.emit_range_check(start_val, zero, max_excl);
                    }
                    // Enforce start <= end <= base_len when an explicit end is provided.
                    if end_check {
                        self.emit_range_check(end_val, start_val, max_excl);
                    }
                }

                let slice_ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.emit_slice_value(
                    slice_ty,
                    elem_ptr_ty,
                    u64_ty,
                    base_ptr,
                    base_len,
                    start_val,
                    end_val,
                ))
            }

            sem::ValueExprKind::Len { place } => {
                // Length is an internal node for array/slice iteration.
                let place_ty = self.type_map.type_table().get(place.ty).clone();
                match place_ty {
                    Type::Array { dims, .. } => {
                        let len = dims
                            .first()
                            .copied()
                            .unwrap_or_else(|| panic!("ssa len on array with empty dims"));
                        let ty = self.type_lowerer.lower_type_id(expr.ty);
                        Ok(self.builder.const_int(len as i128, false, 64, ty))
                    }
                    Type::Slice { .. } => {
                        let place_addr = self.lower_place_addr(place)?;
                        let len_ty = self.type_lowerer.lower_type(&Type::uint(64));
                        let len_addr = self.field_addr_typed(place_addr.addr, 1, len_ty);
                        Ok(self.builder.load(len_addr, len_ty))
                    }
                    other => panic!("ssa len on unsupported type {:?}", other),
                }
            }

            sem::ValueExprKind::Move { place } | sem::ValueExprKind::ImplicitMove { place } => {
                match &place.kind {
                    sem::PlaceExprKind::Var { def_id, .. } => {
                        self.set_drop_flag_for_def(*def_id, false);
                        Ok(self.load_local_value(*def_id))
                    }
                    _ => {
                        let place_addr = self.lower_place_addr(place)?;
                        Ok(self.builder.load(place_addr.addr, place_addr.value_ty))
                    }
                }
            }

            sem::ValueExprKind::Coerce { kind, expr: inner } => match kind {
                crate::tree::CoerceKind::ArrayToSlice => {
                    let plan = self.type_map.lookup_slice_plan(expr.id).unwrap_or_else(|| {
                        panic!("ssa coerce missing slice plan for expr {:?}", expr.id);
                    });

                    let Type::Slice { elem_ty } = self.type_map.type_table().get(expr.ty).clone()
                    else {
                        panic!("ssa coerce array-to-slice has non-slice type");
                    };

                    let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                    let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                    let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);

                    let (base_addr, base_len) = match plan.base {
                        sem::SliceBaseKind::Array { len, deref_count } => {
                            let base_ty = self.type_map.type_table().get(inner.ty).clone();

                            // Prefer a place-based lowering to reuse the base address; otherwise
                            // materialize the value into a temporary slot.
                            let (base_addr, base_ty) = match &inner.kind {
                                sem::ValueExprKind::Load { place }
                                | sem::ValueExprKind::Move { place }
                                | sem::ValueExprKind::ImplicitMove { place } => {
                                    self.resolve_deref_base(place, deref_count)?
                                }
                                _ => {
                                    let value = self.lower_linear_value_expr(inner)?;
                                    if deref_count == 0 {
                                        let array_ty = self.type_lowerer.lower_type_id(inner.ty);
                                        let addr = self.alloc_local_addr(array_ty);
                                        self.builder.store(addr, value);
                                        (addr, base_ty)
                                    } else {
                                        self.resolve_deref_base_value(value, base_ty, deref_count)
                                    }
                                }
                            };

                            let Type::Array { .. } = base_ty else {
                                panic!("ssa coerce array-to-slice on {:?}", base_ty);
                            };

                            let zero = self.builder.const_int(0, false, 64, u64_ty);
                            let ptr = self.builder.index_addr(base_addr, zero, elem_ptr_ty);
                            let len_val = self.builder.const_int(len as i128, false, 64, u64_ty);
                            (ptr, len_val)
                        }
                        other => {
                            panic!("ssa coerce array-to-slice with base {:?}", other);
                        }
                    };

                    let start_val = self.builder.const_int(0, false, 64, u64_ty);
                    let end_val = base_len;
                    let slice_ty = self.type_lowerer.lower_type_id(expr.ty);
                    Ok(self.emit_slice_value(
                        slice_ty,
                        elem_ptr_ty,
                        u64_ty,
                        base_addr,
                        base_len,
                        start_val,
                        end_val,
                    ))
                }
            },

            sem::ValueExprKind::Load { place } => match &place.kind {
                sem::PlaceExprKind::Var { def_id, .. } => {
                    if self.locals.get(*def_id).is_some() {
                        Ok(self.load_local_value(*def_id))
                    } else {
                        let def = self
                            .def_table
                            .lookup_def(*def_id)
                            .unwrap_or_else(|| panic!("ssa load missing def {:?}", def_id));
                        match def.kind {
                            crate::resolve::DefKind::FuncDef { .. }
                            | crate::resolve::DefKind::FuncDecl { .. } => {
                                let value_ty = self.type_lowerer.lower_type_id(place.ty);
                                Ok(self.builder.const_func_addr(*def_id, value_ty))
                            }
                            _ => panic!("ssa load missing local for non-function def {:?}", def_id),
                        }
                    }
                }
                _ => {
                    let place_addr = self.lower_place_addr(place)?;
                    Ok(self.builder.load(place_addr.addr, place_addr.value_ty))
                }
            },

            sem::ValueExprKind::AddrOf { place } => {
                let place_addr = self.lower_place_addr(place)?;
                Ok(place_addr.addr)
            }

            sem::ValueExprKind::Call { callee, args } => self.lower_call_expr(expr, callee, args),

            sem::ValueExprKind::MethodCall { receiver, args, .. } => {
                self.lower_method_call_expr(expr, receiver, args)
            }

            sem::ValueExprKind::ClosureRef { def_id } => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_func_addr(*def_id, ty))
            }

            _ => Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr)),
        }
    }

    /// Lowers a linear statement directly from the semantic tree.
    pub(super) fn lower_stmt_expr_linear(
        &mut self,
        stmt: &sem::StmtExpr,
    ) -> Result<StmtOutcome, LoweringError> {
        match &stmt.kind {
            sem::StmtExprKind::LetBind { pattern, value, .. }
            | sem::StmtExprKind::VarBind { pattern, value, .. } => {
                let value_expr = value;
                let value = self.lower_linear_value_expr(value_expr)?;
                let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                let value_ty = self.type_map.type_table().get(value_expr.ty).clone();
                self.bind_pattern(pattern, LocalValue::value(value, ty), &value_ty)?;
                Ok(StmtOutcome::Continue)
            }

            sem::StmtExprKind::VarDecl { def_id, .. } => {
                let def = self
                    .def_table
                    .lookup_def(*def_id)
                    .unwrap_or_else(|| panic!("ssa var decl missing def {:?}", def_id));
                let ty_id = self
                    .type_map
                    .lookup_def_type_id(def)
                    .unwrap_or_else(|| panic!("ssa var decl missing type for {:?}", def_id));
                let ir_ty = self.type_lowerer.lower_type_id(ty_id);
                let addr = self.alloc_local_addr(ir_ty);
                self.locals.insert(*def_id, LocalValue::addr(addr, ir_ty));
                // Start drop tracking as uninitialized until the first init assignment.
                self.set_drop_flag_for_def(*def_id, false);
                Ok(StmtOutcome::Continue)
            }

            sem::StmtExprKind::Assign {
                assignee,
                value,
                init,
                ..
            } => {
                let value_expr = value;
                let value = self.lower_linear_value_expr(value_expr)?;
                match &assignee.kind {
                    sem::PlaceExprKind::Var { def_id, .. } => {
                        let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                        self.assign_local_value(*def_id, value, ty);
                        if init.is_init || init.promotes_full {
                            self.set_drop_flag_for_def(*def_id, true);
                        }
                        Ok(StmtOutcome::Continue)
                    }
                    _ => {
                        let place_addr = self.lower_place_addr(assignee)?;
                        self.builder.store(place_addr.addr, value);
                        Ok(StmtOutcome::Continue)
                    }
                }
            }

            sem::StmtExprKind::Return { value } => {
                let value = match value {
                    Some(expr) => Some(self.lower_linear_value_expr(expr)?),
                    None => None,
                };
                self.emit_drops_for_stmt(stmt.id)?;
                self.builder.terminate(Terminator::Return { value });
                Ok(StmtOutcome::Return)
            }

            sem::StmtExprKind::While { .. }
            | sem::StmtExprKind::For { .. }
            | sem::StmtExprKind::Break
            | sem::StmtExprKind::Continue => {
                Err(self.err_stmt(stmt, LoweringErrorKind::UnsupportedStmt))
            }
        }
    }

    /// Convenience: lower a ValueExpr and return its value in one step.
    pub(super) fn lower_linear_expr_value(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, LoweringError> {
        match self.lower_value_expr(expr)? {
            BranchResult::Value(value) => Ok(value),
            BranchResult::Return => {
                panic!(
                    "ssa lower_linear_expr_value hit return in linear context at {:?}",
                    expr.span
                );
            }
        }
    }

    /// Builds a slice value from a base pointer and length.
    fn emit_slice_value(
        &mut self,
        slice_ty: crate::ssa::IrTypeId,
        elem_ptr_ty: crate::ssa::IrTypeId,
        u64_ty: crate::ssa::IrTypeId,
        base_ptr: ValueId,
        _base_len: ValueId,
        start_val: ValueId,
        end_val: ValueId,
    ) -> LinearValue {
        // Compute the resulting slice pointer and length.
        let ptr_at = self.builder.index_addr(base_ptr, start_val, elem_ptr_ty);
        let len_at = self.builder.binop(BinOp::Sub, end_val, start_val, u64_ty);

        // Materialize the slice struct and load it as a value.
        let slot = self.alloc_value_slot(slice_ty);

        self.store_field(slot.addr, 0, elem_ptr_ty, ptr_at);
        self.store_field(slot.addr, 1, u64_ty, len_at);

        self.load_slot(&slot)
    }
}
