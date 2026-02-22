//! Straight-line (single-block) lowering routines.

use crate::core::backend::lower::LowerToIrError;
use crate::core::backend::lower::locals::LocalValue;
use crate::core::backend::lower::lowerer::{BranchResult, FuncLowerer, LinearValue, StmtOutcome};
use crate::core::backend::lower::mapping::{map_binop, map_cmp};
use crate::core::ir::{BinOp, Callee, CastKind, IrTypeId, RuntimeFn, Terminator, UnOp, ValueId};
use crate::core::resolve::DefKind;
use crate::core::tree::semantic as sem;
use crate::core::tree::{BinaryOp, CoerceKind, ParamMode, UnaryOp};
use crate::core::types::Type;

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Lowers a linear value expression directly from the semantic tree.
    ///
    /// This avoids constructing a parallel linear AST for the common cases.
    pub(super) fn lower_linear_value_expr(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, LowerToIrError> {
        match self.lower_value_expr_value(expr)? {
            BranchResult::Value(value) => Ok(value),
            BranchResult::Return => {
                panic!(
                    "backend lower_linear_value_expr hit return in linear context at {:?}",
                    expr.span
                );
            }
        }
    }

    /// Lowers a value expression, allowing branching subexpressions to return early.
    pub(super) fn lower_value_expr_value(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => self.lower_block_expr(expr, items, tail),

            sem::ValueExprKind::UnitLit => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_unit(ty).into())
            }
            sem::ValueExprKind::IntLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let (signed, bits) = self.type_lowerer.int_info(expr.ty);
                Ok(self
                    .builder
                    .const_int(*value as i128, signed, bits, ty)
                    .into())
            }
            sem::ValueExprKind::BoolLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_bool(*value, ty).into())
            }
            sem::ValueExprKind::CharLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self
                    .builder
                    .const_int(*value as u32 as i128, false, 32, ty)
                    .into())
            }
            sem::ValueExprKind::StringLit { value } => self.lower_string_lit_expr(expr, value),

            sem::ValueExprKind::Range { start, end } => {
                let (sem::ValueExprKind::IntLit(start), sem::ValueExprKind::IntLit(_end)) =
                    (&start.kind, &end.kind)
                else {
                    panic!("backend range values require literal bounds");
                };
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_int(*start as i128, false, 64, ty).into())
            }

            sem::ValueExprKind::StringFmt { plan } => {
                let string_ty = self.type_lowerer.lower_type_id(expr.ty);
                let value = match plan.kind {
                    sem::FmtKind::View => self.lower_string_fmt_view(plan, string_ty)?,
                    sem::FmtKind::Owned => self.lower_string_fmt_owned(plan, string_ty)?,
                };
                let Some(value) = value else {
                    return Ok(BranchResult::Return);
                };
                Ok(value.into())
            }

            sem::ValueExprKind::ArrayLit { init, .. } => self.lower_array_lit_expr(expr, init),
            sem::ValueExprKind::SetLit { elems, .. } => self.lower_set_lit_expr(expr, elems),
            sem::ValueExprKind::MapLit { entries, .. } => self.lower_map_lit_expr(expr, entries),

            sem::ValueExprKind::TupleLit(items) => self.lower_tuple_lit_expr(expr, items),
            sem::ValueExprKind::StructLit { fields, .. } => {
                self.lower_struct_lit_expr(expr, fields)
            }
            sem::ValueExprKind::StructUpdate { target, fields } => {
                self.lower_struct_update_expr(expr, target, fields)
            }

            sem::ValueExprKind::EnumVariant {
                enum_name: _,
                variant,
                payload,
            } => self.lower_enum_variant_expr(expr, variant, payload),

            sem::ValueExprKind::Try {
                fallible_expr,
                on_error,
            } => {
                if let Some(handler) = on_error {
                    return self.lower_try_handle(expr, fallible_expr, handler);
                }
                self.lower_try_propagate(expr, fallible_expr)
            }

            sem::ValueExprKind::UnaryOp { op, expr: inner } => {
                let Some(value) = self.lower_value_expr_opt(inner)? else {
                    return Ok(BranchResult::Return);
                };
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let result = match op {
                    UnaryOp::Neg => self.builder.unop(UnOp::Neg, value, ty),
                    UnaryOp::LogicalNot => self.builder.unop(UnOp::Not, value, ty),
                    UnaryOp::BitNot => self.builder.unop(UnOp::BitNot, value, ty),
                };
                Ok(result.into())
            }

            sem::ValueExprKind::HeapAlloc { expr: inner } => {
                self.lower_heap_alloc_expr(expr, inner)
            }
            sem::ValueExprKind::BinOp { left, op, right } => {
                self.lower_binop_expr(expr, left, *op, right)
            }
            sem::ValueExprKind::Slice { target, start, end } => {
                self.lower_slice_expr(expr, target, start.as_deref(), end.as_deref())
            }
            sem::ValueExprKind::MapGet { target, key } => {
                self.lower_map_get_expr(expr, target, key)
            }
            sem::ValueExprKind::Len { place } => self.lower_len_expr(expr, place),

            sem::ValueExprKind::Move { place } | sem::ValueExprKind::ImplicitMove { place } => {
                match &place.kind {
                    sem::PlaceExprKind::Var { def_id, .. } => {
                        self.set_drop_flag_for_def(*def_id, false);
                        Ok(self.load_local_value(*def_id).into())
                    }
                    _ => {
                        let place_addr = self.lower_place_addr(place)?;
                        Ok(self
                            .builder
                            .load(place_addr.addr, place_addr.value_ty)
                            .into())
                    }
                }
            }

            sem::ValueExprKind::Coerce { kind, expr: inner } => {
                self.lower_coerce_expr(expr, *kind, inner)
            }

            sem::ValueExprKind::Load { place } => match &place.kind {
                sem::PlaceExprKind::Var { def_id, .. } => {
                    if self.locals.get(*def_id).is_some() {
                        Ok(self.load_local_value(*def_id).into())
                    } else {
                        let def = self.def(*def_id);
                        match def.kind {
                            DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => {
                                let value_ty = self.type_lowerer.lower_type_id(place.ty);
                                Ok(self.builder.const_func_addr(*def_id, value_ty).into())
                            }
                            _ => panic!(
                                "backend load missing local for non-function def {:?}",
                                def_id
                            ),
                        }
                    }
                }
                _ => {
                    let place_addr = self.lower_place_addr(place)?;
                    Ok(self
                        .builder
                        .load(place_addr.addr, place_addr.value_ty)
                        .into())
                }
            },

            sem::ValueExprKind::AddrOf { place } => {
                let place_addr = self.lower_place_addr(place)?;
                Ok(place_addr.addr.into())
            }

            sem::ValueExprKind::Call { callee, args } => {
                let Some(value) = self.lower_call_expr(expr, callee, args)? else {
                    return Ok(BranchResult::Return);
                };
                Ok(value.into())
            }

            sem::ValueExprKind::MethodCall { receiver, args, .. } => {
                let Some(value) = self.lower_method_call_expr(expr, receiver, args)? else {
                    return Ok(BranchResult::Return);
                };
                Ok(value.into())
            }

            sem::ValueExprKind::EmitSend { to, payload } => {
                self.lower_emit_send_expr(expr, to, payload)
            }
            sem::ValueExprKind::EmitRequest {
                to,
                payload,
                request_site_key,
            } => self.lower_emit_request_expr(expr, to, payload, *request_site_key),
            sem::ValueExprKind::Reply { cap, value } => self.lower_reply_expr(expr, cap, value),

            sem::ValueExprKind::ClosureRef { def_id } => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_func_addr(*def_id, ty).into())
            }

            _ => match self.lowering_plan(expr.id) {
                sem::LoweringPlan::Branching => self.lower_branching_value_expr(expr),
                sem::LoweringPlan::Linear => {
                    panic!(
                        "backend lower_value_expr_value unsupported expr {:?} at {:?}",
                        expr.kind, expr.span
                    );
                }
            },
        }
    }

    /// Lowers a linear statement directly from the semantic tree.
    pub(super) fn lower_stmt_expr_linear(
        &mut self,
        stmt: &sem::StmtExpr,
    ) -> Result<StmtOutcome, LowerToIrError> {
        self.annotate_stmt(stmt);
        match &stmt.kind {
            sem::StmtExprKind::LetBind { pattern, value, .. }
            | sem::StmtExprKind::VarBind { pattern, value, .. } => {
                let value_expr = value;
                let value = match self.lower_value_expr_value(value_expr)? {
                    BranchResult::Value(value) => value,
                    BranchResult::Return => return Ok(StmtOutcome::Return),
                };
                let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                let value_ty = self.type_map.type_table().get(value_expr.ty).clone();
                self.bind_pattern(pattern, LocalValue::value(value, ty), &value_ty)?;
                Ok(StmtOutcome::Continue)
            }

            sem::StmtExprKind::VarDecl { def_id, .. } => {
                let ty = self.def_type(*def_id);
                let ir_ty = self.type_lowerer.lower_type(&ty);
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
                let value = match self.lower_value_expr_value(value_expr)? {
                    BranchResult::Value(value) => value,
                    BranchResult::Return => return Ok(StmtOutcome::Return),
                };
                let value_ty = self.type_map.type_table().get(value_expr.ty).clone();
                match &assignee.kind {
                    sem::PlaceExprKind::Var { def_id, .. } => {
                        let dest_ty = self.def_type(*def_id);
                        self.emit_conversion_check(&value_ty, &dest_ty, value);
                        if let Some(mode) = self.param_mode_for(*def_id)
                            && matches!(mode, ParamMode::Out | ParamMode::InOut)
                        {
                            if !init.is_init && !init.promotes_full {
                                self.emit_drop_for_def_if_live(*def_id, value_expr.ty)?;
                            }
                            let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                            self.assign_local_value(*def_id, value, ty);
                            self.set_drop_flag_for_def(*def_id, true);
                            return Ok(StmtOutcome::Continue);
                        }
                        if !init.is_init && !init.promotes_full {
                            self.emit_drop_for_def_if_live(*def_id, value_expr.ty)?;
                        }
                        let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                        self.assign_local_value(*def_id, value, ty);
                        self.set_drop_flag_for_def(*def_id, true);
                        Ok(StmtOutcome::Continue)
                    }
                    _ => {
                        let dest_ty = self.type_map.type_table().get(assignee.ty).clone();
                        self.emit_conversion_check(&value_ty, &dest_ty, value);
                        let place_addr = self.lower_place_addr(assignee)?;
                        if !init.is_init && !init.promotes_full {
                            self.drop_value_at_addr(place_addr.addr, &dest_ty)?;
                        }
                        let ir_ty = self.type_lowerer.lower_type_id(value_expr.ty);
                        let sem_ty = self.type_map.type_table().get(value_expr.ty);
                        self.store_value_into_addr(place_addr.addr, value, sem_ty, ir_ty);
                        if init.promotes_full {
                            let mut cursor = assignee.as_ref();
                            let mut base_def = None;
                            loop {
                                match &cursor.kind {
                                    sem::PlaceExprKind::Var { def_id, .. } => {
                                        base_def = Some(*def_id);
                                        break;
                                    }
                                    sem::PlaceExprKind::StructField { target, .. }
                                    | sem::PlaceExprKind::TupleField { target, .. }
                                    | sem::PlaceExprKind::ArrayIndex { target, .. } => {
                                        cursor = target.as_ref();
                                    }
                                    sem::PlaceExprKind::Deref { .. } => break,
                                }
                            }
                            if let Some(def_id) = base_def {
                                self.set_drop_flag_for_def(def_id, true);
                            }
                        }
                        Ok(StmtOutcome::Continue)
                    }
                }
            }

            sem::StmtExprKind::Return { value } => {
                let value = match value {
                    Some(expr) => match self.lower_value_expr_value(expr)? {
                        BranchResult::Value(value) => {
                            let ty = self.type_map.type_table().get(expr.ty);
                            if matches!(ty, Type::Unit) {
                                None
                            } else {
                                Some(self.coerce_return_value(value, ty))
                            }
                        }
                        BranchResult::Return => return Ok(StmtOutcome::Return),
                    },
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
                panic!(
                    "backend lower_stmt_expr_linear unsupported stmt {:?} at {:?}",
                    stmt.kind, stmt.span
                );
            }
        }
    }

    /// Convenience: lower a ValueExpr and return its value in one step.
    pub(super) fn lower_linear_expr_value(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, LowerToIrError> {
        match self.lower_value_expr(expr)? {
            BranchResult::Value(value) => Ok(value),
            BranchResult::Return => {
                panic!(
                    "backend lower_linear_expr_value hit return in linear context at {:?}",
                    expr.span
                );
            }
        }
    }

    // ── Extracted value-expression helpers ─────────────────────────────

    fn lower_block_expr(
        &mut self,
        expr: &sem::ValueExpr,
        items: &[sem::BlockItem],
        tail: &Option<Box<sem::ValueExpr>>,
    ) -> Result<BranchResult, LowerToIrError> {
        self.with_drop_scope(expr.id, |lowerer| {
            for item in items {
                match item {
                    sem::BlockItem::Stmt(stmt) => {
                        match lowerer.lower_stmt_expr_linear(stmt)? {
                            StmtOutcome::Continue => {}
                            StmtOutcome::Return => {
                                panic!(
                                    "backend lower_linear_value_expr hit return in linear block at {:?}",
                                    stmt.span
                                );
                            }
                        }
                    }
                    sem::BlockItem::Expr(expr) => {
                        lowerer.annotate_expr(expr);
                        let _ = lowerer.lower_linear_value_expr(expr)?;
                    }
                }
            }

            if let Some(tail) = tail {
                lowerer.annotate_expr(tail);
                return lowerer
                    .lower_linear_value_expr(tail)
                    .map(Into::into);
            }

            let ty = lowerer.type_lowerer.lower_type_id(expr.ty);
            Ok(lowerer.builder.const_unit(ty).into())
        })
    }

    fn lower_string_lit_expr(
        &mut self,
        expr: &sem::ValueExpr,
        value: &str,
    ) -> Result<BranchResult, LowerToIrError> {
        let string_ty = self.type_lowerer.lower_type_id(expr.ty);
        let slot = self.alloc_value_slot(string_ty);

        // pointer to global bytes
        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
        let ptr_val = if value.is_empty() {
            let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
            let zero = self.builder.const_int(0, false, 64, u64_ty);
            self.builder.cast(CastKind::IntToPtr, zero, u8_ptr_ty)
        } else {
            let global_id = self.add_global_bytes(value.as_bytes().to_vec());
            self.builder.const_global_addr(global_id, u8_ptr_ty)
        };

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

        Ok(self.load_slot(&slot).into())
    }

    fn lower_array_lit_expr(
        &mut self,
        expr: &sem::ValueExpr,
        init: &sem::ArrayLitInit,
    ) -> Result<BranchResult, LowerToIrError> {
        let array_ty = self.type_lowerer.lower_type_id(expr.ty);
        let addr = self.alloc_local_addr(array_ty);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));

        match init {
            sem::ArrayLitInit::Elems(elems) => {
                for (i, elem_expr) in elems.iter().enumerate() {
                    let Some(value) = self.lower_value_expr_opt(elem_expr)? else {
                        return Ok(BranchResult::Return);
                    };
                    let index_val = self.builder.const_int(i as i128, false, 64, u64_ty);
                    let elem_ty = self.type_lowerer.lower_type_id(elem_expr.ty);
                    let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ty);
                    let elem_addr = self.builder.index_addr(addr, index_val, elem_ptr_ty);
                    self.builder.store(elem_addr, value);
                }
            }
            sem::ArrayLitInit::Repeat(repeat_expr, count) => {
                let Some(value) = self.lower_value_expr_opt(repeat_expr)? else {
                    return Ok(BranchResult::Return);
                };
                let elem_ty = self.type_lowerer.lower_type_id(repeat_expr.ty);
                let sem_ty = self.type_map.type_table().get(repeat_expr.ty);
                if matches!(
                    sem_ty,
                    Type::Int {
                        signed: false,
                        bits: 8,
                        ..
                    }
                ) {
                    let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ty);
                    let zero = self.builder.const_int(0, false, 64, u64_ty);
                    let len = self.builder.const_int(*count as i128, false, 64, u64_ty);
                    let base_ptr = self.builder.index_addr(addr, zero, elem_ptr_ty);
                    self.builder.memset(base_ptr, value, len);
                } else {
                    for i in 0..*count {
                        let index_val = self.builder.const_int(i as i128, false, 64, u64_ty);
                        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ty);
                        let elem_addr = self.builder.index_addr(addr, index_val, elem_ptr_ty);
                        self.builder.store(elem_addr, value);
                    }
                }
            }
        }

        Ok(self.builder.load(addr, array_ty).into())
    }

    fn lower_set_lit_expr(
        &mut self,
        expr: &sem::ValueExpr,
        elems: &[sem::ValueExpr],
    ) -> Result<BranchResult, LowerToIrError> {
        let set_sem_ty = self.type_map.type_table().get(expr.ty).clone();
        let Type::Set { elem_ty } = set_sem_ty else {
            panic!("backend set literal has non-set type");
        };

        let set_ir_ty = self.type_lowerer.lower_type_id(expr.ty);
        let slot = self.alloc_value_slot(set_ir_ty);

        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
        let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let zero_u64 = self.builder.const_int(0, false, 64, u64_ty);
        let zero_ptr = self.builder.cast(CastKind::IntToPtr, zero_u64, elem_ptr_ty);
        let zero_u32 = self.builder.const_int(0, false, 32, u32_ty);

        // Start with an empty, non-owned set; runtime insert promotes to owned.
        self.store_field(slot.addr, 0, elem_ptr_ty, zero_ptr);
        self.store_field(slot.addr, 1, u32_ty, zero_u32);
        self.store_field(slot.addr, 2, u32_ty, zero_u32);

        let layout = self.type_lowerer.ir_type_cache.layout(elem_ir_ty);
        let elem_size = self
            .builder
            .const_int(layout.size() as i128, false, 64, u64_ty);
        let elem_align = self
            .builder
            .const_int(layout.align() as i128, false, 64, u64_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);

        for elem_expr in elems.iter() {
            let Some(value) = self.lower_value_expr_opt(elem_expr)? else {
                return Ok(BranchResult::Return);
            };
            let elem_value_ty = self.type_map.type_table().get(elem_expr.ty).clone();
            let elem_addr = self.materialize_value_addr(value, &elem_value_ty);
            let _ = self.builder.call(
                Callee::Runtime(RuntimeFn::SetInsertElem),
                vec![slot.addr, elem_addr, elem_size, elem_align],
                bool_ty,
            );
        }

        Ok(self.load_slot(&slot).into())
    }

    fn lower_map_lit_expr(
        &mut self,
        expr: &sem::ValueExpr,
        entries: &[sem::MapLitEntry],
    ) -> Result<BranchResult, LowerToIrError> {
        let map_sem_ty = self.type_map.type_table().get(expr.ty).clone();
        let Type::Map { key_ty, value_ty } = map_sem_ty else {
            panic!("backend map literal has non-map type");
        };

        let map_ir_ty = self.type_lowerer.lower_type_id(expr.ty);
        let slot = self.alloc_value_slot(map_ir_ty);

        let key_ir_ty = self.type_lowerer.lower_type(&key_ty);
        let key_ptr_ty = self.type_lowerer.ptr_to(key_ir_ty);
        let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let zero_u64 = self.builder.const_int(0, false, 64, u64_ty);
        let zero_ptr = self.builder.cast(CastKind::IntToPtr, zero_u64, key_ptr_ty);
        let zero_u32 = self.builder.const_int(0, false, 32, u32_ty);

        // Start with an empty, non-owned map; runtime insert promotes to owned.
        self.store_field(slot.addr, 0, key_ptr_ty, zero_ptr);
        self.store_field(slot.addr, 1, u32_ty, zero_u32);
        self.store_field(slot.addr, 2, u32_ty, zero_u32);

        let key_layout = self.type_lowerer.ir_type_cache.layout(key_ir_ty);
        let value_ir_ty = self.type_lowerer.lower_type(&value_ty);
        let value_layout = self.type_lowerer.ir_type_cache.layout(value_ir_ty);
        let key_size = self
            .builder
            .const_int(key_layout.size() as i128, false, 64, u64_ty);
        let value_size = self
            .builder
            .const_int(value_layout.size() as i128, false, 64, u64_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);

        for entry in entries.iter() {
            let Some(key_value) = self.lower_value_expr_opt(&entry.key)? else {
                return Ok(BranchResult::Return);
            };
            let key_value_ty = self.type_map.type_table().get(entry.key.ty).clone();
            let key_addr = self.materialize_value_addr(key_value, &key_value_ty);

            let Some(value_value) = self.lower_value_expr_opt(&entry.value)? else {
                return Ok(BranchResult::Return);
            };
            let value_value_ty = self.type_map.type_table().get(entry.value.ty).clone();
            let value_addr = self.materialize_value_addr(value_value, &value_value_ty);

            let _ = self.builder.call(
                Callee::Runtime(RuntimeFn::MapInsertOrAssign),
                vec![slot.addr, key_addr, value_addr, key_size, value_size],
                bool_ty,
            );
        }

        Ok(self.load_slot(&slot).into())
    }

    fn lower_tuple_lit_expr(
        &mut self,
        expr: &sem::ValueExpr,
        items: &[sem::ValueExpr],
    ) -> Result<BranchResult, LowerToIrError> {
        let tuple_ty = self.type_lowerer.lower_type_id(expr.ty);
        let slot = self.alloc_value_slot(tuple_ty);

        for (i, elem_expr) in items.iter().enumerate() {
            let Some(value) = self.lower_value_expr_opt(elem_expr)? else {
                return Ok(BranchResult::Return);
            };
            let field_ty = self.lower_tuple_field_ty(expr.ty, i);
            self.store_field(slot.addr, i, field_ty, value);
        }

        Ok(self.load_slot(&slot).into())
    }

    fn lower_struct_lit_expr(
        &mut self,
        expr: &sem::ValueExpr,
        fields: &[sem::StructLitField],
    ) -> Result<BranchResult, LowerToIrError> {
        let struct_ty = self.type_lowerer.lower_type_id(expr.ty);
        let slot = self.alloc_value_slot(struct_ty);

        for field in fields.iter() {
            let Some(value) = self.lower_value_expr_opt(&field.value)? else {
                return Ok(BranchResult::Return);
            };
            let (field_index, field_ty) = self.lower_struct_field_ty(expr.ty, &field.name);
            self.store_field(slot.addr, field_index, field_ty, value);
        }

        Ok(self.load_slot(&slot).into())
    }

    fn lower_struct_update_expr(
        &mut self,
        expr: &sem::ValueExpr,
        target: &sem::ValueExpr,
        fields: &[sem::StructUpdateField],
    ) -> Result<BranchResult, LowerToIrError> {
        let struct_ty = self.type_lowerer.lower_type_id(expr.ty);
        let slot = self.alloc_value_slot(struct_ty);

        // Copy the base struct
        let Some(base_value) = self.lower_value_expr_opt(target)? else {
            return Ok(BranchResult::Return);
        };
        let base_ty = self.type_map.type_table().get(expr.ty);
        self.store_value_into_addr(slot.addr, base_value, base_ty, struct_ty);

        // Overwrite the updated fields
        for field in fields.iter() {
            let Some(value) = self.lower_value_expr_opt(&field.value)? else {
                return Ok(BranchResult::Return);
            };
            let (field_index, field_ty) = self.lower_struct_field_ty(expr.ty, &field.name);
            self.store_field(slot.addr, field_index, field_ty, value);
        }

        Ok(self.load_slot(&slot).into())
    }

    fn lower_enum_variant_expr(
        &mut self,
        expr: &sem::ValueExpr,
        variant: &str,
        payload: &[sem::ValueExpr],
    ) -> Result<BranchResult, LowerToIrError> {
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
                "backend enum variant payload mismatch for {}: {} offsets, {} tys, {} values",
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
            let Some(value) = self.lower_value_expr_opt(value_expr)? else {
                return Ok(BranchResult::Return);
            };
            self.store_into_blob(payload_ptr, *offset, value, value_ty);
        }

        Ok(self.load_slot(&slot).into())
    }

    fn lower_heap_alloc_expr(
        &mut self,
        expr: &sem::ValueExpr,
        inner: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let heap_ty = self.type_map.type_table().get(expr.ty).clone();
        let Type::Heap { elem_ty } = heap_ty else {
            panic!("backend heap alloc expects heap type, got {:?}", heap_ty);
        };

        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
        let layout = self.type_lowerer.ir_type_cache.layout(elem_ir_ty);

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let size_val = self
            .builder
            .const_int(layout.size() as i128, false, 64, u64_ty);
        let align_val = self
            .builder
            .const_int(layout.align() as i128, false, 64, u64_ty);

        let ptr_val = self.builder.call(
            Callee::Runtime(RuntimeFn::Alloc),
            vec![size_val, align_val],
            ptr_ir_ty,
        );

        if layout.size() != 0 {
            let Some(value) = self.lower_value_expr_opt(inner)? else {
                return Ok(BranchResult::Return);
            };
            self.store_value_into_addr(ptr_val, value, &elem_ty, elem_ir_ty);
        }

        Ok(ptr_val.into())
    }

    fn lower_binop_expr(
        &mut self,
        expr: &sem::ValueExpr,
        left: &sem::ValueExpr,
        op: BinaryOp,
        right: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
            return self.lower_branching_value_expr(expr);
        }
        let Some(lhs) = self.lower_value_expr_opt(left)? else {
            return Ok(BranchResult::Return);
        };
        let Some(rhs) = self.lower_value_expr_opt(right)? else {
            return Ok(BranchResult::Return);
        };
        let ty = self.type_lowerer.lower_type_id(expr.ty);
        let sem_ty = self.type_map.type_table().get(expr.ty);

        if let Some(binop) = map_binop(op) {
            if matches!(op, BinaryOp::Div | BinaryOp::Mod) {
                self.emit_div_by_zero_check(rhs, sem_ty);
            }
            return Ok(self.builder.binop(binop, lhs, rhs, ty).into());
        }
        if matches!(op, BinaryOp::Eq | BinaryOp::Ne) {
            let operand_ty = self.type_map.type_table().get(left.ty).clone();
            let eq_value = self.lower_eq_value(lhs, rhs, &operand_ty);
            let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
            let value = if matches!(op, BinaryOp::Eq) {
                eq_value
            } else {
                self.builder.unop(UnOp::Not, eq_value, bool_ty)
            };
            return Ok(value.into());
        }
        if let Some(cmp) = map_cmp(op) {
            return Ok(self.builder.cmp(cmp, lhs, rhs, ty).into());
        }
        panic!(
            "backend lower_value_expr_value unsupported binop {:?} at {:?}",
            op, expr.span
        );
    }

    fn lower_slice_expr(
        &mut self,
        expr: &sem::ValueExpr,
        target: &sem::PlaceExpr,
        start: Option<&sem::ValueExpr>,
        end: Option<&sem::ValueExpr>,
    ) -> Result<BranchResult, LowerToIrError> {
        let plan = self.slice_plan(expr.id);

        let Type::Slice { elem_ty } = self.type_map.type_table().get(expr.ty).clone() else {
            panic!("backend slice expr has non-slice type");
        };

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);

        // Resolve the base pointer and length from the slice plan.
        let (base_ptr, base_len) = match plan.base {
            sem::SliceBaseKind::Array { len, deref_count } => {
                let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                let Type::Array { .. } = base_ty else {
                    panic!("backend slice on non-array base {:?}", base_ty);
                };
                let view = self.load_array_view(base_addr, elem_ptr_ty, len);
                (view.ptr, view.len)
            }
            sem::SliceBaseKind::Slice { deref_count } => {
                let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                let Type::Slice { .. } = base_ty else {
                    panic!("backend slice on non-slice base {:?}", base_ty);
                };
                let view = self.load_slice_view(base_addr, elem_ptr_ty);
                (view.ptr, view.len)
            }
            sem::SliceBaseKind::String { deref_count } => {
                let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                let Type::String = base_ty else {
                    panic!("backend slice on non-string base {:?}", base_ty);
                };
                let view = self.load_string_view(base_addr);
                (view.ptr, view.len)
            }
            sem::SliceBaseKind::DynArray { deref_count } => {
                let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                let Type::DynArray { .. } = base_ty else {
                    panic!("backend slice on non-dyn-array base {:?}", base_ty);
                };
                let view = self.load_dyn_array_view(base_addr, elem_ptr_ty);
                (view.ptr, view.len)
            }
        };

        // Evaluate bounds (default start=0, end=base_len).
        let start_val = match start {
            Some(start_expr) => {
                let Some(v) = self.lower_value_expr_opt(start_expr)? else {
                    return Ok(BranchResult::Return);
                };
                v
            }
            None => self.builder.const_int(0, false, 64, u64_ty),
        };
        let end_val = match end {
            Some(end_expr) => {
                let Some(v) = self.lower_value_expr_opt(end_expr)? else {
                    return Ok(BranchResult::Return);
                };
                v
            }
            None => base_len,
        };

        let start_check =
            start.is_some_and(|expr| !matches!(expr.kind, sem::ValueExprKind::IntLit(0)));
        let end_check = end.is_some();

        if start_check || end_check {
            let zero = self.builder.const_int(0, false, 64, u64_ty);
            let one = self.builder.const_int(1, false, 64, u64_ty);
            let max_excl = self.builder.binop(BinOp::Add, base_len, one, u64_ty);

            if start_check {
                self.emit_range_check(start_val, zero, max_excl, false);
            }
            if end_check {
                self.emit_range_check(end_val, start_val, max_excl, false);
            }
        }

        let slice_ty = self.type_lowerer.lower_type_id(expr.ty);
        Ok(self
            .emit_slice_value(
                slice_ty,
                elem_ptr_ty,
                u64_ty,
                base_ptr,
                base_len,
                start_val,
                end_val,
            )
            .into())
    }

    fn lower_map_get_expr(
        &mut self,
        expr: &sem::ValueExpr,
        target: &sem::ValueExpr,
        key: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let Some(map_value) = self.lower_value_expr_opt(target)? else {
            return Ok(BranchResult::Return);
        };
        let map_target_ty = self.type_map.type_table().get(target.ty).clone();
        let (map_addr, map_ty) = {
            let (peeled_ty, deref_count) = map_target_ty.peel_heap_with_count();
            if deref_count == 0 {
                let addr = self.materialize_value_addr(map_value, &map_target_ty);
                (addr, peeled_ty)
            } else {
                self.resolve_deref_base_value(map_value, map_target_ty.clone(), deref_count)
            }
        };
        let Type::Map { key_ty, value_ty } = map_ty else {
            panic!("backend map index on non-map type");
        };

        let Some(key_value) = self.lower_value_expr_opt(key)? else {
            return Ok(BranchResult::Return);
        };
        let key_addr = self.materialize_value_addr(key_value, &key_ty);

        let value_ir_ty = self.type_lowerer.lower_type(&value_ty);
        let value_slot = self.alloc_value_slot(value_ir_ty);

        let key_ir_ty = self.type_lowerer.lower_type(&key_ty);
        let key_layout = self.type_lowerer.ir_type_cache.layout(key_ir_ty);
        let value_layout = self.type_lowerer.ir_type_cache.layout(value_ir_ty);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let key_size = self
            .builder
            .const_int(key_layout.size() as i128, false, 64, u64_ty);
        let value_size = self
            .builder
            .const_int(value_layout.size() as i128, false, 64, u64_ty);

        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let hit = self.builder.call(
            Callee::Runtime(RuntimeFn::MapGetValue),
            vec![map_addr, key_addr, key_size, value_size, value_slot.addr],
            bool_ty,
        );

        // Build `V | KeyNotFound`: tag 0 when key is present, tag 1 otherwise.
        let union_ir_ty = self.type_lowerer.lower_type_id(expr.ty);
        let union_slot = self.alloc_value_slot(union_ir_ty);
        let (tag_ty, blob_ty, payload_offset, payload_ty) = {
            let layout = self.type_lowerer.enum_layout(expr.ty);
            let ok_variant = layout
                .variants
                .first()
                .unwrap_or_else(|| panic!("backend map get missing ok union variant"));
            if ok_variant.field_offsets.len() != 1 || ok_variant.field_tys.len() != 1 {
                panic!("backend map get expects single-payload ok union variant");
            }
            (
                layout.tag_ty,
                layout.blob_ty,
                ok_variant.field_offsets[0],
                ok_variant.field_tys[0],
            )
        };

        let hit_u32 = self.builder.int_extend(hit, tag_ty, false);
        let one_u32 = self.builder.const_int(1, false, 32, tag_ty);
        let tag = self.builder.binop(BinOp::Xor, one_u32, hit_u32, tag_ty);
        self.store_field(union_slot.addr, 0, tag_ty, tag);

        let payload = self.builder.load(value_slot.addr, value_ir_ty);
        let blob_ptr = self.field_addr_typed(union_slot.addr, 1, blob_ty);
        self.store_into_blob(blob_ptr, payload_offset, payload, payload_ty);

        Ok(self.load_slot(&union_slot).into())
    }

    fn lower_len_expr(
        &mut self,
        expr: &sem::ValueExpr,
        place: &sem::PlaceExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let place_ty = self.type_map.type_table().get(place.ty).clone();
        match place_ty {
            Type::Array { dims, .. } => {
                let len = dims
                    .first()
                    .copied()
                    .unwrap_or_else(|| panic!("backend len on array with empty dims"));
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_int(len as i128, false, 64, ty).into())
            }
            Type::Slice { .. } => {
                let place_addr = self.lower_place_addr(place)?;
                let len_ty = self.type_lowerer.lower_type(&Type::uint(64));
                let len_addr = self.field_addr_typed(place_addr.addr, 1, len_ty);
                Ok(self.builder.load(len_addr, len_ty).into())
            }
            Type::String => {
                let place_addr = self.lower_place_addr(place)?;
                let view = self.load_string_view(place_addr.addr);
                Ok(view.len.into())
            }
            Type::DynArray { .. } => {
                let place_addr = self.lower_place_addr(place)?;
                let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
                let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
                let view = self.load_dyn_array_view(place_addr.addr, u8_ptr_ty);
                Ok(view.len.into())
            }
            other => panic!("backend len on unsupported type {:?}", other),
        }
    }

    fn lower_coerce_expr(
        &mut self,
        expr: &sem::ValueExpr,
        kind: CoerceKind,
        inner: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        match kind {
            CoerceKind::ArrayToSlice => self.lower_coerce_array_to_slice(expr, inner),
            CoerceKind::ArrayToDynArray => self.lower_coerce_array_to_dyn_array(expr, inner),
            CoerceKind::DynArrayToSlice => self.lower_coerce_dyn_array_to_slice(expr, inner),
        }
    }

    fn lower_coerce_array_to_slice(
        &mut self,
        expr: &sem::ValueExpr,
        inner: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let plan = self.slice_plan(expr.id);

        let Type::Slice { elem_ty } = self.type_map.type_table().get(expr.ty).clone() else {
            panic!("backend coerce array-to-slice has non-slice type");
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
                        let Some(value) = self.lower_value_expr_opt(inner)? else {
                            return Ok(BranchResult::Return);
                        };
                        if deref_count == 0 {
                            let array_ty = self.type_lowerer.lower_type_id(inner.ty);
                            let addr = self.alloc_local_addr(array_ty);
                            let array_sem_ty = self.type_map.type_table().get(inner.ty);
                            self.store_value_into_addr(addr, value, array_sem_ty, array_ty);
                            (addr, base_ty)
                        } else {
                            self.resolve_deref_base_value(value, base_ty, deref_count)
                        }
                    }
                };

                let Type::Array { .. } = base_ty else {
                    panic!("backend coerce array-to-slice on {:?}", base_ty);
                };

                let zero = self.builder.const_int(0, false, 64, u64_ty);
                let ptr = self.builder.index_addr(base_addr, zero, elem_ptr_ty);
                let len_val = self.builder.const_int(len as i128, false, 64, u64_ty);
                (ptr, len_val)
            }
            other => {
                panic!("backend coerce array-to-slice with base {:?}", other);
            }
        };

        let start_val = self.builder.const_int(0, false, 64, u64_ty);
        let end_val = base_len;
        let slice_ty = self.type_lowerer.lower_type_id(expr.ty);
        Ok(self
            .emit_slice_value(
                slice_ty,
                elem_ptr_ty,
                u64_ty,
                base_addr,
                base_len,
                start_val,
                end_val,
            )
            .into())
    }

    fn lower_coerce_array_to_dyn_array(
        &mut self,
        expr: &sem::ValueExpr,
        inner: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let target_dyn_ty = self.type_map.type_table().get(expr.ty).clone();
        let Type::DynArray { elem_ty } = target_dyn_ty else {
            panic!("backend coerce array-to-dyn-array has non-dyn-array type");
        };

        let source_array_ty = self.type_map.type_table().get(inner.ty).clone();
        let Type::Array { dims, .. } = source_array_ty else {
            panic!(
                "backend coerce array-to-dyn-array expects array source, got {:?}",
                source_array_ty
            );
        };
        let len = dims
            .first()
            .copied()
            .unwrap_or_else(|| panic!("backend array-to-dyn-array source missing dims"));
        let len_u32 = u32::try_from(len).unwrap_or_else(|_| {
            panic!(
                "backend array length {} exceeds u32::MAX for dyn array",
                len
            )
        });

        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
        let dyn_ir_ty = self.type_lowerer.lower_type_id(expr.ty);
        let dyn_slot = self.alloc_value_slot(dyn_ir_ty);

        // Lower the source as an addressable array value.
        let source_addr = match &inner.kind {
            sem::ValueExprKind::Load { place }
            | sem::ValueExprKind::Move { place }
            | sem::ValueExprKind::ImplicitMove { place } => self.lower_place_addr(place)?.addr,
            _ => {
                let Some(value) = self.lower_value_expr_opt(inner)? else {
                    return Ok(BranchResult::Return);
                };
                let array_ir_ty = self.type_lowerer.lower_type_id(inner.ty);
                let addr = self.alloc_local_addr(array_ir_ty);
                let array_sem_ty = self.type_map.type_table().get(inner.ty);
                self.store_value_into_addr(addr, value, array_sem_ty, array_ir_ty);
                addr
            }
        };

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let layout = self.type_lowerer.ir_type_cache.layout(elem_ir_ty);
        let elem_size = layout.size();
        let elem_align = layout.align();

        let data_ptr = if len_u32 == 0 || elem_size == 0 {
            let zero = self.builder.const_int(0, false, 64, u64_ty);
            self.builder.cast(CastKind::IntToPtr, zero, elem_ptr_ty)
        } else {
            let bytes =
                self.builder
                    .const_int((len_u32 as u64 * elem_size) as i128, false, 64, u64_ty);
            let align = self
                .builder
                .const_int(elem_align as i128, false, 64, u64_ty);
            let dst_ptr = self.builder.call(
                Callee::Runtime(RuntimeFn::Alloc),
                vec![bytes, align],
                elem_ptr_ty,
            );

            let zero = self.builder.const_int(0, false, 64, u64_ty);
            let src_ptr = self.builder.index_addr(source_addr, zero, elem_ptr_ty);
            self.builder.memcopy(dst_ptr, src_ptr, bytes);
            dst_ptr
        };

        let len_val = self.builder.const_int(len_u32 as i128, false, 32, u32_ty);
        let cap_raw = (len_u32 | 0x8000_0000) as i128;
        let cap_val = self.builder.const_int(cap_raw, false, 32, u32_ty);

        self.store_field(dyn_slot.addr, 0, elem_ptr_ty, data_ptr);
        self.store_field(dyn_slot.addr, 1, u32_ty, len_val);
        self.store_field(dyn_slot.addr, 2, u32_ty, cap_val);

        Ok(self.load_slot(&dyn_slot).into())
    }

    fn lower_coerce_dyn_array_to_slice(
        &mut self,
        expr: &sem::ValueExpr,
        inner: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let plan = self.slice_plan(expr.id);

        let Type::Slice { elem_ty } = self.type_map.type_table().get(expr.ty).clone() else {
            panic!("backend coerce dyn-array-to-slice has non-slice type");
        };

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);

        let (base_ptr, base_len) = match plan.base {
            sem::SliceBaseKind::DynArray { deref_count } => {
                let base_ty = self.type_map.type_table().get(inner.ty).clone();
                let (base_addr, base_ty) = match &inner.kind {
                    sem::ValueExprKind::Load { place }
                    | sem::ValueExprKind::Move { place }
                    | sem::ValueExprKind::ImplicitMove { place } => {
                        self.resolve_deref_base(place, deref_count)?
                    }
                    _ => {
                        let Some(value) = self.lower_value_expr_opt(inner)? else {
                            return Ok(BranchResult::Return);
                        };
                        if deref_count == 0 {
                            let dyn_ir_ty = self.type_lowerer.lower_type_id(inner.ty);
                            let addr = self.alloc_local_addr(dyn_ir_ty);
                            let dyn_sem_ty = self.type_map.type_table().get(inner.ty);
                            self.store_value_into_addr(addr, value, dyn_sem_ty, dyn_ir_ty);
                            (addr, base_ty)
                        } else {
                            self.resolve_deref_base_value(value, base_ty, deref_count)
                        }
                    }
                };

                let Type::DynArray { .. } = base_ty else {
                    panic!("backend coerce dyn-array-to-slice on {:?}", base_ty);
                };
                let view = self.load_dyn_array_view(base_addr, elem_ptr_ty);
                (view.ptr, view.len)
            }
            other => {
                panic!("backend coerce dyn-array-to-slice with base {:?}", other);
            }
        };

        let start_val = self.builder.const_int(0, false, 64, u64_ty);
        let slice_ty = self.type_lowerer.lower_type_id(expr.ty);
        Ok(self
            .emit_slice_value(
                slice_ty,
                elem_ptr_ty,
                u64_ty,
                base_ptr,
                base_len,
                start_val,
                base_len,
            )
            .into())
    }

    fn lower_emit_send_expr(
        &mut self,
        expr: &sem::ValueExpr,
        to: &sem::ValueExpr,
        payload: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let Some(dst) = self.lower_value_expr_opt(to)? else {
            return Ok(BranchResult::Return);
        };
        let Some(payload_value) = self.lower_value_expr_opt(payload)? else {
            return Ok(BranchResult::Return);
        };
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let payload_ty = self.type_map.type_table().get(payload.ty).clone();
        let event_kind = self.machine_payload_event_kind(&payload_ty).unwrap_or(0);
        let kind = self
            .builder
            .const_int(event_kind as i128, false, 64, u64_ty);
        let (payload0, payload1) = self.pack_machine_payload_words(payload_value, &payload_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let _status = self.builder.call(
            Callee::Runtime(RuntimeFn::MachineEmitSend),
            vec![dst, kind, payload0, payload1],
            bool_ty,
        );
        let unit_ty = self.type_lowerer.lower_type_id(expr.ty);
        Ok(self.builder.const_int(0, false, 8, unit_ty).into())
    }

    fn lower_emit_request_expr(
        &mut self,
        expr: &sem::ValueExpr,
        to: &sem::ValueExpr,
        payload: &sem::ValueExpr,
        request_site_key: u64,
    ) -> Result<BranchResult, LowerToIrError> {
        let Some(dst) = self.lower_value_expr_opt(to)? else {
            return Ok(BranchResult::Return);
        };
        let Some(payload_value) = self.lower_value_expr_opt(payload)? else {
            return Ok(BranchResult::Return);
        };
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let payload_ty = self.type_map.type_table().get(payload.ty).clone();
        let event_kind = self.machine_payload_event_kind(&payload_ty).unwrap_or(0);
        let kind = self
            .builder
            .const_int(event_kind as i128, false, 64, u64_ty);
        let (payload0, payload1) = self.pack_machine_payload_words(payload_value, &payload_ty);
        let request_site = self
            .builder
            .const_int(request_site_key as i128, false, 64, u64_ty);
        let pending_ty = self.type_lowerer.lower_type_id(expr.ty);
        let pending = self.builder.call(
            Callee::Runtime(RuntimeFn::MachineEmitRequest),
            vec![dst, kind, payload0, payload1, request_site],
            pending_ty,
        );
        Ok(pending.into())
    }

    fn lower_reply_expr(
        &mut self,
        expr: &sem::ValueExpr,
        cap: &sem::ValueExpr,
        value: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let Some(cap_value) = self.lower_value_expr_opt(cap)? else {
            return Ok(BranchResult::Return);
        };
        let Some(reply_value) = self.lower_value_expr_opt(value)? else {
            return Ok(BranchResult::Return);
        };
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let response_ty = self.type_map.type_table().get(value.ty).clone();
        let event_kind = self.machine_response_event_kind(&response_ty).unwrap_or(0);
        let kind = self
            .builder
            .const_int(event_kind as i128, false, 64, u64_ty);
        let (payload0, payload1) = self.pack_machine_payload_words(reply_value, &response_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let _status = self.builder.call(
            Callee::Runtime(RuntimeFn::MachineEmitReply),
            vec![cap_value, kind, payload0, payload1],
            bool_ty,
        );
        let unit_ty = self.type_lowerer.lower_type_id(expr.ty);
        Ok(self.builder.const_int(0, false, 8, unit_ty).into())
    }

    /// Builds a slice value from a base pointer and length.
    #[allow(clippy::too_many_arguments)]
    fn emit_slice_value(
        &mut self,
        slice_ty: IrTypeId,
        elem_ptr_ty: IrTypeId,
        u64_ty: IrTypeId,
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
