//! Straight-line (single-block) lowering routines.

use crate::core::ast::{
    ArrayLitInit, BinaryOp, BindPatternKind, BlockItem, CoerceKind, EmitKind, Expr, ExprKind,
    MapLitEntry, ParamMode, StmtExpr, StmtExprKind, StructLitField, StructUpdateField, UnaryOp,
};
use crate::core::backend::lower::LowerToIrError;
use crate::core::backend::lower::locals::LocalValue;
use crate::core::backend::lower::lowerer::{BranchResult, FuncLowerer, LinearValue, StmtOutcome};
use crate::core::backend::lower::mapping::{map_binop, map_cmp};
use crate::core::ir::{BinOp, Callee, CastKind, IrTypeId, RuntimeFn, Terminator, UnOp, ValueId};
use crate::core::plans::{FmtKind, LoweringPlan, SliceBaseKind};
use crate::core::resolve::DefKind;
use crate::core::types::{Type, TypeId};

impl<'a, 'g> FuncLowerer<'a, 'g> {
    fn store_tuple_items(
        &mut self,
        tuple_ty: TypeId,
        slot_addr: ValueId,
        items: &[Expr],
    ) -> Result<bool, LowerToIrError> {
        for (i, elem_expr) in items.iter().enumerate() {
            let Some(value) = self.lower_value_expr_opt(elem_expr)? else {
                return Ok(false);
            };
            let field_ty = self.lower_tuple_field_ty(tuple_ty, i);
            self.store_field(slot_addr, i, field_ty, value);
        }
        Ok(true)
    }

    fn store_struct_lit_fields(
        &mut self,
        struct_ty: TypeId,
        slot_addr: ValueId,
        fields: &[StructLitField],
    ) -> Result<bool, LowerToIrError> {
        for field in fields.iter() {
            let Some(value) = self.lower_value_expr_opt(&field.value)? else {
                return Ok(false);
            };
            let (field_index, field_ty) = self.lower_struct_field_ty(struct_ty, &field.name);
            self.store_field(slot_addr, field_index, field_ty, value);
        }
        Ok(true)
    }

    fn store_struct_update_fields(
        &mut self,
        struct_ty: TypeId,
        slot_addr: ValueId,
        fields: &[StructUpdateField],
    ) -> Result<bool, LowerToIrError> {
        for field in fields.iter() {
            let Some(value) = self.lower_value_expr_opt(&field.value)? else {
                return Ok(false);
            };
            let (field_index, field_ty) = self.lower_struct_field_ty(struct_ty, &field.name);
            self.store_field(slot_addr, field_index, field_ty, value);
        }
        Ok(true)
    }

    /// Lowers a linear value expression (single basic block, no branching).
    pub(super) fn lower_linear_value_expr(
        &mut self,
        expr: &Expr,
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
        expr: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        match &expr.kind {
            ExprKind::Block { items, tail } => self.lower_block_expr(expr, items, tail),

            ExprKind::UnitLit => {
                let ty = self
                    .type_lowerer
                    .lower_type_id(self.type_map.type_of(expr.id));
                Ok(self.builder.const_unit(ty).into())
            }
            ExprKind::IntLit(value) => {
                let expr_ty = self.type_map.type_of(expr.id);
                let ty = self.type_lowerer.lower_type_id(expr_ty);
                let (signed, bits) = self.type_lowerer.int_info(expr_ty);
                Ok(self
                    .builder
                    .const_int(*value as i128, signed, bits, ty)
                    .into())
            }
            ExprKind::BoolLit(value) => {
                let ty = self
                    .type_lowerer
                    .lower_type_id(self.type_map.type_of(expr.id));
                Ok(self.builder.const_bool(*value, ty).into())
            }
            ExprKind::CharLit(value) => {
                let ty = self
                    .type_lowerer
                    .lower_type_id(self.type_map.type_of(expr.id));
                Ok(self
                    .builder
                    .const_int(*value as u32 as i128, false, 32, ty)
                    .into())
            }
            ExprKind::StringLit { value } => self.lower_string_lit_expr(expr, value),

            ExprKind::Range { start, end } => {
                let (ExprKind::IntLit(start), ExprKind::IntLit(_end)) = (&start.kind, &end.kind)
                else {
                    panic!("backend range values require literal bounds");
                };
                let ty = self
                    .type_lowerer
                    .lower_type_id(self.type_map.type_of(expr.id));
                Ok(self.builder.const_int(*start as i128, false, 64, ty).into())
            }

            ExprKind::StringFmt { segments: _ } => {
                let plan = self
                    .lowering_plans
                    .string_fmt_plans
                    .get(&expr.id)
                    .unwrap_or_else(|| panic!("backend missing string_fmt_plan for {:?}", expr.id));
                let plan = plan.clone();
                let string_ty = self
                    .type_lowerer
                    .lower_type_id(self.type_map.type_of(expr.id));
                let value = match plan.kind {
                    FmtKind::View => self.lower_string_fmt_view(&plan, string_ty)?,
                    FmtKind::Owned => self.lower_string_fmt_owned(&plan, string_ty)?,
                };
                let Some(value) = value else {
                    return Ok(BranchResult::Return);
                };
                Ok(value.into())
            }

            ExprKind::ArrayLit { init, .. } => self.lower_array_lit_expr(expr, init),
            ExprKind::SetLit { elems, .. } => self.lower_set_lit_expr(expr, elems),
            ExprKind::MapLit { entries, .. } => self.lower_map_lit_expr(expr, entries),

            ExprKind::TupleLit(items) => self.lower_tuple_lit_expr(expr, items),
            ExprKind::StructLit { fields, .. } => self.lower_struct_lit_expr(expr, fields),
            ExprKind::StructUpdate { target, fields } => {
                self.lower_struct_update_expr(expr, target, fields)
            }

            ExprKind::EnumVariant {
                variant, payload, ..
            } => self.lower_enum_variant_expr(expr, variant, payload),

            ExprKind::Try {
                fallible_expr,
                on_error,
            } => {
                if let Some(handler) = on_error {
                    return self.lower_try_handle(expr, fallible_expr, handler);
                }
                self.lower_try_propagate(expr, fallible_expr)
            }

            ExprKind::UnaryOp { op, expr: inner } => {
                let Some(value) = self.lower_value_expr_opt(inner)? else {
                    return Ok(BranchResult::Return);
                };
                let ty = self
                    .type_lowerer
                    .lower_type_id(self.type_map.type_of(expr.id));
                let result = match op {
                    UnaryOp::Neg => self.builder.unop(UnOp::Neg, value, ty),
                    UnaryOp::LogicalNot => self.builder.unop(UnOp::Not, value, ty),
                    UnaryOp::BitNot => self.builder.unop(UnOp::BitNot, value, ty),
                };
                Ok(result.into())
            }

            ExprKind::HeapAlloc { expr: inner } => self.lower_heap_alloc_expr(expr, inner),
            ExprKind::BinOp { left, op, right } => self.lower_binop_expr(expr, left, *op, right),
            ExprKind::Slice { target, start, end } => {
                self.lower_slice_expr(expr, target, start.as_deref(), end.as_deref())
            }
            ExprKind::MapGet { target, key } => self.lower_map_get_expr(expr, target, key),
            ExprKind::Len { expr: place } => self.lower_len_expr(expr, place),

            ExprKind::Move { expr: place } | ExprKind::ImplicitMove { expr: place } => {
                let value = match &place.kind {
                    ExprKind::Var { .. } => {
                        let def_id = self.def_table.def_id(place.id);
                        self.set_drop_flag_for_def(def_id, false);
                        self.load_local_value(def_id)
                    }
                    _ => {
                        let place_addr = self.lower_place_addr(place)?;
                        let value = self.builder.load(place_addr.addr, place_addr.value_ty);
                        let expr_sem_ty = self
                            .type_map
                            .type_table()
                            .get(self.type_map.type_of(expr.id))
                            .clone();
                        return Ok(self
                            .coerce_value(value, &place_addr.sem_ty, &expr_sem_ty)
                            .into());
                    }
                };
                let place_sem_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_map.type_of(place.id))
                    .clone();
                let expr_sem_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_map.type_of(expr.id))
                    .clone();
                Ok(self.coerce_value(value, &place_sem_ty, &expr_sem_ty).into())
            }

            ExprKind::Coerce { kind, expr: inner } => self.lower_coerce_expr(expr, *kind, inner),

            ExprKind::Load { expr: place } => match &place.kind {
                ExprKind::Var { .. } => {
                    let def_id = self.def_table.def_id(place.id);
                    if self.locals.get(def_id).is_some() {
                        let value = self.load_local_value(def_id);
                        let place_sem_ty = self
                            .type_map
                            .type_table()
                            .get(self.type_map.type_of(place.id))
                            .clone();
                        let expr_sem_ty = self
                            .type_map
                            .type_table()
                            .get(self.type_map.type_of(expr.id))
                            .clone();
                        Ok(self.coerce_value(value, &place_sem_ty, &expr_sem_ty).into())
                    } else {
                        let def = self.def(def_id);
                        match def.kind {
                            DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => {
                                let value_ty = self
                                    .type_lowerer
                                    .lower_type_id(self.type_map.type_of(place.id));
                                Ok(self.builder.const_func_addr(def_id, value_ty).into())
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
                    let value = self.builder.load(place_addr.addr, place_addr.value_ty);
                    let expr_sem_ty = self
                        .type_map
                        .type_table()
                        .get(self.type_map.type_of(expr.id))
                        .clone();
                    Ok(self
                        .coerce_value(value, &place_addr.sem_ty, &expr_sem_ty)
                        .into())
                }
            },

            ExprKind::AddrOf { expr: place } => {
                let place_addr = self.lower_place_addr(place)?;
                Ok(place_addr.addr.into())
            }

            ExprKind::Call { callee, args } => {
                let Some(value) = self.lower_call_expr(expr, callee, args)? else {
                    return Ok(BranchResult::Return);
                };
                Ok(value.into())
            }

            ExprKind::MethodCall { callee, args, .. } => {
                let Some(value) = self.lower_method_call_expr(expr, callee, args)? else {
                    return Ok(BranchResult::Return);
                };
                Ok(value.into())
            }

            ExprKind::Emit {
                kind: EmitKind::Send { to, payload },
            } => self.lower_emit_send_expr(expr, to, payload),
            ExprKind::Emit {
                kind:
                    EmitKind::Request {
                        to,
                        payload,
                        request_site_key,
                        ..
                    },
            } => self.lower_emit_request_expr(expr, to, payload, request_site_key.unwrap()),
            ExprKind::Reply { cap, value } => self.lower_reply_expr(expr, cap, value),

            ExprKind::ClosureRef { .. } => {
                let def_id = self.def_table.def_id(expr.id);
                let ty = self
                    .type_lowerer
                    .lower_type_id(self.type_map.type_of(expr.id));
                Ok(self.builder.const_func_addr(def_id, ty).into())
            }

            _ => match self.lowering_plan(expr.id) {
                LoweringPlan::Branching => self.lower_branching_value_expr(expr),
                LoweringPlan::Linear => {
                    panic!(
                        "backend lower_value_expr_value unsupported expr {:?} at {:?}",
                        expr.kind, expr.span
                    );
                }
            },
        }
    }

    /// Lowers a linear statement (single basic block, no branching).
    pub(super) fn lower_stmt_expr_linear(
        &mut self,
        stmt: &StmtExpr,
    ) -> Result<StmtOutcome, LowerToIrError> {
        self.annotate_stmt(stmt);
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, value, .. }
            | StmtExprKind::VarBind { pattern, value, .. } => {
                let value_expr = value;
                let value = match self.lower_value_expr_value(value_expr)? {
                    BranchResult::Value(value) => value,
                    BranchResult::Return => return Ok(StmtOutcome::Return),
                };
                let value_expr_ty = self.type_map.type_of(value_expr.id);
                let ty = self.type_lowerer.lower_type_id(value_expr_ty);
                let value_ty = self.type_map.type_table().get(value_expr_ty).clone();
                if matches!(pattern.kind, BindPatternKind::Name { .. })
                    && matches!(value_ty, Type::String)
                    && self.expr_is_static_string_literal(value_expr)
                {
                    let def_id = self.def_table.def_id(pattern.id);
                    self.locals.insert(def_id, LocalValue::value(value, ty));
                    self.set_drop_flag_for_def(def_id, true);
                } else {
                    self.bind_pattern(pattern, LocalValue::value(value, ty), &value_ty)?;
                }
                Ok(StmtOutcome::Continue)
            }

            StmtExprKind::VarDecl { .. } => {
                let def_id = self.def_table.def_id(stmt.id);
                let ty = self.def_type(def_id);
                let ir_ty = self.type_lowerer.lower_type(&ty);
                let addr = self.alloc_local_addr(ir_ty);
                self.locals.insert(def_id, LocalValue::addr(addr, ir_ty));
                // Start drop tracking as uninitialized until the first init assignment.
                self.set_drop_flag_for_def(def_id, false);
                Ok(StmtOutcome::Continue)
            }

            StmtExprKind::Assign {
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
                let value_expr_ty = self.type_map.type_of(value_expr.id);
                let value_ty = self.type_map.type_table().get(value_expr_ty).clone();
                match &assignee.kind {
                    ExprKind::Var { .. } => {
                        let def_id = self.def_table.def_id(assignee.id);
                        let dest_ty = self.def_type(def_id);
                        self.emit_conversion_check(&value_ty, &dest_ty, value);
                        if let Some(mode) = self.param_mode_for(def_id)
                            && matches!(mode, ParamMode::Out | ParamMode::InOut)
                        {
                            if !init.is_init && !init.promotes_full {
                                self.emit_drop_for_def_if_live(def_id, value_expr_ty)?;
                            }
                            let ty = self.type_lowerer.lower_type_id(value_expr_ty);
                            self.assign_local_value(def_id, value, ty);
                            self.set_drop_flag_for_def(def_id, true);
                            return Ok(StmtOutcome::Continue);
                        }
                        if !init.is_init && !init.promotes_full {
                            self.emit_drop_for_def_if_live(def_id, value_expr_ty)?;
                        }
                        let ty = self.type_lowerer.lower_type_id(value_expr_ty);
                        if matches!(value_ty, Type::String)
                            && self.expr_is_static_string_literal(value_expr)
                        {
                            self.assign_local_storage_value(def_id, value, ty);
                        } else {
                            self.assign_local_value(def_id, value, ty);
                        }
                        self.set_drop_flag_for_def(def_id, true);
                        Ok(StmtOutcome::Continue)
                    }
                    _ => {
                        let assignee_ty = self.type_map.type_of(assignee.id);
                        let dest_ty = self.type_map.type_table().get(assignee_ty).clone();
                        self.emit_conversion_check(&value_ty, &dest_ty, value);
                        let place_addr = self.lower_place_addr(assignee)?;
                        if !init.is_init && !init.promotes_full {
                            self.drop_value_at_addr(place_addr.addr, &dest_ty)?;
                        }
                        let ir_ty = self.type_lowerer.lower_type_id(value_expr_ty);
                        let sem_ty = self.type_map.type_table().get(value_expr_ty);
                        self.store_value_into_addr(place_addr.addr, value, sem_ty, ir_ty);
                        if init.promotes_full {
                            let mut cursor = assignee.as_ref();
                            let mut base_def = None;
                            loop {
                                match &cursor.kind {
                                    ExprKind::Var { .. } => {
                                        base_def = Some(self.def_table.def_id(cursor.id));
                                        break;
                                    }
                                    ExprKind::StructField { target, .. }
                                    | ExprKind::TupleField { target, .. }
                                    | ExprKind::ArrayIndex { target, .. } => {
                                        cursor = target.as_ref();
                                    }
                                    ExprKind::Deref { .. } => break,
                                    _ => break,
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

            StmtExprKind::Return { value } => {
                let value = match value {
                    Some(expr) => match self.lower_consuming_value_expr(expr)? {
                        BranchResult::Value(value) => {
                            let ty = self
                                .type_map
                                .type_table()
                                .get(self.type_map.type_of(expr.id));
                            if matches!(ty, Type::Unit) {
                                None
                            } else {
                                let value = self.prepare_owned_return_value(expr, value, ty);
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

            StmtExprKind::While { .. }
            | StmtExprKind::For { .. }
            | StmtExprKind::Break
            | StmtExprKind::Continue => {
                panic!(
                    "backend lower_stmt_expr_linear unsupported stmt {:?} at {:?}",
                    stmt.kind, stmt.span
                );
            }
            StmtExprKind::CompoundAssign { .. }
            | StmtExprKind::Defer { .. }
            | StmtExprKind::Using { .. } => {
                unreachable!(
                    "syntax desugar must remove compound-assign/defer/using before backend lowering"
                );
            }
        }
    }

    /// Convenience: lower an expression and return its value in one step.
    pub(super) fn lower_linear_expr_value(
        &mut self,
        expr: &Expr,
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
        expr: &Expr,
        items: &[BlockItem],
        tail: &Option<Box<Expr>>,
    ) -> Result<BranchResult, LowerToIrError> {
        self.with_drop_scope(expr.id, |lowerer| {
            for item in items {
                match item {
                    BlockItem::Stmt(stmt) => {
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
                    BlockItem::Expr(expr) => {
                        lowerer.annotate_expr(expr);
                        let _ = lowerer.lower_linear_value_expr(expr)?;
                    }
                }
            }

            if let Some(tail) = tail {
                lowerer.annotate_expr(tail);
                return match lowerer.lower_consuming_value_expr(tail)? {
                    BranchResult::Value(value) => {
                        let tail_sem_ty = lowerer
                            .type_map
                            .type_table()
                            .get(lowerer.type_map.type_of(tail.id))
                            .clone();
                        let value =
                            lowerer.prepare_owned_return_value(tail, value, &tail_sem_ty);
                        let block_sem_ty = lowerer
                            .type_map
                            .type_table()
                            .get(lowerer.type_map.type_of(expr.id))
                            .clone();
                        let coerced = lowerer.coerce_value(value, &tail_sem_ty, &block_sem_ty);
                        Ok(coerced.into())
                    }
                    BranchResult::Return => Ok(BranchResult::Return),
                };
            }

            let unit_ty = lowerer.type_lowerer.lower_type(&Type::Unit);
            let value = lowerer.builder.const_unit(unit_ty);
            let block_sem_ty = lowerer
                .type_map
                .type_table()
                .get(lowerer.type_map.type_of(expr.id))
                .clone();
            let coerced = lowerer.coerce_value(value, &Type::Unit, &block_sem_ty);
            Ok(coerced.into())
        })
    }

    fn lower_string_lit_expr(
        &mut self,
        expr: &Expr,
        value: &str,
    ) -> Result<BranchResult, LowerToIrError> {
        let string_ty = self
            .type_lowerer
            .lower_type_id(self.type_map.type_of(expr.id));
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
        expr: &Expr,
        init: &ArrayLitInit,
    ) -> Result<BranchResult, LowerToIrError> {
        let array_ty = self
            .type_lowerer
            .lower_type_id(self.type_map.type_of(expr.id));
        let addr = self.alloc_local_addr(array_ty);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));

        match init {
            ArrayLitInit::Elems(elems) => {
                for (i, elem_expr) in elems.iter().enumerate() {
                    let Some(value) = self.lower_value_expr_opt(elem_expr)? else {
                        return Ok(BranchResult::Return);
                    };
                    let index_val = self.builder.const_int(i as i128, false, 64, u64_ty);
                    let elem_ty = self
                        .type_lowerer
                        .lower_type_id(self.type_map.type_of(elem_expr.id));
                    let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ty);
                    let elem_addr = self.builder.index_addr(addr, index_val, elem_ptr_ty);
                    self.builder.store(elem_addr, value);
                }
            }
            ArrayLitInit::Repeat(repeat_expr, count) => {
                let Some(value) = self.lower_value_expr_opt(repeat_expr)? else {
                    return Ok(BranchResult::Return);
                };
                let repeat_expr_ty = self.type_map.type_of(repeat_expr.id);
                let elem_ty = self.type_lowerer.lower_type_id(repeat_expr_ty);
                let sem_ty = self.type_map.type_table().get(repeat_expr_ty);
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
        expr: &Expr,
        elems: &[Expr],
    ) -> Result<BranchResult, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        let set_sem_ty = self.type_map.type_table().get(expr_ty).clone();
        let Type::Set { elem_ty } = set_sem_ty else {
            panic!("backend set literal has non-set type");
        };

        let set_ir_ty = self.type_lowerer.lower_type_id(expr_ty);
        let slot = self.alloc_value_slot(set_ir_ty);

        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
        self.init_empty_hash_collection(slot.addr, elem_ptr_ty);

        let elem_size = self.runtime_size_const(&elem_ty);
        let elem_align = self.runtime_align_const(&elem_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);

        for elem_expr in elems.iter() {
            let Some(value) = self.lower_value_expr_opt(elem_expr)? else {
                return Ok(BranchResult::Return);
            };
            let elem_value_ty = self
                .type_map
                .type_table()
                .get(self.type_map.type_of(elem_expr.id))
                .clone();
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
        expr: &Expr,
        entries: &[MapLitEntry],
    ) -> Result<BranchResult, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        let map_sem_ty = self.type_map.type_table().get(expr_ty).clone();
        let Type::Map { key_ty, value_ty } = map_sem_ty else {
            panic!("backend map literal has non-map type");
        };

        let map_ir_ty = self.type_lowerer.lower_type_id(expr_ty);
        let slot = self.alloc_value_slot(map_ir_ty);

        let key_ir_ty = self.type_lowerer.lower_type(&key_ty);
        let key_ptr_ty = self.type_lowerer.ptr_to(key_ir_ty);
        self.init_empty_hash_collection(slot.addr, key_ptr_ty);

        let key_size = self.runtime_size_const(&key_ty);
        let value_size = self.runtime_size_const(&value_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);

        for entry in entries.iter() {
            let Some(key_value) = self.lower_value_expr_opt(&entry.key)? else {
                return Ok(BranchResult::Return);
            };
            let key_value_ty = self
                .type_map
                .type_table()
                .get(self.type_map.type_of(entry.key.id))
                .clone();
            let key_addr = self.materialize_value_addr(key_value, &key_value_ty);

            let Some(value_value) = self.lower_value_expr_opt(&entry.value)? else {
                return Ok(BranchResult::Return);
            };
            let value_value_ty = self
                .type_map
                .type_table()
                .get(self.type_map.type_of(entry.value.id))
                .clone();
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
        expr: &Expr,
        items: &[Expr],
    ) -> Result<BranchResult, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        let tuple_ty = self.type_lowerer.lower_type_id(expr_ty);
        let slot = self.alloc_value_slot(tuple_ty);
        if !self.store_tuple_items(expr_ty, slot.addr, items)? {
            return Ok(BranchResult::Return);
        }

        Ok(self.load_slot(&slot).into())
    }

    fn lower_struct_lit_expr(
        &mut self,
        expr: &Expr,
        fields: &[StructLitField],
    ) -> Result<BranchResult, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        let struct_ty = self.type_lowerer.lower_type_id(expr_ty);
        let slot = self.alloc_value_slot(struct_ty);
        if !self.store_struct_lit_fields(expr_ty, slot.addr, fields)? {
            return Ok(BranchResult::Return);
        }

        Ok(self.load_slot(&slot).into())
    }

    fn lower_struct_update_expr(
        &mut self,
        expr: &Expr,
        target: &Expr,
        fields: &[StructUpdateField],
    ) -> Result<BranchResult, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        let struct_ty = self.type_lowerer.lower_type_id(expr_ty);
        let slot = self.alloc_value_slot(struct_ty);

        // Copy the base struct
        let Some(base_value) = self.lower_value_expr_opt(target)? else {
            return Ok(BranchResult::Return);
        };
        let base_ty = self.type_map.type_table().get(expr_ty);
        self.store_value_into_addr(slot.addr, base_value, base_ty, struct_ty);

        // Overwrite the updated fields
        if !self.store_struct_update_fields(expr_ty, slot.addr, fields)? {
            return Ok(BranchResult::Return);
        }

        Ok(self.load_slot(&slot).into())
    }

    fn lower_enum_variant_expr(
        &mut self,
        expr: &Expr,
        variant: &str,
        payload: &[Expr],
    ) -> Result<BranchResult, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        let (tag_ty, blob_ty, variant_tag, field_offsets, field_tys) = {
            let layout = self.type_lowerer.enum_layout(expr_ty);
            let variant_layout = layout.variant_by_name(variant);
            (
                layout.tag_ty,
                layout.blob_ty,
                variant_layout.tag,
                variant_layout.field_offsets.clone(),
                variant_layout.field_tys.clone(),
            )
        };

        let enum_ty = self.type_lowerer.lower_type_id(expr_ty);
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
        expr: &Expr,
        inner: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        let heap_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(expr.id))
            .clone();
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
        expr: &Expr,
        left: &Expr,
        op: BinaryOp,
        right: &Expr,
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
        let expr_ty = self.type_map.type_of(expr.id);
        let ty = self.type_lowerer.lower_type_id(expr_ty);
        let sem_ty = self.type_map.type_table().get(expr_ty);

        if let Some(binop) = map_binop(op) {
            if matches!(op, BinaryOp::Div | BinaryOp::Mod) {
                self.emit_div_by_zero_check(rhs, sem_ty);
            }
            return Ok(self.builder.binop(binop, lhs, rhs, ty).into());
        }
        if matches!(op, BinaryOp::Eq | BinaryOp::Ne) {
            let operand_ty = self
                .type_map
                .type_table()
                .get(self.type_map.type_of(left.id))
                .clone();
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
        expr: &Expr,
        target: &Expr,
        start: Option<&Expr>,
        end: Option<&Expr>,
    ) -> Result<BranchResult, LowerToIrError> {
        let plan = self.slice_plan(expr.id);

        let Type::Slice { elem_ty } = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(expr.id))
            .clone()
        else {
            panic!("backend slice expr has non-slice type");
        };

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);

        // Resolve the base pointer and length from the slice plan.
        let (base_ptr, base_len) = match plan.base {
            SliceBaseKind::Array { len, deref_count } => {
                let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                let Type::Array { .. } = base_ty else {
                    panic!("backend slice on non-array base {:?}", base_ty);
                };
                let view = self.load_array_view(base_addr, elem_ptr_ty, len);
                (view.ptr, view.len)
            }
            SliceBaseKind::Slice { deref_count } => {
                let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                let Type::Slice { .. } = base_ty else {
                    panic!("backend slice on non-slice base {:?}", base_ty);
                };
                let view = self.load_slice_view(base_addr, elem_ptr_ty);
                (view.ptr, view.len)
            }
            SliceBaseKind::String { deref_count } => {
                let (base_addr, base_ty) = self.resolve_deref_base(target, deref_count)?;
                let Type::String = base_ty else {
                    panic!("backend slice on non-string base {:?}", base_ty);
                };
                let view = self.load_string_view(base_addr);
                (view.ptr, view.len)
            }
            SliceBaseKind::DynArray { deref_count } => {
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

        let start_check = start.is_some_and(|expr| !matches!(expr.kind, ExprKind::IntLit(0)));
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

        let slice_ty = self
            .type_lowerer
            .lower_type_id(self.type_map.type_of(expr.id));
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
        expr: &Expr,
        target: &Expr,
        key: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        let Some(map_value) = self.lower_value_expr_opt(target)? else {
            return Ok(BranchResult::Return);
        };
        let map_target_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(target.id))
            .clone();
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

        let key_size = self.runtime_size_const(&key_ty);
        let value_size = self.runtime_size_const(&value_ty);

        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let hit = self.builder.call(
            Callee::Runtime(RuntimeFn::MapGetValue),
            vec![map_addr, key_addr, key_size, value_size, value_slot.addr],
            bool_ty,
        );

        // Build `V | KeyNotFound`: tag 0 when key is present, tag 1 otherwise.
        let expr_ty = self.type_map.type_of(expr.id);
        let union_ir_ty = self.type_lowerer.lower_type_id(expr_ty);
        let union_slot = self.alloc_value_slot(union_ir_ty);
        let (tag_ty, blob_ty, payload_offset, payload_ty) = {
            let layout = self.type_lowerer.enum_layout(expr_ty);
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
        expr: &Expr,
        place: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        let place_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(place.id))
            .clone();
        match place_ty {
            Type::Array { dims, .. } => {
                let len = dims
                    .first()
                    .copied()
                    .unwrap_or_else(|| panic!("backend len on array with empty dims"));
                let ty = self
                    .type_lowerer
                    .lower_type_id(self.type_map.type_of(expr.id));
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
        expr: &Expr,
        kind: CoerceKind,
        inner: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        match kind {
            CoerceKind::ArrayToSlice => self.lower_coerce_array_to_slice(expr, inner),
            CoerceKind::ArrayToDynArray => self.lower_coerce_array_to_dyn_array(expr, inner),
            CoerceKind::DynArrayToSlice => self.lower_coerce_dyn_array_to_slice(expr, inner),
        }
    }

    fn lower_coerce_array_to_slice(
        &mut self,
        expr: &Expr,
        inner: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        let plan = self.slice_plan(expr.id);

        let expr_ty_id = self.type_map.type_of(expr.id);
        let Type::Slice { elem_ty } = self.type_map.type_table().get(expr_ty_id).clone() else {
            panic!("backend coerce array-to-slice has non-slice type");
        };

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);

        let (base_addr, base_len) = match plan.base {
            SliceBaseKind::Array { len, deref_count } => {
                let inner_ty = self.type_map.type_of(inner.id);
                let base_ty = self.type_map.type_table().get(inner_ty).clone();

                // Prefer a place-based lowering to reuse the base address; otherwise
                // materialize the value into a temporary slot.
                let (base_addr, base_ty) = match &inner.kind {
                    ExprKind::Load { expr: place }
                    | ExprKind::Move { expr: place }
                    | ExprKind::ImplicitMove { expr: place } => {
                        self.resolve_deref_base(place, deref_count)?
                    }
                    _ => {
                        let Some(value) = self.lower_value_expr_opt(inner)? else {
                            return Ok(BranchResult::Return);
                        };
                        if deref_count == 0 {
                            let array_ty = self.type_lowerer.lower_type_id(inner_ty);
                            let addr = self.alloc_local_addr(array_ty);
                            let array_sem_ty = self.type_map.type_table().get(inner_ty);
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
        let slice_ty = self.type_lowerer.lower_type_id(expr_ty_id);
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
        expr: &Expr,
        inner: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        let target_dyn_ty = self.type_map.type_table().get(expr_ty).clone();
        let Type::DynArray { elem_ty } = target_dyn_ty else {
            panic!("backend coerce array-to-dyn-array has non-dyn-array type");
        };

        let inner_ty = self.type_map.type_of(inner.id);
        let source_array_ty = self.type_map.type_table().get(inner_ty).clone();
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
        let dyn_ir_ty = self.type_lowerer.lower_type_id(expr_ty);
        let dyn_slot = self.alloc_value_slot(dyn_ir_ty);

        // Lower the source as an addressable array value.
        let source_addr = match &inner.kind {
            ExprKind::Load { expr: place }
            | ExprKind::Move { expr: place }
            | ExprKind::ImplicitMove { expr: place } => self.lower_place_addr(place)?.addr,
            _ => {
                let Some(value) = self.lower_value_expr_opt(inner)? else {
                    return Ok(BranchResult::Return);
                };
                let array_ir_ty = self.type_lowerer.lower_type_id(inner_ty);
                let addr = self.alloc_local_addr(array_ir_ty);
                let array_sem_ty = self.type_map.type_table().get(inner_ty);
                self.store_value_into_addr(addr, value, array_sem_ty, array_ir_ty);
                addr
            }
        };

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let layout = self.type_lowerer.ir_type_cache.layout(elem_ir_ty);
        let elem_size = layout.size();
        let elem_align = layout.align();

        let len_val = self.builder.const_int(len_u32 as i128, false, 32, u32_ty);
        let zero_u64 = self.builder.const_int(0, false, 64, u64_ty);
        let zero_ptr = self.builder.cast(CastKind::IntToPtr, zero_u64, elem_ptr_ty);
        let zero_u32 = self.builder.const_int(0, false, 32, u32_ty);
        self.store_field(dyn_slot.addr, 0, elem_ptr_ty, zero_ptr);
        self.store_field(dyn_slot.addr, 1, u32_ty, zero_u32);
        self.store_field(dyn_slot.addr, 2, u32_ty, zero_u32);

        let ensure_cap = self.builder.const_int(len_u32 as i128, false, 32, u32_ty);
        let elem_size_val = self.builder.const_int(elem_size as i128, false, 64, u64_ty);
        let elem_align_val = self.builder.const_int(elem_align as i128, false, 64, u64_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::DynArrayEnsure),
            vec![dyn_slot.addr, ensure_cap, elem_size_val, elem_align_val],
            unit_ty,
        );

        let data_ptr = if len_u32 == 0 || elem_size == 0 {
            zero_ptr
        } else {
            let bytes =
                self.builder
                    .const_int((len_u32 as u64 * elem_size) as i128, false, 64, u64_ty);
            let src_ptr = self.builder.index_addr(source_addr, zero_u64, elem_ptr_ty);
            let dst_ptr = self.load_field(dyn_slot.addr, 0, elem_ptr_ty);
            self.builder.memcopy(dst_ptr, src_ptr, bytes);
            dst_ptr
        };

        let cap_val = self.load_field(dyn_slot.addr, 2, u32_ty);

        self.store_field(dyn_slot.addr, 0, elem_ptr_ty, data_ptr);
        self.store_field(dyn_slot.addr, 1, u32_ty, len_val);
        self.store_field(dyn_slot.addr, 2, u32_ty, cap_val);

        Ok(self.load_slot(&dyn_slot).into())
    }

    fn lower_coerce_dyn_array_to_slice(
        &mut self,
        expr: &Expr,
        inner: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        let plan = self.slice_plan(expr.id);

        let expr_ty_id = self.type_map.type_of(expr.id);
        let Type::Slice { elem_ty } = self.type_map.type_table().get(expr_ty_id).clone() else {
            panic!("backend coerce dyn-array-to-slice has non-slice type");
        };

        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);

        let (base_ptr, base_len) = match plan.base {
            SliceBaseKind::DynArray { deref_count } => {
                let inner_ty = self.type_map.type_of(inner.id);
                let base_ty = self.type_map.type_table().get(inner_ty).clone();
                let (base_addr, base_ty) = match &inner.kind {
                    ExprKind::Load { expr: place }
                    | ExprKind::Move { expr: place }
                    | ExprKind::ImplicitMove { expr: place } => {
                        self.resolve_deref_base(place, deref_count)?
                    }
                    _ => {
                        let Some(value) = self.lower_value_expr_opt(inner)? else {
                            return Ok(BranchResult::Return);
                        };
                        if deref_count == 0 {
                            let dyn_ir_ty = self.type_lowerer.lower_type_id(inner_ty);
                            let addr = self.alloc_local_addr(dyn_ir_ty);
                            let dyn_sem_ty = self.type_map.type_table().get(inner_ty);
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
        let slice_ty = self.type_lowerer.lower_type_id(expr_ty_id);
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
        expr: &Expr,
        to: &Expr,
        payload: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        let Some(dst) = self.lower_value_expr_opt(to)? else {
            return Ok(BranchResult::Return);
        };
        let Some(payload_value) = self.lower_value_expr_opt(payload)? else {
            return Ok(BranchResult::Return);
        };
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let dst_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(to.id))
            .clone();
        let dst = self.emit_target_machine_id(dst, &dst_ty);
        let payload_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(payload.id))
            .clone();
        let event_kind = self
            .machine_payload_event_kind(&payload_ty, Some(&dst_ty))
            .unwrap_or(0);
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
        let unit_ty = self
            .type_lowerer
            .lower_type_id(self.type_map.type_of(expr.id));
        Ok(self.builder.const_int(0, false, 8, unit_ty).into())
    }

    fn lower_emit_request_expr(
        &mut self,
        expr: &Expr,
        to: &Expr,
        payload: &Expr,
        request_site_key: u64,
    ) -> Result<BranchResult, LowerToIrError> {
        let Some(dst) = self.lower_value_expr_opt(to)? else {
            return Ok(BranchResult::Return);
        };
        let Some(payload_value) = self.lower_value_expr_opt(payload)? else {
            return Ok(BranchResult::Return);
        };
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let dst_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(to.id))
            .clone();
        let dst = self.emit_target_machine_id(dst, &dst_ty);
        let payload_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(payload.id))
            .clone();
        let event_kind = self
            .machine_payload_event_kind(&payload_ty, Some(&dst_ty))
            .unwrap_or(0);
        let kind = self
            .builder
            .const_int(event_kind as i128, false, 64, u64_ty);
        let (payload0, payload1) = self.pack_machine_payload_words(payload_value, &payload_ty);
        let request_site = self
            .builder
            .const_int(request_site_key as i128, false, 64, u64_ty);
        let pending_ty = self
            .type_lowerer
            .lower_type_id(self.type_map.type_of(expr.id));
        let pending = self.builder.call(
            Callee::Runtime(RuntimeFn::MachineEmitRequest),
            vec![dst, kind, payload0, payload1, request_site],
            pending_ty,
        );
        Ok(pending.into())
    }

    fn emit_target_machine_id(&mut self, dst: ValueId, dst_ty: &Type) -> ValueId {
        let Some(handle_name) = self.hosted_machine_handle_type_name(dst_ty) else {
            return dst;
        };
        let handle_ir_ty = self.type_lowerer.lower_type(dst_ty);
        let handle_slot = self.materialize_value_slot(dst, handle_ir_ty);
        let host = self
            .linear_index
            .machine_hosts
            .values()
            .find(|host| host.handle_type_name == handle_name)
            .unwrap_or_else(|| {
                panic!("backend missing hosted machine handle metadata for {handle_name}")
            });
        let (field_index, _field_ty, field_ir_ty) = self.struct_field_from_type(dst_ty, "_id");
        debug_assert_eq!(host.handle_type_name, handle_name);
        self.load_field(handle_slot.addr, field_index, field_ir_ty)
    }

    fn lower_reply_expr(
        &mut self,
        expr: &Expr,
        cap: &Expr,
        value: &Expr,
    ) -> Result<BranchResult, LowerToIrError> {
        let Some(cap_value) = self.lower_value_expr_opt(cap)? else {
            return Ok(BranchResult::Return);
        };
        let Some(reply_value) = self.lower_value_expr_opt(value)? else {
            return Ok(BranchResult::Return);
        };
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let response_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(value.id))
            .clone();
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
        let unit_ty = self
            .type_lowerer
            .lower_type_id(self.type_map.type_of(expr.id));
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
