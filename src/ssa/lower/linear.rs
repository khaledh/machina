//! Straight-line (single-block) lowering routines.

use crate::ssa::lower::locals::LocalValue;
use crate::ssa::lower::lowerer::{BranchResult, FuncLowerer, LinearValue, StmtOutcome};
use crate::ssa::lower::mapping::{map_binop, map_cmp};
use crate::ssa::lower::{LoweringError, LoweringErrorKind};
use crate::ssa::model::ir::{Terminator, UnOp};
use crate::tree::UnaryOp;
use crate::tree::semantic as sem;
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    /// Lowers a linear value expression directly from the semantic tree.
    ///
    /// This avoids constructing a parallel linear AST for the common cases.
    pub(super) fn lower_value_expr_linear(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, LoweringError> {
        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        sem::BlockItem::Stmt(stmt) => match self.lower_stmt_expr_linear(stmt)? {
                            StmtOutcome::Continue => {}
                            StmtOutcome::Return => {
                                panic!(
                                    "ssa lower_value_expr_linear hit return in linear block at {:?}",
                                    stmt.span
                                );
                            }
                        },
                        sem::BlockItem::Expr(expr) => {
                            let _ = self.lower_value_expr_linear(expr)?;
                        }
                    }
                }

                if let Some(tail) = tail {
                    return self.lower_value_expr_linear(tail);
                }

                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_unit(ty))
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

            sem::ValueExprKind::ArrayLit { init, .. } => {
                // Allocate a local for the array and get its address
                let array_ty = self.type_lowerer.lower_type_id(expr.ty);
                let local = self.builder.add_local(array_ty, None);
                let ptr_ty = self.type_lowerer.ptr_to(array_ty);
                let addr = self.builder.addr_of_local(local, ptr_ty);

                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));

                // Store each element
                match init {
                    sem::ArrayLitInit::Elems(elems) => {
                        for (i, elem_expr) in elems.iter().enumerate() {
                            let value = self.lower_value_expr_linear(elem_expr)?;
                            let index_val = self.builder.const_int(i as i128, false, 64, u64_ty);
                            let elem_ty = self.type_lowerer.lower_type_id(elem_expr.ty);
                            let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ty);
                            let elem_addr = self.builder.index_addr(addr, index_val, elem_ptr_ty);
                            self.builder.store(elem_addr, value);
                        }
                    }
                    sem::ArrayLitInit::Repeat(expr, count) => {
                        let value = self.lower_value_expr_linear(expr)?;
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
                let local = self.builder.add_local(tuple_ty, None);
                let ptr_ty = self.type_lowerer.ptr_to(tuple_ty);
                let addr = self.builder.addr_of_local(local, ptr_ty);

                // Store each field
                for (i, elem_expr) in items.iter().enumerate() {
                    let value = self.lower_value_expr_linear(elem_expr)?;
                    let field_ty = self.lower_tuple_field_ty(expr.ty, i);
                    let field_ptr_ty = self.type_lowerer.ptr_to(field_ty);
                    let field_addr = self.builder.field_addr(addr, i, field_ptr_ty);
                    self.builder.store(field_addr, value);
                }

                // Load the tuple value
                Ok(self.builder.load(addr, tuple_ty))
            }

            sem::ValueExprKind::StructLit { fields, .. } => {
                // Allocate a local for the struct and get its address
                let struct_ty = self.type_lowerer.lower_type_id(expr.ty);
                let local = self.builder.add_local(struct_ty, None);
                let ptr_ty = self.type_lowerer.ptr_to(struct_ty);
                let addr = self.builder.addr_of_local(local, ptr_ty);

                // Store each field
                for field in fields.iter() {
                    let value = self.lower_value_expr_linear(&field.value)?;
                    let (field_index, field_ty) = self.lower_struct_field_ty(expr.ty, &field.name);
                    let field_ptr_ty = self.type_lowerer.ptr_to(field_ty);
                    let field_addr = self.builder.field_addr(addr, field_index, field_ptr_ty);
                    self.builder.store(field_addr, value);
                }

                // Load the struct value
                Ok(self.builder.load(addr, struct_ty))
            }

            sem::ValueExprKind::StructUpdate { target, fields } => {
                // Allocate a local for the updated struct and get its address
                let struct_ty = self.type_lowerer.lower_type_id(expr.ty);
                let local = self.builder.add_local(struct_ty, None);
                let ptr_ty = self.type_lowerer.ptr_to(struct_ty);
                let addr = self.builder.addr_of_local(local, ptr_ty);

                // Copy the base struct
                let base_value = self.lower_value_expr_linear(target)?;
                self.builder.store(addr, base_value);

                // Overwrite the updated fields
                for field in fields.iter() {
                    let value = self.lower_value_expr_linear(&field.value)?;
                    let (field_index, field_ty) = self.lower_struct_field_ty(expr.ty, &field.name);
                    let field_ptr_ty = self.type_lowerer.ptr_to(field_ty);
                    let field_addr = self.builder.field_addr(addr, field_index, field_ptr_ty);
                    self.builder.store(field_addr, value);
                }

                // Load the updated struct value
                Ok(self.builder.load(addr, struct_ty))
            }

            sem::ValueExprKind::UnaryOp { op, expr: inner } => {
                let value = self.lower_value_expr_linear(inner)?;
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let result = match op {
                    UnaryOp::Neg => self.builder.unop(UnOp::Neg, value, ty),
                    UnaryOp::LogicalNot => self.builder.unop(UnOp::Not, value, ty),
                    UnaryOp::BitNot => self.builder.unop(UnOp::BitNot, value, ty),
                };
                Ok(result)
            }

            sem::ValueExprKind::BinOp { left, op, right } => {
                let lhs = self.lower_value_expr_linear(left)?;
                let rhs = self.lower_value_expr_linear(right)?;
                let ty = self.type_lowerer.lower_type_id(expr.ty);

                if let Some(binop) = map_binop(*op) {
                    return Ok(self.builder.binop(binop, lhs, rhs, ty));
                }
                if let Some(cmp) = map_cmp(*op) {
                    return Ok(self.builder.cmp(cmp, lhs, rhs, ty));
                }
                Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr))
            }

            sem::ValueExprKind::Load { place } => match &place.kind {
                sem::PlaceExprKind::Var { def_id, .. } => Ok(self.load_local_value(*def_id)),
                _ => {
                    let place_addr = self.lower_place_addr(place)?;
                    Ok(self.builder.load(place_addr.addr, place_addr.value_ty))
                }
            },

            sem::ValueExprKind::AddrOf { place } => {
                let place_addr = self.lower_place_addr(place)?;
                Ok(place_addr.addr)
            }

            sem::ValueExprKind::Call { callee: _, args } => self.lower_call_expr(expr, args),

            sem::ValueExprKind::MethodCall { receiver, args, .. } => {
                self.lower_method_call_expr(expr, receiver, args)
            }

            sem::ValueExprKind::ClosureRef { .. } => {
                Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr))
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
                let value = self.lower_value_expr_linear(value_expr)?;
                let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                self.bind_pattern(pattern, LocalValue::value(value, ty))?;
                Ok(StmtOutcome::Continue)
            }

            sem::StmtExprKind::Assign {
                assignee, value, ..
            } => {
                let value_expr = value;
                let value = self.lower_value_expr_linear(value_expr)?;
                match &assignee.kind {
                    sem::PlaceExprKind::Var { def_id, .. } => {
                        let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                        self.assign_local_value(*def_id, value, ty);
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
                    Some(expr) => Some(self.lower_value_expr_linear(expr)?),
                    None => None,
                };
                self.builder.terminate(Terminator::Return { value });
                Ok(StmtOutcome::Return)
            }

            sem::StmtExprKind::VarDecl { .. }
            | sem::StmtExprKind::While { .. }
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

    /// Binds a pattern to a value, updating the locals map.
    /// Only simple name patterns are supported.
    pub(super) fn bind_pattern(
        &mut self,
        pattern: &sem::BindPattern,
        value: LocalValue,
    ) -> Result<(), LoweringError> {
        match &pattern.kind {
            sem::BindPatternKind::Name { def_id, .. } => {
                self.locals.insert(*def_id, value);
                Ok(())
            }
            _ => Err(LoweringError {
                kind: LoweringErrorKind::UnsupportedStmt,
                span: pattern.span,
            }),
        }
    }
}
