use crate::ast::{BinaryOp, Expr, ExprKind as EK, StringTag, StructLitField};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{ExprValue, FuncLowerer};
use crate::mcir::types::*;
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    /// Lower an expression in value position into a scalar operand or aggregate place.
    pub(super) fn lower_expr_value(&mut self, expr: &Expr) -> Result<ExprValue, LowerError> {
        match &expr.kind {
            EK::Block { items, tail } => self.lower_block_expr(items, tail.as_deref()),

            EK::If {
                cond,
                then_body,
                else_body,
            } => self.lower_if_expr(cond, then_body, else_body),

            EK::Call { callee, args } => self.lower_call_expr(expr, callee, args),

            EK::Match { scrutinee, arms } => self.lower_match_expr(expr, scrutinee, arms),

            // everything else: decide scalar vs aggregate by type
            _ => {
                let ty = self.ty_for_node(expr.id)?;
                if ty.is_scalar() {
                    Ok(ExprValue::Scalar(self.lower_scalar_expr(expr)?))
                } else {
                    // prefer place, fall back to aggregate literal
                    let place = self
                        .lower_place_agg(expr)
                        .or_else(|_| self.lower_agg_expr_to_temp(expr))?;
                    Ok(ExprValue::Aggregate(place))
                }
            }
        }
    }

    // --- Expression (Scalar) ---

    /// Lower an expression expected to produce a scalar operand.
    pub(super) fn lower_scalar_expr(&mut self, expr: &Expr) -> Result<Operand, LowerError> {
        match &expr.kind {
            // Literals
            EK::UInt64Lit(value) => {
                let c = Const::Int {
                    signed: false,
                    bits: 64,
                    value: *value as i128,
                };
                Ok(Operand::Const(c))
            }
            EK::BoolLit(value) => {
                let c = Const::Bool(*value);
                Ok(Operand::Const(c))
            }
            EK::CharLit(value) => {
                let c = Const::Int {
                    signed: false,
                    bits: 8,
                    value: *value as i128,
                };
                Ok(Operand::Const(c))
            }
            EK::UnitLit => {
                let c = Const::Unit;
                Ok(Operand::Const(c))
            }

            // Enum variant
            EK::EnumVariant { variant, .. } => {
                // Only handle enums with no payload here
                let enum_ty = self.ty_for_node(expr.id)?;
                if !enum_ty.is_scalar() {
                    return Err(LowerError::UnsupportedOperandExpr(expr.id));
                }

                let variant_index = enum_ty.enum_variant_index(variant);
                let variant_tag = Const::Int {
                    signed: false,
                    bits: 64,
                    value: variant_index as i128,
                };
                Ok(Operand::Const(variant_tag))
            }

            // Place-based reads
            EK::Var(_) | EK::ArrayIndex { .. } | EK::TupleField { .. } | EK::StructField { .. } => {
                let place = self.lower_place_scalar(expr)?;
                Ok(Operand::Copy(place))
            }

            // Unary/Binary ops
            EK::UnaryOp { op, expr: arg_expr } => {
                let ty = self.ty_for_node(expr.id)?;
                let ty_id = self.ty_lowerer.lower_ty(&ty);

                let arg_operand = self.lower_scalar_expr(arg_expr)?;
                let operand = self.emit_scalar_rvalue(
                    ty_id,
                    Rvalue::UnOp {
                        op: Self::map_unop(*op),
                        arg: arg_operand,
                    },
                );
                Ok(operand)
            }
            EK::BinOp { left, op, right } => {
                let ty = self.ty_for_node(expr.id)?;
                let ty_id = self.ty_lowerer.lower_ty(&ty);

                let lhs_operand = self.lower_scalar_expr(left)?;
                let rhs_operand = self.lower_scalar_expr(right)?;

                // Check for division by zero
                if *op == BinaryOp::Div {
                    let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);
                    let zero = Operand::Const(Const::Int {
                        signed: false,
                        bits: 64,
                        value: 0,
                    });
                    let cond_op = self.emit_scalar_rvalue(
                        bool_ty_id,
                        Rvalue::BinOp {
                            op: BinOp::Ne,
                            lhs: rhs_operand.clone(),
                            rhs: zero,
                        },
                    );
                    self.emit_runtime_check(cond_op, CheckKind::DivByZero);
                }

                let operand = self.emit_scalar_rvalue(
                    ty_id,
                    Rvalue::BinOp {
                        op: Self::map_binop(*op),
                        lhs: lhs_operand,
                        rhs: rhs_operand,
                    },
                );
                Ok(operand)
            }

            // Function calls, conditionals, blocks
            EK::Call { .. } | EK::If { .. } | EK::Block { .. } => {
                match self.lower_expr_value(expr)? {
                    ExprValue::Scalar(op) => Ok(op),
                    ExprValue::Aggregate(_) => Err(LowerError::UnsupportedOperandExpr(expr.id)),
                }
            }

            _ => Err(LowerError::UnsupportedOperandExpr(expr.id)),
        }
    }

    /// Emit a scalar rvalue into a temp and return it as an operand.
    pub(super) fn emit_scalar_rvalue(&mut self, ty_id: TyId, rvalue: Rvalue) -> Operand {
        // Materialize into a temp so later uses can be a place.
        let temp_place = self.new_temp_scalar(ty_id);

        // Emit the assignment
        self.emit_copy_scalar(temp_place.clone(), rvalue);

        // Return a copy of the temp place as an operand
        Operand::Copy(temp_place)
    }

    // --- Expression (Aggregate) ---

    /// Lower an aggregate expression into a fresh temp place.
    pub(super) fn lower_agg_expr_to_temp(
        &mut self,
        expr: &Expr,
    ) -> Result<Place<Aggregate>, LowerError> {
        let aggr_ty = self.ty_for_node(expr.id)?;
        let aggr_ty_id = self.ty_lowerer.lower_ty(&aggr_ty);
        if !self.is_aggregate(aggr_ty_id) {
            return Err(LowerError::ExprIsNotAggregate(expr.id));
        }

        // Create a temp to hold the result.
        let temp_place = self.new_temp_aggregate(aggr_ty_id);

        // Lower the aggregate into the temp place
        self.lower_agg_value_into(temp_place.clone(), expr)?;
        Ok(temp_place)
    }

    /// Lower an aggregate expression directly into a destination place.
    pub(super) fn lower_agg_value_into(
        &mut self,
        dst: Place<Aggregate>,
        expr: &Expr,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            EK::StringLit { .. }
            | EK::ArrayLit(..)
            | EK::TupleLit(..)
            | EK::StructLit { .. }
            | EK::StructUpdate { .. }
            | EK::EnumVariant { .. } => {
                // Aggregate literal: build in place.
                self.lower_agg_lit_into(dst, expr)
            }
            EK::Var(_) | EK::ArrayIndex { .. } | EK::TupleField { .. } | EK::StructField { .. } => {
                // Aggregate place: copy unless it's already the destination.
                let src = self.lower_place_agg(expr)?;
                if src.base() == dst.base() && src.projections() == dst.projections() {
                    return Ok(());
                }
                self.emit_copy_aggregate(dst, src);
                Ok(())
            }
            EK::Call { callee, args } => {
                // Aggregate call result: direct into destination.
                self.lower_call_into(PlaceAny::Aggregate(dst), callee, args)
            }
            EK::Block { items, tail } => {
                self.lower_block_into(dst, items, tail.as_deref(), expr.id)
            }
            EK::If {
                cond,
                then_body,
                else_body,
            } => self.lower_if_expr_into(dst, cond, then_body, else_body),
            EK::Match { scrutinee, arms } => self.lower_match_into_agg(dst, scrutinee, arms),
            _ => match self.lower_expr_value(expr)? {
                ExprValue::Aggregate(place) => {
                    self.emit_copy_aggregate(dst, place);
                    Ok(())
                }
                ExprValue::Scalar(_) => Err(LowerError::UnsupportedAggregateRhs(expr.id)),
            },
        }
    }

    /// Lower an aggregate literal directly into a destination place.
    pub(super) fn lower_agg_lit_into(
        &mut self,
        dst: Place<Aggregate>,
        expr: &Expr,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            EK::StringLit { value, tag } => {
                // 1) intern payload
                let gid = self
                    .global_interner
                    .intern(GlobalPayload::String(value.clone()), GlobalSection::RoData);

                // 2) build constants for fields
                let ptr_const = Operand::Const(Const::GlobalAddr { id: gid });
                let len_const = Operand::Const(Const::Int {
                    signed: false,
                    bits: 32,
                    value: value.len() as i128,
                });
                let tag_const = Operand::Const(Const::Int {
                    signed: false,
                    bits: 8,
                    value: match tag {
                        StringTag::Ascii => 0,
                        StringTag::Utf8 => 1,
                    },
                });

                // 3) build aggregate temp {ptr, len, tag}
                // field 0: ptr
                let ptr_ty_id = self.ty_lowerer.lower_ty(&Type::UInt64);
                self.emit_operand_into_agg_projection(
                    &dst,
                    Projection::Field { index: 0 },
                    ptr_const,
                    ptr_ty_id,
                )?;
                // field 1: len
                let len_ty_id = self.ty_lowerer.lower_ty(&Type::UInt32);
                self.emit_operand_into_agg_projection(
                    &dst,
                    Projection::Field { index: 1 },
                    len_const,
                    len_ty_id,
                )?;
                // field 2: tag
                let tag_ty_id = self.ty_lowerer.lower_ty(&Type::UInt8);
                self.emit_operand_into_agg_projection(
                    &dst,
                    Projection::Field { index: 2 },
                    tag_const,
                    tag_ty_id,
                )?;

                Ok(())
            }
            EK::TupleLit(fields) => {
                // Lower each tuple field into its slot.
                for (i, field_expr) in fields.iter().enumerate() {
                    let field_ty = self.ty_for_node(field_expr.id)?;
                    let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);

                    self.lower_expr_into_agg_projection(
                        &dst,
                        field_ty_id,
                        field_expr,
                        Projection::Field { index: i },
                    )?;
                }
                Ok(())
            }

            EK::StructLit { fields, .. } => {
                // Lower each struct field by name.
                for StructLitField {
                    name: field_name,
                    value: field_expr,
                    ..
                } in fields.iter()
                {
                    let field_ty = self.ty_for_node(field_expr.id)?;
                    let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);
                    let field_index = {
                        let dst_ty = self.ty_for_node(expr.id)?;
                        dst_ty.struct_field_index(field_name)
                    };

                    self.lower_expr_into_agg_projection(
                        &dst,
                        field_ty_id,
                        field_expr,
                        Projection::Field { index: field_index },
                    )?;
                }

                Ok(())
            }

            EK::StructUpdate { target, fields } => {
                // Evaluate the base expression first
                let base_place = match self.lower_expr_value(target)? {
                    ExprValue::Aggregate(place) => place,
                    ExprValue::Scalar(_) => return Err(LowerError::ExprIsNotAggregate(target.id)),
                };

                // Copy base into dst unless it's already the same place
                if base_place.base() != dst.base() || base_place.projections() != dst.projections()
                {
                    self.emit_copy_aggregate(dst.clone(), base_place);
                }

                // Overwrite updated fields
                let struct_ty = self.ty_for_node(expr.id)?;
                for field in fields {
                    let field_ty = struct_ty.struct_field_type(&field.name);
                    let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);
                    let field_index = struct_ty.struct_field_index(&field.name);

                    self.lower_expr_into_agg_projection(
                        &dst,
                        field_ty_id,
                        &field.value,
                        Projection::Field { index: field_index },
                    )?;
                }

                Ok(())
            }

            EK::ArrayLit(elem_exprs) => {
                // Lower each element into its index.
                let elem_ty = {
                    let dst_ty = self.ty_for_node(expr.id)?;
                    match dst_ty {
                        Type::Array { elem_ty, .. } => elem_ty,
                        _ => panic!("Expected array type"),
                    }
                };
                let elem_ty_id = self.ty_lowerer.lower_ty(&elem_ty);

                for (i, elem_expr) in elem_exprs.iter().enumerate() {
                    let index_proj = Projection::Index {
                        index: Operand::Const(Const::Int {
                            signed: false,
                            bits: 64,
                            value: i as i128,
                        }),
                    };

                    self.lower_expr_into_agg_projection(&dst, elem_ty_id, elem_expr, index_proj)?;
                }
                Ok(())
            }

            EK::EnumVariant {
                variant, payload, ..
            } => {
                // 1) tag field (index 0)
                let enum_ty = self.ty_for_node(expr.id)?;
                let tag = Const::Int {
                    signed: false,
                    bits: 64,
                    value: enum_ty.enum_variant_index(variant) as i128,
                };

                let tag_ty_id = self.ty_lowerer.lower_ty(&Type::UInt64);
                let mut tag_proj = dst.projections().to_vec();
                tag_proj.push(Projection::Field { index: 0 });
                let tag_place = Place::new(dst.base(), tag_ty_id, tag_proj);

                self.emit_copy_scalar(tag_place, Rvalue::Use(Operand::Const(tag)));

                // 2) payload fields (indices 1+)
                for (i, payload_expr) in payload.iter().enumerate() {
                    let field_ty = self.ty_for_node(payload_expr.id)?;
                    let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);

                    // Compute the byte offset of the payload field
                    let byte_offset = enum_ty.enum_variant_payload_offsets(variant)[i];

                    let mut projs = dst.projections().to_vec();
                    projs.push(Projection::Field { index: 1 });
                    projs.push(Projection::ByteOffset {
                        offset: byte_offset,
                    });

                    if self.is_scalar(field_ty_id) {
                        let field_place = Place::new(dst.base(), field_ty_id, projs);
                        let src_op = self.lower_scalar_expr(payload_expr)?;
                        self.emit_copy_scalar(field_place, Rvalue::Use(src_op));
                    } else {
                        let field_place = Place::new(dst.base(), field_ty_id, projs);
                        let src_place = self.lower_agg_expr_to_temp(payload_expr)?;
                        self.emit_copy_aggregate(field_place, src_place);
                    }
                }
                Ok(())
            }

            _ => Err(LowerError::ExprIsNotAggregate(expr.id)),
        }
    }

    /// Lower an expression value into a projected field/element of an aggregate.
    pub(super) fn lower_expr_into_agg_projection(
        &mut self,
        dst: &Place<Aggregate>,
        field_ty_id: TyId,
        field_expr: &Expr,
        extra_proj: Projection,
    ) -> Result<(), LowerError> {
        // Extend the projection and write into the selected field/element.
        let mut projs = dst.projections().to_vec();
        projs.push(extra_proj);

        if self.is_scalar(field_ty_id) {
            let field_place = Place::new(dst.base(), field_ty_id, projs);
            let field_operand = self.lower_scalar_expr(field_expr)?;
            self.emit_copy_scalar(field_place, Rvalue::Use(field_operand));
            Ok(())
        } else {
            let field_place = Place::new(dst.base(), field_ty_id, projs);
            self.lower_agg_value_into(field_place, field_expr)
        }
    }

    // Emit an operand into a projected field/element of an aggregate.
    pub(super) fn emit_operand_into_agg_projection(
        &mut self,
        dst: &Place<Aggregate>,
        extra_proj: Projection,
        op: Operand,
        op_ty_id: TyId,
    ) -> Result<(), LowerError> {
        // Extend the projection and write into the selected field/element.
        let mut projs = dst.projections().to_vec();
        projs.push(extra_proj);

        let field_place = Place::new(dst.base(), op_ty_id, projs);
        self.emit_copy_scalar(field_place, Rvalue::Use(op));
        Ok(())
    }
}
