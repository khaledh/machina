use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, Value};
use crate::lower::lower_util::u64_const;
use crate::mcir::types::*;
use crate::resolve::DefKind;
use crate::tree::semantic::{
    ArrayLitInit, PlaceExpr, PlaceExprKind as PEK, StructLitField, ValueExpr, ValueExprKind as VEK,
};
use crate::tree::{BinaryOp, CoerceKind, UnaryOp};
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    /// Lower an expression in value position into a scalar operand or aggregate place.
    pub(super) fn lower_expr_value(&mut self, expr: &ValueExpr) -> Result<Value, LowerError> {
        match &expr.kind {
            VEK::Block { items, tail } => {
                let block_ty = self.ty_from_id(expr.ty);
                self.lower_block_expr(&block_ty, items, tail.as_deref())
            }

            VEK::If {
                cond,
                then_body,
                else_body,
            } => self.lower_if_expr(cond, then_body, else_body),

            VEK::Call { callee, args } => self.lower_call_expr(expr, callee, args),

            VEK::MethodCall { receiver, args, .. } => {
                self.lower_method_call_expr(expr, receiver, args)
            }

            VEK::Match { scrutinee, arms } => self.lower_match_expr(expr, scrutinee, arms),

            VEK::Move { place } | VEK::ImplicitMove { place } => {
                // Explicit or implicit move: suppress drops for moved heap bindings.
                self.record_move_place(place);
                self.lower_place_value(place)
            }

            VEK::Coerce { .. } => {
                let ty = self.ty_from_id(expr.ty);
                if ty.is_scalar() {
                    Ok(Value::Scalar(self.lower_scalar_expr(expr)?))
                } else {
                    let place = self.lower_agg_expr_to_temp(expr)?;
                    Ok(Value::Aggregate(place))
                }
            }

            VEK::Load { place } => self.lower_place_value(place),

            // everything else: decide scalar vs aggregate by type
            _ => {
                let ty = self.ty_from_id(expr.ty);
                if ty.is_scalar() {
                    Ok(Value::Scalar(self.lower_scalar_expr(expr)?))
                } else {
                    let place = self.lower_agg_expr_to_temp(expr)?;
                    Ok(Value::Aggregate(place))
                }
            }
        }
    }

    fn lower_place_value(&mut self, place: &PlaceExpr) -> Result<Value, LowerError> {
        if let PEK::ArrayIndex { target, indices } = &place.kind {
            let target_ty = self.ty_from_id(target.ty);
            let peeled_ty = target_ty.peel_heap();
            if matches!(peeled_ty, Type::String) {
                let op = self.lower_string_index(place, target, indices)?;
                return Ok(Value::Scalar(op));
            }
        }
        let ty = self.ty_from_id(place.ty);
        if ty.is_scalar() {
            let op = Operand::Copy(self.lower_place_scalar(place)?);
            Ok(Value::Scalar(op))
        } else {
            Ok(Value::Aggregate(self.lower_place_agg(place)?))
        }
    }

    fn lower_scalar_place(&mut self, place: &PlaceExpr) -> Result<Operand, LowerError> {
        match &place.kind {
            PEK::Var { def_id, .. } => {
                let def = self.def_for_id(*def_id, place.id)?;
                if matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. }) {
                    return Ok(Operand::Const(Const::FuncAddr { def: def.id }));
                }
            }
            PEK::ArrayIndex { target, indices } => {
                let target_ty = self.ty_from_id(target.ty);
                let peeled_ty = target_ty.peel_heap();
                if matches!(peeled_ty, Type::String) {
                    return self.lower_string_index(place, target, indices);
                }
            }
            _ => {}
        }
        let place = self.lower_place_scalar(place)?;
        Ok(Operand::Copy(place))
    }

    // --- Expression (Scalar) ---

    /// Lower an expression expected to produce a scalar operand.
    pub(super) fn lower_scalar_expr(&mut self, expr: &ValueExpr) -> Result<Operand, LowerError> {
        match &expr.kind {
            VEK::Coerce { expr, .. } => self.lower_scalar_expr(expr),
            VEK::Move { place } | VEK::ImplicitMove { place } => {
                self.record_move_place(place);
                self.lower_scalar_place(place)
            }
            VEK::Load { place } => self.lower_scalar_place(place),
            VEK::AddrOf { place } => {
                let ty = self.ty_from_id(expr.ty);
                let ty_id = self.ty_lowerer.lower_ty(&ty);
                let place = self.lower_place(place)?;
                Ok(self.emit_scalar_rvalue(ty_id, Rvalue::AddrOf(place)))
            }

            // Literals
            VEK::IntLit(value) => {
                let ty = self.ty_from_id(expr.ty);
                let Type::Int { signed, bits } = ty else {
                    unreachable!(
                        "compiler bug: int literal with non-int type (type checker should have caught this)"
                    );
                };
                let c = Const::Int {
                    signed,
                    bits,
                    value: *value as i128,
                };
                Ok(Operand::Const(c))
            }
            VEK::BoolLit(value) => {
                let c = Const::Bool(*value);
                Ok(Operand::Const(c))
            }
            VEK::CharLit(value) => {
                let c = Const::Int {
                    signed: false,
                    bits: 32,
                    value: *value as i128,
                };
                Ok(Operand::Const(c))
            }
            VEK::UnitLit => {
                let c = Const::Unit;
                Ok(Operand::Const(c))
            }

            // Enum variant
            VEK::EnumVariant { variant, .. } => {
                // Only handle enums with no payload here
                let enum_ty = self.ty_from_id(expr.ty);
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

            VEK::ClosureRef { def_id } => Ok(Operand::Const(Const::FuncAddr { def: *def_id })),

            // Unary/Binary ops
            VEK::UnaryOp { op, expr: arg_expr } => {
                let arg_operand = self.lower_scalar_expr(arg_expr)?;

                match op {
                    UnaryOp::Neg => {
                        let ty = self.ty_from_id(expr.ty);
                        let ty_id = self.ty_lowerer.lower_ty(&ty);

                        let operand = self.emit_scalar_rvalue(
                            ty_id,
                            Rvalue::UnOp {
                                op: Self::map_unop(*op),
                                arg: arg_operand,
                            },
                        );
                        Ok(operand)
                    }
                    UnaryOp::LogicalNot => {
                        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);

                        let operand = self.emit_scalar_rvalue(
                            bool_ty_id,
                            Rvalue::BinOp {
                                op: BinOp::Eq,
                                lhs: arg_operand,
                                rhs: Operand::Const(Const::Bool(false)),
                            },
                        );
                        Ok(operand)
                    }
                    UnaryOp::BitNot => {
                        let ty = self.ty_from_id(expr.ty);
                        let ty_id = self.ty_lowerer.lower_ty(&ty);

                        let operand = self.emit_scalar_rvalue(
                            ty_id,
                            Rvalue::UnOp {
                                op: Self::map_unop(*op),
                                arg: arg_operand,
                            },
                        );
                        Ok(operand)
                    }
                }
            }

            VEK::HeapAlloc { expr } => self.lower_heap_alloc(expr),

            VEK::BinOp { left, op, right } => {
                let ty = self.ty_from_id(expr.ty);
                let ty_id = self.ty_lowerer.lower_ty(&ty);

                match op {
                    // Arithmetic operators
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        let Type::Int { signed, bits } = ty else {
                            unreachable!(
                                "compiler bug: arithmetic op with non-int type (type checker should have caught this)"
                            );
                        };

                        let lhs = self.lower_scalar_expr(left)?;
                        let rhs = self.lower_scalar_expr(right)?;

                        // Emit runtime check for division by zero
                        if *op == BinaryOp::Div {
                            let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);
                            let zero = Operand::Const(Const::Int {
                                signed,
                                bits,
                                value: 0,
                            });
                            let cond_op = self.emit_scalar_rvalue(
                                bool_ty_id,
                                Rvalue::BinOp {
                                    op: BinOp::Ne,
                                    lhs: rhs.clone(),
                                    rhs: zero,
                                },
                            );
                            self.emit_runtime_check(cond_op, CheckKind::DivByZero);
                        }

                        // Emit the binary operation
                        let operand = self.emit_scalar_rvalue(
                            ty_id,
                            Rvalue::BinOp {
                                op: Self::map_binop(*op),
                                lhs,
                                rhs,
                            },
                        );
                        Ok(operand)
                    }
                    // Bitwise and shift operators
                    BinaryOp::BitOr
                    | BinaryOp::BitXor
                    | BinaryOp::BitAnd
                    | BinaryOp::Shl
                    | BinaryOp::Shr => {
                        let Type::Int { .. } = ty else {
                            unreachable!(
                                "compiler bug: bitwise op with non-int type (type checker should have caught this)"
                            );
                        };

                        let lhs = self.lower_scalar_expr(left)?;
                        let rhs = self.lower_scalar_expr(right)?;

                        let operand = self.emit_scalar_rvalue(
                            ty_id,
                            Rvalue::BinOp {
                                op: Self::map_binop(*op),
                                lhs,
                                rhs,
                            },
                        );
                        Ok(operand)
                    }
                    BinaryOp::Mod => {
                        let Type::Int { signed, bits } = ty else {
                            unreachable!(
                                "compiler bug: arithmetic op with non-int type (type checker should have caught this)"
                            );
                        };

                        let lhs = self.lower_scalar_expr(left)?;
                        let rhs = self.lower_scalar_expr(right)?;

                        // Emit runtime check for modulo by zero
                        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);
                        let zero = Operand::Const(Const::Int {
                            signed,
                            bits,
                            value: 0,
                        });
                        let cond_op = self.emit_scalar_rvalue(
                            bool_ty_id,
                            Rvalue::BinOp {
                                op: BinOp::Ne,
                                lhs: rhs.clone(),
                                rhs: zero,
                            },
                        );
                        self.emit_runtime_check(cond_op, CheckKind::DivByZero);

                        // TODO: consider lowering to a target-specific remainder (e.g., udiv+msub).
                        // rem = lhs - (lhs / rhs) * rhs
                        let quot = self.emit_scalar_rvalue(
                            ty_id,
                            Rvalue::BinOp {
                                op: BinOp::Div,
                                lhs: lhs.clone(),
                                rhs: rhs.clone(),
                            },
                        );
                        let prod = self.emit_scalar_rvalue(
                            ty_id,
                            Rvalue::BinOp {
                                op: BinOp::Mul,
                                lhs: quot,
                                rhs: rhs.clone(),
                            },
                        );
                        let rem = self.emit_scalar_rvalue(
                            ty_id,
                            Rvalue::BinOp {
                                op: BinOp::Sub,
                                lhs,
                                rhs: prod,
                            },
                        );
                        Ok(rem)
                    }

                    // Comparison operators
                    BinaryOp::Eq
                    | BinaryOp::Ne
                    | BinaryOp::Lt
                    | BinaryOp::LtEq
                    | BinaryOp::Gt
                    | BinaryOp::GtEq => {
                        let lhs = self.lower_scalar_expr(left)?;
                        let rhs = self.lower_scalar_expr(right)?;

                        let operand = self.emit_scalar_rvalue(
                            ty_id,
                            Rvalue::BinOp {
                                op: Self::map_binop(*op),
                                lhs,
                                rhs,
                            },
                        );
                        Ok(operand)
                    }

                    // Logical operators
                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                        self.lower_logical_binop(*op, left, right)
                    }
                }
            }

            // Function calls, conditionals, blocks
            VEK::Call { .. } | VEK::MethodCall { .. } | VEK::If { .. } | VEK::Block { .. } => {
                match self.lower_expr_value(expr)? {
                    Value::Scalar(op) => Ok(op),
                    Value::Aggregate(_) => Err(LowerError::UnsupportedOperandExpr(expr.id)),
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

    fn lower_heap_alloc(&mut self, expr: &ValueExpr) -> Result<Operand, LowerError> {
        let elem_ty = self.ty_from_id(expr.ty);
        let size = elem_ty.size_of() as u64;
        let align = elem_ty.align_of() as u64;
        let elem_ty_id = self.ty_lowerer.lower_ty(&elem_ty);
        let heap_ty_id = self.ty_lowerer.lower_ty(&Type::Heap {
            elem_ty: Box::new(elem_ty.clone()),
        });

        // Allocate the heap storage (returns a typed pointer).
        let heap_ptr = self.emit_runtime_alloc(heap_ty_id, u64_const(size), u64_const(align));
        if size == 0 {
            return Ok(heap_ptr);
        }

        let (Operand::Copy(ptr_place) | Operand::Move(ptr_place)) = heap_ptr.clone() else {
            return Err(LowerError::UnsupportedOperandExpr(expr.id));
        };
        let dst_proj = vec![Projection::Deref];

        if elem_ty.is_scalar() {
            let value_op = self.lower_scalar_expr(expr)?;
            let dst_place = Place::new(ptr_place.base(), elem_ty_id, dst_proj);
            self.emit_copy_scalar(dst_place, Rvalue::Use(value_op));
        } else {
            let dst_place = Place::new(ptr_place.base(), elem_ty_id, dst_proj);
            self.lower_agg_value_into(dst_place, expr)?;
        }

        Ok(heap_ptr)
    }

    fn lower_logical_binop(
        &mut self,
        op: BinaryOp,
        left: &ValueExpr,
        right: &ValueExpr,
    ) -> Result<Operand, LowerError> {
        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);
        let temp = self.new_temp_scalar(bool_ty_id);
        let lhs = self.lower_scalar_expr(left)?;

        // Lower into short-circuiting if-expr
        match op {
            BinaryOp::LogicalAnd => {
                // If left is false, short-circuit and return false
                self.lower_if_join(
                    lhs,
                    |this| {
                        let rhs = this.lower_scalar_expr(right)?;
                        this.emit_copy_scalar(temp.clone(), Rvalue::Use(rhs));
                        Ok(())
                    },
                    |this| {
                        this.emit_copy_scalar(
                            temp.clone(),
                            Rvalue::Use(Operand::Const(Const::Bool(false))),
                        );
                        Ok(())
                    },
                )?;
            }
            BinaryOp::LogicalOr => {
                // If left is true, short-circuit and return true
                self.lower_if_join(
                    lhs,
                    |this| {
                        this.emit_copy_scalar(
                            temp.clone(),
                            Rvalue::Use(Operand::Const(Const::Bool(true))),
                        );
                        Ok(())
                    },
                    |this| {
                        let rhs = this.lower_scalar_expr(right)?;
                        this.emit_copy_scalar(temp.clone(), Rvalue::Use(rhs));
                        Ok(())
                    },
                )?;
            }
            _ => unreachable!("compiler bug: invalid logical binary op"),
        }
        Ok(Operand::Copy(temp))
    }

    // --- Expression (Aggregate) ---

    /// Lower an aggregate expression into a fresh temp place.
    pub(super) fn lower_agg_expr_to_temp(
        &mut self,
        expr: &ValueExpr,
    ) -> Result<Place<Aggregate>, LowerError> {
        let aggr_ty = self.ty_from_id(expr.ty);
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
        expr: &ValueExpr,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            VEK::StringFmt { plan } => self.lower_string_fmt_into(dst, plan),
            VEK::StringLit { .. }
            | VEK::ArrayLit { .. }
            | VEK::TupleLit(..)
            | VEK::StructLit { .. }
            | VEK::StructUpdate { .. }
            | VEK::EnumVariant { .. } => {
                // Aggregate literal: build in place.
                self.lower_agg_lit_into(dst, expr)
            }
            VEK::Coerce { kind, expr } => match kind {
                CoerceKind::ArrayToSlice => self.lower_array_to_slice_into(&dst, expr),
            },
            VEK::Load { place } => {
                let src = self.lower_place_agg(place)?;
                if src.base() == dst.base() && src.projections() == dst.projections() {
                    return Ok(());
                }
                self.emit_copy_aggregate(dst, src);
                Ok(())
            }
            VEK::Move { place } | VEK::ImplicitMove { place } => {
                self.record_move_place(place);
                let src = self.lower_place_agg(place)?;
                if src.base() == dst.base() && src.projections() == dst.projections() {
                    return Ok(());
                }
                self.emit_copy_aggregate(dst, src);
                Ok(())
            }
            VEK::Slice { target, start, end } => self.lower_slice_into(&dst, target, start, end),
            VEK::Call { callee, args } => {
                // Aggregate call result: direct into destination.
                self.lower_call_into(
                    Some(PlaceAny::Aggregate(dst.clone())),
                    expr,
                    Some(callee),
                    None,
                    args,
                )
            }
            VEK::MethodCall { receiver, args, .. } => self.lower_call_into(
                Some(PlaceAny::Aggregate(dst.clone())),
                expr,
                None,
                Some(receiver),
                args,
            ),
            VEK::Block { items, tail } => {
                self.lower_block_into(dst, items, tail.as_deref(), expr.id)
            }
            VEK::If {
                cond,
                then_body,
                else_body,
            } => self.lower_if_expr_into(dst, cond, then_body, else_body),
            VEK::Match { scrutinee, arms } => self.lower_match_into_agg(dst, scrutinee, arms),
            _ => match self.lower_expr_value(expr)? {
                Value::Aggregate(place) => {
                    self.emit_copy_aggregate(dst, place);
                    Ok(())
                }
                Value::Scalar(_) => Err(LowerError::UnsupportedAggregateRhs(expr.id)),
            },
        }
    }

    fn lower_array_to_slice_into(
        &mut self,
        dst: &Place<Aggregate>,
        expr: &ValueExpr,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            VEK::Load { place } => self.lower_slice_into(dst, place, &None, &None),
            VEK::Move { place } | VEK::ImplicitMove { place } => {
                self.record_move_place(place);
                self.lower_slice_into(dst, place, &None, &None)
            }
            _ => {
                let array_ty = self.ty_from_id(expr.ty);
                let Type::Array { dims, .. } = array_ty else {
                    return Err(LowerError::UnsupportedAggregateRhs(expr.id));
                };
                if dims.is_empty() {
                    return Err(LowerError::UnsupportedAggregateRhs(expr.id));
                }

                let array_place = self.lower_agg_expr_to_temp(expr)?;
                let len = dims[0];
                let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));

                let base_ptr_place = self.new_temp_scalar(u64_ty_id);
                self.emit_copy_scalar(
                    base_ptr_place.clone(),
                    Rvalue::AddrOf(PlaceAny::Aggregate(array_place)),
                );
                let base_ptr_op = Operand::Copy(base_ptr_place);

                let base_len_op = Operand::Const(Const::Int {
                    signed: false,
                    bits: 64,
                    value: len as i128,
                });

                let mut ptr_proj = dst.projections().to_vec();
                ptr_proj.push(Projection::Field { index: 0 });
                let ptr_field = Place::new(dst.base(), u64_ty_id, ptr_proj);

                let mut len_proj = dst.projections().to_vec();
                len_proj.push(Projection::Field { index: 1 });
                let len_field = Place::new(dst.base(), u64_ty_id, len_proj);

                self.emit_copy_scalar(ptr_field, Rvalue::Use(base_ptr_op));
                self.emit_copy_scalar(len_field, Rvalue::Use(base_len_op));

                Ok(())
            }
        }
    }

    /// Lower an aggregate literal directly into a destination place.
    pub(super) fn lower_agg_lit_into(
        &mut self,
        dst: Place<Aggregate>,
        expr: &ValueExpr,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            VEK::StringLit { value } => {
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
                let cap_const = Operand::Const(Const::Int {
                    signed: false,
                    bits: 32,
                    value: 0,
                });

                // 3) build aggregate temp {ptr, len, cap}
                // field 0: ptr
                let ptr_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
                self.emit_operand_into_agg_projection(
                    &dst,
                    Projection::Field { index: 0 },
                    ptr_const,
                    ptr_ty_id,
                )?;
                // field 1: len
                let len_ty_id = self.ty_lowerer.lower_ty(&Type::uint(32));
                self.emit_operand_into_agg_projection(
                    &dst,
                    Projection::Field { index: 1 },
                    len_const,
                    len_ty_id,
                )?;
                // field 2: cap
                let cap_ty_id = self.ty_lowerer.lower_ty(&Type::uint(32));
                self.emit_operand_into_agg_projection(
                    &dst,
                    Projection::Field { index: 2 },
                    cap_const,
                    cap_ty_id,
                )?;

                Ok(())
            }
            VEK::TupleLit(fields) => {
                // Lower each tuple field into its slot.
                for (i, field_expr) in fields.iter().enumerate() {
                    let field_ty = self.ty_from_id(field_expr.ty);
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

            VEK::StructLit { fields, .. } => {
                // Lower each struct field by name.
                for StructLitField {
                    name: field_name,
                    value: field_expr,
                    ..
                } in fields.iter()
                {
                    let field_ty = self.ty_from_id(field_expr.ty);
                    let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);
                    let field_index = {
                        let dst_ty = self.ty_from_id(expr.ty);
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

            VEK::StructUpdate { target, fields } => {
                // Evaluate the base expression first
                let base_place = match self.lower_expr_value(target)? {
                    Value::Aggregate(place) => place,
                    Value::Scalar(_) => return Err(LowerError::ExprIsNotAggregate(target.id)),
                };

                // Copy base into dst unless it's already the same place
                if base_place.base() != dst.base() || base_place.projections() != dst.projections()
                {
                    self.emit_copy_aggregate(dst.clone(), base_place);
                }

                // Overwrite updated fields
                let struct_ty = self.ty_from_id(expr.ty);
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

            VEK::ArrayLit { init, .. } => {
                // Lower each element into its index.
                let elem_ty = {
                    let dst_ty = self.ty_from_id(expr.ty);
                    dst_ty
                        .array_item_type()
                        .unwrap_or_else(|| panic!("Expected array type"))
                };
                let elem_ty_id = self.ty_lowerer.lower_ty(&elem_ty);

                match init {
                    ArrayLitInit::Elems(elems) => {
                        // Lower each element into its index.
                        for (i, elem) in elems.iter().enumerate() {
                            let index_proj = Projection::Index {
                                index: Operand::Const(Const::Int {
                                    signed: false,
                                    bits: 64,
                                    value: i as i128,
                                }),
                            };

                            self.lower_expr_into_agg_projection(
                                &dst, elem_ty_id, elem, index_proj,
                            )?;
                        }
                    }
                    ArrayLitInit::Repeat(expr, count) => {
                        // Evaluate the repeat expression once and copy to each element.
                        let count = *count as usize;
                        if elem_ty == Type::uint(8) {
                            // Special case: for u8 arrays -> use memset intrinsic.
                            let value_op = self.lower_scalar_expr(expr)?;
                            self.fb.push_stmt(
                                self.curr_block,
                                Statement::MemSet {
                                    dst: dst.clone(),
                                    value: value_op,
                                    len: count as u64,
                                },
                            );
                        } else if self.is_scalar(elem_ty_id) {
                            // Non u8 scalar: copy each element.
                            let value_op = self.lower_scalar_expr(expr)?;
                            for i in 0..count {
                                let index_proj = Projection::Index {
                                    index: Operand::Const(Const::Int {
                                        signed: false,
                                        bits: 64,
                                        value: i as i128,
                                    }),
                                };
                                let mut projs = dst.projections().to_vec();
                                projs.push(index_proj);
                                let field_place = Place::new(dst.base(), elem_ty_id, projs);
                                self.emit_copy_scalar(field_place, Rvalue::Use(value_op.clone()));
                            }
                        } else {
                            // Aggregate: copy each element.
                            let value_place = self.lower_agg_expr_to_temp(expr)?;
                            for i in 0..count {
                                let index_proj = Projection::Index {
                                    index: Operand::Const(Const::Int {
                                        signed: false,
                                        bits: 64,
                                        value: i as i128,
                                    }),
                                };
                                let mut projs = dst.projections().to_vec();
                                projs.push(index_proj);
                                let field_place = Place::new(dst.base(), elem_ty_id, projs);
                                self.emit_copy_aggregate(field_place, value_place.clone());
                            }
                        }
                    }
                }
                Ok(())
            }

            VEK::EnumVariant {
                variant, payload, ..
            } => {
                // 1) tag field (index 0)
                let enum_ty = self.ty_from_id(expr.ty);
                let tag = Const::Int {
                    signed: false,
                    bits: 64,
                    value: enum_ty.enum_variant_index(variant) as i128,
                };

                let tag_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
                let mut tag_proj = dst.projections().to_vec();
                tag_proj.push(Projection::Field { index: 0 });
                let tag_place = Place::new(dst.base(), tag_ty_id, tag_proj);

                self.emit_copy_scalar(tag_place, Rvalue::Use(Operand::Const(tag)));

                // 2) payload fields (indices 1+)
                for (i, payload_expr) in payload.iter().enumerate() {
                    let field_ty = self.ty_from_id(payload_expr.ty);
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
        field_expr: &ValueExpr,
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
