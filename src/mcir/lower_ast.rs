//! Lowering from AST to MCIR.
//!
//! Design notes:
//! - The lowering is place-based: aggregate values are modeled as places (locations),
//!   while scalars are modeled as operands.
//! - Lowering distinguishes value position from block-item position. Expressions that
//!   only make sense for side effects (let/var/assign/while) are rejected in value
//!   context.
//! - Aggregate construction is done in-place via projections whenever possible to
//!   avoid extra temporaries and copies (NRVO/RVO-style behavior).
//! - Control-flow expressions (if/block) are lowered by emitting side effects in
//!   each branch and joining with a destination place or a temp.
//!
//! Implementation notes:
//! - `lower_agg_value_into` is the central entry for writing aggregate values into
//!   a destination place.
//! - `emit_call_into` centralizes call emission so call sites can reuse it for both
//!   scalar and aggregate destinations.

use std::collections::HashMap;

use thiserror::Error;

use crate::ast::NodeId;
use crate::ast::{self, ExprKind as EK, PatternKind as PK, *};
use crate::context::{AnalyzedContext, LoweredMcirContext};
use crate::mcir::func_builder::FuncBuilder;
use crate::mcir::lower_ty::TyLowerer;
use crate::mcir::types::*;
use crate::resolve::def_map::{Def, DefId, DefKind};
use crate::types::*;

#[derive(Debug, Error)]
pub enum LowerError {
    #[error("expression def not found for node {0:?}")]
    ExprDefNotFound(NodeId),

    #[error("expression type not found for node {0:?}")]
    ExprTypeNotFound(NodeId),

    #[error("expression is not a place for node {0:?}")]
    ExprIsNotPlace(NodeId),

    #[error("expression is not an aggregate for node {0:?}")]
    ExprIsNotAggregate(NodeId),

    #[error("expression not allowed in value context for node {0:?}")]
    ExprNotAllowedInValueContext(NodeId),

    #[error("place is not {expected:?} for node {node_id:?}")]
    PlaceKindMismatch {
        node_id: NodeId,
        expected: PlaceKind,
    },

    #[error("variable local not found for node {0:?} (def {1:?})")]
    VarLocalNotFound(NodeId, DefId),

    #[error("unsupported operand expression for node {0:?}")]
    UnsupportedOperandExpr(NodeId),

    #[error("pattern does not match expected shape for node {0:?}")]
    PatternMismatch(NodeId),

    #[error("unsupported aggregate rhs for node {0:?}")]
    UnsupportedAggregateRhs(NodeId),
}

enum ExprValue {
    Scalar(Operand),
    Aggregate(Place<Aggregate>),
}

#[derive(Debug, Clone, Copy)]
pub enum PlaceKind {
    Scalar,
    Aggregate,
}

#[derive(Debug)]
pub struct FuncLowerer<'a> {
    ctx: &'a AnalyzedContext,
    func: &'a Function,
    fb: FuncBuilder,
    locals: HashMap<DefId, LocalId>,
    ty_lowerer: TyLowerer,
    curr_block: BlockId,
}

impl<'a> FuncLowerer<'a> {
    /// Create a lowering context for one function.
    pub fn new(ctx: &'a AnalyzedContext, func: &'a Function) -> Self {
        let mut ty_lowerer = TyLowerer::new();

        // Lower the function return type
        let ast_ret_ty = ctx
            .type_map
            .lookup_node_type(func.id)
            .expect("Function return type not found");
        let ret_ty_id = ty_lowerer.lower_ty(&ast_ret_ty);

        // Initialize the function builder
        let fb = FuncBuilder::new(ret_ty_id);
        let entry = fb.body.entry;

        Self {
            ctx,
            func,
            fb,
            locals: HashMap::new(),
            ty_lowerer,
            curr_block: entry,
        }
    }

    /// Lower the function AST into MCIR.
    pub fn lower(&mut self) -> Result<FuncBody, LowerError> {
        // Create locals for params.
        for (i, param) in self.func.params.iter().enumerate() {
            let ty = self.ty_for_node(param.id)?;
            let ty_id = self.ty_lowerer.lower_ty(&ty);
            let def_id = self.def_for_node(param.id)?.id;
            let local_id = self.fb.new_local(
                ty_id,
                LocalKind::Param { index: i as u32 },
                Some(param.name.clone()),
            );
            self.locals.insert(def_id, local_id);
        }

        // Lower the body into the return local (scalar vs aggregate).
        let ret_id = self.fb.body.ret_local;
        let ret_ty = self.fb.body.locals[ret_id.0 as usize].ty;

        let body_ty = self.ty_for_node(self.func.body.id)?;
        if body_ty.is_scalar() {
            let ret_place = Place::<Scalar>::new(ret_id, ret_ty, vec![]);
            let body_value = self.lower_expr_value(&self.func.body)?;
            if let ExprValue::Scalar(op) = body_value {
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyScalar {
                        dst: ret_place,
                        src: Rvalue::Use(op),
                    },
                );
            }
        } else {
            let ret_place = Place::<Aggregate>::new(ret_id, ret_ty, vec![]);
            self.lower_agg_value_into(ret_place, &self.func.body)?;
        }

        // Terminate the entry block
        self.fb.set_terminator(self.curr_block, Terminator::Return);

        self.fb.body.types = std::mem::take(&mut self.ty_lowerer.table);

        Ok(self.fb.body.clone())
    }

    /// Lower an expression in value position into a scalar operand or aggregate place.
    fn lower_expr_value(&mut self, expr: &Expr) -> Result<ExprValue, LowerError> {
        match &expr.kind {
            // These are only allowed as block items.
            EK::LetBind { .. } | EK::VarBind { .. } | EK::Assign { .. } | EK::While { .. } => {
                Err(LowerError::ExprNotAllowedInValueContext(expr.id))
            }

            EK::Block(exprs) => self.lower_block_expr(exprs),

            EK::If {
                cond,
                then_body,
                else_body,
            } => self.lower_if_expr(cond, then_body, else_body),

            EK::Call { callee, args } => self.lower_call_expr(expr, callee, args),

            // everything else: decide scalar vs aggregate by type
            _ => {
                let ty = self.ty_for_node(expr.id)?;
                if ty.is_scalar() {
                    Ok(ExprValue::Scalar(self.lower_scalar_expr(expr)?))
                } else {
                    // prefer place, fall back to aggregate literal
                    let place = self
                        .lower_place_agg(expr)
                        .or_else(|_| self.lower_agg_expr(expr))?;
                    Ok(ExprValue::Aggregate(place))
                }
            }
        }
    }

    // --- Expression (Block) ---

    /// Lower a block used as an expression.
    fn lower_block_expr(&mut self, exprs: &[Expr]) -> Result<ExprValue, LowerError> {
        // Evaluate all but the last expression for side effects.
        self.lower_block_prefix(exprs)?;

        match exprs.last() {
            None => {
                // Empty block: return unit
                let c = Const::Unit;
                Ok(ExprValue::Scalar(Operand::Const(c)))
            }
            Some(last_expr) => self.lower_expr_value(last_expr),
        }
    }

    /// Lower all but the last expression in a block (side effects only).
    fn lower_block_prefix(&mut self, exprs: &[Expr]) -> Result<(), LowerError> {
        // Block prefix: execute in order, discard results.
        for e in exprs.iter().take(exprs.len().saturating_sub(1)) {
            self.lower_block_item(e)?;
        }
        Ok(())
    }

    /// Lower a block item (stmt-like expression) for side effects.
    fn lower_block_item(&mut self, expr: &Expr) -> Result<(), LowerError> {
        match &expr.kind {
            // Block-item-only forms.
            EK::LetBind { pattern, value, .. } => self.lower_binding(pattern, value),
            EK::VarBind { pattern, value, .. } => self.lower_binding(pattern, value),
            EK::Assign { assignee, value } => self.lower_assign(assignee, value),
            EK::While { cond, body } => {
                // while in block‑item position: just lower and discard
                self.lower_while_expr(cond, body)?;
                Ok(())
            }
            EK::If {
                cond,
                then_body,
                else_body,
            } => {
                // if in block‑item position: just lower and discard
                let _ = self.lower_if_expr(cond, then_body, else_body)?;
                Ok(())
            }
            _ => {
                // Regular expression used for side effects only.
                let _ = self.lower_expr_value(expr)?;
                Ok(())
            }
        }
    }

    // --- Expression (Scalar) ---

    /// Lower an expression expected to produce a scalar operand.
    fn lower_scalar_expr(&mut self, expr: &Expr) -> Result<Operand, LowerError> {
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
            EK::Call { .. } | EK::If { .. } | EK::Block(..) => {
                match self.lower_expr_value(expr)? {
                    ExprValue::Scalar(op) => Ok(op),
                    ExprValue::Aggregate(_) => Err(LowerError::UnsupportedOperandExpr(expr.id)),
                }
            }

            _ => Err(LowerError::UnsupportedOperandExpr(expr.id)),
        }
    }

    /// Emit a scalar rvalue into a temp and return it as an operand.
    fn emit_scalar_rvalue(&mut self, ty_id: TyId, rvalue: Rvalue) -> Operand {
        // Materialize into a temp so later uses can be a place.
        let temp_local_id = self.fb.new_local(ty_id, LocalKind::Temp, None);
        let temp_place = Place::new(temp_local_id, ty_id, vec![]);

        // Emit the assignment
        self.fb.push_stmt(
            self.curr_block,
            Statement::CopyScalar {
                dst: temp_place.clone(),
                src: rvalue,
            },
        );

        // Return a copy of the temp place as an operand
        Operand::Copy(temp_place)
    }

    // --- Expression (Aggregate) ---

    /// Lower an aggregate expression into a fresh temp place.
    fn lower_agg_expr(&mut self, expr: &Expr) -> Result<Place<Aggregate>, LowerError> {
        let aggr_ty = self.ty_for_node(expr.id)?;
        let aggr_ty_id = self.ty_lowerer.lower_ty(&aggr_ty);
        if !self.is_aggregate(aggr_ty_id) {
            return Err(LowerError::ExprIsNotAggregate(expr.id));
        }

        // Create a temp to hold the result.
        let temp_local_id = self.fb.new_local(aggr_ty_id, LocalKind::Temp, None);
        let temp_place = Place::new(temp_local_id, aggr_ty_id, vec![]);

        // Lower the aggregate into the temp place
        self.lower_agg_value_into(temp_place.clone(), expr)?;
        Ok(temp_place)
    }

    /// Emit a write into a projected field/element of an aggregate.
    fn emit_agg_projection(
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
            self.fb.push_stmt(
                self.curr_block,
                Statement::CopyScalar {
                    dst: field_place,
                    src: Rvalue::Use(field_operand),
                },
            );
            Ok(())
        } else {
            let field_place = Place::new(dst.base(), field_ty_id, projs);
            self.lower_agg_value_into(field_place, field_expr)
        }
    }

    /// Lower an aggregate expression directly into a destination place.
    fn lower_agg_value_into(
        &mut self,
        dst: Place<Aggregate>,
        expr: &Expr,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            EK::ArrayLit(..)
            | EK::TupleLit(..)
            | EK::StructLit { .. }
            | EK::StructUpdate { .. }
            | EK::EnumVariant { .. } => {
                // Aggregate literal: build in place.
                self.lower_agg_into(dst, expr)
            }
            EK::Var(_) | EK::ArrayIndex { .. } | EK::TupleField { .. } | EK::StructField { .. } => {
                // Aggregate place: copy unless it's already the destination.
                let src = self.lower_place_agg(expr)?;
                if src.base() == dst.base() && src.projections() == dst.projections() {
                    return Ok(());
                }
                self.fb
                    .push_stmt(self.curr_block, Statement::CopyAggregate { dst, src });
                Ok(())
            }
            EK::Call { callee, args } => {
                // Aggregate call result: direct into destination.
                self.emit_call_into(PlaceAny::Aggregate(dst), callee, args)?;
                Ok(())
            }
            EK::Block(exprs) => self.lower_block_into(dst, exprs, expr.id),
            EK::If {
                cond,
                then_body,
                else_body,
            } => self.lower_if_expr_into(dst, cond, then_body, else_body),
            _ => match self.lower_expr_value(expr)? {
                ExprValue::Aggregate(place) => {
                    self.fb.push_stmt(
                        self.curr_block,
                        Statement::CopyAggregate { dst, src: place },
                    );
                    Ok(())
                }
                ExprValue::Scalar(_) => Err(LowerError::UnsupportedAggregateRhs(expr.id)),
            },
        }
    }

    /// Lower an aggregate literal directly into a destination place.
    fn lower_agg_into(&mut self, dst: Place<Aggregate>, expr: &Expr) -> Result<(), LowerError> {
        match &expr.kind {
            EK::TupleLit(fields) => {
                // Lower each tuple field into its slot.
                for (i, field_expr) in fields.iter().enumerate() {
                    let field_ty = self.ty_for_node(field_expr.id)?;
                    let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);

                    self.emit_agg_projection(
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

                    self.emit_agg_projection(
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
                    self.fb.push_stmt(
                        self.curr_block,
                        Statement::CopyAggregate {
                            dst: dst.clone(),
                            src: base_place,
                        },
                    );
                }

                // Overwrite updated fields
                let struct_ty = self.ty_for_node(expr.id)?;
                for field in fields {
                    let field_ty = struct_ty.struct_field_type(&field.name);
                    let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);
                    let field_index = struct_ty.struct_field_index(&field.name);

                    self.emit_agg_projection(
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

                    self.emit_agg_projection(&dst, elem_ty_id, elem_expr, index_proj)?;
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

                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyScalar {
                        dst: tag_place,
                        src: Rvalue::Use(Operand::Const(tag)),
                    },
                );

                // 2) payload fields (indices 1+)
                for (i, payload_expr) in payload.iter().enumerate() {
                    let field_ty = self.ty_for_node(payload_expr.id)?;
                    let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);
                    self.emit_agg_projection(
                        &dst,
                        field_ty_id,
                        payload_expr,
                        Projection::Field { index: i + 1 },
                    )?;
                }
                Ok(())
            }

            _ => Err(LowerError::ExprIsNotAggregate(expr.id)),
        }
    }

    // --- Place (Lvalue) ---

    /// Lower an lvalue expression into a place.
    fn lower_place(&mut self, expr: &Expr) -> Result<PlaceAny, LowerError> {
        match &expr.kind {
            EK::Var(_) => self.lower_var(expr),
            EK::ArrayIndex { target, indices } => self.lower_array_index(expr, target, indices),
            EK::TupleField { target, index } => self.lower_tuple_field(target, *index),
            EK::StructField { target, field } => self.lower_struct_field(target, field),
            _ => Err(LowerError::ExprIsNotPlace(expr.id)),
        }
    }

    /// Lower an lvalue expression expected to be scalar.
    fn lower_place_scalar(&mut self, expr: &Expr) -> Result<Place<Scalar>, LowerError> {
        match self.lower_place(expr)? {
            PlaceAny::Scalar(p) => Ok(p),
            PlaceAny::Aggregate(_) => Err(LowerError::PlaceKindMismatch {
                node_id: expr.id,
                expected: PlaceKind::Scalar,
            }),
        }
    }

    /// Lower an lvalue expression expected to be aggregate.
    fn lower_place_agg(&mut self, expr: &Expr) -> Result<Place<Aggregate>, LowerError> {
        match self.lower_place(expr)? {
            PlaceAny::Scalar(_) => Err(LowerError::PlaceKindMismatch {
                node_id: expr.id,
                expected: PlaceKind::Aggregate,
            }),
            PlaceAny::Aggregate(p) => Ok(p),
        }
    }

    /// Resolve a variable reference into its local place.
    fn lower_var(&mut self, expr: &Expr) -> Result<PlaceAny, LowerError> {
        let def = self.def_for_node(expr.id)?;
        let local_id = *self
            .locals
            .get(&def.id)
            .ok_or(LowerError::VarLocalNotFound(expr.id, def.id))?;

        let local_ty = self.fb.body.locals[local_id.0 as usize].ty;

        let place = self.place_from_ty(local_id, local_ty, vec![]);

        Ok(place)
    }

    /// Lower array indexing into a projected place.
    fn lower_array_index(
        &mut self,
        expr: &Expr,
        target: &Expr,
        indices: &[Expr],
    ) -> Result<PlaceAny, LowerError> {
        let target_place = self.lower_place_agg(target)?;

        // Lower each index expression to an operand.
        let mut index_operands = Vec::new();
        for idx in indices {
            let idx_op = self.lower_scalar_expr(idx)?;
            index_operands.push(idx_op);
        }

        // Get the element type at the given indices
        let result_ty = self.ty_for_node(expr.id)?;
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        let mut projs = target_place.projections().to_vec();
        projs.extend(
            index_operands
                .into_iter()
                .map(|index_place| Projection::Index { index: index_place }),
        );

        let place = self.place_from_ty(target_place.base(), result_ty_id, projs);
        Ok(place)
    }

    /// Lower tuple field access into a projected place.
    fn lower_tuple_field(&mut self, target: &Expr, index: usize) -> Result<PlaceAny, LowerError> {
        let target_place = self.lower_place_agg(target)?;
        let target_ty = self.ty_for_node(target.id)?;

        let field_ty = target_ty.tuple_field_type(index);
        let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);

        let mut projs = target_place.projections().to_vec();
        projs.push(Projection::Field { index });

        let place = self.place_from_ty(target_place.base(), field_ty_id, projs);

        Ok(place)
    }

    /// Lower struct field access into a projected place.
    fn lower_struct_field(
        &mut self,
        target: &Expr,
        field_name: &str,
    ) -> Result<PlaceAny, LowerError> {
        let target_place = self.lower_place_agg(target)?;
        let target_ty = self.ty_for_node(target.id)?;

        let field_ty = target_ty.struct_field_type(field_name);
        let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);
        // Map field name to index
        let field_index = target_ty.struct_field_index(field_name);

        let mut projs = target_place.projections().to_vec();
        projs.push(Projection::Field { index: field_index });

        let place = self.place_from_ty(target_place.base(), field_ty_id, projs);

        Ok(place)
    }

    /// Construct a typed place from a base local and projections.
    fn place_from_ty(&self, base: LocalId, ty: TyId, proj: Vec<Projection>) -> PlaceAny {
        if self.is_scalar(ty) {
            PlaceAny::Scalar(Place::new(base, ty, proj))
        } else {
            PlaceAny::Aggregate(Place::new(base, ty, proj))
        }
    }

    // --- Bindings / Patterns ---

    /// Lower a let/var binding.
    fn lower_binding(&mut self, pattern: &Pattern, value: &Expr) -> Result<(), LowerError> {
        let value_ty = self.ty_for_node(value.id)?;

        if value_ty.is_scalar() {
            // Scalar binding: compute operand and assign.
            let PK::Ident { name } = &pattern.kind else {
                return Err(LowerError::PatternMismatch(pattern.id));
            };
            let op = self.lower_scalar_expr(value)?;
            let ty_id = self.ty_lowerer.lower_ty(&value_ty);
            let def_id = self.def_for_node(pattern.id)?.id;
            self.bind_ident_operand(def_id, name.clone(), ty_id, op)?;
        } else {
            if let PK::Ident { name } = &pattern.kind {
                // Aggregate ident binding: prefer NRVO when eligible.
                let (def_id, nrvo_eligible) = {
                    let def = self.def_for_node(pattern.id)?;
                    let eligible = matches!(
                        def.kind,
                        DefKind::LocalVar {
                            nrvo_eligible: true
                        }
                    );
                    (def.id, eligible)
                };
                if nrvo_eligible {
                    let ret_id = self.fb.body.ret_local;
                    let ret_ty = self.fb.body.locals[ret_id.0 as usize].ty;
                    self.locals.insert(def_id, ret_id);
                    let dst = Place::new(ret_id, ret_ty, vec![]);
                    self.lower_agg_value_into(dst, value)?;
                    return Ok(());
                }
                let ty_id = self.ty_lowerer.lower_ty(&value_ty);
                let local_id = self.ensure_local_for_def(def_id, ty_id, Some(name.clone()));
                let dst = Place::new(local_id, ty_id, vec![]);
                self.lower_agg_value_into(dst, value)?;
                return Ok(());
            }
            // Aggregate destructuring via patterns.
            let src_place = match self.lower_expr_value(value)? {
                ExprValue::Aggregate(place) => PlaceAny::Aggregate(place),
                ExprValue::Scalar(_) => {
                    return Err(LowerError::PatternMismatch(pattern.id));
                }
            };
            self.bind_pattern_with_type(pattern, src_place, &value_ty)?;
        }

        Ok(())
    }

    /// Bind a pattern to a value place with a known AST type.
    fn bind_pattern_with_type(
        &mut self,
        pattern: &Pattern,
        src_place: PlaceAny,
        src_ty: &Type,
    ) -> Result<(), LowerError> {
        match &pattern.kind {
            PK::Ident { name } => {
                // Bind a single identifier to a place.
                let src_ty_id = self.ty_lowerer.lower_ty(src_ty);
                let def_id = self.def_for_node(pattern.id)?.id;
                self.bind_ident(def_id, name.clone(), src_ty_id, src_place)
            }

            PK::Tuple { patterns } => {
                // Destructure tuple by projecting each field.
                let src_place = match src_place {
                    PlaceAny::Aggregate(place) => place,
                    _ => return Err(LowerError::PatternMismatch(pattern.id)),
                };

                let Type::Tuple { fields } = src_ty else {
                    unreachable!("compiler bug: non-tuple pattern");
                };
                debug_assert_eq!(patterns.len(), fields.len(), "pattern arity mismatch");

                for (i, pat) in patterns.iter().enumerate() {
                    let field_ty = &fields[i];
                    let field_ty_id = self.ty_lowerer.lower_ty(field_ty);

                    let field_place =
                        self.project_place(&src_place, Projection::Field { index: i }, field_ty_id);

                    self.bind_pattern_with_type(pat, field_place, field_ty)?;
                }

                Ok(())
            }

            PK::Array { patterns } => {
                // Destructure array by index.
                let src_place = match src_place {
                    PlaceAny::Aggregate(place) => place,
                    _ => return Err(LowerError::PatternMismatch(pattern.id)),
                };

                let Type::Array { elem_ty, dims } = src_ty else {
                    unreachable!("compiler bug: non-array pattern");
                };

                let elem_ty = if dims.len() == 1 {
                    (**elem_ty).clone()
                } else {
                    Type::Array {
                        elem_ty: elem_ty.clone(),
                        dims: dims[1..].to_vec(),
                    }
                };

                for (i, pat) in patterns.iter().enumerate() {
                    let elem_ty_id = self.ty_lowerer.lower_ty(&elem_ty);

                    let index_proj = Projection::Index {
                        index: Operand::Const(Const::Int {
                            signed: false,
                            bits: 64,
                            value: i as i128,
                        }),
                    };
                    let elem_place = self.project_place(&src_place, index_proj, elem_ty_id);

                    self.bind_pattern_with_type(pat, elem_place, &elem_ty)?;
                }

                Ok(())
            }

            PK::Struct { fields, .. } => {
                // Destructure struct by projecting each field.
                let src_place = match src_place {
                    PlaceAny::Aggregate(place) => place,
                    _ => return Err(LowerError::PatternMismatch(pattern.id)),
                };

                let Type::Struct { .. } = src_ty else {
                    unreachable!("compiler bug: non-struct pattern");
                };

                for field in fields {
                    let field_ty = src_ty.struct_field_type(&field.name);
                    let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);
                    let field_index = src_ty.struct_field_index(&field.name);

                    let field_place = self.project_place(
                        &src_place,
                        Projection::Field { index: field_index },
                        field_ty_id,
                    );

                    self.bind_pattern_with_type(&field.pattern, field_place, &field_ty)?;
                }

                Ok(())
            }
        }
    }

    /// Create or reuse a local for a definition id.
    fn ensure_local_for_def(
        &mut self,
        def_id: DefId,
        ty_id: TyId,
        name: Option<String>,
    ) -> LocalId {
        if let Some(id) = self.locals.get(&def_id) {
            return *id;
        }
        let id = self.fb.new_local(ty_id, LocalKind::User, name);
        self.locals.insert(def_id, id);
        id
    }

    /// Bind an identifier to a place, emitting the appropriate assignment.
    fn bind_ident(
        &mut self,
        def_id: DefId,
        name: String,
        ty_id: TyId,
        src_place: PlaceAny,
    ) -> Result<(), LowerError> {
        let local_id = self.ensure_local_for_def(def_id, ty_id, Some(name));
        match src_place {
            PlaceAny::Scalar(place) => {
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyScalar {
                        dst: Place::new(local_id, ty_id, vec![]),
                        src: Rvalue::Use(Operand::Copy(place)),
                    },
                );
            }
            PlaceAny::Aggregate(place) => {
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyAggregate {
                        dst: Place::new(local_id, ty_id, vec![]),
                        src: place,
                    },
                );
            }
        }
        Ok(())
    }

    /// Bind an identifier to a scalar operand.
    fn bind_ident_operand(
        &mut self,
        def_id: DefId,
        name: String,
        ty_id: TyId,
        op: Operand,
    ) -> Result<(), LowerError> {
        let local_id = self.ensure_local_for_def(def_id, ty_id, Some(name));
        self.fb.push_stmt(
            self.curr_block,
            Statement::CopyScalar {
                dst: Place::new(local_id, ty_id, vec![]),
                src: Rvalue::Use(op),
            },
        );
        Ok(())
    }

    /// Create a projected place from an aggregate base.
    fn project_place(&self, src: &Place<Aggregate>, proj: Projection, ty_id: TyId) -> PlaceAny {
        let mut projs = src.projections().to_vec();
        projs.push(proj);
        self.place_from_ty(src.base(), ty_id, projs)
    }

    // --- Assignment ---

    /// Lower an assignment expression.
    fn lower_assign(&mut self, assignee: &Expr, value: &Expr) -> Result<(), LowerError> {
        let value_ty = self.ty_for_node(value.id)?;

        if value_ty.is_scalar() {
            // Scalar assignment.
            let assignee_place = self.lower_place_scalar(assignee)?;
            let value_operand = self.lower_scalar_expr(value)?;

            self.fb.push_stmt(
                self.curr_block,
                Statement::CopyScalar {
                    dst: assignee_place,
                    src: Rvalue::Use(value_operand),
                },
            );
        } else {
            // Aggregate assignment (value written directly into place).
            let assignee_place = self.lower_place_agg(assignee)?;
            self.lower_agg_value_into(assignee_place, value)?;
        }

        Ok(())
    }

    // --- Control Flow ---

    /// Lower an if expression in value position.
    fn lower_if_expr(
        &mut self,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<ExprValue, LowerError> {
        let cond_op = self.lower_scalar_expr(cond)?;

        // Split control flow into then/else with a join block.
        let then_bb = self.fb.new_block();
        let else_bb = self.fb.new_block();
        let join_bb = self.fb.new_block();

        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond: cond_op,
                then_bb,
                else_bb,
            },
        );

        let result_ty = self.ty_for_node(then_body.id)?;
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty.is_scalar() {
            // Scalar if-expr: write both branches into the same temp.
            let temp_id = self.fb.new_local(result_ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, result_ty_id, vec![]);

            // then
            self.curr_block = then_bb;
            if let ExprValue::Scalar(op) = self.lower_expr_value(then_body)? {
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyScalar {
                        dst: temp_place.clone(),
                        src: Rvalue::Use(op),
                    },
                );
            }
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));

            // else
            self.curr_block = else_bb;
            if let ExprValue::Scalar(op) = self.lower_expr_value(else_body)? {
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyScalar {
                        dst: temp_place.clone(),
                        src: Rvalue::Use(op),
                    },
                );
            }
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));

            self.curr_block = join_bb;
            Ok(ExprValue::Scalar(Operand::Copy(temp_place)))
        } else {
            // Aggregate if-expr: lower both branches into a temp place.
            let temp_id = self.fb.new_local(result_ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, result_ty_id, vec![]);

            // then
            self.curr_block = then_bb;
            self.lower_agg_value_into(temp_place.clone(), then_body)?;
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));

            // else
            self.curr_block = else_bb;
            self.lower_agg_value_into(temp_place.clone(), else_body)?;
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));

            self.curr_block = join_bb;
            Ok(ExprValue::Aggregate(temp_place))
        }
    }

    /// Lower a while expression (returns unit).
    fn lower_while_expr(&mut self, cond: &Expr, body: &Expr) -> Result<ExprValue, LowerError> {
        let loop_cond_bb = self.fb.new_block();
        let loop_body_bb = self.fb.new_block();
        let loop_exit_bb = self.fb.new_block();

        // Jump to loop condition
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(loop_cond_bb));

        // Loop condition
        self.curr_block = loop_cond_bb;
        let cond_op = self.lower_scalar_expr(cond)?;
        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond: cond_op,
                then_bb: loop_body_bb,
                else_bb: loop_exit_bb,
            },
        );

        // Loop body
        self.curr_block = loop_body_bb;
        let _ = self.lower_expr_value(body)?;
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(loop_cond_bb));

        // After loop
        self.curr_block = loop_exit_bb;

        // while loops return unit
        let c = Const::Unit;
        Ok(ExprValue::Scalar(Operand::Const(c)))
    }

    /// Lower a block and write its final aggregate value into dst.
    fn lower_block_into(
        &mut self,
        dst: Place<Aggregate>,
        exprs: &[Expr],
        block_id: NodeId,
    ) -> Result<(), LowerError> {
        // Evaluate prefix, then write the tail into dst.
        self.lower_block_prefix(exprs)?;

        match exprs.last() {
            Some(last_expr) => self.lower_agg_value_into(dst, last_expr),
            None => Err(LowerError::UnsupportedAggregateRhs(block_id)),
        }
    }

    /// Lower an if-expression into a destination aggregate place.
    fn lower_if_expr_into(
        &mut self,
        dst: Place<Aggregate>,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<(), LowerError> {
        let cond_op = self.lower_scalar_expr(cond)?;

        // Lower both branches into dst, then join.
        let then_bb = self.fb.new_block();
        let else_bb = self.fb.new_block();
        let join_bb = self.fb.new_block();

        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond: cond_op,
                then_bb,
                else_bb,
            },
        );

        self.curr_block = then_bb;
        self.lower_agg_value_into(dst.clone(), then_body)?;
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(join_bb));

        self.curr_block = else_bb;
        self.lower_agg_value_into(dst.clone(), else_body)?;
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(join_bb));

        self.curr_block = join_bb;
        Ok(())
    }

    // --- Call ---

    /// Lower a call expression and return the produced value.
    fn lower_call_expr(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<ExprValue, LowerError> {
        let result_ty = self.ty_for_node(call.id)?;
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty.is_scalar() {
            // Scalar call: capture result into a temp.
            let temp_id = self.fb.new_local(result_ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, result_ty_id, vec![]);
            self.emit_call_into(PlaceAny::Scalar(temp_place.clone()), callee, args)?;
            Ok(ExprValue::Scalar(Operand::Copy(temp_place)))
        } else {
            // Aggregate call: capture result into a temp place.
            let temp_id = self.fb.new_local(result_ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, result_ty_id, vec![]);
            self.emit_call_into(PlaceAny::Aggregate(temp_place.clone()), callee, args)?;
            Ok(ExprValue::Aggregate(temp_place))
        }
    }

    /// Emit a call statement that writes into the given destination place.
    fn emit_call_into(
        &mut self,
        dst: PlaceAny,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<(), LowerError> {
        let callee_def = self.def_for_node(callee.id)?;
        let callee_id = callee_def.id;

        let arg_vals = args
            .iter()
            .map(|a| self.lower_call_arg(a))
            .collect::<Result<Vec<_>, _>>()?;

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst,
                callee: Callee::Def(callee_id),
                args: arg_vals,
            },
        );
        Ok(())
    }

    /// Lower a call argument into a place (or temp if needed).
    fn lower_call_arg(&mut self, arg: &Expr) -> Result<PlaceAny, LowerError> {
        let ty = self.ty_for_node(arg.id)?;
        let ty_id = self.ty_lowerer.lower_ty(&ty);

        if ty.is_scalar() {
            // Scalar arg: prefer a place, otherwise spill to temp.
            if let Ok(place) = self.lower_place_scalar(arg) {
                return Ok(PlaceAny::Scalar(place));
            }

            // Otherwise, evaluate to operand and spill into a temp.
            let op = self.lower_scalar_expr(arg)?;
            let temp_id = self.fb.new_local(ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, ty_id, vec![]);
            self.fb.push_stmt(
                self.curr_block,
                Statement::CopyScalar {
                    dst: temp_place.clone(),
                    src: Rvalue::Use(op),
                },
            );
            Ok(PlaceAny::Scalar(temp_place))
        } else {
            // Aggregate arg: prefer a place, otherwise lower into a temp.
            if let Ok(place) = self.lower_place_agg(arg) {
                Ok(PlaceAny::Aggregate(place))
            } else {
                Ok(PlaceAny::Aggregate(self.lower_agg_expr(arg)?))
            }
        }
    }

    // --- Operator Mapping ---

    /// Map AST binary op to MCIR binary op.
    fn map_binop(op: ast::BinaryOp) -> BinOp {
        match op {
            ast::BinaryOp::Add => BinOp::Add,
            ast::BinaryOp::Sub => BinOp::Sub,
            ast::BinaryOp::Mul => BinOp::Mul,
            ast::BinaryOp::Div => BinOp::Div,
            ast::BinaryOp::Eq => BinOp::Eq,
            ast::BinaryOp::Ne => BinOp::Ne,
            ast::BinaryOp::Lt => BinOp::Lt,
            ast::BinaryOp::LtEq => BinOp::LtEq,
            ast::BinaryOp::Gt => BinOp::Gt,
            ast::BinaryOp::GtEq => BinOp::GtEq,
        }
    }

    /// Map AST unary op to MCIR unary op.
    fn map_unop(op: ast::UnaryOp) -> UnOp {
        match op {
            ast::UnaryOp::Neg => UnOp::Neg,
        }
    }

    // --- Helpers ---

    /// Lookup a definition for an AST node id.
    fn def_for_node(&self, node_id: NodeId) -> Result<&Def, LowerError> {
        self.ctx
            .def_map
            .lookup_def(node_id)
            .ok_or(LowerError::ExprDefNotFound(node_id))
    }

    /// Lookup the AST type for a node id.
    fn ty_for_node(&self, node_id: NodeId) -> Result<Type, LowerError> {
        self.ctx
            .type_map
            .lookup_node_type(node_id)
            .ok_or(LowerError::ExprTypeNotFound(node_id))
    }

    /// Check whether a lowered type is scalar.
    fn is_scalar(&self, ty_id: TyId) -> bool {
        self.ty_lowerer.table.get(ty_id).is_scalar()
    }

    /// Check whether a lowered type is aggregate.
    fn is_aggregate(&self, ty_id: TyId) -> bool {
        self.ty_lowerer.table.get(ty_id).is_aggregate()
    }
}

/// Lower all functions in a module into MCIR bodies.
pub fn lower_ast(ctx: AnalyzedContext) -> Result<LoweredMcirContext, LowerError> {
    let mut bodies = Vec::new();
    for func in ctx.module.funcs() {
        let body = FuncLowerer::new(&ctx, func).lower()?;
        bodies.push(body);
    }
    Ok(ctx.with_func_bodies(bodies))
}

#[cfg(test)]
#[path = "../tests/t_lower_ast.rs"]
mod tests;
