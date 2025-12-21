use std::collections::HashMap;

use thiserror::Error;

use crate::analysis::Def;
use crate::ast::{self, ExprKind as EK, PatternKind as PK, *};
use crate::context::AnalyzedContext;
use crate::ids::{DefId, NodeId};
use crate::mcir::func_builder::FuncBuilder;
use crate::mcir::lower_ty::TyLowerer;
use crate::mcir::types::*;
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

    #[error("place is not scalar for node {0:?}")]
    PlaceIsNotScalar(NodeId),
    #[error("place is not aggregate for node {0:?}")]
    PlaceIsNotAggregate(NodeId),

    #[error("variable local not found for node {0:?} (def {1:?})")]
    VarLocalNotFound(NodeId, DefId),

    #[error("unsupported operand expression for node {0:?}")]
    UnsupportedOperandExpr(NodeId),

    #[error("pattern value type mismatch for node {0:?}")]
    PatternValueTypeMismatch(NodeId),
    #[error("unsupported aggregate rhs for node {0:?}")]
    UnsupportedAggregateRhs(NodeId),

    #[error("expression not allowed in value context for node {0:?}")]
    ExprNotAllowedInValueContext(NodeId),
}

enum ExprValue {
    Scalar(Operand),
    Aggregate(Place<Aggregate>),
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

    pub fn lower(&mut self) -> Result<Body, LowerError> {
        // Create locals for params
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

        // Lower body expression
        let body_value = self.lower_expr_value(&self.func.body)?;

        // Store the result in return local
        let ret_id = self.fb.body.ret_local;
        let ret_ty = self.fb.body.locals[ret_id.0 as usize].ty;
        let ret_place = Place::new(ret_id, ret_ty, vec![]);

        // Fast path: lower aggregate literal directly into return place
        if matches!(
            &self.func.body.kind,
            EK::ArrayLit(..) | EK::TupleLit(..) | EK::StructLit { .. }
        ) {
            self.lower_agg_into(ret_place, &self.func.body)?;
            self.fb.set_terminator(self.curr_block, Terminator::Return);
            return Ok(self.fb.body.clone());
        }

        // Fallback: lower normally
        match body_value {
            ExprValue::Scalar(op) => {
                let ret_place = Place::new(ret_id, ret_ty, vec![]);
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::AssignScalar {
                        dst: ret_place,
                        src: Rvalue::Use(op),
                    },
                );
            }
            ExprValue::Aggregate(place) => {
                let ret_place = Place::new(ret_id, ret_ty, vec![]);
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyAggregate {
                        dst: ret_place,
                        src: place,
                    },
                );
            }
        }

        // Terminate the entry block
        self.fb.set_terminator(self.curr_block, Terminator::Return);

        self.fb.body.types = std::mem::take(&mut self.ty_lowerer.table);

        Ok(self.fb.body.clone())
    }

    fn lower_expr_value(&mut self, expr: &Expr) -> Result<ExprValue, LowerError> {
        match &expr.kind {
            EK::LetBind { .. } | EK::VarBind { .. } | EK::Assign { .. } | EK::While { .. } => {
                return Err(LowerError::ExprNotAllowedInValueContext(expr.id));
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

    fn lower_block_expr(&mut self, exprs: &[Expr]) -> Result<ExprValue, LowerError> {
        for e in exprs.iter().take(exprs.len().saturating_sub(1)) {
            self.lower_block_item(e)?;
        }

        match exprs.last() {
            None => {
                // Empty block: return unit
                let c = Const::Unit;
                Ok(ExprValue::Scalar(Operand::Const(c)))
            }
            Some(last_expr) => self.lower_expr_value(last_expr),
        }
    }

    fn lower_block_item(&mut self, expr: &Expr) -> Result<(), LowerError> {
        match &expr.kind {
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
                // regular expression used for side effects only
                let _ = self.lower_expr_value(expr)?;
                Ok(())
            }
        }
    }

    // --- Expression (Scalar) ---

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

    fn emit_scalar_rvalue(&mut self, ty_id: TyId, rvalue: Rvalue) -> Operand {
        // Create a temp to hold the result
        let temp_local_id = self.fb.new_local(ty_id, LocalKind::Temp, None);
        let temp_place = Place::new(temp_local_id, ty_id, vec![]);

        // Emit the assignment
        self.fb.push_stmt(
            self.curr_block,
            Statement::AssignScalar {
                dst: temp_place.clone(),
                src: rvalue,
            },
        );

        // Return a copy of the temp place as an operand
        Operand::Copy(temp_place)
    }

    // --- Expression (Aggregate) ---

    fn lower_agg_expr(&mut self, expr: &Expr) -> Result<Place<Aggregate>, LowerError> {
        let aggr_ty = self.ty_for_node(expr.id)?;
        let aggr_ty_id = self.ty_lowerer.lower_ty(&aggr_ty);
        if !self.is_aggregate(aggr_ty_id) {
            return Err(LowerError::ExprIsNotAggregate(expr.id));
        }

        // Create a temp to hold the result
        let temp_local_id = self.fb.new_local(aggr_ty_id, LocalKind::Temp, None);
        let temp_place = Place::new(temp_local_id, aggr_ty_id, vec![]);

        // Lower the aggregate into the temp place
        self.lower_agg_into(temp_place.clone(), expr)?;
        Ok(temp_place)
    }

    fn lower_agg_into(&mut self, dst: Place<Aggregate>, expr: &Expr) -> Result<(), LowerError> {
        match &expr.kind {
            EK::TupleLit(fields) => {
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

            EK::ArrayLit(elem_exprs) => {
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

            _ => Err(LowerError::ExprIsNotAggregate(expr.id)),
        }
    }

    fn emit_agg_projection(
        &mut self,
        dst: &Place<Aggregate>,
        field_ty_id: TyId,
        field_expr: &Expr,
        extra_proj: Projection,
    ) -> Result<(), LowerError> {
        let mut projs = dst.projections().to_vec();
        projs.push(extra_proj);

        if self.is_scalar(field_ty_id) {
            let field_place = Place::new(dst.base(), field_ty_id, projs);
            let field_operand = self.lower_scalar_expr(field_expr)?;
            self.fb.push_stmt(
                self.curr_block,
                Statement::AssignScalar {
                    dst: field_place,
                    src: Rvalue::Use(field_operand),
                },
            );
            Ok(())
        } else {
            let field_place = Place::new(dst.base(), field_ty_id, projs);
            match &field_expr.kind {
                EK::ArrayLit(_) | EK::TupleLit(_) | EK::StructLit { .. } => {
                    self.lower_agg_into(field_place, field_expr)
                }
                _ => {
                    let src = self.lower_place_agg(field_expr)?;
                    self.fb.push_stmt(
                        self.curr_block,
                        Statement::CopyAggregate {
                            dst: field_place,
                            src,
                        },
                    );
                    Ok(())
                }
            }
        }
    }

    // --- Place (Lvalue) ---

    fn lower_place(&mut self, expr: &Expr) -> Result<PlaceAny, LowerError> {
        match &expr.kind {
            EK::Var(_) => self.lower_var(expr),
            EK::ArrayIndex { target, indices } => self.lower_array_index(expr, target, indices),
            EK::TupleField { target, index } => self.lower_tuple_field(target, *index),
            EK::StructField { target, field } => self.lower_struct_field(target, field),
            _ => Err(LowerError::ExprIsNotPlace(expr.id)),
        }
    }

    fn lower_place_scalar(&mut self, expr: &Expr) -> Result<Place<Scalar>, LowerError> {
        match self.lower_place(expr)? {
            PlaceAny::Scalar(p) => Ok(p),
            PlaceAny::Aggregate(_) => Err(LowerError::PlaceIsNotScalar(expr.id)),
        }
    }

    fn lower_place_agg(&mut self, expr: &Expr) -> Result<Place<Aggregate>, LowerError> {
        match self.lower_place(expr)? {
            PlaceAny::Scalar(_) => Err(LowerError::PlaceIsNotAggregate(expr.id)),
            PlaceAny::Aggregate(p) => Ok(p),
        }
    }

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

    fn lower_array_index(
        &mut self,
        expr: &Expr,
        target: &Expr,
        indices: &[Expr],
    ) -> Result<PlaceAny, LowerError> {
        let target_place = self.lower_place_agg(target)?;

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

    fn place_from_ty(&self, base: LocalId, ty: TyId, proj: Vec<Projection>) -> PlaceAny {
        if self.is_scalar(ty) {
            PlaceAny::Scalar(Place::new(base, ty, proj))
        } else {
            PlaceAny::Aggregate(Place::new(base, ty, proj))
        }
    }

    // --- Bindings / Patterns ---

    fn lower_binding(&mut self, pattern: &Pattern, value: &Expr) -> Result<(), LowerError> {
        let value_ty = self.ty_for_node(value.id)?;

        if value_ty.is_scalar() {
            let PK::Ident { name } = &pattern.kind else {
                return Err(LowerError::PatternValueTypeMismatch(pattern.id));
            };
            let op = self.lower_scalar_expr(value)?;
            let ty_id = self.ty_lowerer.lower_ty(&value_ty);
            let def_id = self.def_for_node(pattern.id)?.id;
            let local_id = self.ensure_local_for_def(def_id, ty_id, Some(name.clone()));
            self.fb.push_stmt(
                self.curr_block,
                Statement::AssignScalar {
                    dst: Place::new(local_id, ty_id, vec![]),
                    src: Rvalue::Use(op),
                },
            );
        } else {
            let src_place = match self.lower_expr_value(value)? {
                ExprValue::Aggregate(place) => PlaceAny::Aggregate(place),
                ExprValue::Scalar(_) => {
                    return Err(LowerError::PatternValueTypeMismatch(pattern.id));
                }
            };
            self.bind_pattern_with_type(pattern, src_place, &value_ty)?;
        }

        Ok(())
    }

    fn bind_pattern_with_type(
        &mut self,
        pattern: &Pattern,
        src_place: PlaceAny,
        src_ty: &Type,
    ) -> Result<(), LowerError> {
        match &pattern.kind {
            PK::Ident { name } => {
                // Create a local for the binding
                let src_ty_id = self.ty_lowerer.lower_ty(src_ty);
                let def_id = self.def_for_node(pattern.id)?.id;
                let local_id = self.ensure_local_for_def(def_id, src_ty_id, Some(name.clone()));

                // Assign the value to the local
                match src_place {
                    PlaceAny::Scalar(place) => {
                        self.fb.push_stmt(
                            self.curr_block,
                            Statement::AssignScalar {
                                dst: Place::new(local_id, src_ty_id, vec![]),
                                src: Rvalue::Use(Operand::Copy(place)),
                            },
                        );
                    }
                    PlaceAny::Aggregate(place) => {
                        self.fb.push_stmt(
                            self.curr_block,
                            Statement::CopyAggregate {
                                dst: Place::new(local_id, src_ty_id, vec![]),
                                src: place,
                            },
                        );
                    }
                }
                Ok(())
            }

            PK::Tuple { patterns } => {
                let src_place = match src_place {
                    PlaceAny::Aggregate(place) => place,
                    _ => return Err(LowerError::PatternValueTypeMismatch(pattern.id)),
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
                let src_place = match src_place {
                    PlaceAny::Aggregate(place) => place,
                    _ => return Err(LowerError::PatternValueTypeMismatch(pattern.id)),
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
        }
    }

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

    fn project_place(&self, src: &Place<Aggregate>, proj: Projection, ty_id: TyId) -> PlaceAny {
        let mut projs = src.projections().to_vec();
        projs.push(proj);
        self.place_from_ty(src.base(), ty_id, projs)
    }

    // --- Assignent ---

    fn lower_assign(&mut self, assignee: &Expr, value: &Expr) -> Result<(), LowerError> {
        let value_ty = self.ty_for_node(value.id)?;

        if value_ty.is_scalar() {
            let assignee_place = self.lower_place_scalar(assignee)?;
            let value_operand = self.lower_scalar_expr(value)?;

            self.fb.push_stmt(
                self.curr_block,
                Statement::AssignScalar {
                    dst: assignee_place,
                    src: Rvalue::Use(value_operand),
                },
            );
        } else {
            let assignee_place = self.lower_place_agg(assignee)?;

            if let Ok(value_place) = self.lower_place_agg(value) {
                // value is a place, do a copy
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyAggregate {
                        dst: assignee_place,
                        src: value_place,
                    },
                );
            } else {
                // If it's an aggregate literal, build a temp and fill it
                match &value.kind {
                    EK::ArrayLit(..) | EK::TupleLit(..) | EK::StructLit { .. } => {
                        self.lower_agg_into(assignee_place, value)?;
                    }
                    _ => {
                        match self.lower_expr_value(value)? {
                            ExprValue::Aggregate(place) => {
                                self.fb.push_stmt(
                                    self.curr_block,
                                    Statement::CopyAggregate {
                                        dst: assignee_place,
                                        src: place,
                                    },
                                );
                            }
                            ExprValue::Scalar(_) => {
                                return Err(LowerError::UnsupportedAggregateRhs(value.id));
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    // --- Control Flow ---

    fn lower_if_expr(
        &mut self,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<ExprValue, LowerError> {
        let cond_op = self.lower_scalar_expr(cond)?;

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
            let temp_id = self.fb.new_local(result_ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, result_ty_id, vec![]);

            // then
            self.curr_block = then_bb;
            if let ExprValue::Scalar(op) = self.lower_expr_value(then_body)? {
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::AssignScalar {
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
                    Statement::AssignScalar {
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
            let temp_id = self.fb.new_local(result_ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, result_ty_id, vec![]);

            // then
            self.curr_block = then_bb;
            if let ExprValue::Aggregate(place) = self.lower_expr_value(then_body)? {
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyAggregate {
                        dst: temp_place.clone(),
                        src: place,
                    },
                );
            }
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));

            // else
            self.curr_block = else_bb;
            if let ExprValue::Aggregate(place) = self.lower_expr_value(else_body)? {
                self.fb.push_stmt(
                    self.curr_block,
                    Statement::CopyAggregate {
                        dst: temp_place.clone(),
                        src: place,
                    },
                );
            }
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));

            self.curr_block = join_bb;
            Ok(ExprValue::Aggregate(temp_place))
        }
    }

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

    // --- Call ---

    fn lower_call_expr(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<ExprValue, LowerError> {
        let callee_def = self.def_for_node(callee.id)?;
        let callee_id = callee_def.id;

        let arg_vals = args
            .iter()
            .map(|a| self.lower_call_arg(a))
            .collect::<Result<Vec<_>, _>>()?;

        let result_ty = self.ty_for_node(call.id)?;
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty.is_scalar() {
            let temp_id = self.fb.new_local(result_ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, result_ty_id, vec![]);

            self.fb.push_stmt(
                self.curr_block,
                Statement::Call {
                    dst: PlaceAny::Scalar(temp_place.clone()),
                    callee: Callee::Def(callee_id),
                    args: arg_vals,
                },
            );

            Ok(ExprValue::Scalar(Operand::Copy(temp_place)))
        } else {
            let temp_id = self.fb.new_local(result_ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, result_ty_id, vec![]);

            self.fb.push_stmt(
                self.curr_block,
                Statement::Call {
                    dst: PlaceAny::Aggregate(temp_place.clone()),
                    callee: Callee::Def(callee_id),
                    args: arg_vals,
                },
            );

            Ok(ExprValue::Aggregate(temp_place))
        }
    }

    fn lower_call_arg(&mut self, arg: &Expr) -> Result<PlaceAny, LowerError> {
        let ty = self.ty_for_node(arg.id)?;
        let ty_id = self.ty_lowerer.lower_ty(&ty);

        if ty.is_scalar() {
            // If it's already a place, use it.
            if let Ok(place) = self.lower_place_scalar(arg) {
                return Ok(PlaceAny::Scalar(place));
            }

            // Otherwise, evaluate to operand and spill into a temp.
            let op = self.lower_scalar_expr(arg)?;
            let temp_id = self.fb.new_local(ty_id, LocalKind::Temp, None);
            let temp_place = Place::new(temp_id, ty_id, vec![]);
            self.fb.push_stmt(
                self.curr_block,
                Statement::AssignScalar {
                    dst: temp_place.clone(),
                    src: Rvalue::Use(op),
                },
            );
            Ok(PlaceAny::Scalar(temp_place))
        } else {
            if let Ok(place) = self.lower_place_agg(arg) {
                if place.projections().is_empty() {
                    Ok(PlaceAny::Aggregate(place))
                } else {
                    let temp_id = self.fb.new_local(ty_id, LocalKind::Temp, None);
                    let temp_place = Place::new(temp_id, ty_id, vec![]);
                    self.fb.push_stmt(
                        self.curr_block,
                        Statement::CopyAggregate {
                            dst: temp_place.clone(),
                            src: place,
                        },
                    );
                    Ok(PlaceAny::Aggregate(temp_place))
                }
            } else {
                Ok(PlaceAny::Aggregate(self.lower_agg_expr(arg)?))
            }
        }
    }

    // --- Operator Mapping ---

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

    fn map_unop(op: ast::UnaryOp) -> UnOp {
        match op {
            ast::UnaryOp::Neg => UnOp::Neg,
        }
    }

    // --- Helpers ---

    fn def_for_node(&self, node_id: NodeId) -> Result<&Def, LowerError> {
        self.ctx
            .def_map
            .lookup_def(node_id)
            .ok_or(LowerError::ExprDefNotFound(node_id))
    }

    fn ty_for_node(&self, node_id: NodeId) -> Result<Type, LowerError> {
        self.ctx
            .type_map
            .lookup_node_type(node_id)
            .ok_or(LowerError::ExprTypeNotFound(node_id))
    }

    fn is_scalar(&self, ty_id: TyId) -> bool {
        self.ty_lowerer.table.get(ty_id).is_scalar()
    }

    fn is_aggregate(&self, ty_id: TyId) -> bool {
        self.ty_lowerer.table.get(ty_id).is_aggregate()
    }
}

pub fn lower_ast(ctx: AnalyzedContext) -> Result<Vec<Body>, LowerError> {
    let mut bodies = Vec::new();
    for func in ctx.module.funcs() {
        let body = FuncLowerer::new(&ctx, func).lower()?;
        bodies.push(body);
    }
    Ok(bodies)
}

#[cfg(test)]
#[path = "tests/t_lower_ast.rs"]
mod tests;
