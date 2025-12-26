use crate::ast::{Expr, ExprKind as EK};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, PlaceKind};
use crate::mcir::types::*;
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    // --- Place (Lvalue) ---

    /// Lower an lvalue expression into a place.
    pub(super) fn lower_place(&mut self, expr: &Expr) -> Result<PlaceAny, LowerError> {
        match &expr.kind {
            EK::Var(_) => self.lower_var(expr),
            EK::ArrayIndex { target, indices } => self.lower_array_index(expr, target, indices),
            EK::TupleField { target, index } => self.lower_tuple_field(target, *index),
            EK::StructField { target, field } => self.lower_struct_field(target, field),
            _ => Err(LowerError::ExprIsNotPlace(expr.id)),
        }
    }

    /// Lower an lvalue expression expected to be scalar.
    pub(super) fn lower_place_scalar(&mut self, expr: &Expr) -> Result<Place<Scalar>, LowerError> {
        match self.lower_place(expr)? {
            PlaceAny::Scalar(p) => Ok(p),
            PlaceAny::Aggregate(_) => Err(LowerError::PlaceKindMismatch {
                node_id: expr.id,
                expected: PlaceKind::Scalar,
            }),
        }
    }

    /// Lower an lvalue expression expected to be aggregate.
    pub(super) fn lower_place_agg(&mut self, expr: &Expr) -> Result<Place<Aggregate>, LowerError> {
        match self.lower_place(expr)? {
            PlaceAny::Scalar(_) => Err(LowerError::PlaceKindMismatch {
                node_id: expr.id,
                expected: PlaceKind::Aggregate,
            }),
            PlaceAny::Aggregate(p) => Ok(p),
        }
    }

    /// Resolve a variable reference into its local place.
    pub(super) fn lower_var(&mut self, expr: &Expr) -> Result<PlaceAny, LowerError> {
        let def = self.def_for_node(expr.id)?;
        let local_id = *self
            .locals
            .get(&def.id)
            .ok_or(LowerError::VarLocalNotFound(expr.id, def.id))?;

        let local_ty = self.fb.body.locals[local_id.0 as usize].ty;

        let place = self.place_from_ty_id(local_id, local_ty, vec![]);

        Ok(place)
    }

    /// Lower array indexing into a projected place.
    pub(super) fn lower_array_index(
        &mut self,
        expr: &Expr,
        target: &Expr,
        indices: &[Expr],
    ) -> Result<PlaceAny, LowerError> {
        let target_place = self.lower_place_agg(target)?;
        let target_ty = self.ty_for_node(target.id)?;

        let Type::Array { dims, .. } = target_ty else {
            panic!("compiler bug: non-array target (type checker should catch this)");
        };
        if indices.len() > dims.len() {
            panic!("compiler bug: too many indices for array (type checker should catch this)");
        }

        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);

        // Lower each index expression to an operand + emit bounds checks.
        let mut index_operands = Vec::with_capacity(indices.len());
        for (idx_expr, dim) in indices.iter().zip(dims) {
            let idx_op = self.lower_scalar_expr(idx_expr)?;
            let dim_op = Operand::Const(Const::Int {
                signed: false,
                bits: 64,
                value: dim as i128,
            });

            let cond_op = self.emit_scalar_rvalue(
                bool_ty_id,
                Rvalue::BinOp {
                    op: BinOp::Lt,
                    lhs: idx_op.clone(),
                    rhs: dim_op.clone(),
                },
            );
            self.emit_runtime_check(
                cond_op,
                CheckKind::Bounds {
                    index: idx_op.clone(),
                    len: dim_op,
                },
            );

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

        let place = self.place_from_ty_id(target_place.base(), result_ty_id, projs);
        Ok(place)
    }

    /// Lower tuple field access into a projected place.
    pub(super) fn lower_tuple_field(
        &mut self,
        target: &Expr,
        index: usize,
    ) -> Result<PlaceAny, LowerError> {
        let target_place = self.lower_place_agg(target)?;
        let target_ty = self.ty_for_node(target.id)?;

        let field_ty = target_ty.tuple_field_type(index);
        let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);

        let mut projs = target_place.projections().to_vec();
        projs.push(Projection::Field { index });

        let place = self.place_from_ty_id(target_place.base(), field_ty_id, projs);

        Ok(place)
    }

    /// Lower struct field access into a projected place.
    pub(super) fn lower_struct_field(
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

        let place = self.place_from_ty_id(target_place.base(), field_ty_id, projs);

        Ok(place)
    }

    /// Create a projected place from an aggregate base.
    pub(super) fn project_place(
        &self,
        src: &Place<Aggregate>,
        proj: Projection,
        ty_id: TyId,
    ) -> PlaceAny {
        let mut projs = src.projections().to_vec();
        projs.push(proj);
        self.place_from_ty_id(src.base(), ty_id, projs)
    }

    /// Construct a typed place from a base local and projections.
    pub(super) fn place_from_ty_id(
        &self,
        base: LocalId,
        ty: TyId,
        proj: Vec<Projection>,
    ) -> PlaceAny {
        if self.is_scalar(ty) {
            PlaceAny::Scalar(Place::new(base, ty, proj))
        } else {
            PlaceAny::Aggregate(Place::new(base, ty, proj))
        }
    }
}
