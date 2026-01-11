use crate::hir::model::{BindPattern, BindPatternKind as PK, Expr};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::ExprValue;
use crate::lower::lower_ast::FuncLowerer;
use crate::mcir::types::*;
use crate::resolve::def_map::{DefId, DefKind};
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    // --- Bindings / Patterns ---

    /// Lower a let/var binding.
    pub(super) fn lower_binding(
        &mut self,
        pattern: &BindPattern,
        value: &Expr,
    ) -> Result<(), LowerError> {
        let value_ty = self.ty_for_node(value.id)?;

        if value_ty.is_scalar() {
            // Scalar binding: compute operand and assign.
            let PK::Name(def_id) = &pattern.kind else {
                return Err(LowerError::PatternMismatch(pattern.id));
            };
            let op = self.lower_scalar_expr(value)?;
            let target_ty = self.def_ty_for_id(*def_id, pattern.id)?;
            self.emit_conversion_check(&value_ty, &target_ty, &op);

            let ty_id = self.ty_lowerer.lower_ty(&target_ty);
            let name = self.def_name(*def_id, pattern.id)?;

            // Track owned heap values for drop at scope exit.
            let is_initialized = self.create_is_initialized(&name, &target_ty, true);
            self.register_drop(*def_id, &target_ty, is_initialized);

            // Bind identifier to operand.
            self.bind_ident_operand(*def_id, name, ty_id, op)?;
            return Ok(());
        }

        // Aggregate binding
        let PK::Name(def_id) = &pattern.kind else {
            // Aggregate destructuring via patterns.
            let src_place = match self.lower_expr_value(value)? {
                ExprValue::Aggregate(place) => PlaceAny::Aggregate(place),
                ExprValue::Scalar(_) => {
                    return Err(LowerError::PatternMismatch(pattern.id));
                }
            };

            return self.bind_pattern_with_type(pattern, src_place, &value_ty);
        };

        // Aggregate ident binding: prefer NRVO when eligible.
        let (def_id, nrvo_eligible) = {
            let def = self.def_for_id(*def_id, pattern.id)?;
            let eligible = matches!(
                def.kind,
                DefKind::LocalVar {
                    nrvo_eligible: true,
                    ..
                }
            );
            (def.id, eligible)
        };

        if nrvo_eligible {
            // Bind variable def to return local.
            let ret_id = self.fb.body.ret_local;
            self.locals.insert(def_id, ret_id);

            // Directly lower into the return place.
            let ret_ty = self.fb.body.locals[ret_id.0 as usize].ty;
            let dst = Place::new(ret_id, ret_ty, vec![]);
            self.lower_agg_value_into(dst, value)?;

            // NRVO still needs drop tracking for owned values.
            let name = self.def_name(def_id, pattern.id)?;
            let is_initialized = self.create_is_initialized(&name, &value_ty, true);
            self.register_drop(def_id, &value_ty, is_initialized);

            return Ok(());
        }

        // Create a local for the aggregate.
        let ty_id = self.ty_lowerer.lower_ty(&value_ty);
        let name = self.def_name(def_id, pattern.id)?;
        let local_id = self.ensure_local_for_def(def_id, ty_id, Some(name.clone()));

        // Lower aggregate into the local place.
        let dst = Place::new(local_id, ty_id, vec![]);
        self.lower_agg_value_into(dst, value)?;

        // Track owned heap values for drop at scope exit.
        let is_initialized = self.create_is_initialized(&name, &value_ty, true);
        self.register_drop(def_id, &value_ty, is_initialized);

        Ok(())
    }

    /// Bind a pattern to a value place with a known AST type.
    pub(super) fn bind_pattern_with_type(
        &mut self,
        pattern: &BindPattern,
        src_place: PlaceAny,
        src_ty: &Type,
    ) -> Result<(), LowerError> {
        match &pattern.kind {
            PK::Name(def_id) => {
                // Bind a single identifier to a place.
                let src_ty_id = self.ty_lowerer.lower_ty(src_ty);
                let name = self.def_name(*def_id, pattern.id)?;

                // Track owned heap values for drop at scope exit.
                let is_initialized = self.create_is_initialized(&name, src_ty, true);
                self.register_drop(*def_id, src_ty, is_initialized);

                self.bind_ident(*def_id, name, src_ty_id, src_place)
            }

            PK::Tuple { patterns } => {
                // Destructure tuple by projecting each field.
                let src_place = match src_place {
                    PlaceAny::Aggregate(place) => place,
                    _ => return Err(LowerError::PatternMismatch(pattern.id)),
                };

                let Type::Tuple { field_tys } = src_ty else {
                    unreachable!("compiler bug: non-tuple pattern");
                };
                debug_assert_eq!(patterns.len(), field_tys.len(), "pattern arity mismatch");

                for (i, pat) in patterns.iter().enumerate() {
                    let field_ty = &field_tys[i];
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

                let Type::Array { .. } = src_ty else {
                    unreachable!("compiler bug: non-array pattern");
                };

                let elem_ty = src_ty
                    .array_item_type()
                    .unwrap_or_else(|| panic!("compiler bug: empty array dims"));

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

    /// Bind an identifier to a place, emitting the appropriate assignment.
    pub(super) fn bind_ident(
        &mut self,
        def_id: DefId,
        name: String,
        ty_id: TyId,
        src_place: PlaceAny,
    ) -> Result<(), LowerError> {
        let local_id = self.ensure_local_for_def(def_id, ty_id, Some(name));
        match src_place {
            PlaceAny::Scalar(place) => {
                self.emit_copy_scalar(
                    Place::new(local_id, ty_id, vec![]),
                    Rvalue::Use(Operand::Copy(place)),
                );
            }
            PlaceAny::Aggregate(place) => {
                self.emit_copy_aggregate(Place::new(local_id, ty_id, vec![]), place);
            }
        }
        Ok(())
    }

    /// Bind an identifier to a scalar operand.
    pub(super) fn bind_ident_operand(
        &mut self,
        def_id: DefId,
        name: String,
        ty_id: TyId,
        op: Operand,
    ) -> Result<(), LowerError> {
        let local_id = self.ensure_local_for_def(def_id, ty_id, Some(name));
        self.emit_copy_scalar(Place::new(local_id, ty_id, vec![]), Rvalue::Use(op));
        Ok(())
    }

    /// Create or reuse a local for a definition id.
    pub(super) fn ensure_local_for_def(
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
}
