use crate::ast::{Expr, MatchArm, MatchPattern, MatchPatternBinding};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{ExprValue, FuncLowerer};
use crate::mcir::types::*;
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    // --- Match Expression ---

    pub(super) fn lower_match_expr(
        &mut self,
        expr: &Expr,
        scrutinee: &Expr,
        arms: &[MatchArm],
    ) -> Result<ExprValue, LowerError> {
        let result_ty = self.ty_for_node(expr.id)?;
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty.is_scalar() {
            let temp_place = self.new_temp_scalar(result_ty_id);
            self.lower_match_into_scalar(temp_place.clone(), scrutinee, arms)?;
            Ok(ExprValue::Scalar(Operand::Copy(temp_place)))
        } else {
            let temp_place = self.new_temp_aggregate(result_ty_id);
            self.lower_match_into_agg(temp_place.clone(), scrutinee, arms)?;
            Ok(ExprValue::Aggregate(temp_place))
        }
    }

    pub(super) fn lower_match_into_scalar(
        &mut self,
        dst: Place<Scalar>,
        scrutinee: &Expr,
        arms: &[MatchArm],
    ) -> Result<(), LowerError> {
        self.lower_match_with_switch(scrutinee, arms, |this, arm| {
            let op = match this.lower_expr_value(&arm.body)? {
                ExprValue::Scalar(op) => op,
                ExprValue::Aggregate(_) => {
                    return Err(LowerError::UnsupportedOperandExpr(arm.body.id));
                }
            };
            this.emit_copy_scalar(dst.clone(), Rvalue::Use(op));
            Ok(())
        })
    }

    pub(super) fn lower_match_into_agg(
        &mut self,
        dst: Place<Aggregate>,
        scrutinee: &Expr,
        arms: &[MatchArm],
    ) -> Result<(), LowerError> {
        self.lower_match_with_switch(scrutinee, arms, |this, arm| {
            this.lower_agg_value_into(dst.clone(), &arm.body)
        })
    }

    pub(super) fn lower_match_with_switch<F>(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
        mut emit_arm_body: F,
    ) -> Result<(), LowerError>
    where
        F: FnMut(&mut Self, &MatchArm) -> Result<(), LowerError>,
    {
        let scrutinee_ty = self.ty_for_node(scrutinee.id)?;
        let (enum_ty, deref_count) = self.peel_heap_for_match(scrutinee_ty.clone());

        // lower scrutinee into a temp place
        let (discr, scrutinee_place) = self.lower_match_discr(scrutinee, &enum_ty, deref_count)?;
        let join_bb = self.fb.new_block();

        let mut cases = Vec::new();
        let mut default_bb = None;
        let mut arm_blocks = Vec::new();

        // build switch cases and default block
        for arm in arms {
            let arm_bb = self.fb.new_block();
            arm_blocks.push((arm_bb, arm));

            match &arm.pattern {
                MatchPattern::Wildcard { .. } => default_bb = Some(arm_bb),
                MatchPattern::EnumVariant { variant_name, .. } => {
                    let tag = enum_ty.enum_variant_index(variant_name) as u64;
                    cases.push(SwitchCase {
                        value: tag,
                        target: arm_bb,
                    });
                }
            }
        }

        let default_bb = match default_bb {
            Some(bb) => bb,
            None => {
                // Matches can be exhaustive without an explicit wildcard arm.
                // Use an unreachable default to satisfy the switch terminator.
                let bb = self.fb.new_block();
                self.fb.set_terminator(bb, Terminator::Unreachable);
                bb
            }
        };

        // set switch terminator
        self.fb.set_terminator(
            self.curr_block,
            Terminator::Switch {
                discr,
                cases,
                default: default_bb,
            },
        );

        // lower each arm
        for (arm_bb, arm) in arm_blocks {
            self.curr_block = arm_bb;

            if let MatchPattern::EnumVariant {
                variant_name,
                bindings,
                ..
            } = &arm.pattern
                && let Some(place) = &scrutinee_place
            {
                self.bind_match_payloads(&enum_ty, place, variant_name, bindings)?;
            }

            emit_arm_body(self, arm)?;
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));
        }

        self.curr_block = join_bb;
        Ok(())
    }

    pub(super) fn lower_match_discr(
        &mut self,
        scrutinee: &Expr,
        enum_ty: &Type,
        deref_count: usize,
    ) -> Result<(Operand, Option<Place<Aggregate>>), LowerError> {
        if enum_ty.is_scalar() {
            if deref_count == 0 {
                let discr = self.lower_scalar_expr(scrutinee)?;
                return Ok((discr, None));
            }

            let place = self.lower_match_deref_place(scrutinee, enum_ty, deref_count)?;
            let PlaceAny::Scalar(place) = place else {
                return Err(LowerError::ExprIsNotAggregate(scrutinee.id));
            };
            Ok((Operand::Copy(place), None))
        } else {
            let place = self.lower_match_deref_place(scrutinee, enum_ty, deref_count)?;
            let PlaceAny::Aggregate(place) = place else {
                return Err(LowerError::ExprIsNotAggregate(scrutinee.id));
            };

            let tag_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
            let mut projs = place.projections().to_vec();
            projs.push(Projection::Field { index: 0 });
            let tag_place = Place::new(place.base(), tag_ty_id, projs);

            Ok((Operand::Copy(tag_place), Some(place)))
        }
    }

    fn peel_heap_for_match(&self, mut ty: Type) -> (Type, usize) {
        let mut deref_count = 0usize;
        while let Type::Heap { elem_ty } = ty {
            deref_count += 1;
            ty = *elem_ty;
        }
        (ty, deref_count)
    }

    fn lower_match_deref_place(
        &mut self,
        scrutinee: &Expr,
        enum_ty: &Type,
        deref_count: usize,
    ) -> Result<PlaceAny, LowerError> {
        let scrutinee_ty = self.ty_for_node(scrutinee.id)?;
        let place = self
            .lower_place(scrutinee)
            .or_else(|_| {
                self.lower_agg_expr_to_temp(scrutinee)
                    .map(PlaceAny::Aggregate)
            })
            .or_else(|_| {
                let scrutinee_ty_id = self.ty_lowerer.lower_ty(&scrutinee_ty);
                let temp = self.new_temp_scalar(scrutinee_ty_id);
                let op = self.lower_scalar_expr(scrutinee)?;
                self.emit_copy_scalar(temp.clone(), Rvalue::Use(op));
                Ok(PlaceAny::Scalar(temp))
            })?;

        let (base, mut projs) = match place {
            PlaceAny::Scalar(p) => (p.base(), p.projections().to_vec()),
            PlaceAny::Aggregate(p) => (p.base(), p.projections().to_vec()),
        };
        for _ in 0..deref_count {
            projs.push(Projection::Deref);
        }

        let enum_ty_id = self.ty_lowerer.lower_ty(enum_ty);
        Ok(self.place_from_ty_id(base, enum_ty_id, projs))
    }

    pub(super) fn bind_match_payloads(
        &mut self,
        scrutinee_ty: &Type,
        scrutinee_place: &Place<Aggregate>,
        variant_name: &str,
        bindings: &[MatchPatternBinding],
    ) -> Result<(), LowerError> {
        if bindings.is_empty() {
            return Ok(());
        }

        let Type::Enum { variants, .. } = scrutinee_ty else {
            unreachable!("compiler bug: non-enum type");
        };

        let variant = variants
            .iter()
            .find(|v| v.name == variant_name)
            .expect("compiler bug: missing variant");

        let offsets = scrutinee_ty.enum_variant_payload_offsets(variant_name);

        for (index, (binding, payload_ty)) in
            bindings.iter().zip(variant.payload.iter()).enumerate()
        {
            let MatchPatternBinding::Named { id, name, .. } = binding else {
                continue;
            };

            let payload_ty_id = self.ty_lowerer.lower_ty(payload_ty);
            let def_id = self.def_for_node(*id)?.id;
            let local_id = self.ensure_local_for_def(def_id, payload_ty_id, Some(name.clone()));

            let mut projs = scrutinee_place.projections().to_vec();
            projs.push(Projection::Field { index: 1 });
            projs.push(Projection::ByteOffset {
                offset: offsets[index],
            });

            if self.is_scalar(payload_ty_id) {
                let dst = Place::new(local_id, payload_ty_id, vec![]);
                let payload_place = Place::new(scrutinee_place.base(), payload_ty_id, projs);
                self.emit_copy_scalar(dst, Rvalue::Use(Operand::Copy(payload_place)));
            } else {
                let dst = Place::new(local_id, payload_ty_id, vec![]);
                let payload_place = Place::new(scrutinee_place.base(), payload_ty_id, projs);
                self.emit_copy_aggregate(dst, payload_place);
            }
        }

        Ok(())
    }
}
