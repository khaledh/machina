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

        // lower scrutinee into a temp place
        let (discr, scrutinee_place) = self.lower_match_discr(scrutinee, &scrutinee_ty)?;
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
                    let tag = scrutinee_ty.enum_variant_index(variant_name) as u64;
                    cases.push(SwitchCase {
                        value: tag,
                        target: arm_bb,
                    });
                }
            }
        }

        let default_bb = default_bb.expect("compiler bug: missing default arm");

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
            {
                if let Some(place) = &scrutinee_place {
                    self.bind_match_payloads(&scrutinee_ty, place, variant_name, bindings)?;
                }
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
        scrutinee_ty: &Type,
    ) -> Result<(Operand, Option<Place<Aggregate>>), LowerError> {
        if scrutinee_ty.is_scalar() {
            let discr = self.lower_scalar_expr(scrutinee)?;
            Ok((discr, None))
        } else {
            let place = self
                .lower_place_agg(scrutinee)
                .or_else(|_| self.lower_agg_expr_to_temp(scrutinee))?;

            let tag_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
            let mut projs = place.projections().to_vec();
            projs.push(Projection::Field { index: 0 });
            let tag_place = Place::new(place.base(), tag_ty_id, projs);

            Ok((Operand::Copy(tag_place), Some(place)))
        }
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

        for ((binding, payload_ty), offset) in bindings
            .iter()
            .zip(variant.payload.iter())
            .zip(offsets.iter())
        {
            let payload_ty_id = self.ty_lowerer.lower_ty(payload_ty);
            let def_id = self.def_for_node(binding.id)?.id;
            let local_id =
                self.ensure_local_for_def(def_id, payload_ty_id, Some(binding.name.clone()));

            let mut projs = scrutinee_place.projections().to_vec();
            projs.push(Projection::Field { index: 1 });
            projs.push(Projection::ByteOffset { offset: *offset });

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
