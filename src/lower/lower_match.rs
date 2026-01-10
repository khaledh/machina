use crate::ast::{Expr, MatchArm, MatchPattern, MatchPatternBinding, NodeId};
use crate::lower::decision_tree::{
    ArmDecision, DecisionTreeEmitter, Test, TestKind, build_decision_tree, emit_decision_tree,
};
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
        if self.is_tuple_match_scrutinee(scrutinee)? {
            return self.lower_match_with_tuple(scrutinee, arms, |this, arm| {
                let op = match this.lower_expr_value(&arm.body)? {
                    ExprValue::Scalar(op) => op,
                    ExprValue::Aggregate(_) => {
                        return Err(LowerError::UnsupportedOperandExpr(arm.body.id));
                    }
                };
                this.emit_copy_scalar(dst.clone(), Rvalue::Use(op));
                Ok(())
            });
        }

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
        if self.is_tuple_match_scrutinee(scrutinee)? {
            return self.lower_match_with_tuple(scrutinee, arms, |this, arm| {
                this.lower_agg_value_into(dst.clone(), &arm.body)
            });
        }

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
        let (scrutinee_ty, deref_count) = self.peel_heap_for_match(scrutinee_ty.clone());

        // lower scrutinee into a temp place
        let (discr, scrutinee_place) =
            self.lower_match_discr(scrutinee, &scrutinee_ty, deref_count)?;
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
                MatchPattern::BoolLit { value, .. } => {
                    cases.push(SwitchCase {
                        value: u64::from(*value),
                        target: arm_bb,
                    });
                }
                MatchPattern::IntLit { value, .. } => {
                    cases.push(SwitchCase {
                        value: *value,
                        target: arm_bb,
                    });
                }
                MatchPattern::Binding { .. } => {
                    return Err(LowerError::PatternMismatch(arm.id));
                }
                MatchPattern::Tuple { .. } => {
                    return Err(LowerError::PatternMismatch(arm.id));
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
                self.bind_match_payloads(&scrutinee_ty, place, variant_name, bindings)?;
            }

            emit_arm_body(self, arm)?;
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));
        }

        self.curr_block = join_bb;
        Ok(())
    }

    fn is_tuple_match_scrutinee(&self, scrutinee: &Expr) -> Result<bool, LowerError> {
        let scrutinee_ty = self.ty_for_node(scrutinee.id)?;
        let (scrutinee_ty, _) = self.peel_heap_for_match(scrutinee_ty.clone());
        Ok(matches!(scrutinee_ty, Type::Tuple { .. }))
    }

    fn lower_match_with_tuple<F>(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
        mut emit_arm_body: F,
    ) -> Result<(), LowerError>
    where
        F: FnMut(&mut Self, &MatchArm) -> Result<(), LowerError>,
    {
        let scrutinee_ty = self.ty_for_node(scrutinee.id)?;
        let (scrutinee_ty, deref_count) = self.peel_heap_for_match(scrutinee_ty.clone());
        let place = self.lower_match_deref_place(scrutinee, &scrutinee_ty, deref_count)?;
        let PlaceAny::Aggregate(place) = place else {
            return Err(LowerError::ExprIsNotAggregate(scrutinee.id));
        };

        let join_bb = self.fb.new_block();
        let mut arm_blocks = Vec::new();
        let mut decisions = Vec::new();

        for arm in arms {
            let arm_bb = self.fb.new_block();
            arm_blocks.push((arm_bb, arm));

            // Build the per-arm test list; bindings are applied after the arm is selected.
            let tests = match &arm.pattern {
                MatchPattern::Wildcard { .. } => Vec::new(),
                MatchPattern::Tuple { patterns, .. } => {
                    let mut tests = Vec::new();
                    self.collect_tuple_pattern_tests(
                        &scrutinee_ty,
                        &place,
                        patterns,
                        arm.id,
                        &mut tests,
                    )?;
                    tests
                }
                _ => return Err(LowerError::PatternMismatch(arm.id)),
            };

            decisions.push(ArmDecision {
                tests,
                target: arm_bb,
            });
        }

        let decision_tree = build_decision_tree(&decisions);
        let start_bb = self.curr_block;
        let mut emitter = TupleDecisionEmitter { lowerer: self };
        emit_decision_tree(&decision_tree, start_bb, &mut emitter)?;

        for (arm_bb, arm) in arm_blocks {
            self.curr_block = arm_bb;
            // Bind tuple fields (and any nested enum payloads) only after the arm is chosen.
            if let MatchPattern::Tuple { patterns, .. } = &arm.pattern {
                self.bind_match_tuple_fields(&scrutinee_ty, &place, patterns, arm.id)?;
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
            let MatchPatternBinding::Named { id, ident, .. } = binding else {
                continue;
            };

            let payload_ty_id = self.ty_lowerer.lower_ty(payload_ty);
            let def_id = self.def_for_node(*id)?.id;
            let local_id = self.ensure_local_for_def(def_id, payload_ty_id, Some(ident.clone()));

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

    pub(super) fn bind_match_tuple_fields(
        &mut self,
        scrutinee_ty: &Type,
        scrutinee_place: &Place<Aggregate>,
        patterns: &[MatchPattern],
        arm_id: NodeId,
    ) -> Result<(), LowerError> {
        if patterns.is_empty() {
            return Ok(());
        }

        let Type::Tuple { fields } = scrutinee_ty else {
            return Err(LowerError::PatternMismatch(arm_id));
        };
        if patterns.len() != fields.len() {
            return Err(LowerError::PatternMismatch(arm_id));
        }

        for (index, (pattern, field_ty)) in patterns.iter().zip(fields.iter()).enumerate() {
            let field_ty_id = self.ty_lowerer.lower_ty(field_ty);
            let field_place =
                self.project_place(scrutinee_place, Projection::Field { index }, field_ty_id);

            match pattern {
                MatchPattern::Binding { id, ident, .. } => {
                    let def_id = self.def_for_node(*id)?.id;
                    let local_id =
                        self.ensure_local_for_def(def_id, field_ty_id, Some(ident.clone()));

                    match field_place {
                        PlaceAny::Scalar(place) => {
                            let dst = Place::new(local_id, field_ty_id, vec![]);
                            self.emit_copy_scalar(dst, Rvalue::Use(Operand::Copy(place)));
                        }
                        PlaceAny::Aggregate(place) => {
                            let dst = Place::new(local_id, field_ty_id, vec![]);
                            self.emit_copy_aggregate(dst, place);
                        }
                    }
                }
                MatchPattern::Wildcard { .. } => {}
                MatchPattern::BoolLit { .. } | MatchPattern::IntLit { .. } => {}
                MatchPattern::EnumVariant {
                    variant_name,
                    bindings,
                    ..
                } => {
                    if bindings.is_empty() {
                        continue;
                    }

                    let (peeled_ty, deref_count) = self.peel_heap_for_match(field_ty.clone());
                    let place = self.apply_deref_place_any(field_place, &peeled_ty, deref_count);
                    let Type::Enum { .. } = peeled_ty else {
                        return Err(LowerError::PatternMismatch(arm_id));
                    };
                    let PlaceAny::Aggregate(place) = place else {
                        return Err(LowerError::PatternMismatch(arm_id));
                    };
                    self.bind_match_payloads(&peeled_ty, &place, variant_name, bindings)?;
                }
                MatchPattern::Tuple { patterns, .. } => {
                    let (peeled_ty, deref_count) = self.peel_heap_for_match(field_ty.clone());
                    let Type::Tuple { .. } = peeled_ty else {
                        return Err(LowerError::PatternMismatch(arm_id));
                    };
                    let place = self.apply_deref_place_any(field_place, &peeled_ty, deref_count);
                    let PlaceAny::Aggregate(place) = place else {
                        return Err(LowerError::PatternMismatch(arm_id));
                    };
                    self.bind_match_tuple_fields(&peeled_ty, &place, patterns, arm_id)?;
                }
            }
        }

        Ok(())
    }

    fn collect_tuple_pattern_tests(
        &mut self,
        tuple_ty: &Type,
        tuple_place: &Place<Aggregate>,
        patterns: &[MatchPattern],
        arm_id: NodeId,
        tests: &mut Vec<Test>,
    ) -> Result<(), LowerError> {
        let Type::Tuple { fields } = tuple_ty else {
            return Err(LowerError::PatternMismatch(arm_id));
        };
        if patterns.len() != fields.len() {
            return Err(LowerError::PatternMismatch(arm_id));
        }

        // Each tuple element contributes tests over a projection of the scrutinee.
        for (index, (pattern, field_ty)) in patterns.iter().zip(fields.iter()).enumerate() {
            let field_ty_id = self.ty_lowerer.lower_ty(field_ty);
            let field_place =
                self.project_place(tuple_place, Projection::Field { index }, field_ty_id);
            self.collect_pattern_tests(field_ty, field_place, pattern, arm_id, tests)?;
        }

        Ok(())
    }

    fn collect_pattern_tests(
        &mut self,
        field_ty: &Type,
        field_place: PlaceAny,
        pattern: &MatchPattern,
        arm_id: NodeId,
        tests: &mut Vec<Test>,
    ) -> Result<(), LowerError> {
        match pattern {
            MatchPattern::Binding { .. } | MatchPattern::Wildcard { .. } => Ok(()),
            MatchPattern::BoolLit { value, .. } => {
                // Compare the projected scalar directly to the literal.
                let (peeled_ty, deref_count) = self.peel_heap_for_match(field_ty.clone());
                let Type::Bool = peeled_ty else {
                    return Err(LowerError::PatternMismatch(arm_id));
                };
                let place = self.apply_deref_place_any(field_place, &peeled_ty, deref_count);
                if !matches!(place, PlaceAny::Scalar(_)) {
                    return Err(LowerError::PatternMismatch(arm_id));
                }
                tests.push(Test {
                    place,
                    kind: TestKind::Bool { value: *value },
                });
                Ok(())
            }
            MatchPattern::IntLit { value, .. } => {
                // Compare the projected scalar directly to the literal.
                let (peeled_ty, deref_count) = self.peel_heap_for_match(field_ty.clone());
                let Type::Int { signed, bits } = peeled_ty else {
                    return Err(LowerError::PatternMismatch(arm_id));
                };
                let place = self.apply_deref_place_any(field_place, &peeled_ty, deref_count);
                if !matches!(place, PlaceAny::Scalar(_)) {
                    return Err(LowerError::PatternMismatch(arm_id));
                }
                tests.push(Test {
                    place,
                    kind: TestKind::Int {
                        value: *value,
                        signed,
                        bits,
                    },
                });
                Ok(())
            }
            MatchPattern::EnumVariant { variant_name, .. } => {
                // Compare enum tags; payload bindings are handled after arm selection.
                let (peeled_ty, deref_count) = self.peel_heap_for_match(field_ty.clone());
                let Type::Enum { variants, .. } = &peeled_ty else {
                    return Err(LowerError::PatternMismatch(arm_id));
                };
                let Some(tag) = variants
                    .iter()
                    .position(|variant| variant.name == *variant_name)
                else {
                    return Err(LowerError::PatternMismatch(arm_id));
                };
                let place = self.apply_deref_place_any(field_place, &peeled_ty, deref_count);
                let is_scalar = peeled_ty.is_scalar();
                if is_scalar && !matches!(place, PlaceAny::Scalar(_)) {
                    return Err(LowerError::PatternMismatch(arm_id));
                }
                if !is_scalar && !matches!(place, PlaceAny::Aggregate(_)) {
                    return Err(LowerError::PatternMismatch(arm_id));
                }
                tests.push(Test {
                    place,
                    kind: TestKind::Enum {
                        tag: tag as u64,
                        is_scalar,
                    },
                });
                Ok(())
            }
            MatchPattern::Tuple { patterns, .. } => {
                // Recurse into nested tuples by extending the projection chain.
                let (peeled_ty, deref_count) = self.peel_heap_for_match(field_ty.clone());
                let Type::Tuple { .. } = peeled_ty else {
                    return Err(LowerError::PatternMismatch(arm_id));
                };
                let place = self.apply_deref_place_any(field_place, &peeled_ty, deref_count);
                let PlaceAny::Aggregate(place) = place else {
                    return Err(LowerError::PatternMismatch(arm_id));
                };
                self.collect_tuple_pattern_tests(&peeled_ty, &place, patterns, arm_id, tests)
            }
        }
    }

    fn emit_match_test_operand(&mut self, test: &Test) -> Result<Operand, LowerError> {
        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);
        match &test.kind {
            TestKind::Bool { value } => {
                // Emit (place == literal) as a bool operand.
                let PlaceAny::Scalar(place) = &test.place else {
                    unreachable!("compiler bug: expected scalar place for bool check");
                };
                Ok(self.emit_scalar_rvalue(
                    bool_ty_id,
                    Rvalue::BinOp {
                        op: BinOp::Eq,
                        lhs: Operand::Copy(place.clone()),
                        rhs: Operand::Const(Const::Bool(*value)),
                    },
                ))
            }
            TestKind::Int {
                value,
                signed,
                bits,
            } => {
                // Emit (place == literal) as a bool operand.
                let PlaceAny::Scalar(place) = &test.place else {
                    unreachable!("compiler bug: expected scalar place for int check");
                };
                Ok(self.emit_scalar_rvalue(
                    bool_ty_id,
                    Rvalue::BinOp {
                        op: BinOp::Eq,
                        lhs: Operand::Copy(place.clone()),
                        rhs: Operand::Const(Const::Int {
                            signed: *signed,
                            bits: *bits,
                            value: (*value) as i128,
                        }),
                    },
                ))
            }
            TestKind::Enum { tag, is_scalar } => {
                // Compare the enum tag directly (scalar enums) or via the tag field.
                let discr = if *is_scalar {
                    let PlaceAny::Scalar(place) = &test.place else {
                        unreachable!("compiler bug: expected scalar place for enum tag check");
                    };
                    Operand::Copy(place.clone())
                } else {
                    let PlaceAny::Aggregate(place) = &test.place else {
                        unreachable!("compiler bug: expected aggregate place for enum tag check");
                    };
                    let tag_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
                    let mut projs = place.projections().to_vec();
                    projs.push(Projection::Field { index: 0 });
                    let tag_place = Place::new(place.base(), tag_ty_id, projs);
                    Operand::Copy(tag_place)
                };

                Ok(self.emit_scalar_rvalue(
                    bool_ty_id,
                    Rvalue::BinOp {
                        op: BinOp::Eq,
                        lhs: discr,
                        rhs: Operand::Const(Const::Int {
                            signed: false,
                            bits: 64,
                            value: (*tag) as i128,
                        }),
                    },
                ))
            }
        }
    }

    fn apply_deref_place_any(
        &mut self,
        place: PlaceAny,
        ty: &Type,
        deref_count: usize,
    ) -> PlaceAny {
        let (base, mut projs) = match place {
            PlaceAny::Scalar(place) => (place.base(), place.projections().to_vec()),
            PlaceAny::Aggregate(place) => (place.base(), place.projections().to_vec()),
        };
        for _ in 0..deref_count {
            projs.push(Projection::Deref);
        }
        let ty_id = self.ty_lowerer.lower_ty(ty);
        self.place_from_ty_id(base, ty_id, projs)
    }
}

struct TupleDecisionEmitter<'a, 'b> {
    lowerer: &'a mut FuncLowerer<'b>,
}

impl<'a, 'b> DecisionTreeEmitter for TupleDecisionEmitter<'a, 'b> {
    fn new_block(&mut self) -> BlockId {
        self.lowerer.fb.new_block()
    }

    fn enter_block(&mut self, block: BlockId) {
        self.lowerer.curr_block = block;
    }

    fn emit_test(&mut self, test: &Test) -> Result<Operand, LowerError> {
        self.lowerer.emit_match_test_operand(test)
    }

    fn set_terminator(&mut self, block: BlockId, term: Terminator) {
        self.lowerer.fb.set_terminator(block, term);
    }
}
