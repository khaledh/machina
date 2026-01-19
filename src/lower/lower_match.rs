use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, Value};
use crate::mcir::types::*;
use crate::tree::NodeId;
use crate::tree::semantic::{
    MatchArm, MatchBinding, MatchDecision, MatchDecisionNode, MatchPlace, MatchPlan,
    MatchProjection, MatchSwitch, MatchTest, MatchTestKind, ValueExpr, ValueExprKind as VEK,
};
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    // --- Match Expression ---

    pub(super) fn lower_match_expr(
        &mut self,
        expr: &ValueExpr,
        scrutinee: &ValueExpr,
        arms: &[MatchArm],
    ) -> Result<Value, LowerError> {
        let result_ty = self.ty_from_id(expr.ty);
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty.is_scalar() {
            let temp_place = self.new_temp_scalar(result_ty_id);
            self.lower_match_into_scalar(expr, temp_place.clone(), scrutinee, arms)?;
            Ok(Value::Scalar(Operand::Copy(temp_place)))
        } else {
            let temp_place = self.new_temp_aggregate(result_ty_id);
            self.lower_match_into_agg(expr, temp_place.clone(), scrutinee, arms)?;
            Ok(Value::Aggregate(temp_place))
        }
    }

    pub(super) fn lower_match_into_scalar(
        &mut self,
        expr: &ValueExpr,
        dst: Place<Scalar>,
        scrutinee: &ValueExpr,
        arms: &[MatchArm],
    ) -> Result<(), LowerError> {
        self.lower_match_with_plan(expr, scrutinee, arms, |this, arm| {
            let op = match this.lower_expr_value(&arm.body)? {
                Value::Scalar(op) => op,
                Value::Aggregate(_) => {
                    return Err(LowerError::UnsupportedOperandExpr(arm.body.id));
                }
            };
            if !this.is_curr_block_terminated() {
                this.emit_copy_scalar(dst.clone(), Rvalue::Use(op));
            }
            Ok(())
        })
    }

    pub(super) fn lower_match_into_agg(
        &mut self,
        expr: &ValueExpr,
        dst: Place<Aggregate>,
        scrutinee: &ValueExpr,
        arms: &[MatchArm],
    ) -> Result<(), LowerError> {
        self.lower_match_with_plan(expr, scrutinee, arms, |this, arm| {
            this.lower_agg_value_into(dst.clone(), &arm.body)
        })
    }

    fn lower_match_with_plan<F>(
        &mut self,
        expr: &ValueExpr,
        scrutinee: &ValueExpr,
        arms: &[MatchArm],
        mut emit_arm_body: F,
    ) -> Result<(), LowerError>
    where
        F: FnMut(&mut Self, &MatchArm) -> Result<(), LowerError>,
    {
        let plan = self.match_plan_for(expr)?;
        let scrutinee_place = self.lower_match_scrutinee_place(scrutinee, &plan.scrutinee_ty)?;

        let join_bb = self.fb.new_block();
        let mut arm_blocks = Vec::with_capacity(arms.len());
        for _ in arms {
            arm_blocks.push(self.fb.new_block());
        }

        match &plan.decision {
            MatchDecision::Switch(switch) => {
                self.lower_match_switch(expr.id, &scrutinee_place, switch, &arm_blocks)?;
            }
            MatchDecision::DecisionTree(tree) => {
                let start_bb = self.curr_block;
                self.emit_match_decision_node(
                    expr.id,
                    start_bb,
                    tree,
                    &scrutinee_place,
                    &arm_blocks,
                )?;
            }
        }

        for (index, arm) in arms.iter().enumerate() {
            self.curr_block = arm_blocks[index];
            let arm_plan = plan
                .arms
                .get(index)
                .unwrap_or_else(|| panic!("compiler bug: missing match arm plan"));
            self.bind_match_plan_bindings(&scrutinee_place, &arm_plan.bindings)?;
            emit_arm_body(self, arm)?;
            if !self.is_curr_block_terminated() {
                self.fb
                    .set_terminator(self.curr_block, Terminator::Goto(join_bb));
            }
        }

        self.curr_block = join_bb;
        Ok(())
    }

    fn match_plan_for(&self, expr: &ValueExpr) -> Result<MatchPlan, LowerError> {
        self.ctx
            .type_map
            .lookup_match_plan(expr.id)
            .ok_or(LowerError::ExprTypeNotFound(expr.id))
    }

    fn lower_match_scrutinee_place(
        &mut self,
        scrutinee: &ValueExpr,
        scrutinee_ty: &Type,
    ) -> Result<PlaceAny, LowerError> {
        match &scrutinee.kind {
            VEK::Load { place } => self.lower_place(place),
            VEK::Move { place } | VEK::ImplicitMove { place } => {
                self.record_move_place(place);
                self.lower_place(place)
            }
            _ => {
                if scrutinee_ty.is_scalar() {
                    let ty_id = self.ty_lowerer.lower_ty(scrutinee_ty);
                    let temp = self.new_temp_scalar(ty_id);
                    let op = self.lower_scalar_expr(scrutinee)?;
                    self.emit_copy_scalar(temp.clone(), Rvalue::Use(op));
                    Ok(PlaceAny::Scalar(temp))
                } else {
                    Ok(PlaceAny::Aggregate(self.lower_agg_expr_to_temp(scrutinee)?))
                }
            }
        }
    }

    fn lower_match_switch(
        &mut self,
        node_id: NodeId,
        base_place: &PlaceAny,
        switch: &MatchSwitch,
        arm_blocks: &[BlockId],
    ) -> Result<(), LowerError> {
        let discr_place = self.lower_match_place_any(node_id, base_place, &switch.discr)?;
        let discr = match discr_place {
            PlaceAny::Scalar(place) => Operand::Copy(place),
            PlaceAny::Aggregate(_) => return Err(LowerError::ExprIsNotAggregate(node_id)),
        };

        let mut cases = Vec::with_capacity(switch.cases.len());
        for case in &switch.cases {
            let target = *arm_blocks
                .get(case.arm_index)
                .unwrap_or_else(|| panic!("compiler bug: missing arm block"));
            cases.push(SwitchCase {
                value: case.value,
                target,
            });
        }

        let default_bb = match switch.default {
            Some(index) => *arm_blocks
                .get(index)
                .unwrap_or_else(|| panic!("compiler bug: missing arm block")),
            None => {
                let bb = self.fb.new_block();
                self.fb.set_terminator(bb, Terminator::Unreachable);
                bb
            }
        };

        self.fb.set_terminator(
            self.curr_block,
            Terminator::Switch {
                discr,
                cases,
                default: default_bb,
            },
        );
        Ok(())
    }

    fn emit_match_decision_node(
        &mut self,
        node_id: NodeId,
        start_bb: BlockId,
        node: &MatchDecisionNode,
        base_place: &PlaceAny,
        arm_blocks: &[BlockId],
    ) -> Result<(), LowerError> {
        match node {
            MatchDecisionNode::Leaf { arm_index } => {
                let target = *arm_blocks
                    .get(*arm_index)
                    .unwrap_or_else(|| panic!("compiler bug: missing arm block"));
                self.fb.set_terminator(start_bb, Terminator::Goto(target));
                Ok(())
            }
            MatchDecisionNode::Unreachable => {
                self.fb.set_terminator(start_bb, Terminator::Unreachable);
                Ok(())
            }
            MatchDecisionNode::Tests {
                tests,
                on_match,
                on_fail,
            } => {
                let then_bb = self.fb.new_block();
                let else_bb = self.fb.new_block();
                self.emit_match_test_chain(node_id, start_bb, then_bb, else_bb, tests, base_place)?;
                self.emit_match_decision_node(node_id, then_bb, on_match, base_place, arm_blocks)?;
                self.emit_match_decision_node(node_id, else_bb, on_fail, base_place, arm_blocks)?;
                Ok(())
            }
        }
    }

    fn emit_match_test_chain(
        &mut self,
        node_id: NodeId,
        start_bb: BlockId,
        success_bb: BlockId,
        failure_bb: BlockId,
        tests: &[MatchTest],
        base_place: &PlaceAny,
    ) -> Result<(), LowerError> {
        let mut curr_bb = start_bb;
        for (index, test) in tests.iter().enumerate() {
            let then_bb = if index + 1 == tests.len() {
                success_bb
            } else {
                self.fb.new_block()
            };

            self.curr_block = curr_bb;
            let cond = self.emit_match_test_operand(node_id, base_place, test)?;
            self.fb.set_terminator(
                curr_bb,
                Terminator::If {
                    cond,
                    then_bb,
                    else_bb: failure_bb,
                },
            );

            if index + 1 == tests.len() {
                break;
            }

            curr_bb = then_bb;
        }

        Ok(())
    }

    fn emit_match_test_operand(
        &mut self,
        node_id: NodeId,
        base_place: &PlaceAny,
        test: &MatchTest,
    ) -> Result<Operand, LowerError> {
        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);
        let place_any = self.lower_match_place_any(node_id, base_place, &test.place)?;

        match &test.kind {
            MatchTestKind::Bool { value } => {
                let PlaceAny::Scalar(place) = place_any else {
                    return Err(LowerError::ExprIsNotAggregate(node_id));
                };
                Ok(self.emit_scalar_rvalue(
                    bool_ty_id,
                    Rvalue::BinOp {
                        op: BinOp::Eq,
                        lhs: Operand::Copy(place),
                        rhs: Operand::Const(Const::Bool(*value)),
                    },
                ))
            }
            MatchTestKind::Int {
                value,
                signed,
                bits,
            } => {
                let PlaceAny::Scalar(place) = place_any else {
                    return Err(LowerError::ExprIsNotAggregate(node_id));
                };
                Ok(self.emit_scalar_rvalue(
                    bool_ty_id,
                    Rvalue::BinOp {
                        op: BinOp::Eq,
                        lhs: Operand::Copy(place),
                        rhs: Operand::Const(Const::Int {
                            signed: *signed,
                            bits: *bits,
                            value: (*value) as i128,
                        }),
                    },
                ))
            }
            MatchTestKind::EnumTag { tag, is_scalar } => {
                let discr = if *is_scalar {
                    let PlaceAny::Scalar(place) = place_any else {
                        return Err(LowerError::ExprIsNotAggregate(node_id));
                    };
                    Operand::Copy(place)
                } else {
                    let PlaceAny::Aggregate(place) = place_any else {
                        return Err(LowerError::ExprIsNotAggregate(node_id));
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

    fn bind_match_plan_bindings(
        &mut self,
        base_place: &PlaceAny,
        bindings: &[MatchBinding],
    ) -> Result<(), LowerError> {
        for binding in bindings {
            let ty_id = self.ty_lowerer.lower_ty(&binding.source.ty);
            let name = self.def_name(binding.def_id, binding.node_id)?;
            let local_id = self.ensure_local_for_def(binding.def_id, ty_id, Some(name));
            let source_place =
                self.lower_match_place_any(binding.node_id, base_place, &binding.source)?;

            match source_place {
                PlaceAny::Scalar(place) => {
                    let dst = Place::new(local_id, ty_id, vec![]);
                    self.emit_copy_scalar(dst, Rvalue::Use(Operand::Copy(place)));
                }
                PlaceAny::Aggregate(place) => {
                    let dst = Place::new(local_id, ty_id, vec![]);
                    self.emit_copy_aggregate(dst, place);
                }
            }
        }

        Ok(())
    }

    fn lower_match_place_any(
        &mut self,
        _node_id: NodeId,
        base_place: &PlaceAny,
        place: &MatchPlace,
    ) -> Result<PlaceAny, LowerError> {
        let (base, mut projections) = match base_place {
            PlaceAny::Scalar(place) => (place.base(), place.projections().to_vec()),
            PlaceAny::Aggregate(place) => (place.base(), place.projections().to_vec()),
        };

        for proj in &place.projections {
            projections.push(match proj {
                MatchProjection::Deref => Projection::Deref,
                MatchProjection::Field { index } => Projection::Field { index: *index },
                MatchProjection::ByteOffset { offset } => {
                    Projection::ByteOffset { offset: *offset }
                }
            });
        }

        let ty_id = self.ty_lowerer.lower_ty(&place.ty);
        if place.ty.is_scalar() {
            Ok(PlaceAny::Scalar(Place::new(base, ty_id, projections)))
        } else {
            Ok(PlaceAny::Aggregate(Place::new(base, ty_id, projections)))
        }
    }
}
