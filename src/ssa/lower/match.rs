//! Match expression lowering.
//!
//! Implements switch-based lowering for enum/bool/int matches, using
//! precomputed match plans to avoid pattern re-derivation.

use crate::ssa::IrTypeId;
use crate::ssa::lower::LoweringError;
use crate::ssa::lower::locals::LocalValue;
use crate::ssa::lower::lowerer::{BranchResult, FuncLowerer};
use crate::ssa::model::ir::{CmpOp, ConstValue, SwitchCase, Terminator, ValueId};
use crate::tree::semantic as sem;
use crate::types::{Type, TypeId};

pub(super) struct MatchLowerer<'a, 'b, 'g> {
    lowerer: &'a mut FuncLowerer<'b, 'g>,
    expr: &'a sem::ValueExpr,
    scrutinee_addr: ValueId,
    scrutinee_ty_id: TypeId,
}

impl<'a, 'b, 'g> MatchLowerer<'a, 'b, 'g> {
    pub(super) fn lower(
        lowerer: &'a mut FuncLowerer<'b, 'g>,
        expr: &'a sem::ValueExpr,
        scrutinee: &sem::ValueExpr,
        arms: &'a [sem::MatchArm],
    ) -> Result<BranchResult, LoweringError> {
        let plan = lowerer
            .type_map
            .lookup_match_plan(expr.id)
            .unwrap_or_else(|| panic!("ssa lower_func missing match plan {:?}", expr.id))
            .clone();

        // Evaluate the scrutinee once and store it for address-based projections.
        let scrutinee_value = lowerer.lower_linear_expr_value(scrutinee)?;
        let scrutinee_ty_id = scrutinee.ty;
        let scrutinee_ir_ty = lowerer.type_lowerer.lower_type_id(scrutinee_ty_id);
        let scrutinee_addr = lowerer.alloc_local_addr(scrutinee_ir_ty);
        let scrutinee_ty = lowerer.type_map.type_table().get(scrutinee.ty);
        lowerer.store_value_into_addr(
            scrutinee_addr,
            scrutinee_value,
            scrutinee_ty,
            scrutinee_ir_ty,
        );

        let mut helper = MatchLowerer {
            lowerer,
            expr,
            scrutinee_addr,
            scrutinee_ty_id,
        };

        let decision = plan.decision;
        let arm_plans = plan.arms;

        match decision {
            sem::MatchDecision::Switch(switch) => helper.lower_switch(arms, &arm_plans, &switch),
            sem::MatchDecision::DecisionTree(tree) => {
                helper.lower_decision_tree(arms, &arm_plans, &tree)
            }
        }
    }

    fn lower_decision_tree(
        &mut self,
        arms: &[sem::MatchArm],
        arm_plans: &[sem::MatchArmPlan],
        tree: &sem::MatchDecisionNode,
    ) -> Result<BranchResult, LoweringError> {
        if arms.len() != arm_plans.len() {
            panic!(
                "ssa match plan arm mismatch: {} arms vs {} plans",
                arms.len(),
                arm_plans.len()
            );
        }

        // Pre-allocate arm blocks and mark which arms are reachable in the tree.
        let mut arm_blocks = Vec::with_capacity(arms.len());
        for _ in 0..arms.len() {
            arm_blocks.push(self.lowerer.builder.add_block());
        }
        let mut reachable = vec![false; arms.len()];
        self.collect_reachable_arms(tree, &mut reachable);

        // Emit the decision tree starting from the current block.
        let entry_bb = self.lowerer.builder.current_block();
        self.emit_decision_node(tree, entry_bb, &arm_blocks)?;

        let join = self.lowerer.begin_join(self.expr);

        // Lower each reachable arm and branch into the join.
        let mut returned = vec![false; arms.len()];
        for (arm_index, (arm, arm_plan)) in arms.iter().zip(arm_plans).enumerate() {
            if !reachable[arm_index] {
                continue;
            }

            join.restore_locals(self.lowerer);
            self.lowerer.builder.select_block(arm_blocks[arm_index]);

            self.lower_bindings(&arm_plan.bindings)?;

            match self.lowerer.lower_branching_value_expr(&arm.body)? {
                BranchResult::Value(value) => {
                    join.emit_branch(self.lowerer, value, arm.body.span)?;
                }
                BranchResult::Return => {
                    returned[arm_index] = true;
                }
            }
        }

        let all_returned = reachable
            .iter()
            .zip(returned.iter())
            .all(|(is_reachable, did_return)| !is_reachable || *did_return);
        if all_returned {
            return Ok(BranchResult::Return);
        }

        let join_value = join.join_value();
        join.finalize(self.lowerer);
        Ok(BranchResult::Value(join_value))
    }

    fn lower_switch(
        &mut self,
        arms: &[sem::MatchArm],
        arm_plans: &[sem::MatchArmPlan],
        switch: &sem::MatchSwitch,
    ) -> Result<BranchResult, LoweringError> {
        if arms.len() != arm_plans.len() {
            panic!(
                "ssa match plan arm mismatch: {} arms vs {} plans",
                arms.len(),
                arm_plans.len()
            );
        }

        // Compute the discriminant value used by the switch.
        let (discr_value, discr_ty) = self.lower_discriminant(&switch.discr)?;

        // Pre-allocate arm blocks and record which arms are reachable.
        let mut arm_blocks = Vec::with_capacity(arms.len());
        let mut reachable = vec![false; arms.len()];
        for _ in 0..arms.len() {
            arm_blocks.push(self.lowerer.builder.add_block());
        }
        for case in &switch.cases {
            reachable[case.arm_index] = true;
        }
        if let Some(default_idx) = switch.default {
            reachable[default_idx] = true;
        }

        // Default target is either the wildcard arm or an unreachable block.
        let default_bb = match switch.default {
            Some(index) => arm_blocks[index],
            None => self.lowerer.builder.add_block(),
        };

        // Build the switch terminator in the current block.
        let mut cases = Vec::with_capacity(switch.cases.len());
        for case in &switch.cases {
            let target = arm_blocks[case.arm_index];
            let value = self.case_const(case.value, &discr_ty);
            cases.push(SwitchCase {
                value,
                target,
                args: Vec::new(),
            });
        }
        self.lowerer.builder.terminate(Terminator::Switch {
            value: discr_value,
            cases,
            default: default_bb,
            default_args: Vec::new(),
        });

        // If there is no default arm, make the default block explicitly unreachable.
        if switch.default.is_none() {
            self.lowerer.builder.select_block(default_bb);
            self.lowerer.builder.terminate(Terminator::Unreachable);
        }

        let join = self.lowerer.begin_join(self.expr);

        // Lower each reachable arm and branch into the join.
        let mut returned = vec![false; arms.len()];
        for (arm_index, (arm, arm_plan)) in arms.iter().zip(arm_plans).enumerate() {
            if !reachable[arm_index] {
                continue;
            }

            join.restore_locals(self.lowerer);
            self.lowerer.builder.select_block(arm_blocks[arm_index]);

            self.lower_bindings(&arm_plan.bindings)?;

            match self.lowerer.lower_branching_value_expr(&arm.body)? {
                BranchResult::Value(value) => {
                    join.emit_branch(self.lowerer, value, arm.body.span)?;
                }
                BranchResult::Return => {
                    returned[arm_index] = true;
                }
            }
        }

        let all_returned = reachable
            .iter()
            .zip(returned.iter())
            .all(|(is_reachable, did_return)| !is_reachable || *did_return);
        if all_returned {
            return Ok(BranchResult::Return);
        }

        let join_value = join.join_value();
        join.finalize(self.lowerer);
        Ok(BranchResult::Value(join_value))
    }

    fn emit_decision_node(
        &mut self,
        node: &sem::MatchDecisionNode,
        entry_bb: crate::ssa::model::ir::BlockId,
        arm_blocks: &[crate::ssa::model::ir::BlockId],
    ) -> Result<(), LoweringError> {
        match node {
            sem::MatchDecisionNode::Leaf { arm_index } => {
                self.lowerer.builder.select_block(entry_bb);
                self.lowerer.builder.terminate(Terminator::Br {
                    target: arm_blocks[*arm_index],
                    args: Vec::new(),
                });
                Ok(())
            }
            sem::MatchDecisionNode::Unreachable => {
                self.lowerer.builder.select_block(entry_bb);
                self.lowerer.builder.terminate(Terminator::Unreachable);
                Ok(())
            }
            sem::MatchDecisionNode::Tests {
                tests,
                on_match,
                on_fail,
            } => {
                let on_match_bb = self.lowerer.builder.add_block();
                let on_fail_bb = self.lowerer.builder.add_block();

                // Emit child subtrees before wiring the test chain.
                self.emit_decision_node(on_match, on_match_bb, arm_blocks)?;
                self.emit_decision_node(on_fail, on_fail_bb, arm_blocks)?;

                self.emit_tests_chain(entry_bb, tests, on_match_bb, on_fail_bb)?;
                Ok(())
            }
        }
    }

    fn emit_tests_chain(
        &mut self,
        entry_bb: crate::ssa::model::ir::BlockId,
        tests: &[sem::MatchTest],
        on_match_bb: crate::ssa::model::ir::BlockId,
        on_fail_bb: crate::ssa::model::ir::BlockId,
    ) -> Result<(), LoweringError> {
        if tests.is_empty() {
            self.lowerer.builder.select_block(entry_bb);
            self.lowerer.builder.terminate(Terminator::Br {
                target: on_match_bb,
                args: Vec::new(),
            });
            return Ok(());
        }

        let mut current_bb = entry_bb;
        for (index, test) in tests.iter().enumerate() {
            self.lowerer.builder.select_block(current_bb);
            let cond = self.lower_test_cond(test)?;

            let then_bb = if index + 1 == tests.len() {
                on_match_bb
            } else {
                self.lowerer.builder.add_block()
            };

            // Each test either advances to the next test or jumps to the fail subtree.
            self.lowerer.builder.terminate(Terminator::CondBr {
                cond,
                then_bb,
                then_args: Vec::new(),
                else_bb: on_fail_bb,
                else_args: Vec::new(),
            });

            current_bb = then_bb;
        }

        Ok(())
    }

    fn lower_test_cond(&mut self, test: &sem::MatchTest) -> Result<ValueId, LoweringError> {
        let bool_ty = self.lowerer.type_lowerer.lower_type(&Type::Bool);

        match &test.kind {
            sem::MatchTestKind::Bool { value } => {
                let (lhs, lhs_ty) = self.lower_place_value(&test.place)?;
                let rhs = self.lowerer.builder.const_bool(*value, lhs_ty);
                Ok(self.lowerer.builder.cmp(CmpOp::Eq, lhs, rhs, bool_ty))
            }
            sem::MatchTestKind::Int {
                value,
                signed,
                bits,
            } => {
                let (lhs, lhs_ty) = self.lower_place_value(&test.place)?;
                let rhs = self
                    .lowerer
                    .builder
                    .const_int(*value as i128, *signed, *bits, lhs_ty);
                Ok(self.lowerer.builder.cmp(CmpOp::Eq, lhs, rhs, bool_ty))
            }
            sem::MatchTestKind::EnumTag { tag, .. } => {
                let (lhs, tag_ty) = self.lower_discriminant(&test.place)?;
                let tag_ir_ty = self.lowerer.type_lowerer.lower_type(&tag_ty);
                let rhs = self
                    .lowerer
                    .builder
                    .const_int(*tag as i128, false, 32, tag_ir_ty);
                Ok(self.lowerer.builder.cmp(CmpOp::Eq, lhs, rhs, bool_ty))
            }
        }
    }

    fn collect_reachable_arms(&self, node: &sem::MatchDecisionNode, reachable: &mut [bool]) {
        match node {
            sem::MatchDecisionNode::Leaf { arm_index } => {
                reachable[*arm_index] = true;
            }
            sem::MatchDecisionNode::Tests {
                on_match, on_fail, ..
            } => {
                self.collect_reachable_arms(on_match, reachable);
                self.collect_reachable_arms(on_fail, reachable);
            }
            sem::MatchDecisionNode::Unreachable => {}
        }
    }

    fn lower_discriminant(
        &mut self,
        discr: &sem::MatchPlace,
    ) -> Result<(ValueId, Type), LoweringError> {
        let (addr, ty) = self.lower_place_addr(discr)?;

        // Enums are lowered as tagged structs in SSA, so extract field 0.
        if let Type::Enum { .. } = ty {
            let enum_ty_id = self
                .lowerer
                .type_map
                .type_table()
                .lookup_id(&ty)
                .unwrap_or_else(|| panic!("ssa match missing enum type id for {:?}", ty));
            let tag_ty = {
                let layout = self.lowerer.type_lowerer.enum_layout(enum_ty_id);
                layout.tag_ty
            };
            let tag_ptr = self.lowerer.field_addr_typed(addr, 0, tag_ty);
            let tag_val = self.lowerer.builder.load(tag_ptr, tag_ty);
            return Ok((
                tag_val,
                Type::Int {
                    signed: false,
                    bits: 32,
                },
            ));
        }

        let ir_ty = self.lowerer.type_lowerer.lower_type(&ty);
        let value = self.lowerer.builder.load(addr, ir_ty);
        Ok((value, ty))
    }

    fn lower_bindings(&mut self, bindings: &[sem::MatchBinding]) -> Result<(), LoweringError> {
        for binding in bindings {
            let (value, value_ty) = self.lower_place_value(&binding.source)?;
            self.lowerer
                .locals
                .insert(binding.def_id, LocalValue::value(value, value_ty));
        }
        Ok(())
    }

    fn lower_place_value(
        &mut self,
        place: &sem::MatchPlace,
    ) -> Result<(ValueId, IrTypeId), LoweringError> {
        let (addr, ty) = self.lower_place_addr(place)?;
        let ir_ty = self.lowerer.type_lowerer.lower_type(&ty);
        let value = self.lowerer.builder.load(addr, ir_ty);
        Ok((value, ir_ty))
    }

    fn lower_place_addr(
        &mut self,
        place: &sem::MatchPlace,
    ) -> Result<(ValueId, Type), LoweringError> {
        let mut addr = self.scrutinee_addr;
        let mut curr_ty = self
            .lowerer
            .type_map
            .type_table()
            .get(self.scrutinee_ty_id)
            .clone();

        for proj in &place.projections {
            match proj {
                sem::MatchProjection::Deref => {
                    let elem_ty = match curr_ty {
                        Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } => elem_ty,
                        other => panic!("ssa match deref on non-pointer type {:?}", other),
                    };
                    let elem_ir_ty = self.lowerer.type_lowerer.lower_type(&elem_ty);
                    let ptr_ir_ty = self.lowerer.type_lowerer.ptr_to(elem_ir_ty);
                    addr = self.lowerer.builder.load(addr, ptr_ir_ty);
                    curr_ty = (*elem_ty).clone();
                }
                sem::MatchProjection::Field { index } => match &curr_ty {
                    Type::Tuple { .. } => {
                        let (field_ty, field_ir_ty) =
                            self.lowerer.tuple_field_from_type(&curr_ty, *index);
                        addr = self.lowerer.field_addr_typed(addr, *index, field_ir_ty);
                        curr_ty = field_ty;
                    }
                    Type::Struct { .. } => {
                        let (field_ty, field_ir_ty) =
                            self.lowerer.struct_field_from_index(&curr_ty, *index);
                        addr = self.lowerer.field_addr_typed(addr, *index, field_ir_ty);
                        curr_ty = field_ty;
                    }
                    Type::Enum { .. } => {
                        let enum_ty_id = self
                            .lowerer
                            .type_map
                            .type_table()
                            .lookup_id(&curr_ty)
                            .unwrap_or_else(|| {
                                panic!("ssa match missing enum type id for {:?}", curr_ty)
                            });
                        let (tag_ty, blob_ty) = {
                            let layout = self.lowerer.type_lowerer.enum_layout(enum_ty_id);
                            (layout.tag_ty, layout.blob_ty)
                        };
                        let field_ir_ty = match *index {
                            0 => tag_ty,
                            1 => blob_ty,
                            _ => panic!("ssa match enum field out of range {index}"),
                        };
                        addr = self.lowerer.field_addr_typed(addr, *index, field_ir_ty);
                        curr_ty = match *index {
                            0 => Type::Int {
                                signed: false,
                                bits: 32,
                            },
                            1 => Type::uint(8),
                            _ => unreachable!(),
                        };
                    }
                    other => panic!("ssa match field projection on {:?}", other),
                },
                sem::MatchProjection::ByteOffset { offset } => {
                    addr = self.lowerer.byte_offset_addr(addr, *offset as u64);
                    curr_ty = place.ty.clone();
                }
            }
        }

        Ok((addr, curr_ty))
    }

    fn case_const(&self, value: u64, discr_ty: &Type) -> ConstValue {
        match discr_ty {
            Type::Bool => ConstValue::Bool(value != 0),
            Type::Int { signed, bits } => ConstValue::Int {
                value: value as i128,
                signed: *signed,
                bits: *bits,
            },
            other => panic!("ssa match switch on unsupported type {:?}", other),
        }
    }
}
