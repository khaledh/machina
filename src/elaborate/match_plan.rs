//! Match expression planning.
//!
//! Pre-computes decision trees for pattern matching so that lowering can
//! emit switches and conditional branches without re-analyzing patterns.
//!
//! ## Match strategies
//!
//! - **Switch**: For enums, booleans, and integers where each arm matches a
//!   single value. Generates a switch on the discriminant.
//!
//! - **Decision tree**: For tuple patterns with nested tests. Generates a
//!   sequence of conditional branches that test each component.
//!
//! ## Components of a match plan
//!
//! - **Decision**: How to dispatch to the correct arm (switch table or tree)
//! - **Arm plans**: Per-arm bindings that extract values from the scrutinee
//! - **Match places**: Paths into the scrutinee (projections like `.0`, `.field`)
//!
//! The plan encodes everything lowering needs to emit efficient matching code
//! without re-examining pattern structure.

use crate::elaborate::elaborator::Elaborator;
use crate::tree::NodeId;
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::types::Type;

impl<'a> Elaborator<'a> {
    /// Build a complete match plan for a match expression.
    ///
    /// Analyzes the scrutinee type and arm patterns to produce either a
    /// switch-based or decision-tree-based dispatch strategy.
    pub(super) fn build_match_plan(
        &mut self,
        _match_id: NodeId,
        scrutinee: &norm::Expr,
        arms: &[norm::MatchArm],
    ) -> sem::MatchPlan {
        let scrutinee_ty = self.type_map.type_table().get(scrutinee.ty).clone();
        let (peeled_ty, deref_count) = scrutinee_ty.peel_heap_with_count();
        let scrutinee_place = self.scrutinee_place(&peeled_ty, deref_count);

        let (decision, arm_plans) = match &peeled_ty {
            Type::Tuple { .. } => self.build_tuple_match_plan(&peeled_ty, &scrutinee_place, arms),
            _ => self.build_switch_match_plan(&peeled_ty, &scrutinee_place, arms),
        };

        sem::MatchPlan {
            scrutinee_ty,
            decision,
            arms: arm_plans,
        }
    }

    /// Build a switch-based match plan for simple discriminant matching.
    ///
    /// Used for scalar enums, booleans, and integers where each arm tests a single value.
    fn build_switch_match_plan(
        &mut self,
        scrutinee_ty: &Type,
        scrutinee_place: &sem::MatchPlace,
        arms: &[norm::MatchArm],
    ) -> (sem::MatchDecision, Vec<sem::MatchArmPlan>) {
        let mut cases = Vec::new();
        let mut default = None;
        let mut arm_plans = Vec::with_capacity(arms.len());

        for (index, arm) in arms.iter().enumerate() {
            let mut bindings = Vec::new();
            match &arm.pattern {
                norm::MatchPattern::Wildcard { .. } => {
                    default = Some(index);
                }
                norm::MatchPattern::EnumVariant {
                    variant_name,
                    bindings: pattern_bindings,
                    ..
                } => {
                    let tag = scrutinee_ty.enum_variant_index(variant_name) as u64;
                    cases.push(sem::MatchSwitchCase {
                        value: tag,
                        arm_index: index,
                    });
                    self.collect_enum_bindings(
                        scrutinee_ty,
                        scrutinee_place,
                        variant_name,
                        pattern_bindings,
                        &mut bindings,
                    );
                }
                norm::MatchPattern::BoolLit { value, .. } => {
                    cases.push(sem::MatchSwitchCase {
                        value: u64::from(*value),
                        arm_index: index,
                    });
                }
                norm::MatchPattern::IntLit { value, .. } => {
                    cases.push(sem::MatchSwitchCase {
                        value: *value,
                        arm_index: index,
                    });
                }
                norm::MatchPattern::Binding { .. } | norm::MatchPattern::Tuple { .. } => {
                    panic!(
                        "compiler bug: unexpected match pattern in switch plan: {:?}",
                        arm.pattern
                    );
                }
            }

            arm_plans.push(sem::MatchArmPlan { bindings });
        }

        let discr = self.switch_discriminant(scrutinee_ty, scrutinee_place);
        let decision = sem::MatchDecision::Switch(sem::MatchSwitch {
            discr,
            cases,
            default,
        });
        (decision, arm_plans)
    }

    /// Build a decision-tree-based match plan for tuple patterns.
    ///
    /// Each arm may have multiple nested tests (one per tuple field).
    /// The decision tree tries each arm's tests in order.
    fn build_tuple_match_plan(
        &mut self,
        scrutinee_ty: &Type,
        scrutinee_place: &sem::MatchPlace,
        arms: &[norm::MatchArm],
    ) -> (sem::MatchDecision, Vec<sem::MatchArmPlan>) {
        let mut decisions = Vec::with_capacity(arms.len());
        let mut arm_plans = Vec::with_capacity(arms.len());

        for (index, arm) in arms.iter().enumerate() {
            let mut tests = Vec::new();
            let mut bindings = Vec::new();

            match &arm.pattern {
                norm::MatchPattern::Wildcard { .. } => {}
                norm::MatchPattern::Tuple { patterns, .. } => {
                    self.collect_tuple_pattern_tests(
                        scrutinee_ty,
                        scrutinee_place,
                        patterns,
                        arm.id,
                        &mut tests,
                    );
                    self.collect_tuple_bindings(
                        scrutinee_ty,
                        scrutinee_place,
                        patterns,
                        &mut bindings,
                    );
                }
                _ => {
                    panic!(
                        "compiler bug: unexpected match pattern in tuple plan: {:?}",
                        arm.pattern
                    );
                }
            }

            decisions.push((index, tests));
            arm_plans.push(sem::MatchArmPlan { bindings });
        }

        let tree = self.build_decision_tree(&decisions);
        (sem::MatchDecision::DecisionTree(tree), arm_plans)
    }

    /// Recursively build a decision tree from a list of arms with their tests.
    ///
    /// Arms are tried in order. If the first arm's tests all pass, it matches;
    /// otherwise, recursively try the remaining arms.
    fn build_decision_tree(&self, arms: &[(usize, Vec<sem::MatchTest>)]) -> sem::MatchDecisionNode {
        if arms.is_empty() {
            return sem::MatchDecisionNode::Unreachable;
        }

        let (arm_index, tests) = &arms[0];
        if tests.is_empty() {
            return sem::MatchDecisionNode::Leaf {
                arm_index: *arm_index,
            };
        }

        sem::MatchDecisionNode::Tests {
            tests: tests.clone(),
            on_match: Box::new(sem::MatchDecisionNode::Leaf {
                arm_index: *arm_index,
            }),
            on_fail: Box::new(self.build_decision_tree(&arms[1..])),
        }
    }

    /// Compute the place of the discriminant value for a switch.
    ///
    /// For scalar enums, bools, and ints, the scrutinee itself is the discriminant.
    /// For non-scalar enums, the tag is at field index 0.
    fn switch_discriminant(
        &self,
        scrutinee_ty: &Type,
        scrutinee_place: &sem::MatchPlace,
    ) -> sem::MatchPlace {
        match scrutinee_ty {
            Type::Enum { .. } => {
                if scrutinee_ty.is_scalar() {
                    scrutinee_place.clone()
                } else {
                    self.project_place(
                        scrutinee_place,
                        sem::MatchProjection::Field { index: 0 },
                        Type::uint(64),
                    )
                }
            }
            Type::Bool | Type::Int { .. } => scrutinee_place.clone(),
            _ => panic!("compiler bug: unexpected scrutinee type for match switch"),
        }
    }

    /// Create the base match place for the scrutinee, applying any needed derefs
    /// (e.g., for heap-allocated scrutinees).
    fn scrutinee_place(&self, ty: &Type, deref_count: usize) -> sem::MatchPlace {
        let mut projections = Vec::with_capacity(deref_count);
        projections.extend(std::iter::repeat(sem::MatchProjection::Deref).take(deref_count));
        sem::MatchPlace {
            base: sem::MatchBase::Scrutinee,
            projections,
            ty: ty.clone(),
        }
    }

    /// Extend a match place with an additional projection (field access, deref, etc.).
    fn project_place(
        &self,
        place: &sem::MatchPlace,
        projection: sem::MatchProjection,
        ty: Type,
    ) -> sem::MatchPlace {
        let mut projections = place.projections.clone();
        projections.push(projection);
        sem::MatchPlace {
            base: place.base.clone(),
            projections,
            ty,
        }
    }

    /// Extend a match place with multiple deref projections (for nested heap types).
    fn apply_deref_place(
        &self,
        place: &sem::MatchPlace,
        deref_count: usize,
        ty: Type,
    ) -> sem::MatchPlace {
        let mut projections = place.projections.clone();
        projections.extend(std::iter::repeat(sem::MatchProjection::Deref).take(deref_count));
        sem::MatchPlace {
            base: place.base.clone(),
            projections,
            ty,
        }
    }

    /// Collect tests for each field in a tuple pattern.
    fn collect_tuple_pattern_tests(
        &self,
        tuple_ty: &Type,
        tuple_place: &sem::MatchPlace,
        patterns: &[norm::MatchPattern],
        arm_id: NodeId,
        tests: &mut Vec<sem::MatchTest>,
    ) {
        let Type::Tuple { field_tys } = tuple_ty else {
            panic!("compiler bug: expected tuple type for match tests");
        };
        if patterns.len() != field_tys.len() {
            panic!("compiler bug: tuple pattern arity mismatch in arm {arm_id}");
        }

        for (index, (pattern, field_ty)) in patterns.iter().zip(field_tys.iter()).enumerate() {
            let field_place = self.project_place(
                tuple_place,
                sem::MatchProjection::Field { index },
                field_ty.clone(),
            );
            self.collect_pattern_tests(field_ty, &field_place, pattern, arm_id, tests);
        }
    }

    /// Generate a test for a single pattern element (literal, enum tag, or nested tuple).
    fn collect_pattern_tests(
        &self,
        field_ty: &Type,
        field_place: &sem::MatchPlace,
        pattern: &norm::MatchPattern,
        arm_id: NodeId,
        tests: &mut Vec<sem::MatchTest>,
    ) {
        match pattern {
            norm::MatchPattern::Binding { .. } | norm::MatchPattern::Wildcard { .. } => {}
            norm::MatchPattern::BoolLit { value, .. } => {
                let (peeled_ty, deref_count) = field_ty.peel_heap_with_count();
                if !matches!(peeled_ty, Type::Bool) {
                    panic!("compiler bug: bool pattern on non-bool in arm {arm_id}");
                }
                let place = self.apply_deref_place(field_place, deref_count, peeled_ty.clone());
                tests.push(sem::MatchTest {
                    place,
                    kind: sem::MatchTestKind::Bool { value: *value },
                });
            }
            norm::MatchPattern::IntLit { value, .. } => {
                let (peeled_ty, deref_count) = field_ty.peel_heap_with_count();
                let Type::Int { signed, bits } = peeled_ty else {
                    panic!("compiler bug: int pattern on non-int in arm {arm_id}");
                };
                let place = self.apply_deref_place(field_place, deref_count, peeled_ty.clone());
                tests.push(sem::MatchTest {
                    place,
                    kind: sem::MatchTestKind::Int {
                        value: *value,
                        signed,
                        bits,
                    },
                });
            }
            norm::MatchPattern::EnumVariant { variant_name, .. } => {
                let (peeled_ty, deref_count) = field_ty.peel_heap_with_count();
                let Type::Enum { .. } = &peeled_ty else {
                    panic!("compiler bug: enum pattern on non-enum in arm {arm_id}");
                };
                let tag = peeled_ty.enum_variant_index(variant_name) as u64;
                let place = self.apply_deref_place(field_place, deref_count, peeled_ty.clone());
                tests.push(sem::MatchTest {
                    place,
                    kind: sem::MatchTestKind::EnumTag {
                        tag,
                        is_scalar: peeled_ty.is_scalar(),
                    },
                });
            }
            norm::MatchPattern::Tuple { patterns, .. } => {
                let (peeled_ty, deref_count) = field_ty.peel_heap_with_count();
                let Type::Tuple { .. } = &peeled_ty else {
                    panic!("compiler bug: tuple pattern on non-tuple in arm {arm_id}");
                };
                let place = self.apply_deref_place(field_place, deref_count, peeled_ty.clone());
                self.collect_tuple_pattern_tests(&peeled_ty, &place, patterns, arm_id, tests);
            }
        }
    }

    /// Collect variable bindings for each field in a tuple pattern.
    fn collect_tuple_bindings(
        &self,
        tuple_ty: &Type,
        tuple_place: &sem::MatchPlace,
        patterns: &[norm::MatchPattern],
        bindings: &mut Vec<sem::MatchBinding>,
    ) {
        let Type::Tuple { field_tys } = tuple_ty else {
            panic!("compiler bug: expected tuple type for match bindings");
        };
        if patterns.len() != field_tys.len() {
            panic!("compiler bug: tuple pattern arity mismatch");
        }

        for (index, (pattern, field_ty)) in patterns.iter().zip(field_tys.iter()).enumerate() {
            let field_place = self.project_place(
                tuple_place,
                sem::MatchProjection::Field { index },
                field_ty.clone(),
            );
            match pattern {
                norm::MatchPattern::Binding { id, def_id, .. } => {
                    bindings.push(sem::MatchBinding {
                        def_id: *def_id,
                        node_id: *id,
                        source: field_place,
                    });
                }
                norm::MatchPattern::Wildcard { .. }
                | norm::MatchPattern::BoolLit { .. }
                | norm::MatchPattern::IntLit { .. } => {}
                norm::MatchPattern::EnumVariant {
                    variant_name,
                    bindings: pattern_bindings,
                    ..
                } => {
                    let (peeled_ty, deref_count) = field_ty.peel_heap_with_count();
                    let Type::Enum { .. } = &peeled_ty else {
                        panic!("compiler bug: enum pattern on non-enum in tuple binding");
                    };
                    let enum_place =
                        self.apply_deref_place(&field_place, deref_count, peeled_ty.clone());
                    self.collect_enum_bindings(
                        &peeled_ty,
                        &enum_place,
                        variant_name,
                        pattern_bindings,
                        bindings,
                    );
                }
                norm::MatchPattern::Tuple { patterns, .. } => {
                    let (peeled_ty, deref_count) = field_ty.peel_heap_with_count();
                    let Type::Tuple { .. } = &peeled_ty else {
                        panic!("compiler bug: tuple pattern on non-tuple in tuple binding");
                    };
                    let nested_place =
                        self.apply_deref_place(&field_place, deref_count, peeled_ty.clone());
                    self.collect_tuple_bindings(&peeled_ty, &nested_place, patterns, bindings);
                }
            }
        }
    }

    /// Extract bindings for an enum variant's payload fields.
    ///
    /// Computes byte offsets into the enum's payload area for each bound field.
    fn collect_enum_bindings(
        &self,
        enum_ty: &Type,
        enum_place: &sem::MatchPlace,
        variant_name: &str,
        pattern_bindings: &[norm::MatchPatternBinding],
        bindings: &mut Vec<sem::MatchBinding>,
    ) {
        let Type::Enum { variants, .. } = enum_ty else {
            panic!("compiler bug: expected enum type for match bindings");
        };

        let variant = variants
            .iter()
            .find(|variant| variant.name == variant_name)
            .unwrap_or_else(|| panic!("compiler bug: unknown enum variant {variant_name}"));
        let offsets = enum_ty.enum_variant_payload_offsets(variant_name);

        for (index, (binding, payload_ty)) in pattern_bindings
            .iter()
            .zip(variant.payload.iter())
            .enumerate()
        {
            let norm::MatchPatternBinding::Named { id, def_id, .. } = binding else {
                continue;
            };

            let mut projections = enum_place.projections.clone();
            projections.push(sem::MatchProjection::Field { index: 1 });
            projections.push(sem::MatchProjection::ByteOffset {
                offset: offsets[index],
            });
            bindings.push(sem::MatchBinding {
                def_id: *def_id,
                node_id: *id,
                source: sem::MatchPlace {
                    base: enum_place.base.clone(),
                    projections,
                    ty: payload_ty.clone(),
                },
            });
        }
    }
}
