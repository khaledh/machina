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

use crate::core::elaborate::elaborator::Elaborator;
use crate::core::tree::NodeId;
use crate::core::tree::normalized as norm;
use crate::core::tree::semantic as sem;
use crate::core::types::Type;

impl<'a> Elaborator<'a> {
    /// Build a complete match plan for a match expression.
    ///
    /// Analyzes the scrutinee type and arm patterns to produce either a
    /// switch-based or decision-tree-based dispatch strategy.
    pub(super) fn build_match_plan(
        &mut self,
        _match_id: NodeId,
        scrutinee_ty: Type,
        arms: &[norm::MatchArm],
    ) -> sem::MatchPlan {
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
                norm::MatchPattern::TypedBinding {
                    id,
                    def_id,
                    ty_expr,
                    ..
                } => {
                    let Type::ErrorUnion { ok_ty, err_tys } = scrutinee_ty else {
                        panic!(
                            "compiler bug: typed binding pattern in switch plan requires error-union scrutinee"
                        );
                    };
                    let payload_ty = self
                        .type_map
                        .lookup_node_type(ty_expr.id)
                        .or_else(|| {
                            self.error_union_variant_from_type_expr(ok_ty, err_tys, ty_expr)
                                .cloned()
                        })
                        .unwrap_or_else(|| {
                            panic!(
                                "compiler bug: missing typed-binding type for match pattern {}",
                                ty_expr.id
                            )
                        });
                    let tag = self
                        .error_union_variant_index(ok_ty, err_tys, &payload_ty)
                        .unwrap_or_else(|| {
                            panic!(
                                "compiler bug: typed-binding pattern type {} is not a union variant",
                                payload_ty
                            )
                        });
                    cases.push(sem::MatchSwitchCase {
                        value: tag,
                        arm_index: index,
                    });
                    bindings.push(sem::MatchBinding {
                        def_id: *def_id,
                        node_id: *id,
                        source: self.error_union_payload_place(scrutinee_place, payload_ty),
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
            Type::ErrorUnion { .. } => self.project_place(
                scrutinee_place,
                sem::MatchProjection::Field { index: 0 },
                Type::uint(64),
            ),
            Type::Bool | Type::Int { .. } => scrutinee_place.clone(),
            _ => panic!("compiler bug: unexpected scrutinee type for match switch"),
        }
    }

    fn error_union_variant_index(
        &self,
        ok_ty: &Type,
        err_tys: &[Type],
        pattern_ty: &Type,
    ) -> Option<u64> {
        if pattern_ty == ok_ty {
            return Some(0);
        }
        err_tys
            .iter()
            .position(|err_ty| err_ty == pattern_ty)
            .map(|idx| (idx + 1) as u64)
    }

    fn error_union_payload_place(
        &self,
        scrutinee_place: &sem::MatchPlace,
        payload_ty: Type,
    ) -> sem::MatchPlace {
        let mut place = self.project_place(
            scrutinee_place,
            sem::MatchProjection::Field { index: 1 },
            Type::uint(8),
        );
        place
            .projections
            .push(sem::MatchProjection::ByteOffset { offset: 0 });
        place.ty = payload_ty;
        place
    }

    fn error_union_variant_from_type_expr<'b>(
        &self,
        ok_ty: &'b Type,
        err_tys: &'b [Type],
        ty_expr: &norm::TypeExpr,
    ) -> Option<&'b Type> {
        let norm::TypeExprKind::Named { ident, .. } = &ty_expr.kind else {
            return None;
        };
        std::iter::once(ok_ty)
            .chain(err_tys.iter())
            .find(|variant_ty| type_name_matches(ident, variant_ty))
    }

    /// Create the base match place for the scrutinee, applying any needed derefs
    /// (e.g., for heap-allocated scrutinees).
    fn scrutinee_place(&self, ty: &Type, deref_count: usize) -> sem::MatchPlace {
        let mut projections = Vec::with_capacity(deref_count);
        projections.extend(std::iter::repeat_n(
            sem::MatchProjection::Deref,
            deref_count,
        ));
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
        projections.extend(std::iter::repeat_n(
            sem::MatchProjection::Deref,
            deref_count,
        ));
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
            norm::MatchPattern::Binding { .. }
            | norm::MatchPattern::TypedBinding { .. }
            | norm::MatchPattern::Wildcard { .. } => {}
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
                let Type::Int { signed, bits, .. } = peeled_ty else {
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
                norm::MatchPattern::TypedBinding { id, def_id, .. } => {
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

fn type_name_matches(name: &str, ty: &Type) -> bool {
    match ty {
        Type::Unit => name == "()",
        Type::Int {
            signed: false,
            bits,
            ..
        } => name == format!("u{bits}"),
        Type::Int {
            signed: true, bits, ..
        } => name == format!("i{bits}"),
        Type::Bool => name == "bool",
        Type::Char => name == "char",
        Type::String => name == "string",
        Type::Struct { name: ty_name, .. } | Type::Enum { name: ty_name, .. } => {
            ty_name == name
                || ty_name
                    .split_once('<')
                    .is_some_and(|(base, _)| base == name)
        }
        _ => false,
    }
}
