use std::collections::HashSet;

use crate::ast::{Expr, MatchArm, MatchPattern};
use crate::context::TypeCheckedContext;
use crate::diag::Span;
use crate::semck::SemCheckError;
use crate::types::{EnumVariant, Type};

pub(super) fn check_match(
    ctx: &TypeCheckedContext,
    scrutinee: &Expr,
    arms: &[MatchArm],
    span: Span,
    errors: &mut Vec<SemCheckError>,
) {
    if let Some((index, arm)) = arms
        .iter()
        .enumerate()
        .find(|(_, arm)| matches!(arm.pattern, MatchPattern::Wildcard { .. }))
        && index + 1 != arms.len()
    {
        errors.push(SemCheckError::WildcardArmNotLast(pattern_span(
            &arm.pattern,
        )));
    }

    let Some(scrutinee_ty) = ctx.type_map.lookup_node_type(scrutinee.id) else {
        return;
    };

    let mut peeled_ty = scrutinee_ty.clone();
    while let Type::Heap { elem_ty } = peeled_ty {
        peeled_ty = *elem_ty;
    }

    let rule = MatchRuleKind::for_type(&peeled_ty);
    rule.check(ctx, &scrutinee_ty, arms, span, scrutinee.span, errors);
}

enum MatchRuleKind<'a> {
    Enum(EnumRule<'a>),
    Bool,
    Int(IntRule),
    Unsupported,
}

impl<'a> MatchRuleKind<'a> {
    fn for_type(ty: &'a Type) -> Self {
        match ty {
            Type::Enum { name, variants } => Self::Enum(EnumRule { name, variants }),
            Type::Bool => Self::Bool,
            Type::Int { signed, bits } => Self::Int(IntRule {
                signed: *signed,
                bits: *bits,
            }),
            _ => Self::Unsupported,
        }
    }

    fn check(
        &self,
        _ctx: &TypeCheckedContext,
        scrutinee_ty: &Type,
        arms: &[MatchArm],
        span: Span,
        scrutinee_span: Span,
        errors: &mut Vec<SemCheckError>,
    ) {
        match self {
            MatchRuleKind::Enum(rule) => rule.check(arms, span, errors),
            MatchRuleKind::Bool => check_bool_match(arms, span, errors),
            MatchRuleKind::Int(rule) => rule.check(arms, span, errors),
            MatchRuleKind::Unsupported => {
                errors.push(SemCheckError::MatchTargetNotEnum(
                    scrutinee_ty.clone(),
                    scrutinee_span,
                ));
            }
        }
    }
}

struct EnumRule<'a> {
    name: &'a str,
    variants: &'a [EnumVariant],
}

impl<'a> EnumRule<'a> {
    fn check(&self, arms: &[MatchArm], span: Span, errors: &mut Vec<SemCheckError>) {
        let mut seen_variants = HashSet::new();
        let mut has_wildcard = false;

        for arm in arms {
            match &arm.pattern {
                MatchPattern::Wildcard { .. } => {
                    has_wildcard = true;
                }
                MatchPattern::EnumVariant {
                    enum_name: pat_enum_name,
                    variant_name,
                    bindings,
                    span,
                } => {
                    if let Some(pat_enum_name) = pat_enum_name
                        && pat_enum_name != self.name
                    {
                        errors.push(SemCheckError::MatchPatternEnumMismatch(
                            self.name.to_string(),
                            pat_enum_name.clone(),
                            *span,
                        ));
                    }

                    if !seen_variants.insert(variant_name.clone()) {
                        errors.push(SemCheckError::DuplicateMatchVariant(
                            variant_name.clone(),
                            *span,
                        ));
                    }

                    let Some(variant) = self.variants.iter().find(|v| v.name == *variant_name)
                    else {
                        errors.push(SemCheckError::UnknownEnumVariant(
                            self.name.to_string(),
                            variant_name.clone(),
                            *span,
                        ));
                        continue;
                    };

                    if bindings.len() != variant.payload.len() {
                        errors.push(SemCheckError::EnumVariantPayloadArityMismatch(
                            variant_name.clone(),
                            variant.payload.len(),
                            bindings.len(),
                            *span,
                        ));
                    }
                }
                _ => {
                    errors.push(SemCheckError::InvalidMatchPattern(
                        Type::Enum {
                            name: self.name.to_string(),
                            variants: self.variants.to_vec(),
                        },
                        pattern_span(&arm.pattern),
                    ));
                }
            }
        }

        if !has_wildcard && seen_variants.len() != self.variants.len() {
            errors.push(SemCheckError::NonExhaustiveMatch(span));
        }
    }
}

struct IntRule {
    signed: bool,
    bits: u8,
}

impl IntRule {
    fn check(&self, arms: &[MatchArm], span: Span, errors: &mut Vec<SemCheckError>) {
        let mut seen = HashSet::new();
        let mut has_wildcard = false;
        let max_value = self.max_value();

        for arm in arms {
            match &arm.pattern {
                MatchPattern::Wildcard { .. } => {
                    has_wildcard = true;
                }
                MatchPattern::IntLit { value, span } => {
                    if *value > max_value {
                        errors.push(SemCheckError::ValueOutOfRange(
                            *value as i128,
                            0,
                            (max_value as i128) + 1,
                            *span,
                        ));
                        continue;
                    }

                    if !seen.insert(*value) {
                        errors.push(SemCheckError::DuplicateMatchVariant(
                            value.to_string(),
                            *span,
                        ));
                    }
                }
                _ => {
                    errors.push(SemCheckError::InvalidMatchPattern(
                        Type::Int {
                            signed: self.signed,
                            bits: self.bits,
                        },
                        pattern_span(&arm.pattern),
                    ));
                }
            }
        }

        if !has_wildcard {
            errors.push(SemCheckError::NonExhaustiveMatch(span));
        }
    }

    fn max_value(&self) -> u64 {
        if self.signed {
            if self.bits >= 64 {
                i64::MAX as u64
            } else {
                (1u64 << (self.bits - 1)) - 1
            }
        } else if self.bits >= 64 {
            u64::MAX
        } else {
            (1u64 << self.bits) - 1
        }
    }
}

fn check_bool_match(arms: &[MatchArm], span: Span, errors: &mut Vec<SemCheckError>) {
    let mut saw_true = false;
    let mut saw_false = false;
    let mut has_wildcard = false;

    for arm in arms {
        match &arm.pattern {
            MatchPattern::Wildcard { .. } => {
                has_wildcard = true;
            }
            MatchPattern::BoolLit { value, span } => {
                let seen = if *value {
                    &mut saw_true
                } else {
                    &mut saw_false
                };
                if *seen {
                    errors.push(SemCheckError::DuplicateMatchVariant(
                        value.to_string(),
                        *span,
                    ));
                }
                *seen = true;
            }
            _ => {
                errors.push(SemCheckError::InvalidMatchPattern(
                    Type::Bool,
                    pattern_span(&arm.pattern),
                ));
            }
        }
    }

    if !has_wildcard && !(saw_true && saw_false) {
        errors.push(SemCheckError::NonExhaustiveMatch(span));
    }
}

fn pattern_span(pattern: &MatchPattern) -> Span {
    match pattern {
        MatchPattern::Wildcard { span }
        | MatchPattern::BoolLit { span, .. }
        | MatchPattern::IntLit { span, .. }
        | MatchPattern::EnumVariant { span, .. } => *span,
    }
}
