use std::collections::HashSet;

use crate::context::NormalizedContext;
use crate::diag::Span;
use crate::semck::SemCheckError;
use crate::tree::normalized::{Expr, MatchArm};
use crate::tree::resolved::MatchPattern;
use crate::typecheck::type_map::resolve_type_expr;
use crate::types::{EnumVariant, Type};

pub(super) fn check_match(
    ctx: &NormalizedContext,
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

    let scrutinee_ty = ctx.type_map.type_table().get(scrutinee.ty);
    let peeled_ty = scrutinee_ty.peel_heap();

    let rule = MatchRuleKind::for_type(&peeled_ty);
    rule.check(ctx, scrutinee_ty, arms, span, scrutinee.span, errors);
}

enum MatchRuleKind<'a> {
    Enum(EnumRule<'a>),
    Union(UnionRule<'a>),
    Bool,
    Int(IntRule),
    Tuple(TupleRule<'a>),
    Unsupported,
}

impl<'a> MatchRuleKind<'a> {
    fn for_type(ty: &'a Type) -> Self {
        match ty {
            Type::Enum { name, variants } => Self::Enum(EnumRule { name, variants }),
            Type::ErrorUnion { ok_ty, err_tys } => Self::Union(UnionRule { ok_ty, err_tys }),
            Type::Bool => Self::Bool,
            Type::Int { signed, bits, .. } => Self::Int(IntRule {
                signed: *signed,
                bits: *bits,
            }),
            Type::Tuple { field_tys } => Self::Tuple(TupleRule { field_tys }),
            _ => Self::Unsupported,
        }
    }

    fn check(
        &self,
        _ctx: &NormalizedContext,
        scrutinee_ty: &Type,
        arms: &[MatchArm],
        span: Span,
        scrutinee_span: Span,
        errors: &mut Vec<SemCheckError>,
    ) {
        match self {
            MatchRuleKind::Enum(rule) => rule.check(arms, span, errors),
            MatchRuleKind::Union(rule) => rule.check(_ctx, arms, span, errors),
            MatchRuleKind::Bool => check_bool_match(arms, span, errors),
            MatchRuleKind::Int(rule) => rule.check(arms, span, errors),
            MatchRuleKind::Tuple(rule) => rule.check(arms, span, errors),
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
                    type_args: _,
                    variant_name,
                    bindings,
                    span,
                    ..
                } => {
                    if let Some(pat_enum_name) = pat_enum_name
                        && !enum_name_matches(pat_enum_name, self.name)
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

struct UnionRule<'a> {
    ok_ty: &'a Type,
    err_tys: &'a [Type],
}

impl<'a> UnionRule<'a> {
    fn check(
        &self,
        ctx: &NormalizedContext,
        arms: &[MatchArm],
        span: Span,
        errors: &mut Vec<SemCheckError>,
    ) {
        let mut seen_variant_indices = HashSet::new();
        let mut has_wildcard = false;
        let union_ty = self.full_type();

        for arm in arms {
            match &arm.pattern {
                MatchPattern::Wildcard { .. } => {
                    has_wildcard = true;
                }
                MatchPattern::TypedBinding { ty_expr, span, .. } => {
                    let arm_ty = ctx
                        .type_map
                        .lookup_node_type(ty_expr.id)
                        .or_else(|| resolve_type_expr(&ctx.def_table, &ctx.module, ty_expr).ok());
                    let Some(arm_ty) = arm_ty else {
                        continue;
                    };

                    let Some(index) = self.variant_index(&arm_ty) else {
                        errors.push(SemCheckError::InvalidMatchPattern(union_ty.clone(), *span));
                        continue;
                    };

                    if !seen_variant_indices.insert(index) {
                        errors.push(SemCheckError::DuplicateMatchVariant(
                            arm_ty.to_string(),
                            *span,
                        ));
                    }
                }
                _ => {
                    errors.push(SemCheckError::InvalidMatchPattern(
                        union_ty.clone(),
                        pattern_span(&arm.pattern),
                    ));
                }
            }
        }

        let variant_count = 1 + self.err_tys.len();
        if !has_wildcard && seen_variant_indices.len() != variant_count {
            let mut missing = Vec::new();
            if !seen_variant_indices.contains(&0) {
                missing.push(self.ok_ty.clone());
            }
            for (idx, err_ty) in self.err_tys.iter().enumerate() {
                if !seen_variant_indices.contains(&(idx + 1)) {
                    missing.push(err_ty.clone());
                }
            }
            errors.push(SemCheckError::NonExhaustiveUnionMatch(missing, span));
        }
    }

    fn variant_index(&self, ty: &Type) -> Option<usize> {
        if ty == self.ok_ty {
            return Some(0);
        }
        self.err_tys
            .iter()
            .position(|err_ty| err_ty == ty)
            .map(|idx| idx + 1)
    }

    fn full_type(&self) -> Type {
        Type::ErrorUnion {
            ok_ty: Box::new(self.ok_ty.clone()),
            err_tys: self.err_tys.to_vec(),
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
                            bounds: None,
                            nonzero: false,
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

struct TupleRule<'a> {
    field_tys: &'a [Type],
}

impl<'a> TupleRule<'a> {
    fn check(&self, arms: &[MatchArm], span: Span, errors: &mut Vec<SemCheckError>) {
        let mut has_wildcard = false;
        let mut all_irrefutable = true;

        for arm in arms {
            match &arm.pattern {
                MatchPattern::Tuple { patterns, span } => {
                    self.check_tuple_pattern(self.field_tys, patterns, *span, errors);
                    if !pattern_is_irrefutable(&arm.pattern) {
                        all_irrefutable = false;
                    }
                }
                MatchPattern::Wildcard { .. } => {
                    has_wildcard = true;
                }
                _ => {
                    all_irrefutable = false;
                    errors.push(SemCheckError::InvalidMatchPattern(
                        Type::Tuple {
                            field_tys: self.field_tys.to_vec(),
                        },
                        pattern_span(&arm.pattern),
                    ));
                }
            }
        }

        if !has_wildcard && !all_irrefutable {
            errors.push(SemCheckError::NonExhaustiveMatch(span));
        }
    }

    fn check_tuple_pattern(
        &self,
        field_tys: &[Type],
        patterns: &[MatchPattern],
        span: Span,
        errors: &mut Vec<SemCheckError>,
    ) {
        if patterns.len() != field_tys.len() {
            errors.push(SemCheckError::TuplePatternArityMismatch(
                field_tys.len(),
                patterns.len(),
                span,
            ));
        }

        for (field_ty, pattern) in field_tys.iter().zip(patterns.iter()) {
            let peeled_ty = field_ty.peel_heap();
            match pattern {
                MatchPattern::Binding { .. }
                | MatchPattern::TypedBinding { .. }
                | MatchPattern::Wildcard { .. } => {}
                MatchPattern::BoolLit { span, .. } => {
                    if !matches!(peeled_ty, Type::Bool) {
                        errors.push(SemCheckError::InvalidMatchPattern(peeled_ty, *span));
                    }
                }
                MatchPattern::IntLit { value, span } => {
                    let Type::Int { signed, bits, .. } = peeled_ty else {
                        errors.push(SemCheckError::InvalidMatchPattern(peeled_ty, *span));
                        continue;
                    };
                    check_int_pattern_range(*value, signed, bits, *span, errors);
                }
                MatchPattern::EnumVariant { .. } => {
                    let Type::Enum { name, variants } = peeled_ty else {
                        errors.push(SemCheckError::InvalidMatchPattern(
                            peeled_ty,
                            pattern_span(pattern),
                        ));
                        continue;
                    };
                    check_enum_pattern(&name, &variants, pattern, errors);
                }
                MatchPattern::Tuple { patterns, span } => {
                    let Type::Tuple {
                        field_tys: nested_fields,
                    } = peeled_ty
                    else {
                        errors.push(SemCheckError::InvalidMatchPattern(peeled_ty, *span));
                        continue;
                    };
                    self.check_tuple_pattern(&nested_fields, patterns, *span, errors);
                }
            }
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

    if !(has_wildcard || (saw_true && saw_false)) {
        errors.push(SemCheckError::NonExhaustiveMatch(span));
    }
}

fn pattern_span(pattern: &MatchPattern) -> Span {
    match pattern {
        MatchPattern::Wildcard { span }
        | MatchPattern::BoolLit { span, .. }
        | MatchPattern::IntLit { span, .. }
        | MatchPattern::Binding { span, .. }
        | MatchPattern::TypedBinding { span, .. }
        | MatchPattern::Tuple { span, .. }
        | MatchPattern::EnumVariant { span, .. } => *span,
    }
}

fn pattern_is_irrefutable(pattern: &MatchPattern) -> bool {
    match pattern {
        MatchPattern::Wildcard { .. }
        | MatchPattern::Binding { .. }
        | MatchPattern::TypedBinding { .. } => true,
        MatchPattern::Tuple { patterns, .. } => patterns.iter().all(pattern_is_irrefutable),
        _ => false,
    }
}

fn check_int_pattern_range(
    value: u64,
    signed: bool,
    bits: u8,
    span: Span,
    errors: &mut Vec<SemCheckError>,
) {
    let max_value = if signed {
        if bits >= 64 {
            i64::MAX as u64
        } else {
            (1u64 << (bits - 1)) - 1
        }
    } else if bits >= 64 {
        u64::MAX
    } else {
        (1u64 << bits) - 1
    };

    if value > max_value {
        errors.push(SemCheckError::ValueOutOfRange(
            value as i128,
            0,
            (max_value as i128) + 1,
            span,
        ));
    }
}

fn enum_name_matches(pat_enum_name: &str, enum_name: &str) -> bool {
    pat_enum_name == enum_name
        || enum_name
            .split_once('<')
            .map_or(false, |(base, _)| base == pat_enum_name)
}

fn check_enum_pattern(
    enum_name: &str,
    variants: &[EnumVariant],
    pattern: &MatchPattern,
    errors: &mut Vec<SemCheckError>,
) {
    let MatchPattern::EnumVariant {
        enum_name: pat_enum_name,
        type_args: _,
        variant_name,
        bindings,
        span,
        ..
    } = pattern
    else {
        return;
    };

    if let Some(pat_enum_name) = pat_enum_name
        && !enum_name_matches(pat_enum_name, enum_name)
    {
        errors.push(SemCheckError::MatchPatternEnumMismatch(
            enum_name.to_string(),
            pat_enum_name.clone(),
            *span,
        ));
    }

    let Some(variant) = variants.iter().find(|v| v.name == *variant_name) else {
        errors.push(SemCheckError::UnknownEnumVariant(
            enum_name.to_string(),
            variant_name.clone(),
            *span,
        ));
        return;
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
