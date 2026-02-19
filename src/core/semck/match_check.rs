use std::collections::HashSet;

use crate::core::context::NormalizedContext;
use crate::core::diag::Span;
use crate::core::semck::{SEK, SemCheckError, push_error};
use crate::core::tree::normalized::{Expr, MatchArm};
use crate::core::tree::resolved::MatchPattern;
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::types::{EnumVariant, Type};

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
        errors.push(SEK::WildcardArmNotLast.at(pattern_span(&arm.pattern)));
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
                push_error(
                    errors,
                    scrutinee_span,
                    SEK::MatchTargetNotEnum(scrutinee_ty.clone()),
                );
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
                        push_error(
                            errors,
                            *span,
                            SEK::MatchPatternEnumMismatch(
                                self.name.to_string(),
                                pat_enum_name.clone(),
                            ),
                        );
                    }

                    if !seen_variants.insert(variant_name.clone()) {
                        push_error(
                            errors,
                            *span,
                            SEK::DuplicateMatchVariant(variant_name.clone()),
                        );
                    }

                    let Some(variant) = self.variants.iter().find(|v| v.name == *variant_name)
                    else {
                        push_error(
                            errors,
                            *span,
                            SEK::UnknownEnumVariant(self.name.to_string(), variant_name.clone()),
                        );
                        continue;
                    };

                    if bindings.len() != variant.payload.len() {
                        push_error(
                            errors,
                            *span,
                            SEK::EnumVariantPayloadArityMismatch(
                                variant_name.clone(),
                                variant.payload.len(),
                                bindings.len(),
                            ),
                        );
                    }
                }
                _ => {
                    errors.push(
                        SEK::InvalidMatchPattern(Type::Enum {
                            name: self.name.to_string(),
                            variants: self.variants.to_vec(),
                        })
                        .at(pattern_span(&arm.pattern)),
                    );
                }
            }
        }

        if !has_wildcard && seen_variants.len() != self.variants.len() {
            push_error(errors, span, SEK::NonExhaustiveMatch);
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
                        push_error(errors, *span, SEK::InvalidMatchPattern(union_ty.clone()));
                        continue;
                    };

                    if !seen_variant_indices.insert(index) {
                        push_error(
                            errors,
                            *span,
                            SEK::DuplicateMatchVariant(arm_ty.to_string()),
                        );
                    }
                }
                _ => {
                    errors.push(
                        SEK::InvalidMatchPattern(union_ty.clone()).at(pattern_span(&arm.pattern)),
                    );
                }
            }
        }

        let variant_count = 1 + self.err_tys.len();
        if !has_wildcard && seen_variant_indices.len() != variant_count {
            let mut missing = Vec::new();
            if !seen_variant_indices.contains(&0) {
                missing.push(compact_type_name(self.ok_ty));
            }
            for (idx, err_ty) in self.err_tys.iter().enumerate() {
                if !seen_variant_indices.contains(&(idx + 1)) {
                    missing.push(compact_type_name(err_ty));
                }
            }
            push_error(errors, span, SEK::NonExhaustiveUnionMatch(missing));
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
                        push_error(
                            errors,
                            *span,
                            SEK::ValueOutOfRange(*value as i128, 0, (max_value as i128) + 1),
                        );
                        continue;
                    }

                    if !seen.insert(*value) {
                        push_error(errors, *span, SEK::DuplicateMatchVariant(value.to_string()));
                    }
                }
                _ => {
                    errors.push(
                        SEK::InvalidMatchPattern(Type::Int {
                            signed: self.signed,
                            bits: self.bits,
                            bounds: None,
                            nonzero: false,
                        })
                        .at(pattern_span(&arm.pattern)),
                    );
                }
            }
        }

        if !has_wildcard {
            push_error(errors, span, SEK::NonExhaustiveMatch);
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
                    errors.push(
                        SEK::InvalidMatchPattern(Type::Tuple {
                            field_tys: self.field_tys.to_vec(),
                        })
                        .at(pattern_span(&arm.pattern)),
                    );
                }
            }
        }

        if !has_wildcard && !all_irrefutable {
            push_error(errors, span, SEK::NonExhaustiveMatch);
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
            push_error(
                errors,
                span,
                SEK::TuplePatternArityMismatch(field_tys.len(), patterns.len()),
            );
        }

        for (field_ty, pattern) in field_tys.iter().zip(patterns.iter()) {
            let peeled_ty = field_ty.peel_heap();
            match pattern {
                MatchPattern::Binding { .. }
                | MatchPattern::TypedBinding { .. }
                | MatchPattern::Wildcard { .. } => {}
                MatchPattern::BoolLit { span, .. } => {
                    if !matches!(peeled_ty, Type::Bool) {
                        push_error(errors, *span, SEK::InvalidMatchPattern(peeled_ty));
                    }
                }
                MatchPattern::IntLit { value, span } => {
                    let Type::Int { signed, bits, .. } = peeled_ty else {
                        push_error(errors, *span, SEK::InvalidMatchPattern(peeled_ty));
                        continue;
                    };
                    check_int_pattern_range(*value, signed, bits, *span, errors);
                }
                MatchPattern::EnumVariant { .. } => {
                    let Type::Enum { name, variants } = peeled_ty else {
                        errors.push(SEK::InvalidMatchPattern(peeled_ty).at(pattern_span(pattern)));
                        continue;
                    };
                    check_enum_pattern(&name, &variants, pattern, errors);
                }
                MatchPattern::Tuple { patterns, span } => {
                    let Type::Tuple {
                        field_tys: nested_fields,
                    } = peeled_ty
                    else {
                        push_error(errors, *span, SEK::InvalidMatchPattern(peeled_ty));
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
                    push_error(errors, *span, SEK::DuplicateMatchVariant(value.to_string()));
                }
                *seen = true;
            }
            _ => {
                errors.push(SEK::InvalidMatchPattern(Type::Bool).at(pattern_span(&arm.pattern)));
            }
        }
    }

    if !(has_wildcard || (saw_true && saw_false)) {
        push_error(errors, span, SEK::NonExhaustiveMatch);
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
        push_error(
            errors,
            span,
            SEK::ValueOutOfRange(value as i128, 0, (max_value as i128) + 1),
        );
    }
}

fn compact_type_name(ty: &Type) -> String {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        Type::Int { signed, bits, .. } => format!("{}{}", if *signed { "i" } else { "u" }, bits),
        Type::Bool => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::Unit => "()".to_string(),
        _ => ty.to_string(),
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
        push_error(
            errors,
            *span,
            SEK::MatchPatternEnumMismatch(enum_name.to_string(), pat_enum_name.clone()),
        );
    }

    let Some(variant) = variants.iter().find(|v| v.name == *variant_name) else {
        push_error(
            errors,
            *span,
            SEK::UnknownEnumVariant(enum_name.to_string(), variant_name.clone()),
        );
        return;
    };

    if bindings.len() != variant.payload.len() {
        push_error(
            errors,
            *span,
            SEK::EnumVariantPayloadArityMismatch(
                variant_name.clone(),
                variant.payload.len(),
                bindings.len(),
            ),
        );
    }
}
