//! Centralized type compatibility rules.
//!
//! This module defines a small, table-driven set of conversion rules between
//! type categories ("tags"). Exact matches are handled directly, while
//! non-exact conversions consult per-source rule tables. Rules can inspect the
//! target type to carry parameters (e.g., range bounds) into the resulting
//! assignability decision.
//!
//! How to use:
//! - Call `type_assignable(from, to)` to decide if a conversion is allowed.
//! - Call `value_assignable(expr, from, to)` to validate value-sensitive rules.
//! - When adding a new conversion, add a rule in `type_rules_for` and a small
//!   helper like `foo_to_bar`. If it has value-dependent behavior, add a rule
//!   in `value_rules_for` as well.

use crate::ast::{Expr, ExprKind};
use crate::types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeAssignability {
    Exact,
    UInt64ToRange { min: u64, max: u64 },
    IntLitToUInt8,
    IntLitToUInt32,
    RangeToUInt64,
    Incompatible,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueAssignability {
    Assignable(TypeAssignability),
    ValueOutOfRange { value: u64, min: u64, max: u64 },
    Incompatible,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeTag {
    UInt8,
    UInt32,
    UInt64,
    Range,
    String,
    Other,
}

#[derive(Clone, Copy)]
struct TypeRule {
    target: TypeTag,
    apply: fn(&Type, &Type) -> Option<TypeAssignability>,
}

#[derive(Clone, Copy)]
struct ValueRule {
    target: TypeTag,
    apply: fn(&Expr, &Type, &Type) -> Option<ValueAssignability>,
}

pub fn type_assignable(from: &Type, to: &Type) -> TypeAssignability {
    if from == to {
        return TypeAssignability::Exact;
    }

    let from_tag = type_tag(from);
    let to_tag = type_tag(to);
    for rule in type_rules_for(from_tag) {
        if rule.target == to_tag {
            if let Some(assignability) = (rule.apply)(from, to) {
                return assignability;
            }
        }
    }
    TypeAssignability::Incompatible
}

pub fn value_assignable(from_value: &Expr, from_ty: &Type, to_ty: &Type) -> ValueAssignability {
    let from_tag = type_tag(from_ty);
    let to_tag = type_tag(to_ty);
    for rule in value_rules_for(from_tag) {
        if rule.target == to_tag {
            if let Some(result) = (rule.apply)(from_value, from_ty, to_ty) {
                return result;
            }
        }
    }

    let assignability = type_assignable(from_ty, to_ty);
    match assignability {
        TypeAssignability::Exact
        | TypeAssignability::RangeToUInt64
        | TypeAssignability::UInt64ToRange { .. }
        | TypeAssignability::IntLitToUInt8
        | TypeAssignability::IntLitToUInt32 => ValueAssignability::Assignable(assignability),
        TypeAssignability::Incompatible => ValueAssignability::Incompatible,
    }
}

fn type_tag(ty: &Type) -> TypeTag {
    match ty {
        Type::UInt8 => TypeTag::UInt8,
        Type::UInt32 => TypeTag::UInt32,
        Type::UInt64 => TypeTag::UInt64,
        Type::Range { .. } => TypeTag::Range,
        Type::String => TypeTag::String,
        _ => TypeTag::Other,
    }
}

// --- Type Rules ---

const UINT64_TYPE_RULES: &[TypeRule] = &[TypeRule {
    target: TypeTag::Range,
    apply: u64_to_range,
}];

const RANGE_TYPE_RULES: &[TypeRule] = &[TypeRule {
    target: TypeTag::UInt64,
    apply: range_to_u64,
}];

fn type_rules_for(tag: TypeTag) -> &'static [TypeRule] {
    match tag {
        TypeTag::UInt64 => UINT64_TYPE_RULES,
        TypeTag::Range => RANGE_TYPE_RULES,
        _ => &[],
    }
}

fn u64_to_range(_from: &Type, to: &Type) -> Option<TypeAssignability> {
    let Type::Range { min, max } = to else {
        return None;
    };
    Some(TypeAssignability::UInt64ToRange {
        min: *min,
        max: *max,
    })
}

fn range_to_u64(_from: &Type, to: &Type) -> Option<TypeAssignability> {
    matches!(to, Type::UInt64).then_some(TypeAssignability::RangeToUInt64)
}

// --- Value Rules ---

// Literal-only narrowing: integer literals may narrow to smaller integer types
// when the value fits (no non-literal narrowing).
const INT_LIT_VALUE_RULES: &[ValueRule] = &[
    ValueRule {
        target: TypeTag::Range,
        apply: value_u64_to_range,
    },
    ValueRule {
        target: TypeTag::UInt8,
        apply: value_int_lit_to_u8,
    },
    ValueRule {
        target: TypeTag::UInt32,
        apply: value_int_lit_to_u32,
    },
];

fn value_rules_for(tag: TypeTag) -> &'static [ValueRule] {
    match tag {
        TypeTag::UInt64 => INT_LIT_VALUE_RULES,
        _ => &[],
    }
}

fn value_u64_to_range(
    from_value: &Expr,
    _from_ty: &Type,
    to_ty: &Type,
) -> Option<ValueAssignability> {
    let Type::Range { min, max } = to_ty else {
        return None;
    };
    if let ExprKind::IntLit(value) = from_value.kind {
        if value < *min || value >= *max {
            return Some(ValueAssignability::ValueOutOfRange {
                value,
                min: *min,
                max: *max,
            });
        }
    }
    Some(ValueAssignability::Assignable(
        TypeAssignability::UInt64ToRange {
            min: *min,
            max: *max,
        },
    ))
}

fn value_int_lit_to_u8(
    from_value: &Expr,
    _from_ty: &Type,
    to_ty: &Type,
) -> Option<ValueAssignability> {
    if !matches!(to_ty, Type::UInt8) {
        return None;
    }
    let ExprKind::IntLit(value) = from_value.kind else {
        return Some(ValueAssignability::Incompatible);
    };
    let max = u8::MAX as u64 + 1;
    if value >= max {
        return Some(ValueAssignability::ValueOutOfRange { value, min: 0, max });
    }
    Some(ValueAssignability::Assignable(
        TypeAssignability::IntLitToUInt8,
    ))
}

fn value_int_lit_to_u32(
    from_value: &Expr,
    _from_ty: &Type,
    to_ty: &Type,
) -> Option<ValueAssignability> {
    if !matches!(to_ty, Type::UInt32) {
        return None;
    }
    let ExprKind::IntLit(value) = from_value.kind else {
        return Some(ValueAssignability::Incompatible);
    };
    let max = u32::MAX as u64 + 1;
    if value >= max {
        return Some(ValueAssignability::ValueOutOfRange { value, min: 0, max });
    }
    Some(ValueAssignability::Assignable(
        TypeAssignability::IntLitToUInt32,
    ))
}
