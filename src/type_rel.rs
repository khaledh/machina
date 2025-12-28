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
//! - Call `print_arg_kind(ty)` to classify types accepted by print/println.
//! - When adding a new conversion, add a rule in `type_rules_for` and a small
//!   helper like `foo_to_bar`. If it has value-dependent behavior, add a rule
//!   in `value_rules_for` as well.

use crate::ast::{Expr, ExprKind};
use crate::types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeAssignability {
    Exact,
    UInt64ToRange { min: u64, max: u64 },
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
pub enum PrintArgKind {
    String,
    UInt64Like,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeTag {
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
        | TypeAssignability::UInt64ToRange { .. } => ValueAssignability::Assignable(assignability),
        TypeAssignability::Incompatible => ValueAssignability::Incompatible,
    }
}

pub fn print_arg_kind(ty: &Type) -> Option<PrintArgKind> {
    if type_tag(ty) == TypeTag::String {
        return Some(PrintArgKind::String);
    }
    match type_assignable(ty, &Type::UInt64) {
        TypeAssignability::Exact | TypeAssignability::RangeToUInt64 => {
            Some(PrintArgKind::UInt64Like)
        }
        _ => None,
    }
}

fn type_tag(ty: &Type) -> TypeTag {
    match ty {
        Type::UInt64 => TypeTag::UInt64,
        Type::Range { .. } => TypeTag::Range,
        Type::String => TypeTag::String,
        _ => TypeTag::Other,
    }
}

// --- Type Rules ---

const U64_TYPE_RULES: &[TypeRule] = &[TypeRule {
    target: TypeTag::Range,
    apply: u64_to_range,
}];

const RANGE_TYPE_RULES: &[TypeRule] = &[TypeRule {
    target: TypeTag::UInt64,
    apply: range_to_u64,
}];

fn type_rules_for(tag: TypeTag) -> &'static [TypeRule] {
    match tag {
        TypeTag::UInt64 => U64_TYPE_RULES,
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

const U64_VALUE_RULES: &[ValueRule] = &[ValueRule {
    target: TypeTag::Range,
    apply: value_u64_to_range,
}];

fn value_rules_for(tag: TypeTag) -> &'static [ValueRule] {
    match tag {
        TypeTag::UInt64 => U64_VALUE_RULES,
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
    if let ExprKind::UInt64Lit(value) = from_value.kind {
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
