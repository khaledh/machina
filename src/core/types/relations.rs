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

use crate::core::tree::{Expr, ExprKind, UnaryOp};
use crate::core::types::{EnumVariant, StructField, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeAssignability {
    Exact,
    IntToRefined {
        min: Option<i128>,
        max: Option<i128>,
        nonzero: bool,
    },
    IntLitToInt {
        signed: bool,
        bits: u8,
    },
    RefinedToInt,
    Incompatible,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueAssignability {
    Assignable(TypeAssignability),
    ValueOutOfRange { value: i128, min: i128, max: i128 },
    ValueNotNonZero { value: i128 },
    Incompatible,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeTag {
    Int,
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

    if array_to_slice_assignable(from, to)
        || array_to_dyn_array_assignable(from, to)
        || dyn_array_to_slice_assignable(from, to)
    {
        return TypeAssignability::Exact;
    }

    if let Some(assignability) = nominal_instance_assignable(from, to) {
        return assignability;
    }

    if let Type::ErrorUnion { ok_ty, err_tys } = to {
        let any_match = std::iter::once(ok_ty.as_ref())
            .chain(err_tys.iter())
            .any(|variant_ty| type_assignable(from, variant_ty) != TypeAssignability::Incompatible);
        return if any_match {
            TypeAssignability::Exact
        } else {
            TypeAssignability::Incompatible
        };
    }

    if let Type::ErrorUnion {
        ok_ty: from_ok,
        err_tys: from_errs,
    } = from
    {
        let all_match = std::iter::once(from_ok.as_ref())
            .chain(from_errs.iter())
            .all(|variant_ty| type_assignable(variant_ty, to) != TypeAssignability::Incompatible);
        return if all_match {
            TypeAssignability::Exact
        } else {
            TypeAssignability::Incompatible
        };
    }

    let from_tag = type_tag(from);
    let to_tag = type_tag(to);
    for rule in type_rules_for(from_tag) {
        if rule.target == to_tag
            && let Some(assignability) = (rule.apply)(from, to)
        {
            return assignability;
        }
    }
    TypeAssignability::Incompatible
}

fn nominal_instance_assignable(from: &Type, to: &Type) -> Option<TypeAssignability> {
    match (from, to) {
        (
            Type::Struct {
                name: from_name,
                fields: from_fields,
            },
            Type::Struct {
                name: to_name,
                fields: to_fields,
            },
        ) if nominal_base_name(from_name) == nominal_base_name(to_name) => {
            Some(if struct_fields_assignable(from_fields, to_fields) {
                TypeAssignability::Exact
            } else {
                TypeAssignability::Incompatible
            })
        }
        (
            Type::Enum {
                name: from_name,
                variants: from_variants,
            },
            Type::Enum {
                name: to_name,
                variants: to_variants,
            },
        ) if nominal_base_name(from_name) == nominal_base_name(to_name) => {
            Some(if enum_variants_assignable(from_variants, to_variants) {
                TypeAssignability::Exact
            } else {
                TypeAssignability::Incompatible
            })
        }
        _ => None,
    }
}

fn struct_fields_assignable(from_fields: &[StructField], to_fields: &[StructField]) -> bool {
    if from_fields.len() != to_fields.len() {
        return false;
    }

    from_fields.iter().zip(to_fields.iter()).all(|(from, to)| {
        from.name == to.name && type_assignable(&from.ty, &to.ty) != TypeAssignability::Incompatible
    })
}

fn enum_variants_assignable(from_variants: &[EnumVariant], to_variants: &[EnumVariant]) -> bool {
    if from_variants.len() != to_variants.len() {
        return false;
    }

    from_variants
        .iter()
        .zip(to_variants.iter())
        .all(|(from_variant, to_variant)| {
            from_variant.name == to_variant.name
                && from_variant.payload.len() == to_variant.payload.len()
                && from_variant
                    .payload
                    .iter()
                    .zip(to_variant.payload.iter())
                    .all(|(from_payload, to_payload)| {
                        type_assignable(from_payload, to_payload) != TypeAssignability::Incompatible
                    })
        })
}

fn nominal_base_name(name: &str) -> &str {
    name.split('<').next().unwrap_or(name)
}

pub fn value_assignable(from_value: &Expr, from_ty: &Type, to_ty: &Type) -> ValueAssignability {
    let from_tag = type_tag(from_ty);
    let to_tag = type_tag(to_ty);
    for rule in value_rules_for(from_tag) {
        if rule.target == to_tag
            && let Some(result) = (rule.apply)(from_value, from_ty, to_ty)
        {
            return result;
        }
    }

    let assignability = type_assignable(from_ty, to_ty);
    match assignability {
        TypeAssignability::Exact
        | TypeAssignability::RefinedToInt
        | TypeAssignability::IntToRefined { .. }
        | TypeAssignability::IntLitToInt { .. } => ValueAssignability::Assignable(assignability),
        TypeAssignability::Incompatible => ValueAssignability::Incompatible,
    }
}

pub fn array_to_slice_assignable(from: &Type, to: &Type) -> bool {
    let Type::Slice { elem_ty } = to else {
        return false;
    };
    matches!(from.array_item_type(), Some(item) if item == **elem_ty)
}

pub fn array_to_dyn_array_assignable(from: &Type, to: &Type) -> bool {
    let Type::DynArray { elem_ty } = to else {
        return false;
    };
    matches!(from.array_item_type(), Some(item) if item == **elem_ty)
}

pub fn dyn_array_to_slice_assignable(from: &Type, to: &Type) -> bool {
    let Type::DynArray {
        elem_ty: from_elem_ty,
    } = from
    else {
        return false;
    };
    let Type::Slice {
        elem_ty: to_elem_ty,
    } = to
    else {
        return false;
    };
    **from_elem_ty == **to_elem_ty
}

fn type_tag(ty: &Type) -> TypeTag {
    match ty {
        Type::Int { .. } => TypeTag::Int,
        Type::Range { .. } => TypeTag::Range,
        Type::String => TypeTag::String,
        Type::Var(_) => TypeTag::Other,
        _ => TypeTag::Other,
    }
}

// --- Type Rules ---

const INT_TYPE_RULES: &[TypeRule] = &[TypeRule {
    target: TypeTag::Int,
    apply: int_to_int,
}];

fn type_rules_for(tag: TypeTag) -> &'static [TypeRule] {
    match tag {
        TypeTag::Int => INT_TYPE_RULES,
        _ => &[],
    }
}

fn int_to_int(from: &Type, to: &Type) -> Option<TypeAssignability> {
    let Type::Int {
        signed: from_signed,
        bits: from_bits,
        bounds: from_bounds,
        nonzero: from_nonzero,
    } = from
    else {
        return None;
    };
    let Type::Int {
        signed: to_signed,
        bits: to_bits,
        bounds: to_bounds,
        nonzero: to_nonzero,
    } = to
    else {
        return None;
    };
    if from_signed != to_signed || from_bits != to_bits {
        return None;
    }

    if to_bounds.is_some() || *to_nonzero {
        return Some(TypeAssignability::IntToRefined {
            min: to_bounds.map(|bounds| bounds.min),
            max: to_bounds.map(|bounds| bounds.max_excl),
            nonzero: *to_nonzero,
        });
    }

    if from_bounds.is_some() || *from_nonzero {
        return Some(TypeAssignability::RefinedToInt);
    }

    Some(TypeAssignability::Exact)
}

// --- Value Rules ---

// Literal-only narrowing: integer literals may narrow to smaller integer types
// when the value fits (no non-literal narrowing).
const INT_LIT_VALUE_RULES: &[ValueRule] = &[ValueRule {
    target: TypeTag::Int,
    apply: value_int_lit_to_int,
}];

fn value_rules_for(tag: TypeTag) -> &'static [ValueRule] {
    match tag {
        TypeTag::Int => INT_LIT_VALUE_RULES,
        _ => &[],
    }
}

fn value_int_lit_to_int(
    from_value: &Expr,
    _from_ty: &Type,
    to_ty: &Type,
) -> Option<ValueAssignability> {
    let Type::Int {
        signed,
        bits,
        bounds,
        nonzero,
    } = to_ty
    else {
        return None;
    };
    let value = int_lit_value(from_value)?;
    let min = if *signed {
        -(1i128 << (*bits as u32 - 1))
    } else {
        0
    };
    let max_excl = if *signed {
        1i128 << (*bits as u32 - 1)
    } else {
        1i128 << (*bits as u32)
    };
    if let Some(bounds) = bounds
        && (value < bounds.min || value >= bounds.max_excl)
    {
        return Some(ValueAssignability::ValueOutOfRange {
            value,
            min: bounds.min,
            max: bounds.max_excl,
        });
    }
    if *nonzero && value == 0 {
        return Some(ValueAssignability::ValueNotNonZero { value });
    }
    if value < min || value >= max_excl {
        return Some(ValueAssignability::ValueOutOfRange {
            value,
            min,
            max: max_excl,
        });
    }
    Some(ValueAssignability::Assignable(
        TypeAssignability::IntLitToInt {
            signed: *signed,
            bits: *bits,
        },
    ))
}

fn int_lit_value(expr: &Expr) -> Option<i128> {
    match &expr.kind {
        ExprKind::IntLit(value) => Some(*value as i128),
        ExprKind::UnaryOp {
            op: UnaryOp::Neg,
            expr,
        } => match expr.kind {
            ExprKind::IntLit(value) => Some(-(value as i128)),
            _ => None,
        },
        _ => None,
    }
}
