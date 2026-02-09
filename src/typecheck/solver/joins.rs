//! Join/error-union helper utilities.
//!
//! These helpers normalize and infer join result types from arm types.

use crate::types::Type;

pub(super) fn infer_join_type_from_arms(arms: &[Type]) -> Option<Type> {
    let mut variants = Vec::new();
    for arm_ty in arms {
        if super::term_utils::is_unresolved(arm_ty) {
            continue;
        }
        collect_join_variants(arm_ty, &mut variants);
    }
    sort_join_variants_canonical(&mut variants);
    variants.dedup();
    if variants.is_empty() {
        return None;
    }
    if variants.len() == 1 {
        return variants.into_iter().next();
    }
    let ok_ty = variants[0].clone();
    let err_tys = variants.into_iter().skip(1).collect::<Vec<_>>();
    Some(Type::ErrorUnion {
        ok_ty: Box::new(ok_ty),
        err_tys,
    })
}

fn push_unique_join_variant(variants: &mut Vec<Type>, candidate: Type) {
    if !variants.iter().any(|existing| existing == &candidate) {
        variants.push(candidate);
    }
}

fn join_variant_rank(ty: &Type) -> u8 {
    match ty {
        Type::Unit => 0,
        Type::Int { .. } | Type::Bool | Type::Char | Type::String | Type::Range { .. } => 1,
        Type::Tuple { .. }
        | Type::Array { .. }
        | Type::DynArray { .. }
        | Type::Set { .. }
        | Type::Map { .. } => 2,
        Type::Struct { .. } | Type::Enum { .. } => 3,
        Type::Fn { .. } => 4,
        Type::Slice { .. } | Type::Ref { .. } | Type::Heap { .. } => 5,
        Type::Var(_) | Type::Unknown => 6,
        Type::ErrorUnion { .. } => 7,
    }
}

fn sort_join_variants_canonical(variants: &mut [Type]) {
    variants.sort_by(|left, right| {
        let left_key = (join_variant_rank(left), format!("{left:?}"));
        let right_key = (join_variant_rank(right), format!("{right:?}"));
        left_key.cmp(&right_key)
    });
}

fn collect_join_variants(ty: &Type, out: &mut Vec<Type>) {
    match ty {
        Type::ErrorUnion { ok_ty, err_tys } => {
            collect_join_variants(ok_ty, out);
            for err_ty in err_tys {
                collect_join_variants(err_ty, out);
            }
        }
        _ => push_unique_join_variant(out, ty.clone()),
    }
}
