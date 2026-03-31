//! Shared structural template binding helpers.
//!
//! These helpers walk a template `Type` alongside a concrete `Type` and
//! collect bindings for any `Type::Var` occurrences in the template.

use std::collections::HashMap;

use crate::core::types::{TyVarId, Type};

pub(crate) fn bind_template_type_vars(
    template: &Type,
    concrete: &Type,
) -> Option<HashMap<TyVarId, Type>> {
    let mut bindings = HashMap::new();
    if collect_template_type_var_bindings(template, concrete, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn collect_template_type_var_bindings(
    template: &Type,
    concrete: &Type,
    bindings: &mut HashMap<TyVarId, Type>,
) -> bool {
    match (template, concrete) {
        (Type::Var(var), concrete) => match bindings.get(var) {
            Some(bound) => bound.shape_eq(concrete),
            None => {
                bindings.insert(*var, concrete.clone());
                true
            }
        },
        (Type::Unknown, Type::Unknown) => true,
        (Type::Unit, Type::Unit) => true,
        (
            Type::Int {
                signed: l_signed,
                bits: l_bits,
                bounds: l_bounds,
                nonzero: l_nonzero,
            },
            Type::Int {
                signed: r_signed,
                bits: r_bits,
                bounds: r_bounds,
                nonzero: r_nonzero,
            },
        ) => {
            l_signed == r_signed
                && l_bits == r_bits
                && l_bounds == r_bounds
                && l_nonzero == r_nonzero
        }
        (Type::Bool, Type::Bool) => true,
        (Type::Char, Type::Char) => true,
        (Type::String, Type::String) => true,
        (Type::Range { elem_ty: l }, Type::Range { elem_ty: r })
        | (Type::Slice { elem_ty: l }, Type::Slice { elem_ty: r })
        | (Type::DynArray { elem_ty: l }, Type::DynArray { elem_ty: r })
        | (Type::View { elem_ty: l }, Type::View { elem_ty: r })
        | (Type::RawPtr { elem_ty: l }, Type::RawPtr { elem_ty: r })
        | (Type::ViewSlice { elem_ty: l }, Type::ViewSlice { elem_ty: r })
        | (Type::ViewArray { elem_ty: l }, Type::ViewArray { elem_ty: r })
        | (Type::Set { elem_ty: l }, Type::Set { elem_ty: r })
        | (Type::Heap { elem_ty: l }, Type::Heap { elem_ty: r }) => {
            collect_template_type_var_bindings(l, r, bindings)
        }
        (
            Type::Map {
                key_ty: l_key,
                value_ty: l_value,
            },
            Type::Map {
                key_ty: r_key,
                value_ty: r_value,
            },
        ) => {
            collect_template_type_var_bindings(l_key, r_key, bindings)
                && collect_template_type_var_bindings(l_value, r_value, bindings)
        }
        (
            Type::Ref {
                mutable: l_mut,
                elem_ty: l_elem,
            },
            Type::Ref {
                mutable: r_mut,
                elem_ty: r_elem,
            },
        ) => *l_mut == *r_mut && collect_template_type_var_bindings(l_elem, r_elem, bindings),
        (
            Type::Fn {
                params: l_params,
                ret_ty: l_ret,
            },
            Type::Fn {
                params: r_params,
                ret_ty: r_ret,
            },
        ) => {
            l_params.len() == r_params.len()
                && l_params.iter().zip(r_params.iter()).all(|(l, r)| {
                    l.mode == r.mode && collect_template_type_var_bindings(&l.ty, &r.ty, bindings)
                })
                && collect_template_type_var_bindings(l_ret, r_ret, bindings)
        }
        (
            Type::Array {
                elem_ty: l_elem,
                dims: l_dims,
            },
            Type::Array {
                elem_ty: r_elem,
                dims: r_dims,
            },
        ) => l_dims == r_dims && collect_template_type_var_bindings(l_elem, r_elem, bindings),
        (Type::Tuple { field_tys: l }, Type::Tuple { field_tys: r }) => {
            l.len() == r.len()
                && l.iter()
                    .zip(r.iter())
                    .all(|(l, r)| collect_template_type_var_bindings(l, r, bindings))
        }
        (
            Type::Struct {
                fields: l_fields, ..
            },
            Type::Struct {
                fields: r_fields, ..
            },
        ) => {
            l_fields.len() == r_fields.len()
                && l_fields.iter().zip(r_fields.iter()).all(|(l, r)| {
                    l.name == r.name && collect_template_type_var_bindings(&l.ty, &r.ty, bindings)
                })
        }
        (
            Type::Enum {
                variants: l_variants,
                ..
            },
            Type::Enum {
                variants: r_variants,
                ..
            },
        ) => {
            l_variants.len() == r_variants.len()
                && l_variants.iter().zip(r_variants.iter()).all(|(l, r)| {
                    l.name == r.name
                        && l.payload.len() == r.payload.len()
                        && l.payload
                            .iter()
                            .zip(r.payload.iter())
                            .all(|(l, r)| collect_template_type_var_bindings(l, r, bindings))
                })
        }
        (
            Type::ErrorUnion {
                ok_ty: l_ok,
                err_tys: l_errs,
            },
            Type::ErrorUnion {
                ok_ty: r_ok,
                err_tys: r_errs,
            },
        ) => {
            l_errs.len() == r_errs.len()
                && collect_template_type_var_bindings(l_ok, r_ok, bindings)
                && l_errs
                    .iter()
                    .zip(r_errs.iter())
                    .all(|(l, r)| collect_template_type_var_bindings(l, r, bindings))
        }
        (
            Type::Pending {
                response_tys: l_responses,
            },
            Type::Pending {
                response_tys: r_responses,
            },
        )
        | (
            Type::ReplyCap {
                response_tys: l_responses,
            },
            Type::ReplyCap {
                response_tys: r_responses,
            },
        ) => {
            l_responses.len() == r_responses.len()
                && l_responses
                    .iter()
                    .zip(r_responses.iter())
                    .all(|(l, r)| collect_template_type_var_bindings(l, r, bindings))
        }
        _ => false,
    }
}
