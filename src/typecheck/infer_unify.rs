//! Type unification for local type inference.
//!
//! This module implements the unification algorithm used during type checking
//! to solve type constraints and infer types.

use std::collections::HashMap;

use crate::types::{TyVarId, Type};

/// Errors that can occur during type unification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnifyError {
    /// The occurs check failed, indicating an infinite type.
    /// This happens when trying to unify a type variable with a type that contains
    /// that same variable (e.g., unifying `T` with `Array<T>` would create an infinite type).
    OccursCheckFailed(TyVarId, Type),

    /// Two types could not be unified because they have incompatible structures.
    Mismatch(Type, Type),
}

/// A substitution mapping type variables to concrete types.
///
/// This structure maintains the solutions found during unification,
/// mapping type variables to their inferred types.
#[derive(Debug, Default, Clone)]
pub struct Subst {
    map: HashMap<TyVarId, Type>,
}

impl Subst {
    /// Looks up the type bound to a type variable.
    pub fn get(&self, var: TyVarId) -> Option<&Type> {
        self.map.get(&var)
    }

    /// Binds a type variable to a type.
    ///
    /// This is private to prevent external code from creating invalid cycles.
    /// Use `Unifier::unify` instead, which performs proper occurs checking.
    fn insert(&mut self, var: TyVarId, ty: Type) {
        self.map.insert(var, ty);
    }

    /// Applies the substitution to a type, replacing type variables with their bindings.
    ///
    /// This recursively walks the type structure, substituting any type variables
    /// that have been solved. When a type variable maps to another type variable,
    /// the substitution is applied recursively to follow the chain of bindings.
    /// Type variables without bindings are left unchanged.
    pub fn apply(&self, ty: &Type) -> Type {
        ty.map_cow(&|t| match t {
            Type::Var(var) => match self.map.get(var) {
                Some(bound_ty) if !matches!(bound_ty, Type::Var(v) if *v == *var) => {
                    Some(self.apply(bound_ty))
                }
                _ => None,
            },
            _ => None,
        })
        .into_owned()
    }
}

/// The main unification engine for type inference.
///
/// Manages the generation of fresh type variables and maintains the substitution
/// of solved type variables. Uses Robinson's unification algorithm with an occurs check.
#[derive(Debug, Default)]
pub struct Unifier {
    next_var: u32,
    subst: Subst,
}

impl Unifier {
    pub fn new() -> Self {
        Self::default()
    }

    /// Generates a fresh type variable with a unique identifier.
    pub fn new_var(&mut self) -> TyVarId {
        let id = TyVarId::new(self.next_var);
        self.next_var += 1;
        id
    }

    /// Returns a reference to the current substitution.
    pub fn subst(&self) -> &Subst {
        &self.subst
    }

    /// Applies the current substitution to a type.
    pub fn apply(&self, ty: &Type) -> Type {
        self.subst.apply(ty)
    }

    /// Unifies two types, updating the substitution if successful.
    ///
    /// This is the main entry point for unification. It applies the current substitution
    /// to both types before attempting to unify them, ensuring we work with the most
    /// up-to-date type information.
    pub fn unify(&mut self, left: &Type, right: &Type) -> Result<(), UnifyError> {
        let left = self.apply(left);
        let right = self.apply(right);
        self.unify_inner(left, right)
    }

    /// Unifies two types, binding only variables that pass the `is_infer` predicate.
    ///
    /// This is used for local inference where generic type parameters should be treated as rigid.
    pub fn unify_infer(
        &mut self,
        left: &Type,
        right: &Type,
        is_infer: fn(TyVarId) -> bool,
    ) -> Result<(), UnifyError> {
        let left = self.apply(left);
        let right = self.apply(right);
        self.unify_infer_inner(left, right, is_infer)
    }

    /// Internal unification logic that works with already-substituted types.
    ///
    /// This implements the core unification algorithm using structural recursion.
    /// It handles type variables specially via `bind_var`, and recursively unifies
    /// compound types by matching their structure.
    fn unify_inner(&mut self, left: Type, right: Type) -> Result<(), UnifyError> {
        // Fast path: identical types always unify
        if left == right {
            return Ok(());
        }

        match (left, right) {
            // Unifying with a type variable: delegate to bind_var
            (Type::Var(var), ty) | (ty, Type::Var(var)) => self.bind_var(var, ty),

            // Function types: must have same arity, parameter modes, and types
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
                // Check parameter count matches
                if l_params.len() != r_params.len() {
                    return Err(UnifyError::Mismatch(
                        Type::Fn {
                            params: l_params,
                            ret_ty: l_ret,
                        },
                        Type::Fn {
                            params: r_params,
                            ret_ty: r_ret,
                        },
                    ));
                }

                // Unify each parameter (checking mode and type)
                for (l_param, r_param) in l_params.iter().zip(r_params.iter()) {
                    if l_param.mode != r_param.mode {
                        return Err(UnifyError::Mismatch(
                            Type::Fn {
                                params: l_params,
                                ret_ty: l_ret,
                            },
                            Type::Fn {
                                params: r_params,
                                ret_ty: r_ret,
                            },
                        ));
                    }
                    self.unify(&l_param.ty, &r_param.ty)?;
                }

                // Unify return types
                self.unify(&l_ret, &r_ret)
            }

            // Range types: unify element types
            (Type::Range { elem_ty: l }, Type::Range { elem_ty: r }) => self.unify(&l, &r),

            // Array types: must have same dimensions and compatible element types
            (
                Type::Array {
                    elem_ty: l_elem,
                    dims: l_dims,
                },
                Type::Array {
                    elem_ty: r_elem,
                    dims: r_dims,
                },
            ) => {
                if l_dims != r_dims {
                    return Err(UnifyError::Mismatch(
                        Type::Array {
                            elem_ty: l_elem,
                            dims: l_dims,
                        },
                        Type::Array {
                            elem_ty: r_elem,
                            dims: r_dims,
                        },
                    ));
                }
                self.unify(&l_elem, &r_elem)
            }

            // Tuple types: must have same arity and unify field-wise
            (Type::Tuple { field_tys: l }, Type::Tuple { field_tys: r }) => {
                if l.len() != r.len() {
                    return Err(UnifyError::Mismatch(
                        Type::Tuple { field_tys: l },
                        Type::Tuple { field_tys: r },
                    ));
                }
                for (l_ty, r_ty) in l.iter().zip(r.iter()) {
                    self.unify(l_ty, r_ty)?;
                }
                Ok(())
            }

            // Container types: recursively unify element types
            (Type::Slice { elem_ty: l }, Type::Slice { elem_ty: r }) => self.unify(&l, &r),
            (Type::Heap { elem_ty: l }, Type::Heap { elem_ty: r }) => self.unify(&l, &r),

            // Reference types: must have same mutability and compatible element types
            (
                Type::Ref {
                    mutable: l_mut,
                    elem_ty: l_elem,
                },
                Type::Ref {
                    mutable: r_mut,
                    elem_ty: r_elem,
                },
            ) => {
                if l_mut != r_mut {
                    return Err(UnifyError::Mismatch(
                        Type::Ref {
                            mutable: l_mut,
                            elem_ty: l_elem,
                        },
                        Type::Ref {
                            mutable: r_mut,
                            elem_ty: r_elem,
                        },
                    ));
                }
                self.unify(&l_elem, &r_elem)
            }

            // All other combinations are incompatible
            (left, right) => Err(UnifyError::Mismatch(left, right)),
        }
    }

    fn unify_infer_inner(
        &mut self,
        left: Type,
        right: Type,
        is_infer: fn(TyVarId) -> bool,
    ) -> Result<(), UnifyError> {
        if left == right {
            return Ok(());
        }

        match (left, right) {
            (Type::Var(var), ty) if is_infer(var) => self.bind_var(var, ty),
            (ty, Type::Var(var)) if is_infer(var) => self.bind_var(var, ty),
            (Type::Var(l), Type::Var(r)) => Err(UnifyError::Mismatch(Type::Var(l), Type::Var(r))),
            (Type::Var(var), ty) | (ty, Type::Var(var)) => {
                Err(UnifyError::Mismatch(Type::Var(var), ty))
            }

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
                if l_params.len() != r_params.len() {
                    return Err(UnifyError::Mismatch(
                        Type::Fn {
                            params: l_params,
                            ret_ty: l_ret,
                        },
                        Type::Fn {
                            params: r_params,
                            ret_ty: r_ret,
                        },
                    ));
                }

                for (l_param, r_param) in l_params.iter().zip(r_params.iter()) {
                    if l_param.mode != r_param.mode {
                        return Err(UnifyError::Mismatch(
                            Type::Fn {
                                params: l_params,
                                ret_ty: l_ret,
                            },
                            Type::Fn {
                                params: r_params,
                                ret_ty: r_ret,
                            },
                        ));
                    }
                    self.unify_infer(&l_param.ty, &r_param.ty, is_infer)?;
                }
                self.unify_infer(&l_ret, &r_ret, is_infer)
            }

            (Type::Range { elem_ty: l }, Type::Range { elem_ty: r }) => {
                self.unify_infer(&l, &r, is_infer)
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
            ) => {
                if l_dims != r_dims {
                    return Err(UnifyError::Mismatch(
                        Type::Array {
                            elem_ty: l_elem,
                            dims: l_dims,
                        },
                        Type::Array {
                            elem_ty: r_elem,
                            dims: r_dims,
                        },
                    ));
                }
                self.unify_infer(&l_elem, &r_elem, is_infer)
            }
            (Type::Tuple { field_tys: l }, Type::Tuple { field_tys: r }) => {
                if l.len() != r.len() {
                    return Err(UnifyError::Mismatch(
                        Type::Tuple { field_tys: l },
                        Type::Tuple { field_tys: r },
                    ));
                }
                for (l_ty, r_ty) in l.iter().zip(r.iter()) {
                    self.unify_infer(l_ty, r_ty, is_infer)?;
                }
                Ok(())
            }
            (
                Type::Struct {
                    name: l_name,
                    fields: l_fields,
                },
                Type::Struct {
                    name: r_name,
                    fields: r_fields,
                },
            ) => {
                if l_name != r_name || l_fields.len() != r_fields.len() {
                    return Err(UnifyError::Mismatch(
                        Type::Struct {
                            name: l_name,
                            fields: l_fields,
                        },
                        Type::Struct {
                            name: r_name,
                            fields: r_fields,
                        },
                    ));
                }
                for (l_field, r_field) in l_fields.iter().zip(r_fields.iter()) {
                    if l_field.name != r_field.name {
                        return Err(UnifyError::Mismatch(
                            Type::Struct {
                                name: l_name.clone(),
                                fields: l_fields.clone(),
                            },
                            Type::Struct {
                                name: r_name.clone(),
                                fields: r_fields.clone(),
                            },
                        ));
                    }
                    self.unify_infer(&l_field.ty, &r_field.ty, is_infer)?;
                }
                Ok(())
            }
            (
                Type::Enum {
                    name: l_name,
                    variants: l_variants,
                },
                Type::Enum {
                    name: r_name,
                    variants: r_variants,
                },
            ) => {
                if l_name != r_name || l_variants.len() != r_variants.len() {
                    return Err(UnifyError::Mismatch(
                        Type::Enum {
                            name: l_name,
                            variants: l_variants,
                        },
                        Type::Enum {
                            name: r_name,
                            variants: r_variants,
                        },
                    ));
                }
                for (l_variant, r_variant) in l_variants.iter().zip(r_variants.iter()) {
                    if l_variant.name != r_variant.name
                        || l_variant.payload.len() != r_variant.payload.len()
                    {
                        return Err(UnifyError::Mismatch(
                            Type::Enum {
                                name: l_name.clone(),
                                variants: l_variants.clone(),
                            },
                            Type::Enum {
                                name: r_name.clone(),
                                variants: r_variants.clone(),
                            },
                        ));
                    }
                    for (l_ty, r_ty) in l_variant.payload.iter().zip(r_variant.payload.iter()) {
                        self.unify_infer(l_ty, r_ty, is_infer)?;
                    }
                }
                Ok(())
            }
            (Type::Slice { elem_ty: l }, Type::Slice { elem_ty: r }) => {
                self.unify_infer(&l, &r, is_infer)
            }
            (Type::Heap { elem_ty: l }, Type::Heap { elem_ty: r }) => {
                self.unify_infer(&l, &r, is_infer)
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
            ) => {
                if l_mut != r_mut {
                    return Err(UnifyError::Mismatch(
                        Type::Ref {
                            mutable: l_mut,
                            elem_ty: l_elem,
                        },
                        Type::Ref {
                            mutable: r_mut,
                            elem_ty: r_elem,
                        },
                    ));
                }
                self.unify_infer(&l_elem, &r_elem, is_infer)
            }
            (l, r) => Err(UnifyError::Mismatch(l, r)),
        }
    }

    /// Binds a type variable to a type after performing safety checks.
    ///
    /// This method handles three cases:
    /// 1. If the variable already has a binding, unify the existing binding with the new type
    /// 2. If binding a variable to itself, succeed immediately (idempotent)
    /// 3. Perform the occurs check to prevent infinite types, then create the binding
    fn bind_var(&mut self, var: TyVarId, ty: Type) -> Result<(), UnifyError> {
        // If variable is already bound, unify with existing binding
        if let Some(existing) = self.subst.get(var).cloned() {
            return self.unify(&existing, &ty);
        }

        // Binding a variable to itself is always valid
        if matches!(ty, Type::Var(v) if v == var) {
            return Ok(());
        }

        // Occurs check: prevent infinite types like T = Array<T>
        if self.occurs_in(var, &ty) {
            return Err(UnifyError::OccursCheckFailed(var, ty));
        }

        self.subst.insert(var, ty);
        Ok(())
    }

    fn occurs_in(&self, var: TyVarId, ty: &Type) -> bool {
        ty.any(&|t| matches!(t, Type::Var(v) if *v == var))
    }
}
