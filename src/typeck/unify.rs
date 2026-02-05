//! Type unification for local type inference.
//!
//! This module implements the unification algorithm used during type checking
//! to solve type constraints and infer types.

use std::collections::HashMap;

use crate::types::{EnumVariant, FnParam, StructField, TyVarId, Type};

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
        match ty {
            // Substitute type variables with their bindings, or keep them unchanged
            // Recursively apply to handle chains of variable bindings
            Type::Var(var) => match self.map.get(var) {
                Some(bound_ty) => {
                    // Guard against cycles: if bound to itself, just return the variable.
                    // Longer cycles shouldn't be constructible via `Unifier::unify`.
                    if matches!(bound_ty, Type::Var(v) if v == var) {
                        ty.clone()
                    } else {
                        self.apply(bound_ty)
                    }
                }
                None => ty.clone(),
            },
            Type::Fn { params, ret_ty } => Type::Fn {
                params: params
                    .iter()
                    .map(|param| FnParam {
                        mode: param.mode,
                        ty: self.apply(&param.ty),
                    })
                    .collect(),
                ret_ty: Box::new(self.apply(ret_ty)),
            },
            Type::Range { elem_ty } => Type::Range {
                elem_ty: Box::new(self.apply(elem_ty)),
            },
            Type::Array { elem_ty, dims } => Type::Array {
                elem_ty: Box::new(self.apply(elem_ty)),
                dims: dims.clone(),
            },
            Type::Tuple { field_tys } => Type::Tuple {
                field_tys: field_tys.iter().map(|ty| self.apply(ty)).collect(),
            },
            Type::Struct { name, fields } => Type::Struct {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|field| StructField {
                        name: field.name.clone(),
                        ty: self.apply(&field.ty),
                    })
                    .collect(),
            },
            Type::Enum { name, variants } => Type::Enum {
                name: name.clone(),
                variants: variants
                    .iter()
                    .map(|variant| EnumVariant {
                        name: variant.name.clone(),
                        payload: variant.payload.iter().map(|ty| self.apply(ty)).collect(),
                    })
                    .collect(),
            },
            Type::Slice { elem_ty } => Type::Slice {
                elem_ty: Box::new(self.apply(elem_ty)),
            },
            Type::Heap { elem_ty } => Type::Heap {
                elem_ty: Box::new(self.apply(elem_ty)),
            },
            Type::Ref { mutable, elem_ty } => Type::Ref {
                mutable: *mutable,
                elem_ty: Box::new(self.apply(elem_ty)),
            },
            // All other types (primitives, etc.) don't contain type variables
            _ => ty.clone(),
        }
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

    /// Checks if a type variable occurs within a type structure.
    ///
    /// This is the "occurs check" which prevents creating infinite types.
    /// For example, it prevents unifying `T` with `Array<T>`, which would
    /// create an infinitely nested type.
    fn occurs_in(&self, var: TyVarId, ty: &Type) -> bool {
        match ty {
            Type::Var(other) => *other == var,
            Type::Fn { params, ret_ty } => {
                params.iter().any(|param| self.occurs_in(var, &param.ty))
                    || self.occurs_in(var, ret_ty)
            }
            Type::Range { elem_ty } => self.occurs_in(var, elem_ty),
            Type::Array { elem_ty, .. } => self.occurs_in(var, elem_ty),
            Type::Tuple { field_tys } => field_tys.iter().any(|ty| self.occurs_in(var, ty)),
            Type::Struct { fields, .. } => fields.iter().any(|f| self.occurs_in(var, &f.ty)),
            Type::Enum { variants, .. } => variants
                .iter()
                .flat_map(|v| v.payload.iter())
                .any(|ty| self.occurs_in(var, ty)),
            Type::Slice { elem_ty } => self.occurs_in(var, elem_ty),
            Type::Heap { elem_ty } => self.occurs_in(var, elem_ty),
            Type::Ref { elem_ty, .. } => self.occurs_in(var, elem_ty),
            _ => false,
        }
    }
}
