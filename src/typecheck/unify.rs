//! Core type unifier used by the checker solver.
//!
//! This unifier respects variable kinds from `TypeVarStore`, including rigid
//! generic parameters and inference variables.

use crate::types::{TyVarId, Type};

use super::typesys::{TypeVarKind, TypeVarStore};

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub(crate) enum TcUnifyError {
    OccursCheckFailed(TyVarId, Type),
    Mismatch(Type, Type),
    CannotBindRigid(TyVarId, Type),
}

/// Unification engine for the type checker.
#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub(crate) struct TcUnifier {
    vars: TypeVarStore,
}

#[allow(dead_code)]
impl TcUnifier {
    pub(crate) fn new(vars: TypeVarStore) -> Self {
        Self { vars }
    }

    pub(crate) fn vars(&self) -> &TypeVarStore {
        &self.vars
    }

    pub(crate) fn vars_mut(&mut self) -> &mut TypeVarStore {
        &mut self.vars
    }

    pub(crate) fn apply(&self, ty: &Type) -> Type {
        self.vars.apply(ty)
    }

    pub(crate) fn unify(&mut self, left: &Type, right: &Type) -> Result<(), TcUnifyError> {
        let left = self.apply(left);
        let right = self.apply(right);
        self.unify_inner(left, right)
    }

    fn unify_inner(&mut self, left: Type, right: Type) -> Result<(), TcUnifyError> {
        if left == right {
            return Ok(());
        }

        match (left, right) {
            (Type::Var(left_var), Type::Var(right_var)) => {
                if left_var == right_var {
                    return Ok(());
                }
                let left_rigid = self.vars.is_rigid(left_var);
                let right_rigid = self.vars.is_rigid(right_var);
                match (left_rigid, right_rigid) {
                    (true, false) => self.bind_var(right_var, Type::Var(left_var)),
                    (false, true) => self.bind_var(left_var, Type::Var(right_var)),
                    (true, true) => Err(TcUnifyError::Mismatch(
                        Type::Var(left_var),
                        Type::Var(right_var),
                    )),
                    (false, false) => {
                        // Preserve integer-literal constraints by binding the
                        // unconstrained side to the int-constrained var.
                        let left_kind = self.vars.kind(left_var);
                        let right_kind = self.vars.kind(right_var);
                        match (left_kind, right_kind) {
                            (Some(TypeVarKind::InferInt), Some(TypeVarKind::InferInt)) => {
                                self.bind_var(left_var, Type::Var(right_var))
                            }
                            (Some(TypeVarKind::InferInt), _) => {
                                self.bind_var(right_var, Type::Var(left_var))
                            }
                            (_, Some(TypeVarKind::InferInt)) => {
                                self.bind_var(left_var, Type::Var(right_var))
                            }
                            _ => self.bind_var(left_var, Type::Var(right_var)),
                        }
                    }
                }
            }
            (Type::Var(var), ty) | (ty, Type::Var(var)) => self.bind_var(var, ty),
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
                    return Err(TcUnifyError::Mismatch(
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
                        return Err(TcUnifyError::Mismatch(
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

                self.unify(&l_ret, &r_ret)
            }
            (Type::Range { elem_ty: l }, Type::Range { elem_ty: r }) => self.unify(&l, &r),
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
                    return Err(TcUnifyError::Mismatch(
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
            (Type::DynArray { elem_ty: l }, Type::DynArray { elem_ty: r }) => self.unify(&l, &r),
            (Type::Tuple { field_tys: l }, Type::Tuple { field_tys: r }) => {
                if l.len() != r.len() {
                    return Err(TcUnifyError::Mismatch(
                        Type::Tuple { field_tys: l },
                        Type::Tuple { field_tys: r },
                    ));
                }
                for (l_ty, r_ty) in l.iter().zip(r.iter()) {
                    self.unify(l_ty, r_ty)?;
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
                if canonical_nominal_name(&l_name) != canonical_nominal_name(&r_name)
                    || l_fields.len() != r_fields.len()
                {
                    return Err(TcUnifyError::Mismatch(
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
                        return Err(TcUnifyError::Mismatch(
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
                    self.unify(&l_field.ty, &r_field.ty)?;
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
                if canonical_nominal_name(&l_name) != canonical_nominal_name(&r_name)
                    || l_variants.len() != r_variants.len()
                {
                    return Err(TcUnifyError::Mismatch(
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
                        return Err(TcUnifyError::Mismatch(
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
                        self.unify(l_ty, r_ty)?;
                    }
                }
                Ok(())
            }
            (Type::Slice { elem_ty: l }, Type::Slice { elem_ty: r }) => self.unify(&l, &r),
            (Type::Heap { elem_ty: l }, Type::Heap { elem_ty: r }) => self.unify(&l, &r),
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
                    return Err(TcUnifyError::Mismatch(
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
            (left, right) => Err(TcUnifyError::Mismatch(left, right)),
        }
    }

    fn bind_var(&mut self, var: TyVarId, ty: Type) -> Result<(), TcUnifyError> {
        if let Some(existing) = self.vars.lookup(var).cloned() {
            return self.unify(&existing, &ty);
        }

        if matches!(ty, Type::Var(v) if v == var) {
            return Ok(());
        }

        if self.vars.is_rigid(var) {
            return Err(TcUnifyError::CannotBindRigid(var, ty));
        }

        if matches!(self.vars.kind(var), Some(TypeVarKind::InferInt))
            && !matches!(ty, Type::Int { .. } | Type::Var(_))
        {
            return Err(TcUnifyError::Mismatch(Type::Var(var), ty));
        }

        if self.occurs_in(var, &ty) {
            return Err(TcUnifyError::OccursCheckFailed(var, ty));
        }

        self.vars.bind(var, ty);
        Ok(())
    }

    fn occurs_in(&self, var: TyVarId, ty: &Type) -> bool {
        ty.any(&|t| matches!(t, Type::Var(v) if *v == var))
    }
}

fn canonical_nominal_name(name: &str) -> &str {
    name.split('<').next().unwrap_or(name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolve::DefId;
    use crate::types::Type;

    #[test]
    fn test_unify_binds_infer_var() {
        let mut vars = TypeVarStore::default();
        let infer = vars.fresh_infer_local();
        let mut unifier = TcUnifier::new(vars);

        unifier.unify(&Type::Var(infer), &Type::uint(64)).unwrap();
        assert_eq!(unifier.apply(&Type::Var(infer)), Type::uint(64));
    }

    #[test]
    fn test_unify_rejects_rigid_binding() {
        let mut vars = TypeVarStore::default();
        let rigid = TyVarId::new(0);
        vars.register_rigid_param(rigid, DefId(1));
        let mut unifier = TcUnifier::new(vars);

        let err = unifier
            .unify(&Type::Var(rigid), &Type::uint(64))
            .expect_err("expected rigid var error");
        assert!(matches!(err, TcUnifyError::CannotBindRigid(_, _)));
    }

    #[test]
    fn test_unify_occurs_check() {
        let mut vars = TypeVarStore::default();
        let infer = vars.fresh_infer_local();
        let mut unifier = TcUnifier::new(vars);

        let recursive = Type::Array {
            elem_ty: Box::new(Type::Var(infer)),
            dims: vec![1],
        };
        let err = unifier
            .unify(&Type::Var(infer), &recursive)
            .expect_err("expected occurs-check failure");
        assert!(matches!(err, TcUnifyError::OccursCheckFailed(_, _)));
    }
}
