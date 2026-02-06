use std::collections::HashMap;

use crate::resolve::DefId;
use crate::types::{EnumVariant, FnParam, StructField, TyVarId, Type};

pub(crate) const INFER_VAR_BASE: u32 = 1_000_000;
pub(crate) const META_VAR_BASE: u32 = 2_000_000;

/// Type variables used by the new checker.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub(crate) enum TypeVarKind {
    RigidParam(DefId),
    InferLocal,
    Meta,
}

/// Polymorphic type scheme (`forall` vars . ty).
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub(crate) struct TypeScheme {
    pub(crate) quantified: Vec<TyVarId>,
    pub(crate) ty: Type,
}

/// Variable metadata and substitutions owned by the new solver.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct TypeVarStore {
    next_infer: u32,
    next_meta: u32,
    kinds: HashMap<TyVarId, TypeVarKind>,
    subst: HashMap<TyVarId, Type>,
}

impl Default for TypeVarStore {
    fn default() -> Self {
        Self {
            next_infer: INFER_VAR_BASE,
            next_meta: META_VAR_BASE,
            kinds: HashMap::new(),
            subst: HashMap::new(),
        }
    }
}

#[allow(dead_code)]
impl TypeVarStore {
    pub(crate) fn register_rigid_param(&mut self, var: TyVarId, def_id: DefId) {
        self.kinds.insert(var, TypeVarKind::RigidParam(def_id));
    }

    pub(crate) fn register_infer_local(&mut self, var: TyVarId) {
        self.kinds.insert(var, TypeVarKind::InferLocal);
    }

    pub(crate) fn register_meta(&mut self, var: TyVarId) {
        self.kinds.insert(var, TypeVarKind::Meta);
    }

    pub(crate) fn fresh_infer_local(&mut self) -> TyVarId {
        let id = TyVarId::new(self.next_infer);
        self.next_infer = self.next_infer.saturating_add(1);
        self.register_infer_local(id);
        id
    }

    pub(crate) fn fresh_meta(&mut self) -> TyVarId {
        let id = TyVarId::new(self.next_meta);
        self.next_meta = self.next_meta.saturating_add(1);
        self.register_meta(id);
        id
    }

    pub(crate) fn kind(&self, var: TyVarId) -> Option<TypeVarKind> {
        self.kinds.get(&var).copied()
    }

    pub(crate) fn is_rigid(&self, var: TyVarId) -> bool {
        matches!(self.kind(var), Some(TypeVarKind::RigidParam(_)))
    }

    pub(crate) fn bind(&mut self, var: TyVarId, ty: Type) {
        self.subst.insert(var, ty);
    }

    pub(crate) fn lookup(&self, var: TyVarId) -> Option<&Type> {
        self.subst.get(&var)
    }

    pub(crate) fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(var) => match self.subst.get(var) {
                Some(bound_ty) => {
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
            Type::Array { elem_ty, dims } => {
                let applied_elem = self.apply(elem_ty);
                match applied_elem {
                    Type::Array {
                        elem_ty: inner_elem,
                        dims: inner_dims,
                    } => {
                        let mut merged_dims = dims.clone();
                        merged_dims.extend(inner_dims);
                        Type::Array {
                            elem_ty: inner_elem,
                            dims: merged_dims,
                        }
                    }
                    other => Type::Array {
                        elem_ty: Box::new(other),
                        dims: dims.clone(),
                    },
                }
            }
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
            _ => ty.clone(),
        }
    }
}
