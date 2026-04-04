use std::collections::HashMap;

use crate::core::types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

impl TypeId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Default, Clone)]
pub struct TypeCache {
    types: Vec<Type>,
    ids: HashMap<Type, TypeId>,
}

impl TypeCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, ty: Type) -> TypeId {
        if let Some(id) = self.ids.get(&ty).copied() {
            if should_upgrade_interned_type(self.types[id.index()].clone(), &ty) {
                self.types[id.index()] = ty;
            }
            return id;
        }
        let id = TypeId(self.types.len() as u32);
        self.types.push(ty.clone());
        self.ids.insert(ty, id);
        id
    }

    pub fn get(&self, id: TypeId) -> &Type {
        &self.types[id.index()]
    }

    pub fn lookup_id(&self, ty: &Type) -> Option<TypeId> {
        self.ids.get(ty).copied()
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    pub fn entries(&self) -> impl Iterator<Item = (TypeId, &Type)> {
        self.types
            .iter()
            .enumerate()
            .map(|(index, ty)| (TypeId(index as u32), ty))
    }
}

fn should_upgrade_interned_type(existing: Type, incoming: &Type) -> bool {
    // `Type` equality is nominal for structs/enums, so wrappers like `Heap<Node>`
    // can compare equal even when the inner nominal shape gets richer later.
    // Prefer the structurally richer representative whenever an equivalent type
    // is interned again.
    type_info_score(incoming) > type_info_score(&existing)
}

fn type_info_score(ty: &Type) -> usize {
    match ty {
        Type::Unknown | Type::Var(_) => 0,
        Type::Unit
        | Type::PAddr
        | Type::NullablePAddr
        | Type::VAddr
        | Type::NullableVAddr
        | Type::Int { .. }
        | Type::Bool
        | Type::Char
        | Type::String => 1,
        Type::Borrow { elem_ty } => 1 + type_info_score(elem_ty),
        Type::Fn { params, ret_ty } => {
            1 + params
                .iter()
                .map(|param| type_info_score(&param.ty))
                .sum::<usize>()
                + type_info_score(ret_ty)
        }
        Type::ErrorUnion { ok_ty, err_tys } => {
            1 + type_info_score(ok_ty) + err_tys.iter().map(type_info_score).sum::<usize>()
        }
        Type::Range { elem_ty }
        | Type::Slice { elem_ty }
        | Type::DynArray { elem_ty }
        | Type::View { elem_ty }
        | Type::NullableView { elem_ty }
        | Type::NullableViewSlice { elem_ty }
        | Type::NullableViewArray { elem_ty }
        | Type::RawPtr { elem_ty }
        | Type::ViewSlice { elem_ty }
        | Type::ViewArray { elem_ty }
        | Type::Set { elem_ty }
        | Type::Iterable { item_ty: elem_ty }
        | Type::Heap { elem_ty }
        | Type::Ref { elem_ty, .. } => 1 + type_info_score(elem_ty),
        Type::Pending { response_tys } | Type::ReplyCap { response_tys } => {
            1 + response_tys.iter().map(type_info_score).sum::<usize>()
        }
        Type::Map { key_ty, value_ty } => 1 + type_info_score(key_ty) + type_info_score(value_ty),
        Type::Array { elem_ty, dims } => 1 + dims.len() + type_info_score(elem_ty),
        Type::Tuple { field_tys } => 1 + field_tys.iter().map(type_info_score).sum::<usize>(),
        Type::Struct { fields, .. } => {
            if fields.is_empty() {
                1
            } else {
                16 + fields
                    .iter()
                    .map(|field| type_info_score(&field.ty))
                    .sum::<usize>()
            }
        }
        Type::Enum { variants, .. } => {
            if variants.is_empty() {
                1
            } else {
                16 + variants
                    .iter()
                    .map(|variant| 1 + variant.payload.iter().map(type_info_score).sum::<usize>())
                    .sum::<usize>()
            }
        }
    }
}

#[cfg(test)]
#[path = "../../tests/core/types/t_type_cache.rs"]
mod tests;
