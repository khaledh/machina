use std::collections::HashMap;

use crate::types::Type;

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
    match (&existing, incoming) {
        (Type::Struct { name: lhs, .. }, Type::Struct { name: rhs, .. }) if lhs == rhs => {
            type_info_score(incoming) > type_info_score(&existing)
        }
        (Type::Enum { name: lhs, .. }, Type::Enum { name: rhs, .. }) if lhs == rhs => {
            type_info_score(incoming) > type_info_score(&existing)
        }
        _ => false,
    }
}

fn type_info_score(ty: &Type) -> usize {
    match ty {
        Type::Unknown | Type::Var(_) => 0,
        Type::Unit | Type::Int { .. } | Type::Bool | Type::Char | Type::String => 1,
        Type::Fn { params, ret_ty } => {
            1 + params
                .iter()
                .map(|param| type_info_score(&param.ty))
                .sum::<usize>()
                + type_info_score(ret_ty)
        }
        Type::Range { elem_ty }
        | Type::Slice { elem_ty }
        | Type::Heap { elem_ty }
        | Type::Ref { elem_ty, .. } => 1 + type_info_score(elem_ty),
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
mod tests {
    use super::*;
    use crate::types::{EnumVariant, StructField};

    #[test]
    fn test_intern_upgrades_struct_when_richer_type_arrives() {
        let mut cache = TypeCache::new();
        let node_shallow = Type::Struct {
            name: "Node".to_string(),
            fields: Vec::new(),
        };
        let id = cache.intern(node_shallow);
        let node_full = Type::Struct {
            name: "Node".to_string(),
            fields: vec![StructField {
                name: "value".to_string(),
                ty: Type::uint(64),
            }],
        };

        let id2 = cache.intern(node_full);
        assert_eq!(id.index(), id2.index());
        match cache.get(id) {
            Type::Struct { fields, .. } => assert_eq!(fields.len(), 1),
            other => panic!("expected struct type, got {other:?}"),
        }
    }

    #[test]
    fn test_intern_keeps_richer_enum_when_shallow_seen_later() {
        let mut cache = TypeCache::new();
        let link_full = Type::Enum {
            name: "Link".to_string(),
            variants: vec![
                EnumVariant {
                    name: "None".to_string(),
                    payload: Vec::new(),
                },
                EnumVariant {
                    name: "Some".to_string(),
                    payload: vec![Type::Heap {
                        elem_ty: Box::new(Type::Struct {
                            name: "Node".to_string(),
                            fields: vec![StructField {
                                name: "next".to_string(),
                                ty: Type::Enum {
                                    name: "Link".to_string(),
                                    variants: Vec::new(),
                                },
                            }],
                        }),
                    }],
                },
            ],
        };
        let id = cache.intern(link_full);
        let link_shallow = Type::Enum {
            name: "Link".to_string(),
            variants: Vec::new(),
        };
        let id2 = cache.intern(link_shallow);

        assert_eq!(id.index(), id2.index());
        match cache.get(id) {
            Type::Enum { variants, .. } => assert_eq!(variants.len(), 2),
            other => panic!("expected enum type, got {other:?}"),
        }
    }
}
