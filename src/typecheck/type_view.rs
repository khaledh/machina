//! Resolver-backed structural views for nominal types.
//!
//! `TypeViewResolver` materializes struct/enum views from canonical nominal
//! keys. Expansion is memoized and recursion-aware.

use std::collections::{HashMap, HashSet};

use crate::resolve::DefTable;
use crate::tree::resolved as res;
use crate::typecheck::nominal::{
    EnumVariantView, EnumView, ExpansionState, NominalKey, StructFieldView, StructView, TypeView,
};
use crate::typecheck::type_map::{TypeDefLookup, resolve_type_def_with_args};
use crate::types::Type;

pub(crate) struct TypeViewResolver<'a, M: TypeDefLookup> {
    def_table: &'a DefTable,
    module: &'a M,
    cache: HashMap<NominalKey, TypeView>,
    in_progress: HashSet<NominalKey>,
}

impl<'a, M: TypeDefLookup> TypeViewResolver<'a, M> {
    pub(crate) fn new(def_table: &'a DefTable, module: &'a M) -> Self {
        Self {
            def_table,
            module,
            cache: HashMap::new(),
            in_progress: HashSet::new(),
        }
    }

    pub(crate) fn view_of_key(&mut self, key: &NominalKey) -> Option<TypeView> {
        if let Some(cached) = self.cache.get(key) {
            return Some(cached.clone());
        }

        if self.in_progress.contains(key) {
            return self.shallow_view_for_key(key);
        }

        self.in_progress.insert(key.clone());
        let resolved =
            resolve_type_def_with_args(self.def_table, self.module, key.def_id, &key.type_args)
                .ok();

        let view = match resolved {
            Some(ty) => self.type_to_view(key.clone(), ty),
            None => None,
        };

        self.in_progress.remove(key);

        if let Some(view) = view {
            self.cache.insert(key.clone(), view.clone());
            Some(view)
        } else {
            None
        }
    }

    fn shallow_view_for_key(&self, key: &NominalKey) -> Option<TypeView> {
        let type_def = self.module.type_def_by_id(key.def_id)?;
        match &type_def.kind {
            res::TypeDefKind::Struct { .. } => Some(TypeView::Struct(StructView {
                key: key.clone(),
                name: type_def.name.clone(),
                fields: Vec::new(),
                state: ExpansionState::Shallow,
            })),
            res::TypeDefKind::Enum { .. } => Some(TypeView::Enum(EnumView {
                key: key.clone(),
                name: type_def.name.clone(),
                variants: Vec::new(),
                state: ExpansionState::Shallow,
            })),
            _ => None,
        }
    }

    fn type_to_view(&self, key: NominalKey, ty: Type) -> Option<TypeView> {
        match ty {
            Type::Struct { name, fields } => {
                let state = if fields.is_empty() {
                    ExpansionState::Shallow
                } else {
                    ExpansionState::Expanded
                };
                Some(TypeView::Struct(StructView {
                    key,
                    name,
                    fields: fields
                        .into_iter()
                        .map(|field| StructFieldView {
                            name: field.name,
                            ty: field.ty,
                        })
                        .collect(),
                    state,
                }))
            }
            Type::Enum { name, variants } => {
                let state = if variants.is_empty() {
                    ExpansionState::Shallow
                } else {
                    ExpansionState::Expanded
                };
                Some(TypeView::Enum(EnumView {
                    key,
                    name,
                    variants: variants
                        .into_iter()
                        .map(|variant| EnumVariantView {
                            name: variant.name,
                            payload: variant.payload,
                        })
                        .collect(),
                    state,
                }))
            }
            _ => None,
        }
    }
}

#[cfg(test)]
#[path = "../tests/typecheck/t_type_view.rs"]
mod tests;
