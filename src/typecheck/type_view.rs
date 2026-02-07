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
mod tests {
    use super::*;
    use crate::context::ParsedContext;
    use crate::lexer::{LexError, Lexer, Token};
    use crate::parse::Parser;
    use crate::resolve::resolve;

    fn resolve_source(source: &str) -> crate::context::ResolvedContext {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .expect("failed to tokenize");
        let mut parser = Parser::new(&tokens);
        let module = parser.parse().expect("failed to parse");
        let id_gen = parser.into_id_gen();
        let parsed = ParsedContext::new(module, id_gen);
        resolve(parsed).expect("failed to resolve")
    }

    #[test]
    fn test_view_resolver_is_deterministic_for_recursive_nominals() {
        let ctx = resolve_source(
            r#"
            type Link = None | Some(^Node)
            type Node = { value: u64, next: Link }
            fn main() { () }
            "#,
        );
        let link_def_id = ctx
            .def_table
            .lookup_type_def_id("Link")
            .expect("expected Link type def");
        let node_def_id = ctx
            .def_table
            .lookup_type_def_id("Node")
            .expect("expected Node type def");
        let link_key = NominalKey::new(link_def_id, Vec::new());
        let node_key = NominalKey::new(node_def_id, Vec::new());

        let mut first = TypeViewResolver::new(&ctx.def_table, &ctx.module);
        let node_first = first.view_of_key(&node_key).expect("expected Node view");
        let link_first = first.view_of_key(&link_key).expect("expected Link view");

        let mut second = TypeViewResolver::new(&ctx.def_table, &ctx.module);
        let link_second = second.view_of_key(&link_key).expect("expected Link view");
        let node_second = second.view_of_key(&node_key).expect("expected Node view");

        assert_eq!(node_first, node_second);
        assert_eq!(link_first, link_second);
        match node_first {
            TypeView::Struct(struct_view) => {
                assert_eq!(struct_view.state, ExpansionState::Expanded);
                assert_eq!(struct_view.fields.len(), 2);
            }
            other => panic!("expected struct view for Node, got {other:?}"),
        }
        match link_first {
            TypeView::Enum(enum_view) => {
                assert_eq!(enum_view.state, ExpansionState::Expanded);
                assert_eq!(enum_view.variants.len(), 2);
            }
            other => panic!("expected enum view for Link, got {other:?}"),
        }
    }
}
