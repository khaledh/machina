//! Global-scope completion collection.
//!
//! This module keeps the rules for which top-level definitions are visible as
//! scope completions and how `DefKind` maps to completion item kinds.

use std::collections::{HashMap, HashSet};

use crate::core::resolve::DefKind;
use crate::core::tree::NodeId;
use crate::core::tree::resolved as res;
use crate::services::analysis::results::{CompletionItem, CompletionKind};

pub(super) fn global_scope(
    resolved: &crate::core::context::ResolvedContext,
) -> HashMap<String, CompletionItem> {
    let mut allowed_nodes = HashSet::new();
    for item in &resolved.module.top_level_items {
        match item {
            res::TopLevelItem::ProtocolDef(protocol_def) => {
                allowed_nodes.insert(protocol_def.id);
            }
            res::TopLevelItem::TraitDef(trait_def) => {
                allowed_nodes.insert(trait_def.id);
            }
            res::TopLevelItem::TypeDef(type_def) => {
                allowed_nodes.insert(type_def.id);
                if let res::TypeDefKind::Enum { variants } = &type_def.kind {
                    for variant in variants {
                        allowed_nodes.insert(variant.id);
                    }
                }
            }
            res::TopLevelItem::TypestateDef(typestate_def) => {
                allowed_nodes.insert(typestate_def.id);
            }
            res::TopLevelItem::FuncDecl(func_decl) => {
                allowed_nodes.insert(func_decl.id);
            }
            res::TopLevelItem::FuncDef(func_def) => {
                allowed_nodes.insert(func_def.id);
            }
            res::TopLevelItem::MethodBlock(_) | res::TopLevelItem::ClosureDef(_) => {}
        }
    }

    let mut out = HashMap::new();
    for def in resolved.def_table.defs() {
        let Some(kind) = completion_kind_for_def(&def.kind) else {
            continue;
        };
        if matches!(
            def.kind,
            DefKind::LocalVar { .. } | DefKind::Param { .. } | DefKind::TypeParam
        ) {
            continue;
        }
        let node_allowed = resolved
            .def_table
            .lookup_def_node_id(def.id)
            .is_some_and(|id| allowed_nodes.contains(&id))
            || resolved.def_table.lookup_def_node_id(def.id) == Some(NodeId(0));
        if !node_allowed {
            continue;
        }
        out.insert(
            def.name.clone(),
            CompletionItem {
                label: def.name.clone(),
                kind,
                def_id: def.id,
                detail: Some(def.kind.to_string()),
            },
        );
    }
    out
}

pub(super) fn completion_kind_for_def(kind: &DefKind) -> Option<CompletionKind> {
    match kind {
        DefKind::ProtocolDef => Some(CompletionKind::Type),
        DefKind::ProtocolRole => Some(CompletionKind::EnumVariant),
        DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => Some(CompletionKind::Function),
        DefKind::TypeDef { .. } => Some(CompletionKind::Type),
        DefKind::TraitDef { .. } => Some(CompletionKind::Trait),
        DefKind::LocalVar { .. } => Some(CompletionKind::Variable),
        DefKind::Param { .. } => Some(CompletionKind::Parameter),
        DefKind::TypeParam => Some(CompletionKind::TypeParameter),
        DefKind::EnumVariantName => Some(CompletionKind::EnumVariant),
    }
}
