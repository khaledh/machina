//! Member and qualified-path completion providers.
//!
//! This module owns completion candidates that require type/nominal lookup:
//! - `receiver.member` completions
//! - `Name::...` qualified path completions

use crate::core::capsule::ModuleId;
use crate::core::resolve::{DefId, UNKNOWN_DEF_ID};
use crate::core::tree::{MethodItem, TopLevelItem, TypeDefKind};
use crate::core::types::Type;
use crate::services::analysis::results::{CompletionItem, CompletionKind};

pub(super) fn member_completions(
    resolved: &crate::core::context::ResolvedContext,
    receiver_ty: &Type,
    caller_def_id: Option<DefId>,
) -> Vec<CompletionItem> {
    let mut out = Vec::new();
    let owner = receiver_ty.peel_heap();

    if let Type::Struct { name, fields } = &owner {
        if struct_fields_accessible(resolved, name, caller_def_id) {
            for field in fields {
                out.push(CompletionItem {
                    label: field.name.clone(),
                    kind: CompletionKind::Variable,
                    def_id: UNKNOWN_DEF_ID,
                    detail: Some(field.ty.to_string()),
                });
            }
        }
    }

    out.extend(builtin_member_completions(&owner));
    out.extend(nominal_method_completions(resolved, &owner, caller_def_id));
    out
}

pub(super) fn qualified_path_completions(
    resolved: &crate::core::context::ResolvedContext,
    path_segments: &[String],
) -> Vec<CompletionItem> {
    // V1 scope: resolve first-segment nominal paths (`Protocol::Role`,
    // `Enum::Variant`) that appear in typestate/protocol surface syntax.
    if path_segments.len() != 1 {
        return Vec::new();
    }
    let Some(owner_name) = path_segments.first() else {
        return Vec::new();
    };

    for item in &resolved.module.top_level_items {
        match item {
            TopLevelItem::ProtocolDef(protocol_def) if protocol_def.name == *owner_name => {
                return protocol_def
                    .roles
                    .iter()
                    .map(|role| CompletionItem {
                        label: role.name.clone(),
                        kind: CompletionKind::EnumVariant,
                        def_id: resolved
                            .def_table
                            .lookup_node_def_id(role.id)
                            .unwrap_or(UNKNOWN_DEF_ID),
                        detail: Some("protocol role".to_string()),
                    })
                    .collect();
            }
            TopLevelItem::TypeDef(type_def) if type_def.name == *owner_name => {
                if let TypeDefKind::Enum { variants } = &type_def.kind {
                    return variants
                        .iter()
                        .map(|variant| CompletionItem {
                            label: variant.name.clone(),
                            kind: CompletionKind::EnumVariant,
                            def_id: resolved
                                .def_table
                                .lookup_node_def_id(variant.id)
                                .unwrap_or(UNKNOWN_DEF_ID),
                            detail: Some("enum variant".to_string()),
                        })
                        .collect();
                }
                return Vec::new();
            }
            _ => {}
        }
    }
    Vec::new()
}

fn builtin_member_completions(owner: &Type) -> Vec<CompletionItem> {
    let mut out = Vec::new();
    match owner {
        Type::String | Type::Array { .. } | Type::Slice { .. } => {
            push_builtin_prop(&mut out, "len", "u64");
        }
        Type::DynArray { .. } => {
            push_builtin_prop(&mut out, "len", "u64");
            push_builtin_prop(&mut out, "capacity", "u64");
            push_builtin_prop(&mut out, "is_empty", "bool");
            push_builtin_method(&mut out, "append", "builtin");
        }
        Type::Set { .. } => {
            push_builtin_prop(&mut out, "len", "u64");
            push_builtin_prop(&mut out, "capacity", "u64");
            push_builtin_prop(&mut out, "is_empty", "bool");
            push_builtin_method(&mut out, "insert", "builtin");
            push_builtin_method(&mut out, "remove", "builtin");
            push_builtin_method(&mut out, "contains", "builtin");
            push_builtin_method(&mut out, "clear", "builtin");
        }
        Type::Map { .. } => {
            push_builtin_prop(&mut out, "len", "u64");
            push_builtin_prop(&mut out, "capacity", "u64");
            push_builtin_prop(&mut out, "is_empty", "bool");
            push_builtin_method(&mut out, "insert", "builtin");
            push_builtin_method(&mut out, "remove", "builtin");
            push_builtin_method(&mut out, "contains_key", "builtin");
            push_builtin_method(&mut out, "get", "builtin");
            push_builtin_method(&mut out, "clear", "builtin");
        }
        _ => {}
    }
    out
}

fn push_builtin_prop(out: &mut Vec<CompletionItem>, label: &str, detail: &str) {
    out.push(CompletionItem {
        label: label.to_string(),
        kind: CompletionKind::Variable,
        def_id: UNKNOWN_DEF_ID,
        detail: Some(detail.to_string()),
    });
}

fn push_builtin_method(out: &mut Vec<CompletionItem>, label: &str, detail: &str) {
    out.push(CompletionItem {
        label: label.to_string(),
        kind: CompletionKind::Function,
        def_id: UNKNOWN_DEF_ID,
        detail: Some(detail.to_string()),
    });
}

fn nominal_method_completions(
    resolved: &crate::core::context::ResolvedContext,
    owner_ty: &Type,
    caller_def_id: Option<DefId>,
) -> Vec<CompletionItem> {
    let owner_name = match owner_ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name,
        _ => return Vec::new(),
    };

    let mut out = Vec::new();
    for block in resolved.module.method_blocks() {
        if block.type_name != *owner_name {
            continue;
        }
        for item in &block.method_items {
            let (def_id, name, attrs) = match item {
                MethodItem::Decl(decl) => (
                    resolved.def_table.def_id(decl.id),
                    decl.sig.name.clone(),
                    &decl.attrs,
                ),
                MethodItem::Def(def) => (
                    resolved.def_table.def_id(def.id),
                    def.sig.name.clone(),
                    &def.attrs,
                ),
            };
            if !method_accessible(resolved, def_id, caller_def_id) {
                continue;
            }
            let is_prop_get = attrs.iter().any(|a| a.name == "__property_get");
            let is_prop_set = attrs.iter().any(|a| a.name == "__property_set");
            let kind = if is_prop_get || is_prop_set {
                CompletionKind::Variable
            } else {
                CompletionKind::Function
            };
            out.push(CompletionItem {
                label: name,
                kind,
                def_id,
                detail: Some(
                    resolved
                        .def_table
                        .lookup_def(def_id)
                        .map_or_else(|| "member".to_string(), |d| d.kind.to_string()),
                ),
            });
        }
    }
    out
}

fn method_accessible(
    resolved: &crate::core::context::ResolvedContext,
    target_def_id: DefId,
    caller_def_id: Option<DefId>,
) -> bool {
    let Some(def) = resolved.def_table.lookup_def(target_def_id) else {
        return false;
    };
    if def.is_public() {
        return true;
    }
    let Some(caller_def_id) = caller_def_id else {
        // Best-effort fallback for query contexts where we cannot identify an
        // enclosing callable yet.
        return true;
    };
    let caller_module = resolved
        .def_owners
        .get(&caller_def_id)
        .copied()
        .or(Some(ModuleId(0)));
    let target_module = resolved
        .def_owners
        .get(&target_def_id)
        .copied()
        .or(Some(ModuleId(0)));
    caller_module == target_module
}

fn struct_fields_accessible(
    resolved: &crate::core::context::ResolvedContext,
    type_name: &str,
    caller_def_id: Option<DefId>,
) -> bool {
    let Some(type_def_id) = resolved.def_table.lookup_type_def_id(type_name) else {
        return true;
    };
    let Some(type_def) = resolved.def_table.lookup_def(type_def_id) else {
        return true;
    };
    if !type_def.is_opaque() {
        return true;
    }
    let Some(caller_def_id) = caller_def_id else {
        return false;
    };
    let caller_module = resolved
        .def_owners
        .get(&caller_def_id)
        .copied()
        .or(Some(ModuleId(0)));
    let owner_module = resolved
        .def_owners
        .get(&type_def_id)
        .copied()
        .or(Some(ModuleId(0)));
    caller_module == owner_module
}
