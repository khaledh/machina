//! Imported stdlib interface lookups for editor features.
//!
//! These helpers let program-mode analysis answer hover, signature-help, and
//! go-to-definition for imported stdlib symbols from cached `.mci` metadata
//! without depending on source-backed module states.

use crate::core::context::ResolvedContext;
use crate::core::diag::Span;
use crate::core::interface::{
    ExportedDef, ExportedDefKind, JsonModuleInterfaceCodec, MethodExport, TraitDefExport,
    TypeDefExport, TypeDefExportKind, load_stdlib_module_interface_with_codec,
};
use crate::core::resolve::{DefId, UNKNOWN_DEF_ID};
use crate::core::symbol_id::{SymbolId, TypeKey};
use crate::services::analysis::results::{
    CompletionItem, CompletionKind, HoverInfo, Location, SignatureHelp,
};
use crate::services::analysis::snapshot::FileId;

pub(crate) fn hover_for_imported_stdlib_symbol(symbol_id: &SymbolId) -> Option<HoverInfo> {
    let export = load_stdlib_export(symbol_id)?;
    Some(HoverInfo {
        node_id: crate::core::ast::NodeId(0),
        span: export_source_span(&export).unwrap_or_default(),
        def_id: None,
        symbol_id: Some(symbol_id.clone()),
        def_name: Some(export_name(&export).to_string()),
        ty: None,
        display: format_export_label(&export),
    })
}

pub(crate) fn location_for_imported_stdlib_symbol(
    origin_file_id: FileId,
    symbol_id: &SymbolId,
) -> Option<Location> {
    let export = load_stdlib_export(symbol_id)?;
    let location = export.tooling.as_ref()?.source_location.as_ref()?;
    Some(Location {
        file_id: origin_file_id,
        path: Some(location.file.clone()),
        span: location.name_span,
    })
}

pub(crate) fn signature_help_for_imported_stdlib_symbol(
    symbol_id: &SymbolId,
    active_parameter: usize,
) -> Option<SignatureHelp> {
    let export = load_stdlib_export(symbol_id)?;
    match &export.kind {
        ExportedDefKind::Func(func) => Some(SignatureHelp {
            label: format_callable_label(
                &func.signature.name,
                &func.signature.type_params,
                &func.signature.params,
                &func.signature.ret_ty,
            ),
            def_id: None,
            symbol_id: Some(symbol_id.clone()),
            active_parameter: active_parameter.min(func.signature.params.len().saturating_sub(1)),
            parameters: func
                .signature
                .params
                .iter()
                .map(|param| {
                    format_param_for_display_with_names(param, &func.signature.type_params)
                })
                .collect(),
        }),
        ExportedDefKind::Method(method) => Some(SignatureHelp {
            label: format_method_label(method),
            def_id: None,
            symbol_id: Some(symbol_id.clone()),
            active_parameter: active_parameter.min(method.signature.params.len().saturating_sub(1)),
            parameters: method
                .signature
                .params
                .iter()
                .map(|param| {
                    format_param_for_display_with_names(param, &method.signature.type_params)
                })
                .collect(),
        }),
        _ => None,
    }
}

pub(crate) fn completion_for_imported_stdlib_symbol(
    symbol_id: &SymbolId,
    def_id: DefId,
) -> Option<CompletionItem> {
    let export = load_stdlib_export(symbol_id)?;
    Some(CompletionItem {
        label: export_name(&export).to_string(),
        kind: completion_kind_for_export(&export),
        def_id,
        detail: Some(format_export_label(&export)),
    })
}

pub(crate) fn qualified_path_completions_for_imported_stdlib_type(
    resolved: &ResolvedContext,
    path_segments: &[String],
) -> Vec<CompletionItem> {
    if path_segments.len() != 1 {
        return Vec::new();
    }
    let Some(owner_name) = path_segments.first() else {
        return Vec::new();
    };
    let Some(symbol_id) = resolved
        .def_table
        .defs()
        .iter()
        .find(|def| def.name == *owner_name)
        .and_then(|def| resolved.symbol_ids.lookup_symbol_id(def.id))
    else {
        return Vec::new();
    };
    let Some(export) = load_stdlib_export(symbol_id) else {
        return Vec::new();
    };
    let ExportedDefKind::Type(type_export) = &export.kind else {
        return Vec::new();
    };
    let TypeDefExportKind::Enum { variants } = &type_export.kind else {
        return Vec::new();
    };
    variants
        .iter()
        .map(|variant| CompletionItem {
            label: variant.name.clone(),
            kind: CompletionKind::EnumVariant,
            def_id: UNKNOWN_DEF_ID,
            detail: Some(if variant.payload.is_empty() {
                "enum variant".to_string()
            } else {
                format!(
                    "{}({})",
                    variant.name,
                    variant
                        .payload
                        .iter()
                        .map(|ty| format_type_key_with_names(ty, &type_export.type_params))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }),
        })
        .collect()
}

fn load_stdlib_export(symbol_id: &SymbolId) -> Option<ExportedDef> {
    if symbol_id.module.segments().first().map(String::as_str) != Some("std") {
        return None;
    }
    let interface =
        load_stdlib_module_interface_with_codec::<JsonModuleInterfaceCodec>(&symbol_id.module)
            .ok()?;
    interface
        .exports
        .into_iter()
        .find(|export| export.symbol_id == *symbol_id)
}

fn export_source_span(export: &ExportedDef) -> Option<Span> {
    export
        .tooling
        .as_ref()?
        .source_location
        .as_ref()
        .map(|loc| loc.name_span)
}

fn export_name(export: &ExportedDef) -> &str {
    match &export.kind {
        ExportedDefKind::Func(func) => &func.signature.name,
        ExportedDefKind::Method(method) => &method.signature.name,
        ExportedDefKind::Type(ty) => &ty.name,
        ExportedDefKind::Trait(trait_def) => &trait_def.name,
    }
}

fn format_export_label(export: &ExportedDef) -> String {
    match &export.kind {
        ExportedDefKind::Func(func) => format_callable_label(
            &func.signature.name,
            &func.signature.type_params,
            &func.signature.params,
            &func.signature.ret_ty,
        ),
        ExportedDefKind::Method(method) => format_method_label(method),
        ExportedDefKind::Type(ty) => format_type_label(ty),
        ExportedDefKind::Trait(trait_def) => format_trait_label(trait_def),
    }
}

fn completion_kind_for_export(export: &ExportedDef) -> CompletionKind {
    match &export.kind {
        ExportedDefKind::Func(_) | ExportedDefKind::Method(_) => CompletionKind::Function,
        ExportedDefKind::Type(_) => CompletionKind::Type,
        ExportedDefKind::Trait(_) => CompletionKind::Trait,
    }
}

fn format_method_label(method: &MethodExport) -> String {
    let mut params = Vec::with_capacity(method.signature.params.len() + 1);
    params.push(match method.signature.self_param.mode {
        crate::core::ast::ParamMode::In => "self".to_string(),
        crate::core::ast::ParamMode::InOut => "inout self".to_string(),
        crate::core::ast::ParamMode::Out => "out self".to_string(),
        crate::core::ast::ParamMode::Sink => "sink self".to_string(),
    });
    params.extend(
        method
            .signature
            .params
            .iter()
            .map(|param| format_param_for_display_with_names(param, &method.signature.type_params)),
    );
    let tparams = format_type_params(&method.signature.type_params);
    format!(
        "fn {}{}({}) -> {}",
        method.signature.name,
        tparams,
        params.join(", "),
        format_type_key_with_names(&method.signature.ret_ty, &method.signature.type_params)
    )
}

fn format_callable_label(
    name: &str,
    type_params: &[String],
    params: &[crate::core::interface::InterfaceParam],
    ret_ty: &TypeKey,
) -> String {
    format!(
        "fn {name}{}({}) -> {}",
        format_type_params(type_params),
        params
            .iter()
            .map(|param| format_param_for_display_with_names(param, type_params))
            .collect::<Vec<_>>()
            .join(", "),
        format_type_key_with_names(ret_ty, type_params)
    )
}

fn format_type_label(ty: &TypeDefExport) -> String {
    let tparams = format_type_params(&ty.type_params);
    match &ty.kind {
        TypeDefExportKind::Alias { aliased_ty } => {
            format!(
                "type {}{} = {}",
                ty.name,
                tparams,
                format_type_key_with_names(aliased_ty, &ty.type_params)
            )
        }
        TypeDefExportKind::Struct { fields } => format!(
            "type {}{} = {{ {} }}",
            ty.name,
            tparams,
            fields
                .iter()
                .map(|field| {
                    format!(
                        "{}: {}",
                        field.name,
                        format_type_key_with_names(&field.ty, &ty.type_params)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ")
        ),
        TypeDefExportKind::Enum { variants } => format!(
            "type {}{} = {}",
            ty.name,
            tparams,
            variants
                .iter()
                .map(|variant| {
                    if variant.payload.is_empty() {
                        variant.name.clone()
                    } else {
                        format!(
                            "{}({})",
                            variant.name,
                            variant
                                .payload
                                .iter()
                                .map(|ty_key| format_type_key_with_names(ty_key, &ty.type_params))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                })
                .collect::<Vec<_>>()
                .join(" | ")
        ),
        TypeDefExportKind::Linear { .. } => format!("type {}{}", ty.name, tparams),
    }
}

fn format_trait_label(trait_def: &TraitDefExport) -> String {
    format!("trait {}", trait_def.name)
}

fn format_param_for_display_with_names(
    param: &crate::core::interface::InterfaceParam,
    type_param_names: &[String],
) -> String {
    let mode = match param.mode {
        crate::core::ast::ParamMode::In => "",
        crate::core::ast::ParamMode::InOut => "inout ",
        crate::core::ast::ParamMode::Out => "out ",
        crate::core::ast::ParamMode::Sink => "sink ",
    };
    format!(
        "{mode}{}: {}",
        param.name,
        format_type_key_with_names(&param.ty, type_param_names)
    )
}

fn format_type_params(type_params: &[String]) -> String {
    if type_params.is_empty() {
        String::new()
    } else {
        format!("<{}>", type_params.join(", "))
    }
}

fn format_type_key_with_names(ty: &TypeKey, type_param_names: &[String]) -> String {
    match ty {
        TypeKey::Unit => "()".to_string(),
        TypeKey::Bool => "bool".to_string(),
        TypeKey::Char => "char".to_string(),
        TypeKey::String => "string".to_string(),
        TypeKey::Int { signed, bits, .. } => {
            if *signed {
                format!("i{bits}")
            } else {
                format!("u{bits}")
            }
        }
        TypeKey::Named { module, path, args } => {
            let prefix = if module.segments().is_empty() {
                String::new()
            } else {
                format!("{}::", module.segments().join("::"))
            };
            if args.is_empty() {
                format!("{prefix}{path}")
            } else {
                format!(
                    "{prefix}{path}<{}>",
                    args.iter()
                        .map(|arg| format_type_key_with_names(arg, type_param_names))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        TypeKey::Tuple(items) => format!(
            "({})",
            items
                .iter()
                .map(|item| format_type_key_with_names(item, type_param_names))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        TypeKey::Array { elem, dims } => format!(
            "{}[{}]",
            format_type_key_with_names(elem, type_param_names),
            dims.iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join("; ")
        ),
        TypeKey::Slice(elem) => format!("{}[]", format_type_key_with_names(elem, type_param_names)),
        TypeKey::DynArray(elem) => {
            format!("{}[*]", format_type_key_with_names(elem, type_param_names))
        }
        TypeKey::Heap(elem) => format!("~{}", format_type_key_with_names(elem, type_param_names)),
        TypeKey::Set(elem) => {
            format!(
                "set<{}>",
                format_type_key_with_names(elem, type_param_names)
            )
        }
        TypeKey::Map { key, value } => format!(
            "map<{}, {}>",
            format_type_key_with_names(key, type_param_names),
            format_type_key_with_names(value, type_param_names)
        ),
        TypeKey::Fn { params, ret } => format!(
            "fn({}) -> {}",
            params
                .iter()
                .map(|p| format_type_key_with_names(&p.ty, type_param_names))
                .collect::<Vec<_>>()
                .join(", "),
            format_type_key_with_names(ret, type_param_names)
        ),
        TypeKey::ErrorUnion { ok, errs } => {
            let mut parts = vec![format_type_key_with_names(ok, type_param_names)];
            parts.extend(
                errs.iter()
                    .map(|err| format_type_key_with_names(err, type_param_names)),
            );
            parts.join(" | ")
        }
        TypeKey::Ref { mutable, elem } => {
            if *mutable {
                format!(
                    "inout {}",
                    format_type_key_with_names(elem, type_param_names)
                )
            } else {
                format!("&{}", format_type_key_with_names(elem, type_param_names))
            }
        }
        TypeKey::Refined { base, .. } => format_type_key_with_names(base, type_param_names),
        TypeKey::GenericParam(index) => type_param_names
            .get(*index as usize)
            .cloned()
            .unwrap_or_else(|| format!("T{index}")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::capsule::ModulePath;
    use crate::core::interface::load_stdlib_module_interface_with_codec;

    fn stdlib_symbol(module: &str, symbol: &str) -> SymbolId {
        let module_path = ModulePath::new(module.split("::").map(|s| s.to_string()).collect())
            .expect("module path should parse");
        let interface =
            load_stdlib_module_interface_with_codec::<JsonModuleInterfaceCodec>(&module_path)
                .expect("stdlib interface should load");
        interface
            .exports
            .into_iter()
            .find(|export| export.symbol_id.to_string() == symbol)
            .expect("expected exported symbol")
            .symbol_id
    }

    #[test]
    fn imported_stdlib_hover_uses_interface_signature() {
        let symbol = stdlib_symbol("std::io", "std::io::println");
        let hover = hover_for_imported_stdlib_symbol(&symbol)
            .expect("expected stdlib hover from interface");
        assert_eq!(hover.def_name.as_deref(), Some("println"));
        assert!(hover.display.starts_with("fn println("));
    }

    #[test]
    fn imported_stdlib_signature_help_preserves_generic_names() {
        let symbol = stdlib_symbol("std::iter", "std::iter::map");
        let sig = signature_help_for_imported_stdlib_symbol(&symbol, 0)
            .expect("expected stdlib signature from interface");
        assert!(
            sig.label.contains("fn map<S, In, Out>"),
            "expected source-facing generic names, got: {}",
            sig.label
        );
    }

    #[test]
    fn imported_stdlib_completion_uses_interface_signature_detail() {
        let symbol = stdlib_symbol("std::iter", "std::iter::map");
        let item = completion_for_imported_stdlib_symbol(&symbol, UNKNOWN_DEF_ID)
            .expect("expected stdlib completion from interface");
        assert_eq!(item.label, "map");
        assert!(
            item.detail
                .as_deref()
                .is_some_and(|detail| detail.contains("fn map<S, In, Out>(")),
            "expected source-facing generic names, got: {:?}",
            item.detail
        );
    }

    #[test]
    fn imported_stdlib_definition_uses_interface_source_location() {
        let symbol = stdlib_symbol("std::io", "std::io::println");
        let location = location_for_imported_stdlib_symbol(FileId(0), &symbol)
            .expect("expected stdlib location from interface");
        let path = location.path.expect("expected source path");
        assert!(path.ends_with("std/io.mc"), "got path {}", path.display());
        assert!(location.span.start.line > 0);
    }
}
