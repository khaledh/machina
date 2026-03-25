use std::collections::BTreeMap;

use crate::core::ast::{MethodItem, Module, TopLevelItem};
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefTable};
use crate::core::typecheck::type_map::TypeMap;
use crate::core::types::{Type, TypeRenderConfig, render_type};
use crate::services::analysis::syntax_index::span_contains_span;

pub(super) fn hover_type_var_names(
    module: &Module,
    def_table: &DefTable,
    type_map: &TypeMap,
    def_id: Option<DefId>,
    query_span: Span,
) -> Option<BTreeMap<u32, String>> {
    let mut names = def_id
        .and_then(|id| type_map.lookup_def_type_param_names(id).cloned())
        .unwrap_or_default();
    for owner_def_id in [
        enclosing_type_def_owner_def_id(module, def_table, query_span),
        enclosing_callable_owner_def_id(module, def_table, query_span),
    ]
    .into_iter()
    .flatten()
    {
        if let Some(owner_names) = type_map.lookup_def_type_param_names(owner_def_id) {
            for (key, value) in owner_names {
                names.entry(*key).or_insert_with(|| value.clone());
            }
        }
    }
    if names.is_empty() { None } else { Some(names) }
}

pub(super) fn format_type_for_display(
    ty: &Type,
    def_id: Option<DefId>,
    type_map: Option<&TypeMap>,
    type_var_names: Option<&BTreeMap<u32, String>>,
) -> String {
    let resolved_type_var_names = type_var_names.or_else(|| {
        def_id.and_then(|id| type_map.and_then(|map| map.lookup_def_type_param_names(id)))
    });
    render_type(
        ty,
        &TypeRenderConfig {
            show_in_mode: false,
            type_var_names: resolved_type_var_names,
            nominal_name_map: None,
        },
    )
}

fn enclosing_type_def_owner_def_id(
    module: &Module,
    def_table: &DefTable,
    query_span: Span,
) -> Option<DefId> {
    let mut best: Option<(usize, DefId)> = None;

    for item in &module.top_level_items {
        if let TopLevelItem::TypeDef(type_def) = item {
            maybe_update_owner_def_id(
                &mut best,
                def_table.lookup_node_def_id(type_def.id),
                type_def.span,
                query_span,
            );
        }
    }

    best.map(|(_, def_id)| def_id)
}

fn enclosing_callable_owner_def_id(
    module: &Module,
    def_table: &DefTable,
    query_span: Span,
) -> Option<DefId> {
    let mut best: Option<(usize, DefId)> = None;

    for item in &module.top_level_items {
        match item {
            TopLevelItem::FuncDecl(func_decl) => maybe_update_owner_def_id(
                &mut best,
                def_table.lookup_node_def_id(func_decl.id),
                func_decl.span,
                query_span,
            ),
            TopLevelItem::FuncDef(func_def) => maybe_update_owner_def_id(
                &mut best,
                def_table.lookup_node_def_id(func_def.id),
                func_def.span,
                query_span,
            ),
            TopLevelItem::MethodBlock(block) => {
                for method_item in &block.method_items {
                    match method_item {
                        MethodItem::Decl(method_decl) => maybe_update_owner_def_id(
                            &mut best,
                            def_table.lookup_node_def_id(method_decl.id),
                            method_decl.span,
                            query_span,
                        ),
                        MethodItem::Def(method_def) => maybe_update_owner_def_id(
                            &mut best,
                            def_table.lookup_node_def_id(method_def.id),
                            method_def.span,
                            query_span,
                        ),
                    }
                }
            }
            _ => {}
        }
    }

    best.map(|(_, def_id)| def_id)
}

fn maybe_update_owner_def_id(
    best: &mut Option<(usize, DefId)>,
    def_id: Option<DefId>,
    span: Span,
    query_span: Span,
) {
    let Some(def_id) = def_id else {
        return;
    };
    if !span_contains_span(span, query_span) {
        return;
    }
    let width = span.end.offset.saturating_sub(span.start.offset);
    if best
        .as_ref()
        .is_some_and(|(best_width, _)| width >= *best_width)
    {
        return;
    }
    *best = Some((width, def_id));
}
