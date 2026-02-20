//! Hover information lookups and formatting helpers.
//!
//! The main entry point, [`hover_at_span_in_file`], attempts several strategies
//! in priority order (call-site → AST node → typestate role → def table →
//! syntactic field). When only name-resolution (no types) is available, a
//! simpler resolve-only path is used instead.

use std::path::Path;

use crate::core::diag::{Position, Span};
use crate::core::resolve::{DefId, DefTable, UNKNOWN_DEF_ID};
use crate::core::typecheck::type_map::TypeMap;
use crate::core::types::{Type, TypeRenderConfig, render_type};
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::HoverInfo;
use crate::services::analysis::syntax_index::{
    call_site_at_span, node_at_span, node_span_map, span_contains_span,
};

use super::TypestateNameDemangler;
use super::definition::typestate_role_def_at_span;

pub(crate) fn hover_at_span_in_file(
    state: &LookupState,
    query_span: Span,
    current_file_path: Option<&Path>,
    source_text: Option<&str>,
) -> Option<HoverInfo> {
    // Resolve the byte offset and extract the identifier token under the
    // cursor. If source is available but no identifier is found (e.g. the
    // cursor is on whitespace or a keyword), bail early.
    let query_offset =
        source_text.and_then(|source| offset_from_position(source, query_span.start));
    let query_ident = source_text
        .and_then(|source| query_offset.and_then(|offset| identifier_at_offset(source, offset)));
    if source_text.is_some() && query_ident.is_none() {
        return None;
    }

    if state.typed.is_some() {
        try_call_site_hover(state, query_span, query_ident.as_deref())
            .or_else(|| {
                try_node_hover(state, query_span, current_file_path, query_ident.as_deref())
            })
            .or_else(|| try_typestate_role_hover(state, query_span, query_ident.as_deref()))
            .or_else(|| {
                try_def_table_hover(state, query_span, current_file_path, query_ident.as_deref())
            })
            .or_else(|| {
                try_syntactic_field_hover(
                    query_span,
                    source_text,
                    query_offset,
                    query_ident.as_deref(),
                )
            })
    } else {
        try_resolved_hover(state, query_span, current_file_path, query_ident.as_deref())
    }
}

/// Try hover via typed call-site resolution (callee function name at a call
/// expression).
fn try_call_site_hover(
    state: &LookupState,
    query_span: Span,
    query_ident: Option<&str>,
) -> Option<HoverInfo> {
    let typed = state.typed.as_ref()?;
    let query_ident = query_ident?;
    let call = call_site_at_span(&typed.module, query_span)?;
    let sig = typed.call_sigs.get(&call.node_id)?;
    let def_id = sig.def_id?;
    let def = typed.def_table.lookup_def(def_id)?;
    if !(def.name == query_ident
        || def
            .name
            .strip_prefix("__ts_ctor_")
            .is_some_and(|owner| owner == query_ident)
        || def
            .name
            .strip_prefix("__ts_spawn_")
            .is_some_and(|owner| owner == query_ident))
    {
        return None;
    }
    let ty = typed.type_map.lookup_def_type(def);
    let display = format_hover_label(
        Some(&def.name),
        ty.as_ref(),
        Some(def_id),
        Some(&typed.type_map),
        &typed.def_table,
    );
    Some(HoverInfo {
        node_id: call.callee_node_id,
        span: query_span,
        def_id: Some(def_id),
        def_name: Some(def.name.clone()),
        ty,
        display,
    })
}

/// Try hover via typed AST node candidate scoring (best-matching node under
/// the cursor with definition and/or type information).
fn try_node_hover(
    state: &LookupState,
    query_span: Span,
    current_file_path: Option<&Path>,
    query_ident: Option<&str>,
) -> Option<HoverInfo> {
    let typed = state.typed.as_ref()?;
    let demangler = TypestateNameDemangler::from_def_table(&typed.def_table);
    let mut candidates = Vec::new();
    for (node_id, span) in node_span_map(&typed.module) {
        if span_contains_span(span, query_span) {
            candidates.push((node_id, span));
        }
    }
    candidates.sort_by_key(|(_, span)| {
        (
            span.end.offset.saturating_sub(span.start.offset),
            std::cmp::Reverse(span.start.offset),
        )
    });

    let mut best: Option<(u8, usize, usize, HoverInfo)> = None;
    for (node_id, node_span) in candidates {
        if state.poisoned_nodes.contains(&node_id) {
            continue;
        }
        let def_id = typed
            .def_table
            .lookup_node_def_id(node_id)
            .filter(|id| *id != UNKNOWN_DEF_ID);
        if let Some(def_id) = def_id
            && should_skip_runtime_hover_def(&typed.def_table, def_id, current_file_path)
        {
            continue;
        }
        let def_name = def_id.and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()));
        if let (Some(def_name), Some(query_ident)) = (def_name.as_deref(), query_ident)
            && !def_name_matches_query(def_name, query_ident, &demangler)
        {
            continue;
        }
        let ty = typed
            .type_map
            .lookup_node_type(node_id)
            .filter(|ty| !matches!(ty, Type::Unknown));
        if def_id.is_none() && ty.is_none() {
            continue;
        }

        if let (Some(query_ident), Some(ty)) = (query_ident, ty.as_ref())
            && let Some(field_ty) = field_type_in_struct(ty, query_ident)
        {
            let field_ty_display =
                format_type_for_hover(field_ty, None, Some(&typed.type_map), &demangler);
            let info = HoverInfo {
                node_id,
                span: query_span,
                def_id: None,
                def_name: Some(query_ident.to_string()),
                ty: Some(field_ty.clone()),
                display: format!("{query_ident}: {field_ty_display}"),
            };
            update_best(&mut best, 6, &node_span, info);
            continue;
        }

        let display = format_hover_label(
            def_name.as_deref(),
            ty.as_ref(),
            def_id,
            Some(&typed.type_map),
            &typed.def_table,
        );
        let score =
            hover_candidate_score(def_name.as_deref(), ty.is_some(), query_ident, &demangler);
        let info = HoverInfo {
            node_id,
            span: query_span,
            def_id,
            def_name,
            ty,
            display,
        };
        update_best(&mut best, score, &node_span, info);
    }
    best.map(|(_, _, _, info)| info)
}

/// Try hover for typestate role implementation bindings.
fn try_typestate_role_hover(
    state: &LookupState,
    query_span: Span,
    query_ident: Option<&str>,
) -> Option<HoverInfo> {
    let typed = state.typed.as_ref()?;
    let demangler = TypestateNameDemangler::from_def_table(&typed.def_table);
    let def_id =
        typestate_role_def_at_span(&typed.typestate_role_impls, &typed.def_table, query_span)?;
    let def_name = typed
        .def_table
        .lookup_def(def_id)
        .map(|def| def.name.clone());
    if let (Some(def_name), Some(query_ident)) = (def_name.as_deref(), query_ident)
        && !def_name_matches_query(def_name, query_ident, &demangler)
    {
        return None;
    }
    let display = format_hover_label(
        def_name.as_deref(),
        None,
        Some(def_id),
        Some(&typed.type_map),
        &typed.def_table,
    );
    Some(HoverInfo {
        node_id: typed
            .def_table
            .lookup_def_node_id(def_id)
            .unwrap_or(crate::core::tree::NodeId(0)),
        span: query_span,
        def_id: Some(def_id),
        def_name,
        ty: None,
        display,
    })
}

/// Try hover by scanning the def table for a matching definition name.
fn try_def_table_hover(
    state: &LookupState,
    query_span: Span,
    current_file_path: Option<&Path>,
    query_ident: Option<&str>,
) -> Option<HoverInfo> {
    let typed = state.typed.as_ref()?;
    fallback_hover_from_def_table(
        &typed.def_table,
        Some(&typed.type_map),
        query_span,
        current_file_path,
        query_ident,
    )
}

/// Try hover from source-text syntactic field patterns (e.g. `name: Type`).
fn try_syntactic_field_hover(
    query_span: Span,
    source_text: Option<&str>,
    query_offset: Option<usize>,
    query_ident: Option<&str>,
) -> Option<HoverInfo> {
    let source_text = source_text?;
    let query_offset = query_offset?;
    let query_ident = query_ident?;
    let display = syntactic_field_hover_display(source_text, query_offset, query_ident)?;
    Some(HoverInfo {
        node_id: crate::core::tree::NodeId(0),
        span: query_span,
        def_id: None,
        def_name: Some(query_ident.to_string()),
        ty: None,
        display,
    })
}

/// Fallback hover for when only resolved (not typed) state is available.
fn try_resolved_hover(
    state: &LookupState,
    query_span: Span,
    current_file_path: Option<&Path>,
    query_ident: Option<&str>,
) -> Option<HoverInfo> {
    let resolved = state.resolved.as_ref()?;
    let node_id = node_at_span(&resolved.module, query_span)?;
    if state.poisoned_nodes.contains(&node_id) {
        return None;
    }
    let def_id = resolved
        .def_table
        .lookup_node_def_id(node_id)
        .filter(|id| *id != UNKNOWN_DEF_ID)
        .or_else(|| {
            typestate_role_def_at_span(
                &resolved.typestate_role_impls,
                &resolved.def_table,
                query_span,
            )
        })?;
    if should_skip_runtime_hover_def(&resolved.def_table, def_id, current_file_path) {
        return None;
    }
    let demangler = TypestateNameDemangler::from_def_table(&resolved.def_table);
    let def_name = resolved
        .def_table
        .lookup_def(def_id)
        .map(|def| def.name.clone());
    if let (Some(def_name), Some(query_ident)) = (def_name.as_deref(), query_ident)
        && !def_name_matches_query(def_name, query_ident, &demangler)
    {
        return None;
    }
    let display = format_hover_label(
        def_name.as_deref(),
        None,
        Some(def_id),
        None,
        &resolved.def_table,
    );
    Some(HoverInfo {
        node_id,
        span: query_span,
        def_id: Some(def_id),
        def_name,
        ty: None,
        display,
    })
    .or_else(|| {
        fallback_hover_from_def_table(
            &resolved.def_table,
            None,
            query_span,
            current_file_path,
            query_ident,
        )
    })
}

// --- Scoring & candidate helpers ---

/// Replace `best` if the new candidate has a higher score, or on a tie, a
/// narrower span (smaller width), or on a further tie, a later start offset
/// (more specific node).
fn update_best(
    best: &mut Option<(u8, usize, usize, HoverInfo)>,
    score: u8,
    node_span: &Span,
    info: HoverInfo,
) {
    let width = node_span.end.offset.saturating_sub(node_span.start.offset);
    let start = node_span.start.offset;
    let replace = best
        .as_ref()
        .is_none_or(|(best_score, best_width, best_start, _)| {
            score > *best_score
                || (score == *best_score
                    && (width < *best_width || (width == *best_width && start > *best_start)))
        });
    if replace {
        *best = Some((score, width, start, info));
    }
}

/// Score a hover candidate by how informative and relevant it is.
/// Higher score wins. Base score from (def_name, type) presence:
///   0 = self or no info, 1 = type only, 2 = name only, 3 = name + type.
/// A +2 bonus is added when the def name matches the identifier under the
/// cursor (accounting for typestate name demangling).
fn hover_candidate_score(
    def_name: Option<&str>,
    has_type: bool,
    query_ident: Option<&str>,
    demangler: &TypestateNameDemangler,
) -> u8 {
    let mut score: u8 = match (def_name, has_type) {
        (Some("self"), true) => 0,
        (Some("self"), false) => 0,
        (Some(_), true) => 3,
        (Some(_), false) => 2,
        (None, true) => 1,
        (None, false) => 0,
    };
    if let (Some(def_name), Some(query_ident)) = (def_name, query_ident)
        && def_name_matches_query(def_name, query_ident, demangler)
    {
        score = score.saturating_add(2);
    }
    score
}

// --- Definition filtering helpers ---

/// Skip runtime-injected definitions (e.g. `__rt_print`) when hovering in a
/// user source file — they would otherwise shadow user-defined names.
fn should_skip_runtime_hover_def(
    def_table: &DefTable,
    def_id: DefId,
    current_file_path: Option<&Path>,
) -> bool {
    let Some(current_file_path) = current_file_path else {
        return false;
    };
    let Some(def) = def_table.lookup_def(def_id) else {
        return false;
    };
    if !def.is_runtime() {
        return false;
    }
    let Some(def_loc) = def_table.lookup_def_location(def_id) else {
        return false;
    };
    let Some(def_path) = def_loc.path else {
        return false;
    };
    !paths_equivalent(&def_path, current_file_path)
}

fn paths_equivalent(lhs: &Path, rhs: &Path) -> bool {
    if lhs == rhs {
        return true;
    }
    match (lhs.canonicalize(), rhs.canonicalize()) {
        (Ok(a), Ok(b)) => a == b,
        _ => false,
    }
}

/// Last-resort hover: scan the entire def table for a definition whose name
/// matches `query_ident` and whose source location contains the cursor.
fn fallback_hover_from_def_table(
    def_table: &DefTable,
    type_map: Option<&crate::core::typecheck::type_map::TypeMap>,
    query_span: Span,
    current_file_path: Option<&Path>,
    query_ident: Option<&str>,
) -> Option<HoverInfo> {
    let query_ident = query_ident?;
    let current_file_path = current_file_path?;
    for def in def_table.defs() {
        if def.name != query_ident || def.is_runtime() {
            continue;
        }
        let Some(location) = def_table.lookup_def_location(def.id) else {
            continue;
        };
        let Some(path) = location.path.as_deref() else {
            continue;
        };
        if !paths_equivalent(path, current_file_path)
            || !span_contains_span(location.span, query_span)
        {
            continue;
        }
        let ty = type_map.and_then(|map| map.lookup_def_type(def));
        let display = format_hover_label(
            Some(&def.name),
            ty.as_ref(),
            Some(def.id),
            type_map,
            def_table,
        );
        return Some(HoverInfo {
            node_id: def_table
                .lookup_def_node_id(def.id)
                .unwrap_or(crate::core::tree::NodeId(0)),
            span: query_span,
            def_id: Some(def.id),
            def_name: Some(def.name.clone()),
            ty,
            display,
        });
    }
    None
}

/// Check if a definition name matches the user's cursor identifier, handling
/// mangled typestate names (e.g. `__ts_ctor_Conn` matches `Conn`).
fn def_name_matches_query(
    def_name: &str,
    query_ident: &str,
    demangler: &TypestateNameDemangler,
) -> bool {
    if def_name == query_ident {
        return true;
    }
    let demangled = demangler
        .demangle_symbol(def_name)
        .unwrap_or_else(|| def_name.to_string());
    if demangled == query_ident {
        return true;
    }
    demangled
        .rsplit_once("::")
        .is_some_and(|(_, tail)| tail == query_ident)
}

// --- Source text helpers ---

/// Extract the identifier token surrounding the given byte offset, or `None`
/// if the offset is on whitespace, punctuation, or a language keyword.
fn identifier_at_offset(source: &str, offset: usize) -> Option<String> {
    if source.is_empty() {
        return None;
    }
    let mut idx = offset.min(source.len());
    while idx > 0 && !source.is_char_boundary(idx) {
        idx -= 1;
    }
    let bytes = source.as_bytes();
    let is_ident = |b: u8| b == b'_' || b.is_ascii_alphanumeric();

    if idx > 0
        && !is_ident(bytes[idx.saturating_sub(1)])
        && (idx >= bytes.len() || !is_ident(bytes[idx]))
    {
        return None;
    }

    let mut start = idx;
    while start > 0 && is_ident(bytes[start - 1]) {
        start -= 1;
    }
    let mut end = idx;
    while end < bytes.len() && is_ident(bytes[end]) {
        end += 1;
    }
    if start >= end {
        return None;
    }
    let ident = source[start..end].to_string();
    if is_keyword_ident(&ident) {
        return None;
    }
    Some(ident)
}

/// Convert a 1-based line/column position to a byte offset. Uses the
/// pre-computed offset when available; otherwise falls back to a line/column
/// scan (LSP query spans carry line/column but set offset=0).
fn offset_from_position(source: &str, position: Position) -> Option<usize> {
    if position.offset > 0 && position.offset <= source.len() {
        return Some(position.offset);
    }
    let target_line = position.line.max(1);
    let target_col = position.column.max(1);
    let mut line = 1usize;
    let mut col = 1usize;
    for (idx, ch) in source.char_indices() {
        if line == target_line && col == target_col {
            return Some(idx);
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    if line == target_line && col == target_col {
        return Some(source.len());
    }
    None
}

fn is_keyword_ident(ident: &str) -> bool {
    matches!(
        ident,
        "fn" | "let"
            | "var"
            | "if"
            | "else"
            | "while"
            | "for"
            | "in"
            | "match"
            | "return"
            | "break"
            | "continue"
            | "type"
            | "trait"
            | "requires"
            | "state"
            | "fields"
            | "typestate"
            | "true"
            | "false"
    )
}

/// Try to extract a `name: Type` hover from the raw source line when AST-level
/// type info isn't available (e.g. inside a typestate `fields` block before
/// type checking completes).
fn syntactic_field_hover_display(source: &str, offset: usize, ident: &str) -> Option<String> {
    let bytes = source.as_bytes();
    if offset >= bytes.len() {
        return None;
    }
    let mut line_start = offset;
    while line_start > 0 && bytes[line_start - 1] != b'\n' {
        line_start -= 1;
    }
    let mut line_end = offset;
    while line_end < bytes.len() && bytes[line_end] != b'\n' {
        line_end += 1;
    }
    let line = &source[line_start..line_end];
    let ident_pos = line.find(ident)?;
    let mut i = ident_pos + ident.len();
    while i < line.len() && line.as_bytes()[i].is_ascii_whitespace() {
        i += 1;
    }
    if i >= line.len() || line.as_bytes()[i] != b':' {
        return None;
    }
    i += 1;
    while i < line.len() && line.as_bytes()[i].is_ascii_whitespace() {
        i += 1;
    }
    let ty_start = i;
    while i < line.len() {
        let b = line.as_bytes()[i];
        if b == b',' || b == b'}' || b == b';' || b.is_ascii_whitespace() {
            break;
        }
        i += 1;
    }
    if ty_start >= i {
        return None;
    }
    let ty = line[ty_start..i].trim();
    if ty.is_empty() {
        return None;
    }
    Some(format!("{ident}: {ty}"))
}

fn field_type_in_struct<'a>(ty: &'a Type, field_name: &str) -> Option<&'a Type> {
    match ty {
        Type::Struct { fields, .. } => fields
            .iter()
            .find(|field| field.name == field_name)
            .map(|field| &field.ty),
        _ => None,
    }
}

// --- Display formatting ---

fn format_hover_label(
    def_name: Option<&str>,
    ty: Option<&Type>,
    def_id: Option<DefId>,
    type_map: Option<&TypeMap>,
    def_table: &DefTable,
) -> String {
    let demangler = TypestateNameDemangler::from_def_table(def_table);
    let render_name = |name: &str| demangler.demangle_text(name);
    let render_type = |ty: &Type| format_type_for_hover(ty, def_id, type_map, &demangler);
    match (def_name, ty) {
        (Some(name), Some(ty)) => {
            let name = render_name(name);
            let ty = render_type(ty);
            if ty == name
                || ty.starts_with(&format!("{name} "))
                || ty.ends_with(&format!("::{name}"))
            {
                ty
            } else {
                format!("{name}: {ty}")
            }
        }
        (Some(name), None) => render_name(name),
        (None, Some(ty)) => render_type(ty),
        (None, None) => String::new(),
    }
}

fn format_type_for_hover(
    ty: &Type,
    def_id: Option<DefId>,
    type_map: Option<&TypeMap>,
    demangler: &TypestateNameDemangler,
) -> String {
    let type_var_names =
        def_id.and_then(|id| type_map.and_then(|map| map.lookup_def_type_param_names(id)));
    let nominal_name_map = |name: &str| {
        if let Some(state_name) = demangler.demangle_state_qualified(name) {
            state_name
        } else {
            demangler.demangle_text(name)
        }
    };
    render_type(
        ty,
        &TypeRenderConfig {
            show_in_mode: false,
            type_var_names,
            nominal_name_map: Some(&nominal_name_map),
        },
    )
}
