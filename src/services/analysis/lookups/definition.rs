//! Definition resolution lookups (go-to-definition, def-at-span).

use crate::core::ast::{Module, NodeId, TypeDefKind};
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefTable, UNKNOWN_DEF_ID};
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::Location;
use crate::services::analysis::snapshot::{AnalysisSnapshot, FileId};
use crate::services::analysis::syntax_index::{
    call_site_at_span, node_span_map, span_contains_span,
};

use super::identifier_token_at_span;
use std::collections::HashSet;

#[derive(Clone, Debug)]
pub(crate) struct LinearDeclTarget {
    pub node_id: NodeId,
    pub span: Span,
    pub display: String,
}

/// Resolve the definition under `query_span`.
pub(crate) fn def_at_span(
    state: &LookupState,
    query_span: Span,
    source: Option<&str>,
) -> Option<DefId> {
    let token = identifier_token_at_span(source, query_span);
    let query_span = token.as_ref().map(|token| token.span).unwrap_or(query_span);
    let resolved = state.resolved.as_ref()?;
    if let Some(def_id) =
        selected_callable_def_at_span(state, query_span, token.as_ref().map(|t| t.ident.as_str()))
    {
        return Some(def_id);
    }
    if let Some(def_id) = machine_handle_def_at_span(
        &resolved.def_table,
        query_span,
        source,
        token.as_ref().map(|t| t.ident.as_str()),
    ) {
        return Some(def_id);
    }
    best_def_use_at_span(
        &resolved.module,
        &resolved.def_table,
        &state.poisoned_nodes,
        query_span,
    )
    .map(|(_, def_id)| def_id)
}

/// Go-to-definition: resolve the definition under `query_span` and return
/// its source location (file path + span).
pub(crate) fn def_location_at_span(
    snapshot: &AnalysisSnapshot,
    file_id: FileId,
    state: &LookupState,
    query_span: Span,
    source: Option<&str>,
) -> Option<Location> {
    let token = identifier_token_at_span(source, query_span);
    let query_span = token.as_ref().map(|token| token.span).unwrap_or(query_span);
    let resolved = state.resolved.as_ref()?;
    if let Some(target) = linear_decl_target_at_span(
        &resolved.module,
        query_span,
        source,
        token.as_ref().map(|t| t.ident.as_str()),
    ) {
        return Some(Location {
            file_id,
            path: snapshot.path(file_id).map(std::path::Path::to_path_buf),
            span: target.span,
        });
    }
    let def_id = if let Some(def_id) =
        selected_callable_def_at_span(state, query_span, token.as_ref().map(|t| t.ident.as_str()))
    {
        def_id
    } else if let Some(def_id) = machine_handle_def_at_span(
        &resolved.def_table,
        query_span,
        source,
        token.as_ref().map(|t| t.ident.as_str()),
    ) {
        def_id
    } else if let Some((_, def_id)) = best_def_use_at_span(
        &resolved.module,
        &resolved.def_table,
        &state.poisoned_nodes,
        query_span,
    ) {
        def_id
    } else {
        return None;
    };
    let def_loc = resolved.def_table.lookup_def_location(def_id)?;

    Some(Location {
        file_id,
        path: def_loc
            .path
            .or_else(|| snapshot.path(file_id).map(std::path::Path::to_path_buf)),
        span: def_loc.span,
    })
}

fn selected_callable_def_at_span(
    state: &LookupState,
    query_span: Span,
    query_ident: Option<&str>,
) -> Option<DefId> {
    let typed = state.typed.as_ref()?;
    let query_ident = query_ident?;
    let call = call_site_at_span(&typed.module, query_span)?;
    if let Some(method_name) = call.method_name.as_deref() {
        if method_name != query_ident {
            return None;
        }
    } else {
        let callee_span = node_span_map(&typed.module)
            .get(&call.callee_node_id)
            .copied()?;
        if !span_contains_span(callee_span, query_span) {
            return None;
        }
    }
    typed.call_sigs.get(&call.node_id)?.def_id
}

/// Find the narrowest AST node that contains `query_span` and has a known
/// definition. When multiple nodes share the same minimal width (e.g. a name
/// node and its parent decl), we break ties by preferring the latest start
/// offset — which corresponds to the most specific (innermost) node.
fn best_def_use_at_span(
    module: &Module,
    def_table: &DefTable,
    poisoned_nodes: &HashSet<NodeId>,
    query_span: Span,
) -> Option<(NodeId, DefId)> {
    let spans = node_span_map(module);
    let mut candidates = Vec::new();
    for (node_id, span) in spans {
        if !span_contains_span(span, query_span) {
            continue;
        }
        candidates.push((node_id, span));
    }

    // Narrow to the smallest width, then latest start offset among those.
    let min_width = candidates
        .iter()
        .map(|(_, span)| span.end.offset.saturating_sub(span.start.offset))
        .min()?;
    let max_start = candidates
        .iter()
        .filter(|(_, span)| span.end.offset.saturating_sub(span.start.offset) == min_width)
        .map(|(_, span)| span.start.offset)
        .max()?;

    for (node_id, span) in candidates {
        let width = span.end.offset.saturating_sub(span.start.offset);
        if width != min_width || span.start.offset != max_start {
            continue;
        }
        if poisoned_nodes.contains(&node_id) {
            continue;
        }
        let Some(def_id) = def_table.lookup_node_def_id(node_id) else {
            continue;
        };
        if def_id != UNKNOWN_DEF_ID {
            return Some((node_id, def_id));
        }
    }
    None
}

pub(crate) fn linear_decl_target_at_span(
    module: &Module,
    query_span: Span,
    source: Option<&str>,
    query_ident: Option<&str>,
) -> Option<LinearDeclTarget> {
    if let Some(target) = linear_decl_target_from_source(source, query_span, query_ident) {
        return Some(target);
    }

    let ident = query_ident?;
    for type_def in module.type_defs() {
        let TypeDefKind::Linear { linear } = &type_def.kind else {
            continue;
        };

        for state in &linear.states {
            let name_span = token_span_within(state.span, ident, source).unwrap_or(state.span);
            if state.name == ident && span_contains_span(name_span, query_span) {
                return Some(LinearDeclTarget {
                    node_id: state.id,
                    span: name_span,
                    display: format!("state {}::{}", type_def.name, state.name),
                });
            }
        }

        for action in &linear.actions {
            let name_span = token_span_within(action.span, ident, source).unwrap_or(action.span);
            if action.name == ident && span_contains_span(name_span, query_span) {
                return Some(LinearDeclTarget {
                    node_id: action.id,
                    span: name_span,
                    display: format_linear_transition_display(&type_def.name, "action", action),
                });
            }
        }

        for role in &linear.roles {
            if !role.allowed_actions.iter().any(|action| action == ident)
                || !span_contains_span(role.span, query_span)
            {
                continue;
            }
            if let Some(action) = linear.actions.iter().find(|action| action.name == ident) {
                return Some(LinearDeclTarget {
                    node_id: action.id,
                    span: token_span_within(action.span, ident, source).unwrap_or(action.span),
                    display: format_linear_transition_display(&type_def.name, "action", action),
                });
            }
        }

        for trigger in &linear.triggers {
            let name_span = token_span_within(trigger.span, ident, source).unwrap_or(trigger.span);
            if trigger.name == ident && span_contains_span(name_span, query_span) {
                return Some(LinearDeclTarget {
                    node_id: trigger.id,
                    span: name_span,
                    display: format_linear_transition_display(&type_def.name, "trigger", trigger),
                });
            }
        }
    }
    None
}

pub(super) fn machine_handle_def_at_span(
    def_table: &DefTable,
    query_span: Span,
    source: Option<&str>,
    query_ident: Option<&str>,
) -> Option<DefId> {
    let ident = query_ident?;
    if !is_machine_handle_target(source?, query_span) {
        return None;
    }
    def_table
        .defs()
        .iter()
        .find(|def| {
            matches!(def.kind, crate::core::resolve::DefKind::MachineDef) && def.name == ident
        })
        .map(|def| def.id)
}

fn is_machine_handle_target(source: &str, query_span: Span) -> bool {
    let start = query_span.start.offset;
    let end = query_span.end.offset;
    let bytes = source.as_bytes();

    let mut left = start;
    while left > 0 && bytes[left - 1].is_ascii_whitespace() {
        left -= 1;
    }
    if left == 0 || bytes[left - 1] != b'<' {
        return false;
    }
    left -= 1;
    while left > 0 && bytes[left - 1].is_ascii_whitespace() {
        left -= 1;
    }

    let ident_end = left;
    let mut ident_start = ident_end;
    while ident_start > 0
        && (bytes[ident_start - 1].is_ascii_alphanumeric() || bytes[ident_start - 1] == b'_')
    {
        ident_start -= 1;
    }
    if &source[ident_start..ident_end] != "Machine" {
        return false;
    }

    let mut right = end;
    while right < bytes.len() && bytes[right].is_ascii_whitespace() {
        right += 1;
    }
    right < bytes.len() && bytes[right] == b'>'
}

fn format_linear_transition_display(
    type_name: &str,
    kind: &str,
    decl: &crate::core::ast::LinearTransitionDecl,
) -> String {
    let params = if decl.params.is_empty() {
        String::new()
    } else {
        let joined = decl
            .params
            .iter()
            .map(|param| format!("{}: {}", param.name, param.ty))
            .collect::<Vec<_>>()
            .join(", ");
        format!("({joined})")
    };
    let error_suffix = decl
        .error_ty_expr
        .as_ref()
        .map(|ty| format!(" | {ty}"))
        .unwrap_or_default();
    format!(
        "{kind} {type_name}::{}{params}: {} -> {}{error_suffix}",
        decl.name, decl.source_state, decl.target_state
    )
}

fn token_span_within(container: Span, ident: &str, source: Option<&str>) -> Option<Span> {
    let source = source?;
    let start = container.start.offset.min(source.len());
    let end = container.end.offset.min(source.len());
    let snippet = source.get(start..end)?;
    let rel = snippet.find(ident)?;
    let abs_start = start + rel;
    let abs_end = abs_start + ident.len();
    Some(Span {
        start: super::position_at_offset(source, abs_start),
        end: super::position_at_offset(source, abs_end),
    })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LinearSourceSection {
    States,
    Actions,
    Triggers,
    Roles,
}

#[derive(Clone, Debug)]
struct LinearSourceRef {
    span: Span,
    name: String,
}

fn linear_decl_target_from_source(
    source: Option<&str>,
    query_span: Span,
    query_ident: Option<&str>,
) -> Option<LinearDeclTarget> {
    let source = source?;
    let query_ident = query_ident?;

    let mut targets = Vec::<LinearDeclTarget>::new();
    let mut action_targets = Vec::<(String, LinearDeclTarget)>::new();
    let mut role_refs = Vec::<LinearSourceRef>::new();

    let mut pending_linear = false;
    let mut current_type: Option<String> = None;
    let mut current_section: Option<LinearSourceSection> = None;
    let mut type_brace_depth = 0usize;
    let mut line_start_offset = 0usize;

    for (line_index, line) in source.lines().enumerate() {
        let line_no = line_index + 1;
        let trimmed = line.trim();
        let open_braces = line.chars().filter(|&ch| ch == '{').count();
        let close_braces = line.chars().filter(|&ch| ch == '}').count();

        if trimmed == "@linear" {
            pending_linear = true;
            line_start_offset += line.len() + 1;
            continue;
        }

        if current_type.is_none()
            && pending_linear
            && trimmed.starts_with("type ")
            && let Some(rest) = trimmed.strip_prefix("type ")
            && let Some((type_name, _)) = rest.split_once('=')
        {
            current_type = Some(type_name.trim().to_string());
            type_brace_depth = open_braces.saturating_sub(close_braces);
            current_section = None;
            pending_linear = false;
            line_start_offset += line.len() + 1;
            continue;
        }

        if let Some(type_name) = current_type.clone() {
            match trimmed {
                "states {" => current_section = Some(LinearSourceSection::States),
                "actions {" => current_section = Some(LinearSourceSection::Actions),
                "triggers {" => current_section = Some(LinearSourceSection::Triggers),
                "roles {" => current_section = Some(LinearSourceSection::Roles),
                "}" if current_section.is_some() => current_section = None,
                _ => {
                    if let Some(section) = current_section {
                        match section {
                            LinearSourceSection::States => {
                                if let Some(name) = trimmed
                                    .trim_end_matches(',')
                                    .split_whitespace()
                                    .next()
                                    .filter(|name| !name.is_empty() && *name != "}")
                                    && let Some(span) =
                                        token_span_on_line(line, line_start_offset, name, line_no)
                                {
                                    targets.push(LinearDeclTarget {
                                        node_id: NodeId(0),
                                        span,
                                        display: format!("state {}::{}", type_name, name),
                                    });
                                }
                            }
                            LinearSourceSection::Actions | LinearSourceSection::Triggers => {
                                if let Some((name, source_state, target_state, error_ty)) =
                                    parse_linear_transition_line(trimmed)
                                    && let Some(span) =
                                        token_span_on_line(line, line_start_offset, &name, line_no)
                                {
                                    let kind = if section == LinearSourceSection::Actions {
                                        "action"
                                    } else {
                                        "trigger"
                                    };
                                    let display = if let Some(error_ty) = error_ty {
                                        format!(
                                            "{kind} {type_name}::{name}: {source_state} -> {target_state} | {error_ty}"
                                        )
                                    } else {
                                        format!(
                                            "{kind} {type_name}::{name}: {source_state} -> {target_state}"
                                        )
                                    };
                                    let target = LinearDeclTarget {
                                        node_id: NodeId(0),
                                        span,
                                        display,
                                    };
                                    targets.push(target.clone());
                                    if section == LinearSourceSection::Actions {
                                        action_targets.push((name, target));
                                    }
                                }
                            }
                            LinearSourceSection::Roles => {
                                if let Some((_, actions)) = parse_linear_role_line(trimmed) {
                                    for action in actions {
                                        if let Some(span) = token_span_on_line(
                                            line,
                                            line_start_offset,
                                            &action,
                                            line_no,
                                        ) {
                                            role_refs.push(LinearSourceRef { span, name: action });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            type_brace_depth += open_braces;
            type_brace_depth = type_brace_depth.saturating_sub(close_braces);
            if type_brace_depth == 0 {
                current_type = None;
                current_section = None;
            }
        } else {
            pending_linear = false;
        }

        line_start_offset += line.len() + 1;
    }

    for target in &targets {
        if span_contains_span(target.span, query_span)
            && target
                .display
                .rsplit("::")
                .next()
                .is_some_and(|tail| tail.starts_with(query_ident))
        {
            return Some(target.clone());
        }
    }

    for role_ref in &role_refs {
        if !span_contains_span(role_ref.span, query_span) || role_ref.name != query_ident {
            continue;
        }
        if let Some((_, target)) = action_targets
            .iter()
            .find(|(name, _)| *name == role_ref.name)
        {
            return Some(target.clone());
        }
    }

    None
}

fn parse_linear_transition_line(line: &str) -> Option<(String, String, String, Option<String>)> {
    let (left, right) = line.split_once(':')?;
    let name = left.trim().to_string();
    let (path, error_ty) = if let Some((path, error_ty)) = right.split_once('|') {
        (
            path.trim(),
            Some(error_ty.trim().trim_end_matches(',').to_string()),
        )
    } else {
        (right.trim(), None)
    };
    let (source_state, target_state) = path.split_once("->")?;
    Some((
        name,
        source_state.trim().to_string(),
        target_state.trim().trim_end_matches(',').to_string(),
        error_ty,
    ))
}

fn parse_linear_role_line(line: &str) -> Option<(String, Vec<String>)> {
    let (role_name, rest) = line.split_once('{')?;
    let actions = rest
        .trim_end_matches('}')
        .split(',')
        .map(str::trim)
        .filter(|item| !item.is_empty())
        .map(str::to_string)
        .collect::<Vec<_>>();
    Some((role_name.trim().to_string(), actions))
}

fn token_span_on_line(
    line: &str,
    line_start_offset: usize,
    ident: &str,
    line_no: usize,
) -> Option<Span> {
    let rel = line.find(ident)?;
    let start = line_start_offset + rel;
    let end = start + ident.len();
    Some(Span {
        start: crate::core::diag::Position {
            offset: start,
            line: line_no,
            column: rel + 1,
        },
        end: crate::core::diag::Position {
            offset: end,
            line: line_no,
            column: rel + ident.len() + 1,
        },
    })
}
