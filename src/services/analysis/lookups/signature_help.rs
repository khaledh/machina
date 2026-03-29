//! Signature help lookup.

use crate::core::ast::ParamMode;
use crate::core::diag::{Position, Span};
use crate::core::resolve::{DefId, DefTable, UNKNOWN_DEF_ID};
use crate::core::types::{FnParamMode, Type};
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::SignatureHelp;
use crate::services::analysis::signature_help::offset_for_position;
use crate::services::analysis::syntax_index::{
    active_param_index, call_site_at_span, position_leq,
};

use super::callable_signature::{format_source_callable_signature, source_doc_for_def};
use crate::core::context::TypeCheckedContext;

/// Build signature help at a call site. First tries call-site resolution
/// (populated by the type checker for fully resolved calls), then falls back
/// to deriving the signature from the callee expression's `Fn` type for
/// incomplete or mismatched calls.
pub(crate) fn signature_help_at_span(
    state: &LookupState,
    query_span: Span,
    source: Option<&str>,
) -> Option<SignatureHelp> {
    let typed = state.typed.as_ref()?;
    let call = call_site_at_span(&typed.module, query_span).or_else(|| {
        // Cursor-at-boundary editing case: when the caret sits just after the
        // last typed argument token (before `,` / `)`), some spans may exclude
        // that boundary point. Nudge left by one column and retry.
        let nudged = nudge_span_left(query_span)?;
        call_site_at_span(&typed.module, nudged)
    })?;
    let active_parameter_for = |param_count: usize| {
        let source_arg_index = active_param_index_with_comma_context(
            &call.arg_spans,
            query_span.start,
            source,
            param_count,
        );
        remap_active_parameter_index(
            typed
                .call_sigs
                .get(&call.node_id)
                .map(|sig| sig.arg_order.as_slice()),
            source_arg_index,
            param_count,
        )
    };
    let callee_def_id = typed
        .def_table
        .lookup_node_def_id(call.callee_node_id)
        .filter(|id| *id != UNKNOWN_DEF_ID);
    let mut provisional = None;
    if let Some(sig) = typed.call_sigs.get(&call.node_id) {
        let mut params = Vec::with_capacity(sig.params.len());
        for param in &sig.params {
            params.push(format!("{} {}", param_mode_name(&param.mode), param.ty));
        }
        let name = sig
            .def_id
            .and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()))
            .unwrap_or_else(|| "<call>".to_string());
        let label = format_signature_label(&typed.def_table, &name, &params);
        let help = SignatureHelp {
            label,
            def_id: sig.def_id,
            symbol_id: sig
                .def_id
                .and_then(|id| typed.symbol_ids.lookup_symbol_id(id).cloned()),
            active_parameter: active_parameter_for(params.len()),
            parameters: params,
            doc: source_doc_for_def(sig.def_id, Some(&typed.module), &typed.def_table),
        };
        if help.def_id.is_some() {
            return Some(help);
        }
        provisional = Some(help);
    }

    // If we can resolve the callee to a named callable definition, prefer
    // source-level signature rendering even when call-site typing failed (for
    // example, generic arity/type mismatch while the user is still typing).
    if let Some(def_id) = callee_def_id
        && let Some(help) = source_signature_help(typed, Some(def_id), active_parameter_for)
    {
        return Some(help);
    }

    // Fallback for incomplete/mismatched calls where call-site resolution did
    // not produce `call_sigs`: derive the callable signature from the callee
    // expression type.
    let callee_ty = typed.type_map.lookup_node_type(call.callee_node_id)?;
    let Type::Fn { params, .. } = callee_ty else {
        return None;
    };
    let parameters: Vec<String> = params
        .iter()
        .map(|param| format!("{} {}", fn_param_mode_name(&param.mode), param.ty))
        .collect();
    let def_id = callee_def_id;
    let name = def_id
        .and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()))
        .unwrap_or_else(|| "<call>".to_string());
    let label = format_signature_label(&typed.def_table, &name, &parameters);
    if def_id.is_some() {
        return Some(SignatureHelp {
            label,
            def_id,
            symbol_id: def_id.and_then(|id| typed.symbol_ids.lookup_symbol_id(id).cloned()),
            active_parameter: active_parameter_for(parameters.len()),
            parameters,
            doc: source_doc_for_def(def_id, Some(&typed.module), &typed.def_table),
        });
    }
    provisional.or(Some(SignatureHelp {
        label,
        def_id,
        symbol_id: def_id.and_then(|id| typed.symbol_ids.lookup_symbol_id(id).cloned()),
        active_parameter: active_parameter_for(parameters.len()),
        parameters,
        doc: source_doc_for_def(def_id, Some(&typed.module), &typed.def_table),
    }))
}

/// Render source-level signature help for a known callable definition while
/// using the caller-side call site to determine the active parameter index.
pub(crate) fn signature_help_for_def_at_call_site(
    caller_state: &LookupState,
    query_span: Span,
    source: Option<&str>,
    callee_state: &LookupState,
    callee_def_id: DefId,
) -> Option<SignatureHelp> {
    let caller_typed = caller_state.typed.as_ref()?;
    let call = call_site_at_span(&caller_typed.module, query_span).or_else(|| {
        let nudged = nudge_span_left(query_span)?;
        call_site_at_span(&caller_typed.module, nudged)
    })?;
    let active_parameter_for = |param_count: usize| {
        let source_arg_index = active_param_index_with_comma_context(
            &call.arg_spans,
            query_span.start,
            source,
            param_count,
        );
        remap_active_parameter_index(
            caller_typed
                .call_sigs
                .get(&call.node_id)
                .map(|sig| sig.arg_order.as_slice()),
            source_arg_index,
            param_count,
        )
    };
    source_signature_help(
        callee_state.typed.as_ref()?,
        Some(callee_def_id),
        active_parameter_for,
    )
}

pub(crate) fn active_parameter_index_at_call_site(
    caller_state: &LookupState,
    query_span: Span,
    source: Option<&str>,
    param_count: usize,
) -> Option<usize> {
    let caller_typed = caller_state.typed.as_ref()?;
    let call = call_site_at_span(&caller_typed.module, query_span).or_else(|| {
        let nudged = nudge_span_left(query_span)?;
        call_site_at_span(&caller_typed.module, nudged)
    })?;
    let source_arg_index = active_param_index_with_comma_context(
        &call.arg_spans,
        query_span.start,
        source,
        param_count,
    );
    Some(remap_active_parameter_index(
        caller_typed
            .call_sigs
            .get(&call.node_id)
            .map(|sig| sig.arg_order.as_slice()),
        source_arg_index,
        param_count,
    ))
}

fn source_signature_help<F>(
    typed: &TypeCheckedContext,
    render_def_id: Option<DefId>,
    active_parameter_for: F,
) -> Option<SignatureHelp>
where
    F: Fn(usize) -> usize,
{
    let rendered = format_source_callable_signature(
        render_def_id,
        Some(&typed.module),
        Some(&typed.type_map),
        &typed.def_table,
    )?;
    Some(SignatureHelp {
        label: rendered.label,
        def_id: render_def_id,
        symbol_id: render_def_id.and_then(|id| typed.symbol_ids.lookup_symbol_id(id).cloned()),
        active_parameter: active_parameter_for(rendered.parameters.len()),
        parameters: rendered.parameters,
        doc: source_doc_for_def(render_def_id, Some(&typed.module), &typed.def_table),
    })
}

fn active_param_index_with_comma_context(
    arg_spans: &[Span],
    pos: Position,
    source: Option<&str>,
    param_count: usize,
) -> usize {
    if param_count == 0 {
        return 0;
    }
    let mut active = active_param_index(arg_spans, pos).min(param_count.saturating_sub(1));
    if arg_spans.is_empty() {
        return active;
    }

    let Some(source) = source else {
        return active;
    };
    let Some(cursor_offset) = offset_for_position(source, pos) else {
        return active;
    };
    let Some(last_arg) = arg_spans.last() else {
        return active;
    };
    if !position_leq(last_arg.end, pos) {
        return active;
    }
    let start = last_arg.end.offset.min(source.len());
    let end = cursor_offset.min(source.len());
    if start >= end {
        return active;
    }

    if source[start..end].bytes().any(|b| b == b',') {
        active = active.max(arg_spans.len().min(param_count.saturating_sub(1)));
    }
    active
}

fn remap_active_parameter_index(
    arg_order: Option<&[usize]>,
    source_arg_index: usize,
    param_count: usize,
) -> usize {
    if param_count == 0 {
        return 0;
    }

    let source_arg_index = source_arg_index.min(param_count.saturating_sub(1));
    let Some(arg_order) = arg_order else {
        return source_arg_index;
    };
    if arg_order.is_empty() {
        return source_arg_index;
    }
    if let Some(param_index) = arg_order.get(source_arg_index) {
        return (*param_index).min(param_count.saturating_sub(1));
    }
    source_arg_index
}

fn nudge_span_left(span: Span) -> Option<Span> {
    if span.start.column <= 1 || span.end.column <= 1 || span.start.line != span.end.line {
        return None;
    }
    Some(Span {
        start: Position {
            offset: span.start.offset.saturating_sub(1),
            line: span.start.line,
            column: span.start.column - 1,
        },
        end: Position {
            offset: span.end.offset.saturating_sub(1),
            line: span.end.line,
            column: span.end.column - 1,
        },
    })
}

fn format_signature_label(_def_table: &DefTable, name: &str, params: &[String]) -> String {
    format!("{name}({})", params.join(", "))
}

fn param_mode_name(mode: &ParamMode) -> &'static str {
    match mode {
        ParamMode::In => "in",
        ParamMode::InOut => "inout",
        ParamMode::Out => "out",
        ParamMode::Sink => "sink",
    }
}

fn fn_param_mode_name(mode: &FnParamMode) -> &'static str {
    match mode {
        FnParamMode::In => "in",
        FnParamMode::InOut => "inout",
        FnParamMode::Out => "out",
        FnParamMode::Sink => "sink",
    }
}
