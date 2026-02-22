//! Signature help lookup.

use crate::core::diag::Span;
use crate::core::resolve::{DefTable, UNKNOWN_DEF_ID};
use crate::core::types::Type;
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::SignatureHelp;
use crate::services::analysis::signature_help::offset_for_position;
use crate::services::analysis::syntax_index::{
    active_param_index, call_site_at_span, position_leq,
};

use super::TypestateNameDemangler;
use super::callable_signature::format_source_callable_signature;

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
        // last typed argument token (before `,`/`)`), some spans may exclude
        // that boundary point. Nudge left by one column and retry.
        let nudged = nudge_span_left(query_span)?;
        call_site_at_span(&typed.module, nudged)
    })?;
    let active_parameter_for = |param_count: usize| {
        active_param_index_with_comma_context(
            &call.arg_spans,
            query_span.start,
            source,
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
            active_parameter: active_parameter_for(params.len()),
            parameters: params,
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
            active_parameter: active_parameter_for(parameters.len()),
            parameters,
        });
    }
    provisional.or(Some(SignatureHelp {
        label,
        def_id,
        active_parameter: active_parameter_for(parameters.len()),
        parameters,
    }))
}

fn source_signature_help<F>(
    typed: &crate::core::context::TypeCheckedContext,
    render_def_id: Option<crate::core::resolve::DefId>,
    active_parameter_for: F,
) -> Option<SignatureHelp>
where
    F: Fn(usize) -> usize,
{
    let demangler = TypestateNameDemangler::from_def_table(&typed.def_table);
    let rendered = format_source_callable_signature(
        render_def_id,
        Some(&typed.module),
        Some(&typed.type_map),
        &typed.def_table,
        &demangler,
    )?;
    Some(SignatureHelp {
        label: rendered.label,
        def_id: render_def_id,
        active_parameter: active_parameter_for(rendered.parameters.len()),
        parameters: rendered.parameters,
    })
}

fn active_param_index_with_comma_context(
    arg_spans: &[Span],
    pos: crate::core::diag::Position,
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

fn nudge_span_left(span: Span) -> Option<Span> {
    if span.start.column <= 1 || span.end.column <= 1 || span.start.line != span.end.line {
        return None;
    }
    Some(Span {
        start: crate::core::diag::Position {
            offset: span.start.offset.saturating_sub(1),
            line: span.start.line,
            column: span.start.column - 1,
        },
        end: crate::core::diag::Position {
            offset: span.end.offset.saturating_sub(1),
            line: span.end.line,
            column: span.end.column - 1,
        },
    })
}

fn format_signature_label(def_table: &DefTable, name: &str, params: &[String]) -> String {
    let demangler = TypestateNameDemangler::from_def_table(def_table);
    let label = format!("{name}({})", params.join(", "));
    demangler.demangle_text(&label)
}

fn param_mode_name(mode: &crate::core::tree::ParamMode) -> &'static str {
    match mode {
        crate::core::tree::ParamMode::In => "in",
        crate::core::tree::ParamMode::InOut => "inout",
        crate::core::tree::ParamMode::Out => "out",
        crate::core::tree::ParamMode::Sink => "sink",
    }
}

fn fn_param_mode_name(mode: &crate::core::types::FnParamMode) -> &'static str {
    match mode {
        crate::core::types::FnParamMode::In => "in",
        crate::core::types::FnParamMode::InOut => "inout",
        crate::core::types::FnParamMode::Out => "out",
        crate::core::types::FnParamMode::Sink => "sink",
    }
}
