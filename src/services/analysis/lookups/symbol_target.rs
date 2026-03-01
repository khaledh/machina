//! Canonical symbol-target lookup helpers.
//!
//! These helpers let editor-facing features start from a `SymbolId`, resolve
//! the owning local definition inside a specific lookup state, and then reuse
//! the existing source-definition rendering logic.

use crate::core::diag::Span;
use crate::core::resolve::DefId;
use crate::core::symbol_id::SymbolId;
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::{HoverInfo, SignatureHelp};

pub(crate) fn def_id_for_symbol_id_in_state(
    state: &LookupState,
    symbol_id: &SymbolId,
) -> Option<DefId> {
    state
        .typed
        .as_ref()?
        .symbol_ids
        .lookup_local_def_ids(symbol_id)
        .and_then(|defs| (defs.len() == 1).then_some(defs[0]))
}

pub(crate) fn hover_for_symbol_id_in_state(
    state: &LookupState,
    symbol_id: &SymbolId,
) -> Option<HoverInfo> {
    let def_id = def_id_for_symbol_id_in_state(state, symbol_id)?;
    super::hover::hover_for_def_in_state(state, def_id)
}

pub(crate) fn signature_help_for_symbol_id_at_call_site(
    caller_state: &LookupState,
    query_span: Span,
    source: Option<&str>,
    callee_state: &LookupState,
    symbol_id: &SymbolId,
) -> Option<SignatureHelp> {
    let def_id = def_id_for_symbol_id_in_state(callee_state, symbol_id)?;
    super::signature_help::signature_help_for_def_at_call_site(
        caller_state,
        query_span,
        source,
        callee_state,
        def_id,
    )
}
