//! Signature-help helper methods for `AnalysisDb`.
//!
//! Kept in a focused module so `db.rs` stays centered on query entrypoints.

use crate::core::diag::Span;
use crate::core::{api, resolve};
use crate::services::analysis::lookups::signature_help_at_span;
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::query::QueryResult;
use crate::services::analysis::results::SignatureHelp;
use crate::services::analysis::signature_help::synthesize_call_signature_sources;
use crate::services::analysis::snapshot::FileId;

impl super::AnalysisDb {
    pub(super) fn signature_help_with_synthetic_fallback(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id).map(|s| s.to_string()) else {
            return Ok(None);
        };

        let mut fallback_sig = None;
        for synthetic_source in synthesize_call_signature_sources(&source, query_span.start) {
            let Some(synthetic_state) =
                self.lookup_state_for_source(file_id, synthetic_source.clone())?
            else {
                continue;
            };
            if let Some(sig) =
                self.signature_help_for_state(&synthetic_state, query_span, Some(&synthetic_source))
            {
                if sig.def_id.is_some() {
                    return Ok(Some(sig));
                }
                if fallback_sig.is_none() {
                    fallback_sig = Some(sig);
                }
            }
        }
        Ok(fallback_sig)
    }

    pub(super) fn signature_help_for_state(
        &self,
        state: &LookupState,
        query_span: Span,
        source: Option<&str>,
    ) -> Option<SignatureHelp> {
        if let Some(sig) = signature_help_at_span(state, query_span, source) {
            return Some(sig);
        }
        // Signature help should stay available while editing, even when
        // unrelated resolve diagnostics suppress the main typed stage.
        let resolved = state.resolved.as_ref()?.clone();
        let fallback_typed =
            api::typecheck_stage_partial(resolved, resolve::ImportedFacts::default()).context;
        let fallback_state = LookupState {
            resolved: state.resolved.clone(),
            typed: Some(fallback_typed),
            poisoned_nodes: state.poisoned_nodes.clone(),
        };
        signature_help_at_span(&fallback_state, query_span, source)
    }
}
