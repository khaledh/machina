//! Completion helper methods for `AnalysisDb`.

use crate::core::diag::Span;
use crate::core::{api, resolve};
use crate::services::analysis::completion::{
    collect as collect_completions, synthesize_member_completion_source,
};
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::query::QueryResult;
use crate::services::analysis::results::CompletionItem;
use crate::services::analysis::snapshot::FileId;

impl super::AnalysisDb {
    pub(super) fn completions_for_state(
        &mut self,
        file_id: FileId,
        query_span: Span,
        source: String,
        mut state: LookupState,
    ) -> QueryResult<Vec<CompletionItem>> {
        let cursor = Span {
            start: query_span.end,
            end: query_span.end,
        };

        let mut active_source = source;
        if state.resolved.is_none()
            && let Some(synthetic_source) =
                synthesize_member_completion_source(&active_source, cursor.start)
            && let Some(synthetic_state) =
                self.lookup_state_for_source(file_id, synthetic_source.clone())?
        {
            active_source = synthetic_source;
            state = synthetic_state;
        }

        let LookupState {
            resolved,
            typed,
            poisoned_nodes: _,
        } = state;
        let Some(resolved) = resolved else {
            return Ok(Vec::new());
        };
        // Keep member completions useful when unrelated resolve errors suppress
        // typed lookup-state output by running a local best-effort typecheck.
        let fallback_typed = if typed.is_none() {
            Some(
                api::typecheck_stage_partial(resolved.clone(), resolve::ImportedFacts::default())
                    .context,
            )
        } else {
            None
        };
        let typed_for_completion = typed.as_ref().or(fallback_typed.as_ref());
        let mut out = collect_completions(&active_source, cursor, &resolved, typed_for_completion);
        out.sort_by(|a, b| a.label.cmp(&b.label).then(a.def_id.0.cmp(&b.def_id.0)));
        out.dedup_by(|a, b| a.label == b.label && a.kind == b.kind && a.def_id == b.def_id);
        Ok(out)
    }
}
