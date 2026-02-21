//! Pipeline and lookup helper methods for `AnalysisDb`.

use crate::core::capsule::ModuleId;
use crate::services::analysis::frontend_support::stable_source_revision;
use crate::services::analysis::pipeline::{
    LookupState, run_module_pipeline_with_query_input, to_lookup_state,
};
use crate::services::analysis::program_pipeline::{
    ProgramPipelineResult, run_program_pipeline_for_file_with_options,
};
use crate::services::analysis::query::{QueryKey, QueryKind, QueryResult};
use crate::services::analysis::snapshot::FileId;

impl super::AnalysisDb {
    pub(super) fn lookup_state_for_file(&mut self, file_id: FileId) -> QueryResult<LookupState> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id) else {
            return Ok(LookupState::default());
        };
        let revision = snapshot.revision();
        let module_id = ModuleId(file_id.0);
        let experimental_typestate = self.experimental_typestate;
        let query_input = if experimental_typestate { 1 } else { 0 };

        let lookup_key =
            QueryKey::with_input(QueryKind::LookupState, module_id, revision, query_input);
        self.execute_query(lookup_key, move |rt| {
            let state = run_module_pipeline_with_query_input(
                rt,
                module_id,
                revision,
                source,
                query_input,
                experimental_typestate,
            )?;
            Ok(to_lookup_state(&state))
        })
    }

    pub(super) fn lookup_state_for_source(
        &mut self,
        file_id: FileId,
        source: String,
    ) -> QueryResult<Option<LookupState>> {
        let snapshot = self.snapshot();
        if source.is_empty() {
            return Ok(None);
        }

        let revision = snapshot.revision();
        let query_input = stable_source_revision(&source).wrapping_add(1);
        let query_input = query_input.wrapping_add(if self.experimental_typestate { 1 } else { 0 });
        let module_id = ModuleId(file_id.0);
        let pipeline = run_module_pipeline_with_query_input(
            &mut self.runtime,
            module_id,
            revision,
            std::sync::Arc::<str>::from(source),
            query_input,
            self.experimental_typestate,
        )?;
        Ok(Some(to_lookup_state(&pipeline)))
    }

    pub(super) fn entry_lookup_state_for_program_file(
        &mut self,
        file_id: FileId,
    ) -> QueryResult<Option<LookupState>> {
        let program = self.program_pipeline_for_file(file_id)?;
        Ok(program
            .entry_module_id
            .and_then(|entry| program.module_states.get(&entry).cloned()))
    }

    pub(super) fn program_pipeline_for_file(
        &mut self,
        file_id: FileId,
    ) -> QueryResult<ProgramPipelineResult> {
        let snapshot = self.snapshot();
        run_program_pipeline_for_file_with_options(
            &mut self.runtime,
            snapshot,
            file_id,
            self.experimental_typestate,
        )
    }
}
