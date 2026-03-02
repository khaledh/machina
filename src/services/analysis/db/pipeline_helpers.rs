//! Pipeline and lookup helper methods for `AnalysisDb`.

use std::collections::HashMap;

use crate::core::capsule::ModuleId;
use crate::core::context::ResolvedContext;
use crate::core::resolve::DefId;
use crate::core::symbol_id::SymbolId;
use crate::services::analysis::frontend_support::{
    stable_source_revision, strict_frontend_lookup_state_with_path,
};
use crate::services::analysis::lookups::{ResolvedSymbolTarget, resolved_target_def_id};
use crate::services::analysis::pipeline::{
    LookupState, run_module_pipeline_with_query_input, to_lookup_state,
};
use crate::services::analysis::program_pipeline::{
    ProgramPipelineResult, run_program_pipeline_for_file_with_options,
};
use crate::services::analysis::query::{QueryKey, QueryKind, QueryResult};
use crate::services::analysis::results::DefTarget;
use crate::services::analysis::snapshot::AnalysisSnapshot;
use crate::services::analysis::snapshot::FileId;
use std::sync::Arc;

impl super::AnalysisDb {
    pub(super) fn lookup_state_for_target(
        &mut self,
        origin_file_id: FileId,
        target: &DefTarget,
    ) -> QueryResult<Option<LookupState>> {
        if target.program_scoped {
            let snapshot = self.snapshot();
            let program = self.program_pipeline_for_file(origin_file_id)?;
            return Ok(lookup_program_state_for_target(
                &program.module_states,
                target,
                &snapshot,
            ));
        }

        Ok(Some(self.lookup_state_for_file(target.file_id)?))
    }

    pub(super) fn best_lookup_state_for_navigation(
        &mut self,
        origin_file_id: FileId,
        file_id: FileId,
    ) -> QueryResult<LookupState> {
        if let Some(state) = self.lookup_state_in_origin_program(origin_file_id, file_id)? {
            return Ok(state);
        }
        self.lookup_state_for_file(file_id)
    }

    pub(super) fn lookup_state_in_origin_program(
        &mut self,
        origin_file_id: FileId,
        file_id: FileId,
    ) -> QueryResult<Option<LookupState>> {
        let target_path = self.snapshot().path(file_id).map(|p| p.to_path_buf());
        let Some(target_path) = target_path else {
            return Ok(None);
        };

        let program = self.program_pipeline_for_file(origin_file_id)?;
        for state in program.module_states.values() {
            let Some(resolved) = state.resolved.as_ref() else {
                continue;
            };
            if resolved.def_table.source_path() == Some(target_path.as_path()) {
                return Ok(Some(state.clone()));
            }
        }
        Ok(None)
    }

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
            Arc::<str>::from(source),
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

    pub(super) fn strict_lookup_state_for_program_file(
        &mut self,
        file_id: FileId,
    ) -> QueryResult<Option<LookupState>> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id) else {
            return Ok(None);
        };
        let Some(path) = snapshot.path(file_id).map(|p| p.to_path_buf()) else {
            return Ok(None);
        };

        let revision = snapshot.revision();
        let module_id = ModuleId(file_id.0);
        let query_input = if self.experimental_typestate { 1 } else { 0 };
        let key = QueryKey::with_input(
            QueryKind::StrictLookupState,
            module_id,
            revision,
            query_input,
        );
        let experimental_typestate = self.experimental_typestate;

        self.execute_query(key, move |_rt| {
            Ok(
                strict_frontend_lookup_state_with_path(
                    &source,
                    &path,
                    true,
                    experimental_typestate,
                )
                .ok(),
            )
        })
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

    pub(crate) fn def_target_for_symbol_id_in_program(
        &mut self,
        origin_file_id: FileId,
        symbol_id: &SymbolId,
    ) -> QueryResult<Option<DefTarget>> {
        let snapshot = self.snapshot();
        let program = self.program_pipeline_for_file(origin_file_id)?;
        Ok(def_target_for_symbol_id_in_states(
            &snapshot,
            &program.module_states,
            origin_file_id,
            symbol_id,
        ))
    }

    pub(crate) fn resolve_symbol_target_in_program(
        &mut self,
        origin_file_id: FileId,
        symbol_id: &SymbolId,
    ) -> QueryResult<Option<ResolvedSymbolTarget>> {
        let Some(target) = self.def_target_for_symbol_id_in_program(origin_file_id, symbol_id)?
        else {
            return Ok(None);
        };
        self.resolve_target_in_program(origin_file_id, target)
    }

    pub(crate) fn resolve_target_in_program(
        &mut self,
        origin_file_id: FileId,
        target: DefTarget,
    ) -> QueryResult<Option<ResolvedSymbolTarget>> {
        let Some(state) = self.lookup_state_for_target(origin_file_id, &target)? else {
            return Ok(None);
        };
        Ok(Some(ResolvedSymbolTarget {
            local_def_id: resolved_target_def_id(&state, &target),
            target,
            state,
        }))
    }
}

pub(super) fn def_target_for_symbol_id_in_states(
    snapshot: &AnalysisSnapshot,
    module_states: &HashMap<ModuleId, LookupState>,
    origin_file_id: FileId,
    symbol_id: &SymbolId,
) -> Option<DefTarget> {
    for (module_id, state) in module_states {
        let resolved = state.resolved.as_ref()?;
        if resolved.module_path.as_ref() != Some(&symbol_id.module) {
            continue;
        }
        let local_def_id = unique_local_def_id_for_symbol(resolved, symbol_id)?;
        let target_file_id = resolved
            .def_table
            .source_path()
            .and_then(|path| snapshot.file_id(path))
            .unwrap_or(origin_file_id);
        return Some(DefTarget {
            file_id: target_file_id,
            module_id: Some(*module_id),
            def_id: local_def_id,
            symbol_id: Some(symbol_id.clone()),
            program_scoped: true,
        });
    }
    None
}

pub(super) fn lookup_program_state_for_target(
    module_states: &HashMap<ModuleId, LookupState>,
    target: &DefTarget,
    snapshot: &AnalysisSnapshot,
) -> Option<LookupState> {
    if let Some(module_id) = target.module_id
        && let Some(state) = module_states.get(&module_id)
    {
        return Some(state.clone());
    }

    module_states
        .values()
        .find(|candidate| {
            candidate
                .resolved
                .as_ref()
                .and_then(|resolved| resolved.def_table.source_path())
                .and_then(|path| snapshot.file_id(path))
                == Some(target.file_id)
        })
        .cloned()
}

fn unique_local_def_id_for_symbol(
    resolved: &ResolvedContext,
    symbol_id: &SymbolId,
) -> Option<DefId> {
    let defs = resolved.symbol_ids.lookup_local_def_ids(symbol_id)?;
    (defs.len() == 1).then_some(defs[0])
}
