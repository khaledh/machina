//! Program-aware query helper methods for `AnalysisDb`.

use crate::core::diag::Span;
use crate::core::resolve::GlobalDefId;
use crate::core::symbol_id::SelectedCallable;
use crate::core::types::Type;
use crate::services::analysis::lookups::{
    def_at_span, hover_at_span_in_file, hover_for_def_in_state,
    signature_help_for_def_at_call_site, type_at_span,
};
use crate::services::analysis::program_pipeline::resolve_imported_symbol_target_from_import_env;
use crate::services::analysis::query::QueryResult;
use crate::services::analysis::results::{CompletionItem, HoverInfo, Location, SignatureHelp};
use crate::services::analysis::snapshot::FileId;
use crate::services::analysis::syntax_index::{
    call_site_at_span, node_span_map, span_contains_span,
};

impl super::AnalysisDb {
    pub fn def_location_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<Location>> {
        let snapshot = self.snapshot();
        let program_lookup = self.program_pipeline_for_file(file_id)?;
        let Some(entry_module_id) = program_lookup.entry_module_id else {
            return Ok(None);
        };
        let module_states = &program_lookup.module_states;
        let Some(entry_state) = module_states.get(&entry_module_id) else {
            return Ok(None);
        };
        let source = snapshot.text(file_id);
        if let Some(target) = selected_imported_callable_target(&entry_state, query_span, true)
            && let Some(target_state) = module_states.get(&target.module_id)
            && let Some(target_resolved) = target_state.resolved.as_ref()
            && let Some(loc) = target_resolved.def_table.lookup_def_location(target.def_id)
        {
            let target_file_id = loc
                .path
                .as_deref()
                .and_then(|path| snapshot.file_id(path))
                .unwrap_or(file_id);
            return Ok(Some(Location {
                file_id: target_file_id,
                path: loc.path,
                span: loc.span,
            }));
        }
        let Some(def_id) = def_at_span(entry_state, query_span, source.as_deref()) else {
            return Ok(None);
        };
        let Some(entry_resolved) = entry_state.resolved.as_ref() else {
            return Ok(None);
        };

        let target = entry_resolved
            .def_table
            .lookup_def(def_id)
            .and_then(|def| {
                resolve_imported_symbol_target_from_import_env(
                    entry_module_id,
                    def,
                    &program_lookup.import_env_by_module,
                )
            })
            .unwrap_or_else(|| GlobalDefId::new(entry_module_id, def_id));
        let Some(target_state) = module_states.get(&target.module_id) else {
            return Ok(None);
        };
        let Some(target_resolved) = target_state.resolved.as_ref() else {
            return Ok(None);
        };
        let Some(loc) = target_resolved.def_table.lookup_def_location(target.def_id) else {
            return Ok(None);
        };
        let target_file_id = loc
            .path
            .as_deref()
            .and_then(|path| snapshot.file_id(path))
            .unwrap_or(file_id);
        Ok(Some(Location {
            file_id: target_file_id,
            path: loc.path,
            span: loc.span,
        }))
    }

    pub fn type_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<Type>> {
        let state = if let Some(state) = self.entry_lookup_state_for_program_file(file_id)? {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        Ok(type_at_span(&state, query_span))
    }

    pub fn hover_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        let program = self.program_pipeline_for_file(file_id)?;
        let state = if let Some(state) = program
            .entry_module_id
            .and_then(|entry| program.module_states.get(&entry).cloned())
        {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id);
        let hover = hover_at_span_in_file(
            &state,
            query_span,
            snapshot.path(file_id),
            source.as_deref(),
        );
        if let (Some(entry_module_id), Some(_), Some(hover)) = (
            program.entry_module_id,
            state.resolved.as_ref(),
            hover.as_ref(),
        ) && let Some(target) = imported_hover_target(
            &state,
            entry_module_id,
            hover,
            query_span,
            &program.import_env_by_module,
            &program.module_states,
        ) {
            return Ok(Some(target));
        }
        if hover.as_ref().is_some_and(hover_needs_strict_fallback)
            && let Some(strict_state) = self.strict_lookup_state_for_program_file(file_id)?
        {
            return Ok(hover_at_span_in_file(
                &strict_state,
                query_span,
                snapshot.path(file_id),
                source.as_deref(),
            ));
        }
        Ok(hover)
    }

    pub fn completions_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id).map(|s| s.to_string()) else {
            return Ok(Vec::new());
        };
        let program = self.program_pipeline_for_file(file_id)?;
        let state = if let Some(state) = program
            .entry_module_id
            .and_then(|entry| program.module_states.get(&entry).cloned())
        {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        self.completions_for_state(file_id, query_span, source, state)
    }

    pub fn signature_help_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id).map(|s| s.to_string());
        let program = self.program_pipeline_for_file(file_id)?;
        let state = if let Some(state) = self.entry_lookup_state_for_program_file(file_id)? {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        if let Some(target) = selected_imported_callable_target(&state, query_span, false)
            && let Some(target_state) = program.module_states.get(&target.module_id)
            && let Some(sig) = signature_help_for_def_at_call_site(
                &state,
                query_span,
                source.as_deref(),
                target_state,
                target.def_id,
            )
        {
            return Ok(Some(sig));
        }
        if let Some(sig) = self.signature_help_for_state(&state, query_span, source.as_deref()) {
            return Ok(Some(sig));
        }
        if let Some(sig) = self.signature_help_with_synthetic_fallback(file_id, query_span)? {
            return Ok(Some(sig));
        }
        // Best-effort fallback: when program-aware state is unavailable due
        // transient graph/import failures while editing, keep signature help
        // useful by trying the file-local pipeline.
        self.signature_help_at_file(file_id, query_span)
    }
}

fn hover_needs_strict_fallback(info: &HoverInfo) -> bool {
    info.ty.as_ref().is_some_and(Type::contains_unresolved)
}

fn imported_hover_target(
    state: &crate::services::analysis::pipeline::LookupState,
    module_id: crate::core::capsule::ModuleId,
    hover: &HoverInfo,
    query_span: Span,
    import_env_by_module: &std::collections::HashMap<
        crate::core::capsule::ModuleId,
        crate::core::context::ImportEnv,
    >,
    module_states: &std::collections::HashMap<
        crate::core::capsule::ModuleId,
        crate::services::analysis::pipeline::LookupState,
    >,
) -> Option<HoverInfo> {
    if let Some(target) = selected_imported_callable_target(state, query_span, true) {
        let target_state = module_states.get(&target.module_id)?;
        return hover_for_def_in_state(target_state, target.def_id);
    }

    let entry_resolved = state.resolved.as_ref()?;
    let local_def_id = hover.def_id?;
    let local_def = entry_resolved.def_table.lookup_def(local_def_id)?;
    let target =
        resolve_imported_symbol_target_from_import_env(module_id, local_def, import_env_by_module)?;
    let target_state = module_states.get(&target.module_id)?;
    hover_for_def_in_state(target_state, target.def_id)
}

fn selected_imported_callable_target(
    state: &crate::services::analysis::pipeline::LookupState,
    query_span: Span,
    require_callee_span: bool,
) -> Option<GlobalDefId> {
    let typed = state.typed.as_ref()?;
    let call = call_site_at_span(&typed.module, query_span)?;
    if require_callee_span {
        let callee_span = node_span_map(&typed.module)
            .get(&call.callee_node_id)
            .copied()?;
        if !span_contains_span(callee_span, query_span) {
            return None;
        }
    }
    let selected_sig = typed.call_sigs.get(&call.node_id)?;
    match selected_sig.selected.as_ref()? {
        SelectedCallable::Global(target) => Some(*target),
        _ => None,
    }
}
