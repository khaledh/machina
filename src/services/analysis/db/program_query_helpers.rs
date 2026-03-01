//! Program-aware query helper methods for `AnalysisDb`.

use crate::core::diag::Span;
use crate::core::resolve::GlobalDefId;
use crate::core::symbol_id::SelectedCallable;
use crate::core::types::Type;
use crate::services::analysis::db::pipeline_helpers::def_target_for_symbol_id_in_states;
use crate::services::analysis::lookups::{
    def_at_span, hover_at_span_in_file, hover_for_resolved_target, location_for_resolved_target,
    signature_help_for_resolved_target_at_call_site, type_at_span,
};
use crate::services::analysis::program_pipeline::resolve_imported_symbol_id_from_import_env;
use crate::services::analysis::query::QueryResult;
use crate::services::analysis::results::{
    CompletionItem, DefTarget, HoverInfo, Location, SignatureHelp,
};
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
        let resolved_target = resolved_target_at_program_span(
            self,
            file_id,
            &snapshot,
            query_span,
            source.as_deref(),
            entry_module_id,
            entry_state,
            &program_lookup.import_env_by_module,
            module_states,
            true,
        )?;
        let Some(resolved_target) = resolved_target else {
            return Ok(None);
        };
        Ok(location_for_resolved_target(&resolved_target))
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
        if let (Some(entry_module_id), Some(hover)) = (program.entry_module_id, hover.as_ref())
            && let Some(target) = imported_hover_target(
                self,
                file_id,
                &state,
                entry_module_id,
                hover,
                query_span,
                &program.import_env_by_module,
                &program.module_states,
            )
        {
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
        if let Some(target) = selected_callable_target(
            &snapshot,
            file_id,
            &state,
            query_span,
            false,
            &program.module_states,
        ) && let Some(resolved_target) = self.resolve_target_in_program(file_id, target)?
            && let Some(sig) = signature_help_for_resolved_target_at_call_site(
                &state,
                query_span,
                source.as_deref(),
                &resolved_target,
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
    db: &mut super::AnalysisDb,
    origin_file_id: FileId,
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
    let snapshot = db.snapshot();
    if let Some(target) = selected_callable_target(
        &snapshot,
        origin_file_id,
        state,
        query_span,
        true,
        module_states,
    ) {
        let resolved_target = db
            .resolve_target_in_program(origin_file_id, target)
            .ok()??;
        return hover_for_resolved_target(&resolved_target);
    }

    if let Some(symbol_id) = hover.symbol_id.as_ref()
        && let Some(resolved_target) = db
            .resolve_symbol_target_in_program(origin_file_id, symbol_id)
            .ok()?
    {
        return hover_for_resolved_target(&resolved_target);
    }

    let local_def_id = hover.def_id?;
    let target = imported_or_local_def_target(
        &snapshot,
        origin_file_id,
        module_id,
        state,
        local_def_id,
        import_env_by_module,
        module_states,
    )?;
    if target.module_id == Some(module_id) && target.def_id == local_def_id {
        return None;
    }
    let resolved_target = db
        .resolve_target_in_program(origin_file_id, target)
        .ok()??;
    hover_for_resolved_target(&resolved_target)
}

fn resolved_target_at_program_span(
    db: &mut super::AnalysisDb,
    origin_file_id: FileId,
    snapshot: &crate::services::analysis::snapshot::AnalysisSnapshot,
    query_span: Span,
    source: Option<&str>,
    module_id: crate::core::capsule::ModuleId,
    state: &crate::services::analysis::pipeline::LookupState,
    import_env_by_module: &std::collections::HashMap<
        crate::core::capsule::ModuleId,
        crate::core::context::ImportEnv,
    >,
    module_states: &std::collections::HashMap<
        crate::core::capsule::ModuleId,
        crate::services::analysis::pipeline::LookupState,
    >,
    require_callee_span: bool,
) -> QueryResult<Option<crate::services::analysis::lookups::ResolvedSymbolTarget>> {
    let target = if let Some(target) = selected_callable_target(
        snapshot,
        origin_file_id,
        state,
        query_span,
        require_callee_span,
        module_states,
    ) {
        target
    } else {
        let Some(def_id) = def_at_span(state, query_span, source) else {
            return Ok(None);
        };
        let Some(target) = imported_or_local_def_target(
            snapshot,
            origin_file_id,
            module_id,
            state,
            def_id,
            import_env_by_module,
            module_states,
        ) else {
            return Ok(None);
        };
        target
    };

    db.resolve_target_in_program(origin_file_id, target)
}

fn selected_callable_target(
    snapshot: &crate::services::analysis::snapshot::AnalysisSnapshot,
    origin_file_id: FileId,
    state: &crate::services::analysis::pipeline::LookupState,
    query_span: Span,
    require_callee_span: bool,
    module_states: &std::collections::HashMap<
        crate::core::capsule::ModuleId,
        crate::services::analysis::pipeline::LookupState,
    >,
) -> Option<DefTarget> {
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
        SelectedCallable::Global(target) => Some(global_def_target(
            snapshot,
            origin_file_id,
            module_states,
            *target,
        )?),
        SelectedCallable::Canonical(symbol_id) => {
            def_target_for_symbol_id_in_states(snapshot, module_states, origin_file_id, symbol_id)
        }
        _ => None,
    }
}

fn global_def_target(
    snapshot: &crate::services::analysis::snapshot::AnalysisSnapshot,
    origin_file_id: FileId,
    module_states: &std::collections::HashMap<
        crate::core::capsule::ModuleId,
        crate::services::analysis::pipeline::LookupState,
    >,
    target: GlobalDefId,
) -> Option<DefTarget> {
    let state = module_states.get(&target.module_id)?;
    let resolved = state.resolved.as_ref()?;
    let file_id = resolved
        .def_table
        .source_path()
        .and_then(|path| snapshot.file_id(path))
        .unwrap_or(origin_file_id);
    Some(DefTarget {
        file_id,
        module_id: Some(target.module_id),
        def_id: target.def_id,
        symbol_id: resolved.symbol_ids.lookup_symbol_id(target.def_id).cloned(),
        program_scoped: true,
    })
}

fn imported_or_local_def_target(
    snapshot: &crate::services::analysis::snapshot::AnalysisSnapshot,
    origin_file_id: FileId,
    module_id: crate::core::capsule::ModuleId,
    state: &crate::services::analysis::pipeline::LookupState,
    def_id: crate::core::resolve::DefId,
    import_env_by_module: &std::collections::HashMap<
        crate::core::capsule::ModuleId,
        crate::core::context::ImportEnv,
    >,
    module_states: &std::collections::HashMap<
        crate::core::capsule::ModuleId,
        crate::services::analysis::pipeline::LookupState,
    >,
) -> Option<DefTarget> {
    let resolved = state.resolved.as_ref()?;
    if let Some(symbol_id) = resolved.symbol_ids.lookup_symbol_id(def_id)
        && let Some(target) =
            def_target_for_symbol_id_in_states(snapshot, module_states, origin_file_id, symbol_id)
    {
        return Some(target);
    }

    let local_def = resolved.def_table.lookup_def(def_id)?;
    if let Some(symbol_id) =
        resolve_imported_symbol_id_from_import_env(module_id, local_def, import_env_by_module)
        && let Some(target) =
            def_target_for_symbol_id_in_states(snapshot, module_states, origin_file_id, &symbol_id)
    {
        return Some(target);
    }

    let target = GlobalDefId::new(module_id, def_id);
    global_def_target(snapshot, origin_file_id, module_states, target)
}
