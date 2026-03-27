//! Program-aware query helper methods for `AnalysisDb`.

use crate::core::context::ImportEnv;
use crate::core::diag::Span;
use crate::core::symbol_id::{SelectedCallable, SymbolNs};
use crate::core::types::Type;
use crate::services::analysis::db::pipeline_helpers::def_target_for_symbol_id_in_states;
use crate::services::analysis::lookups::{
    ResolvedSymbolTarget, active_parameter_index_at_call_site, binding_value_node_id_for_def,
    def_at_span, hover_at_span_in_file, hover_for_imported_stdlib_symbol,
    hover_for_resolved_target, identifier_token_at_span, linear_decl_target_at_span,
    location_for_imported_stdlib_symbol, location_for_resolved_target,
    resolved_binding_type_for_def, signature_help_for_imported_stdlib_symbol,
    signature_help_for_resolved_target_at_call_site, type_at_span,
};
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::program_pipeline::resolve_imported_symbol_id_from_import_env;
use crate::services::analysis::query::QueryResult;
use crate::services::analysis::results::{
    CompletionItem, DefTarget, HoverInfo, Location, SignatureHelp,
};
use crate::services::analysis::snapshot::AnalysisSnapshot;
use crate::services::analysis::trace::AnalysisTraceCategory;
use std::collections::HashMap;

use crate::core::capsule::ModuleId;
use crate::core::resolve::DefId;
use crate::services::analysis::snapshot::FileId;
use crate::services::analysis::syntax_index::{
    CallSite, call_site_at_span, call_site_by_node_id, node_span_map, span_contains_span,
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
        let token = identifier_token_at_span(source.as_deref(), query_span);
        let token_span = token.as_ref().map(|token| token.span).unwrap_or(query_span);
        if let Some(resolved) = entry_state.resolved.as_ref()
            && let Some(target) = linear_decl_target_at_span(
                &resolved.module,
                token_span,
                source.as_deref(),
                token.as_ref().map(|token| token.ident.as_str()),
            )
        {
            return Ok(Some(Location {
                file_id,
                path: snapshot.path(file_id).map(std::path::Path::to_path_buf),
                span: target.span,
            }));
        }
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
        if let Some(symbol_id) = resolved_target.target.symbol_id.as_ref()
            && let Some(location) = location_for_imported_stdlib_symbol(file_id, symbol_id)
        {
            return Ok(Some(location));
        }
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
        let state = if let Some(entry) = program.entry_module_id {
            self.tracer().emit(
                AnalysisTraceCategory::Hover,
                format!(
                    "program hover entry={entry:?} has_entry_state={} module_states={}",
                    program.module_states.contains_key(&entry),
                    program.module_states.len()
                ),
            );
            if let Some(state) = program.module_states.get(&entry).cloned() {
                self.tracer().emit(
                    AnalysisTraceCategory::Hover,
                    format!(
                        "program hover using entry state typed={} resolved={}",
                        state.typed.is_some(),
                        state.resolved.is_some()
                    ),
                );
                state
            } else {
                let fallback = self.lookup_state_for_file(file_id)?;
                self.tracer().emit(
                    AnalysisTraceCategory::Hover,
                    format!(
                        "program hover fallback lookup_state_for_file typed={} resolved={}",
                        fallback.typed.is_some(),
                        fallback.resolved.is_some()
                    ),
                );
                fallback
            }
        } else {
            let fallback = self.lookup_state_for_file(file_id)?;
            self.tracer().emit(
                AnalysisTraceCategory::Hover,
                format!(
                    "program hover no entry module; fallback typed={} resolved={}",
                    fallback.typed.is_some(),
                    fallback.resolved.is_some()
                ),
            );
            fallback
        };
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id);
        let hover = hover_at_span_in_file(
            &state,
            query_span,
            snapshot.path(file_id),
            source.as_deref(),
            Some(self.tracer()),
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
                Some(self.tracer()),
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
            source.as_deref(),
            false,
            &program.module_states,
        ) {
            if let Some(symbol_id) = target.symbol_id.as_ref()
                && let Some(active_parameter) = active_parameter_index_at_call_site(
                    &state,
                    query_span,
                    source.as_deref(),
                    usize::MAX,
                )
                && let Some(sig) =
                    signature_help_for_imported_stdlib_symbol(symbol_id, active_parameter)
            {
                return Ok(Some(sig));
            }
            if let Some(resolved_target) = self.resolve_target_in_program(file_id, target)?
                && let Some(sig) = signature_help_for_resolved_target_at_call_site(
                    &state,
                    query_span,
                    source.as_deref(),
                    &resolved_target,
                )
            {
                return Ok(Some(sig));
            }
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
    state: &LookupState,
    module_id: ModuleId,
    hover: &HoverInfo,
    query_span: Span,
    import_env_by_module: &HashMap<ModuleId, ImportEnv>,
    module_states: &HashMap<ModuleId, LookupState>,
) -> Option<HoverInfo> {
    let snapshot = db.snapshot();
    let source = snapshot.text(origin_file_id);
    if let Some(target) = selected_callable_target(
        &snapshot,
        origin_file_id,
        state,
        query_span,
        source.as_deref(),
        true,
        module_states,
    ) {
        let resolved_target = db
            .resolve_target_in_program(origin_file_id, target)
            .ok()??;
        return hover_for_resolved_target(&resolved_target);
    }

    if let Some(symbol_id) = hover.symbol_id.as_ref()
        && let Some(imported) = hover_for_imported_stdlib_symbol(symbol_id)
    {
        return Some(imported);
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
    snapshot: &AnalysisSnapshot,
    query_span: Span,
    source: Option<&str>,
    module_id: ModuleId,
    state: &LookupState,
    import_env_by_module: &HashMap<ModuleId, ImportEnv>,
    module_states: &HashMap<ModuleId, LookupState>,
    require_callee_span: bool,
) -> QueryResult<Option<ResolvedSymbolTarget>> {
    let target = if let Some(target) = selected_callable_target(
        snapshot,
        origin_file_id,
        state,
        query_span,
        source,
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
    snapshot: &AnalysisSnapshot,
    origin_file_id: FileId,
    state: &LookupState,
    query_span: Span,
    source: Option<&str>,
    require_callee_span: bool,
    module_states: &HashMap<ModuleId, LookupState>,
) -> Option<DefTarget> {
    let typed = state.typed.as_ref()?;
    let call = call_site_at_span(&typed.module, query_span)?;
    if require_callee_span {
        if let Some(method_name) = call.method_name.as_deref() {
            let token = identifier_token_at_span(source, query_span)?;
            if token.ident != method_name {
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
    }
    let selected_sig = typed.call_sigs.get(&call.node_id)?;
    if let Some(SelectedCallable::Canonical(symbol_id)) = selected_sig.selected.as_ref()
        && let Some(target) =
            def_target_for_symbol_id_in_states(snapshot, module_states, origin_file_id, symbol_id)
    {
        return Some(target);
    }

    if call.method_name.is_some() {
        return selected_method_target(snapshot, origin_file_id, state, &call, module_states);
    }

    None
}

// Method calls can miss a finalized selected callable in best-effort analysis,
// so fall back to matching the resolved receiver/arg/result types against
// source method definitions in the loaded program states.
fn selected_method_target(
    snapshot: &AnalysisSnapshot,
    origin_file_id: FileId,
    state: &LookupState,
    call: &CallSite,
    module_states: &HashMap<ModuleId, LookupState>,
) -> Option<DefTarget> {
    let typed = state.typed.as_ref()?;
    let method_name = call.method_name.as_deref()?;
    let receiver_ty = receiver_type_for_call(snapshot, origin_file_id, state, call, module_states)?;
    let owner_name = method_owner_name(&receiver_ty);
    let arg_tys = call
        .arg_node_ids
        .iter()
        .map(|node_id| typed.type_map.lookup_node_type(*node_id))
        .collect::<Option<Vec<_>>>()?;
    let ret_ty = typed.type_map.lookup_node_type(call.node_id)?;

    let mut matches = Vec::new();
    for (module_id, candidate_state) in module_states {
        let Some(candidate_typed) = candidate_state.typed.as_ref() else {
            continue;
        };
        for def in candidate_typed.def_table.defs() {
            if def.name != method_name {
                continue;
            }
            let Some(symbol_id) = candidate_typed.symbol_ids.lookup_symbol_id(def.id) else {
                continue;
            };
            if symbol_id.ns != SymbolNs::Method {
                continue;
            }
            let Some(owner_segment) = symbol_id.path.segments.iter().rev().nth(1) else {
                continue;
            };
            if let Some(owner_name) = owner_name
                && owner_segment.name != owner_name
            {
                continue;
            }
            let Some(method_ty) = candidate_typed.type_map.lookup_def_type(def) else {
                continue;
            };
            if !method_type_matches_call(&method_ty, &receiver_ty, &arg_tys, &ret_ty) {
                continue;
            }
            let file_id = candidate_typed
                .def_table
                .source_path()
                .and_then(|path| snapshot.file_id(path))
                .unwrap_or(origin_file_id);
            matches.push(DefTarget {
                file_id,
                module_id: Some(*module_id),
                def_id: def.id,
                symbol_id: Some(symbol_id.clone()),
                program_scoped: true,
            });
        }
    }

    if matches.len() == 1 {
        matches.pop()
    } else {
        None
    }
}

fn receiver_type_for_call(
    snapshot: &AnalysisSnapshot,
    origin_file_id: FileId,
    state: &LookupState,
    call: &crate::services::analysis::syntax_index::CallSite,
    module_states: &HashMap<ModuleId, LookupState>,
) -> Option<Type> {
    let typed = state.typed.as_ref()?;
    let receiver_ty = typed.type_map.lookup_node_type(call.callee_node_id);
    let Some(def_id) = typed.def_table.lookup_node_def_id(call.callee_node_id) else {
        return receiver_ty;
    };
    let receiver_ty =
        resolved_binding_type_for_def(&typed.module, typed, &typed.def_table, def_id, receiver_ty);
    if receiver_ty
        .as_ref()
        .is_some_and(|ty| !ty.contains_unresolved())
    {
        return receiver_ty;
    }

    let value_node_id = binding_value_node_id_for_def(&typed.module, &typed.def_table, def_id);
    let value_node_id = value_node_id?;
    let init_sig = typed.call_sigs.get(&value_node_id)?;
    let target = if let Some(SelectedCallable::Canonical(symbol_id)) = init_sig.selected.as_ref() {
        def_target_for_symbol_id_in_states(snapshot, module_states, origin_file_id, symbol_id)?
    } else {
        let init_call = call_site_by_node_id(&typed.module, value_node_id)?;
        selected_method_target(snapshot, origin_file_id, state, &init_call, module_states)?
    };
    let module_id = target.module_id?;
    let target_state = module_states.get(&module_id)?.typed.as_ref()?;
    let def = target_state.def_table.lookup_def(target.def_id)?;
    let Type::Fn { ret_ty, .. } = target_state.type_map.lookup_def_type(def)? else {
        return receiver_ty;
    };
    Some(ret_ty.as_ref().clone())
}

fn method_owner_name(ty: &Type) -> Option<&str> {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.as_str()),
        Type::Heap { elem_ty } | Type::Slice { elem_ty } | Type::Ref { elem_ty, .. } => {
            method_owner_name(elem_ty)
        }
        _ => None,
    }
}

fn method_type_matches_call(
    method_ty: &Type,
    receiver_ty: &Type,
    arg_tys: &[Type],
    ret_ty: &Type,
) -> bool {
    let Type::Fn {
        params,
        ret_ty: method_ret,
    } = method_ty
    else {
        return false;
    };
    if params.len() != arg_tys.len() + 1 {
        return false;
    }
    if !types_compatible(&params[0].ty, receiver_ty) {
        return false;
    }
    if !params[1..]
        .iter()
        .zip(arg_tys)
        .all(|(param, arg)| types_compatible(&param.ty, arg))
    {
        return false;
    }
    types_compatible(method_ret, ret_ty)
}

fn types_compatible(expected: &Type, actual: &Type) -> bool {
    expected == actual
        || matches!(expected, Type::Unknown)
        || matches!(actual, Type::Unknown)
        || expected.contains_unresolved()
        || actual.contains_unresolved()
}

fn local_def_target(
    snapshot: &AnalysisSnapshot,
    origin_file_id: FileId,
    module_id: ModuleId,
    state: &LookupState,
    def_id: DefId,
) -> Option<DefTarget> {
    let resolved = state.resolved.as_ref()?;
    let file_id = resolved
        .def_table
        .source_path()
        .and_then(|path| snapshot.file_id(path))
        .unwrap_or(origin_file_id);
    Some(DefTarget {
        file_id,
        module_id: Some(module_id),
        def_id,
        symbol_id: resolved.symbol_ids.lookup_symbol_id(def_id).cloned(),
        program_scoped: true,
    })
}

fn imported_or_local_def_target(
    snapshot: &AnalysisSnapshot,
    origin_file_id: FileId,
    module_id: ModuleId,
    state: &LookupState,
    def_id: DefId,
    import_env_by_module: &HashMap<ModuleId, ImportEnv>,
    module_states: &HashMap<ModuleId, LookupState>,
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

    local_def_target(snapshot, origin_file_id, module_id, state, def_id)
}
