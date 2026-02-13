//! Program-aware analysis pipeline orchestration.
//!
//! This module owns dependency-closure analysis for an entry file:
//! discovery/parse, per-module pipeline execution, import fact propagation,
//! and merged diagnostics/module lookup state assembly.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use crate::core::api;
use crate::core::capsule::compose::merge_modules;
use crate::core::capsule::{self, ModuleId, ModulePath};
use crate::core::context::{
    ImportEnv, ModuleExportFacts, ResolvedContext, import_env_from_requires,
    module_export_facts_from_def_table,
};
use crate::core::resolve::{DefId, DefKind, DefLocation, GlobalDefId};
use crate::core::tree::NodeIdGen;
use crate::services::analysis::diagnostics::{ANALYSIS_FILE_PATH_KEY, Diagnostic, DiagnosticValue};
use crate::services::analysis::frontend_support::{
    SnapshotOverlayLoader, frontend_error_diagnostics, infer_project_root, stable_source_revision,
};
use crate::services::analysis::pipeline::{
    LookupState, collect_sorted_diagnostics, run_module_pipeline_with_parsed_and_imports,
    to_lookup_state,
};
use crate::services::analysis::program_imports::ProgramImportFactsCache;
use crate::services::analysis::query::{QueryKey, QueryResult, QueryRuntime};
use crate::services::analysis::snapshot::{AnalysisSnapshot, FileId};

#[derive(Clone, Default)]
pub(crate) struct ProgramPipelineResult {
    pub(crate) diagnostics: Vec<Diagnostic>,
    pub(crate) entry_module_id: Option<ModuleId>,
    pub(crate) module_states: HashMap<ModuleId, LookupState>,
    pub(crate) import_env_by_module: HashMap<ModuleId, ImportEnv>,
}

pub(crate) fn run_program_pipeline_for_file(
    rt: &mut QueryRuntime,
    snapshot: AnalysisSnapshot,
    file_id: FileId,
) -> QueryResult<ProgramPipelineResult> {
    let Some(entry_source) = snapshot.text(file_id) else {
        return Ok(ProgramPipelineResult::default());
    };
    let Some(entry_path) = snapshot.path(file_id).map(Path::to_path_buf) else {
        return Ok(ProgramPipelineResult::default());
    };
    let key = QueryKey::new(
        crate::services::analysis::query::QueryKind::ProgramPipeline,
        ModuleId(file_id.0),
        snapshot.revision(),
    );
    rt.execute(key, move |rt| {
        let mut result = ProgramPipelineResult::default();
        let tag_with_entry_path = |mut diagnostics: Vec<Diagnostic>| {
            let entry_file_path = entry_path.to_string_lossy().to_string();
            for diag in &mut diagnostics {
                diag.metadata
                    .entry(ANALYSIS_FILE_PATH_KEY.to_string())
                    .or_insert_with(|| DiagnosticValue::String(entry_file_path.clone()));
            }
            diagnostics
        };
        let project_root = infer_project_root(&entry_path);
        let entry_module_path = match ModulePath::from_file(&entry_path, &project_root) {
            Ok(path) => path,
            Err(err) => {
                result.diagnostics = tag_with_entry_path(frontend_error_diagnostics(err));
                return Ok(result);
            }
        };
        let loader = SnapshotOverlayLoader::new(snapshot.clone(), project_root);
        let program = match capsule::discover_and_parse_capsule_with_loader(
            &entry_source,
            &entry_path,
            entry_module_path,
            &loader,
        ) {
            Ok(program) => program,
            Err(err) => {
                result.diagnostics = tag_with_entry_path(frontend_error_diagnostics(err));
                return Ok(result);
            }
        };
        let program_context = crate::core::context::CapsuleParsedContext::new(program);
        let entry_module_id = program_context.entry();
        let mut import_facts = ProgramImportFactsCache::default();
        let prelude_module = parsed_prelude_decl_module(program_context.next_node_id_gen());
        let mut all_diagnostics = Vec::new();
        let mut module_states = HashMap::<ModuleId, LookupState>::new();
        let mut exports_by_module = HashMap::<ModuleId, ModuleExportFacts>::new();
        let mut import_env_by_module = HashMap::<ModuleId, ImportEnv>::new();

        for module_id in program_context.dependency_order_from_entry() {
            let Some(parsed) = program_context.module(module_id) else {
                continue;
            };
            let source = std::sync::Arc::<str>::from(parsed.source.source.as_str());
            let module_revision = stable_source_revision(&parsed.source.source);
            let parsed_context = crate::core::context::ParsedContext::new(
                module_with_implicit_prelude(parsed, prelude_module.as_ref()),
                program_context.next_node_id_gen().clone(),
            )
            .with_source_path(parsed.source.file_path.clone());
            let imported_modules = import_facts.imported_modules_for(&program_context, module_id);
            let imported_symbols = import_facts.imported_symbols_for(&program_context, module_id);
            let skip_typecheck = ProgramImportFactsCache::should_skip_typecheck(&imported_symbols);
            let mut state = run_module_pipeline_with_parsed_and_imports(
                rt,
                module_id,
                module_revision,
                source,
                Some(parsed_context),
                imported_modules,
                imported_symbols,
                skip_typecheck,
            )?;
            if let Some(resolved) = &mut state.resolved.product {
                apply_prelude_runtime_def_locations(parsed, resolved);
                import_facts.ingest_resolved(module_id, resolved);
                let exports = module_export_facts_from_def_table(
                    module_id,
                    Some(parsed.source.path.clone()),
                    &resolved.def_table,
                );
                let import_env =
                    import_env_from_requires(&program_context, module_id, &exports_by_module);
                exports_by_module.insert(module_id, exports);
                if !import_env.module_aliases.is_empty() || !import_env.symbol_aliases.is_empty() {
                    import_env_by_module.insert(module_id, import_env);
                }
            }
            if let Some(typed) = &state.typechecked.product {
                import_facts.ingest_typed(module_id, typed);
            }
            let mut module_diags = collect_sorted_diagnostics(&state);
            let file_path = parsed.source.file_path.to_string_lossy().to_string();
            for diag in &mut module_diags {
                diag.metadata.insert(
                    ANALYSIS_FILE_PATH_KEY.to_string(),
                    DiagnosticValue::String(file_path.clone()),
                );
            }
            all_diagnostics.extend(module_diags);
            module_states.insert(module_id, to_lookup_state(&state));
        }

        all_diagnostics.sort_by_key(|diag| {
            (
                diag.phase,
                diag.span.start.line,
                diag.span.start.column,
                diag.code.clone(),
            )
        });
        result.diagnostics = all_diagnostics;
        result.entry_module_id = Some(entry_module_id);
        result.module_states = module_states;
        result.import_env_by_module = import_env_by_module;
        Ok(result)
    })
}

pub(crate) fn resolve_imported_symbol_target_from_import_env(
    module_id: ModuleId,
    local_def: &crate::core::resolve::Def,
    import_env_by_module: &HashMap<ModuleId, ImportEnv>,
) -> Option<GlobalDefId> {
    let import_env = import_env_by_module.get(&module_id)?;
    let binding = import_env.symbol_aliases.get(&local_def.name)?;
    let target = match local_def.kind {
        DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => binding.callables.first().copied(),
        DefKind::TypeDef { .. } => binding.type_def,
        DefKind::TraitDef { .. } => binding.trait_def,
        _ => None,
    };
    if target.is_some() {
        return target;
    }

    // Partial/errored resolve may classify imported symbol aliases conservatively.
    // If the import binding has only one possible export target, use it.
    let mut candidates = Vec::new();
    if let Some(callable) = binding.callables.first().copied() {
        candidates.push(callable);
    }
    if let Some(type_def) = binding.type_def {
        candidates.push(type_def);
    }
    if let Some(trait_def) = binding.trait_def {
        candidates.push(trait_def);
    }
    candidates.dedup();
    if candidates.len() == 1 {
        return candidates.first().copied();
    }
    None
}

fn module_with_implicit_prelude(
    parsed: &crate::core::capsule::ParsedModule,
    prelude_module: Option<&crate::core::tree::parsed::Module>,
) -> crate::core::tree::parsed::Module {
    let Some(prelude_module) = prelude_module else {
        return parsed.module.clone();
    };
    if is_std_prelude_decl(parsed) {
        parsed.module.clone()
    } else {
        merge_modules(prelude_module, &parsed.module)
    }
}

fn is_std_prelude_decl(parsed: &crate::core::capsule::ParsedModule) -> bool {
    matches!(
        parsed.source.path.segments(),
        [std_seg, prelude_seg] if std_seg == "std" && prelude_seg == "prelude_decl"
    )
}

fn parsed_prelude_decl_module(id_gen: &NodeIdGen) -> Option<crate::core::tree::parsed::Module> {
    let prelude_path = prelude_decl_path();
    let prelude_src = std::fs::read_to_string(prelude_path).ok()?;
    let (module, _) = api::parse_module_with_id_gen(&prelude_src, id_gen.clone()).ok()?;
    Some(module)
}

fn prelude_decl_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("std")
        .join("prelude_decl.mc")
}

fn apply_prelude_runtime_def_locations(
    parsed: &crate::core::capsule::ParsedModule,
    resolved: &mut ResolvedContext,
) {
    if is_std_prelude_decl(parsed) {
        return;
    }
    let runtime_locations = prelude_decl_runtime_locations();
    if runtime_locations.is_empty() {
        return;
    }
    let runtime_defs: Vec<(DefId, String)> = resolved
        .def_table
        .defs()
        .iter()
        .filter(|def| def.is_runtime())
        .map(|def| (def.id, def.name.clone()))
        .collect();
    for (def_id, name) in runtime_defs {
        if let Some(loc) = runtime_locations.get(&name) {
            resolved.def_table.set_def_location(def_id, loc.clone());
        }
    }
}

fn prelude_decl_runtime_locations() -> &'static HashMap<String, DefLocation> {
    static PRELUDE_RUNTIME_LOCATIONS: OnceLock<HashMap<String, DefLocation>> = OnceLock::new();
    PRELUDE_RUNTIME_LOCATIONS.get_or_init(build_prelude_runtime_locations)
}

fn build_prelude_runtime_locations() -> HashMap<String, DefLocation> {
    let prelude_path = prelude_decl_path();
    let Ok(prelude_src) = std::fs::read_to_string(prelude_path) else {
        return HashMap::new();
    };
    let Ok((module, id_gen)) = api::parse_module_with_id_gen(&prelude_src, NodeIdGen::new()) else {
        return HashMap::new();
    };
    let parsed = crate::core::context::ParsedContext::new(module, id_gen)
        .with_source_path(prelude_decl_path());
    let resolved = api::resolve_stage_partial(parsed, HashMap::new(), HashMap::new());
    let mut locations = HashMap::new();
    for def in resolved.context.def_table.defs() {
        if !def.is_runtime() {
            continue;
        }
        let Some(loc) = resolved.context.def_table.lookup_def_location(def.id) else {
            continue;
        };
        locations.insert(def.name.clone(), loc);
    }
    locations
}
