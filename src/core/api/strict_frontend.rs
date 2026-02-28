//! Shared strict-frontend orchestration used by both the batch driver and
//! analysis-side fallbacks.
//!
//! This module owns the "file path + prelude + capsule flattening + strict
//! resolve/typecheck + monomorphize" workflow so services do not need to reach
//! into driver code to reuse it.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::core::capsule::compose::{flatten_capsule, merge_modules};
use crate::core::capsule::{self, ModuleId};
use crate::core::context::{
    CapsuleParsedContext, ParsedContext, ResolvedContext, TypeCheckedContext,
};
use crate::core::diag::CompileError;
use crate::core::monomorphize;
use crate::core::tree::{Module, NodeId, NodeIdGen};

use super::{
    FrontendPolicy, ParseModuleError, ParseModuleOptions, ResolveInputs,
    parse_module_with_id_gen_and_options, resolve_typecheck_pipeline_with_policy,
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct StrictFrontendOptions {
    pub inject_prelude: bool,
    pub experimental_typestate: bool,
}

pub(crate) struct StrictFrontendParsed {
    pub module: Module,
    pub id_gen: NodeIdGen,
    pub top_level_owners: HashMap<NodeId, ModuleId>,
}

pub fn check_strict_frontend_with_path(
    source: &str,
    source_path: &Path,
    inject_prelude: bool,
    experimental_typestate: bool,
) -> Result<(), Vec<CompileError>> {
    let parsed = build_strict_frontend_input(
        source,
        Some(source_path),
        StrictFrontendOptions {
            inject_prelude,
            experimental_typestate,
        },
    )?;
    let _ = run_strict_frontend(parsed)?;
    Ok(())
}

pub fn lookup_strict_frontend_with_path(
    source: &str,
    source_path: &Path,
    inject_prelude: bool,
    experimental_typestate: bool,
) -> Result<(ResolvedContext, TypeCheckedContext), Vec<CompileError>> {
    let parsed = build_strict_frontend_input(
        source,
        Some(source_path),
        StrictFrontendOptions {
            inject_prelude,
            experimental_typestate,
        },
    )?;
    run_strict_frontend(parsed)
}

pub(crate) fn build_strict_frontend_input(
    source: &str,
    source_path: Option<&Path>,
    opts: StrictFrontendOptions,
) -> Result<StrictFrontendParsed, Vec<CompileError>> {
    let (user_module, id_gen, top_level_owners) = if let Some(path) = source_path {
        let capsule = capsule::discover_and_parse_capsule_with_options(
            source,
            path,
            capsule::CapsuleParseOptions {
                experimental_typestate: opts.experimental_typestate,
            },
        )
        .map_err(|e| vec![e.into()])?;
        let capsule_context = CapsuleParsedContext::new(capsule);
        let flattened = flatten_capsule(&capsule_context)
            .map_err(|errs| errs.into_iter().map(CompileError::from).collect::<Vec<_>>())?;
        (
            flattened.module,
            capsule_context.next_node_id_gen().clone(),
            flattened.top_level_owners,
        )
    } else {
        let id_gen = NodeIdGen::new();
        let (module, id_gen) = parse_with_id_gen(source, id_gen, opts.experimental_typestate)?;
        (module, id_gen, HashMap::new())
    };

    let (module, id_gen) = if opts.inject_prelude {
        inject_prelude_module(user_module, id_gen, opts.experimental_typestate)?
    } else {
        (user_module, id_gen)
    };

    Ok(StrictFrontendParsed {
        module,
        id_gen,
        top_level_owners,
    })
}

pub(crate) fn run_strict_frontend(
    parsed: StrictFrontendParsed,
) -> Result<(ResolvedContext, TypeCheckedContext), Vec<CompileError>> {
    let parsed_context = ParsedContext::new(parsed.module, parsed.id_gen);
    resolve_and_typecheck_strict(parsed_context, &parsed.top_level_owners)
}

fn parse_with_id_gen(
    source: &str,
    id_gen: NodeIdGen,
    experimental_typestate: bool,
) -> Result<(Module, NodeIdGen), Vec<CompileError>> {
    parse_module_with_id_gen_and_options(
        source,
        id_gen,
        ParseModuleOptions {
            experimental_typestate,
        },
    )
    .map_err(|e| match e {
        ParseModuleError::Lex(err) => vec![err.into()],
        ParseModuleError::Parse(err) => vec![err.into()],
    })
}

fn inject_prelude_module(
    user_module: Module,
    id_gen: NodeIdGen,
    experimental_typestate: bool,
) -> Result<(Module, NodeIdGen), Vec<CompileError>> {
    let prelude_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("std")
        .join("prelude_decl.mc");
    let prelude_src = std::fs::read_to_string(&prelude_path)
        .map_err(|e| vec![CompileError::Io(prelude_path.clone(), e)])?;
    let (prelude_module, id_gen) = parse_with_id_gen(&prelude_src, id_gen, experimental_typestate)?;
    Ok((merge_modules(&prelude_module, &user_module), id_gen))
}

fn resolve_and_typecheck_strict(
    parsed_context: ParsedContext,
    top_level_owners: &HashMap<NodeId, ModuleId>,
) -> Result<(ResolvedContext, TypeCheckedContext), Vec<CompileError>> {
    let first_pass = resolve_typecheck_pipeline_with_policy(
        parsed_context,
        ResolveInputs::default(),
        Some(top_level_owners),
        FrontendPolicy::Strict,
    );
    if !first_pass.resolve_errors.is_empty() {
        return Err(first_pass
            .resolve_errors
            .into_iter()
            .map(CompileError::from)
            .collect());
    }
    if !first_pass.type_errors.is_empty() {
        return Err(first_pass
            .type_errors
            .into_iter()
            .map(CompileError::from)
            .collect());
    }

    let resolved_context = first_pass
        .resolved_context
        .expect("strict resolve should produce context when no errors");
    let typed_context = first_pass
        .typed_context
        .expect("strict typecheck should produce context when no errors");

    let (resolved_context, typed_context, _stats) =
        monomorphize::monomorphize(resolved_context, typed_context, Some(top_level_owners))
            .map_err(|e| match e {
                monomorphize::MonomorphizePipelineError::Monomorphize(err) => {
                    vec![CompileError::from(err)]
                }
                monomorphize::MonomorphizePipelineError::Retype(errors) => errors
                    .into_iter()
                    .map(CompileError::from)
                    .collect::<Vec<CompileError>>(),
            })?;
    Ok((resolved_context, typed_context))
}
