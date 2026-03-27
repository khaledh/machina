//! Stdlib interface emission/loading helpers.
//!
//! Phase 2.4 keeps source flattening in place for code generation, but starts
//! consuming `.mci` metadata for stdlib export surfaces. These helpers build and
//! cache stdlib interfaces under `target/machina-support/interfaces/`.

use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use thiserror::Error;

use crate::core::api::{self, ParseModuleError};
use crate::core::capsule::compose::merge_modules;
use crate::core::capsule::{self, CapsuleParseOptions, FsModuleLoader, ModulePath, ParsedModule};
use crate::core::context::{CapsuleParsedContext, ResolvedContext};
use crate::core::resolve::resolve_program;

use super::{
    ModuleArtifactPaths, ModuleInterface, ModuleInterfaceCodec, ModuleInterfaceIoError,
    emit_module_interface_from_module_with_codec, read_module_interface_with_codec,
};

#[derive(Debug, Error)]
pub enum StdlibInterfaceError<E> {
    #[error("module `{0}` is not a stdlib module")]
    NotStdModule(ModulePath),
    #[error("failed to read stdlib source {0}: {1}")]
    Io(PathBuf, std::io::Error),
    #[error("capsule discovery failed: {0}")]
    Capsule(#[from] capsule::CapsuleError),
    #[error("failed to parse implicit prelude: {0}")]
    PreludeParse(String),
    #[error("failed to resolve stdlib module interface: {0}")]
    Resolve(String),
    #[error(transparent)]
    InterfaceIo(#[from] ModuleInterfaceIoError<E>),
}

pub fn ensure_stdlib_module_interface_with_codec<C: ModuleInterfaceCodec>(
    module_path: &ModulePath,
) -> Result<PathBuf, StdlibInterfaceError<C::Error>> {
    if !is_stdlib_module_path(module_path) {
        return Err(StdlibInterfaceError::NotStdModule(module_path.clone()));
    }

    let artifact_root = stdlib_interface_artifact_root()
        .map_err(|e| StdlibInterfaceError::Io(interface_root_path(), e))?;
    let artifact_paths = ModuleArtifactPaths::for_module(&artifact_root, module_path);
    let (parsed_entry, resolved_entry, input_paths) = stdlib_interface_inputs::<C>(module_path)?;
    if !artifact_is_stale(&artifact_paths.interface_path, &input_paths)
        .map_err(|e| StdlibInterfaceError::Io(artifact_paths.interface_path.clone(), e))?
    {
        return Ok(artifact_paths.interface_path);
    }

    emit_module_interface_from_module_with_codec::<C>(
        &artifact_root,
        &parsed_entry.module,
        &resolved_entry,
    )?;
    Ok(artifact_paths.interface_path)
}

pub fn load_stdlib_module_interface_with_codec<C: ModuleInterfaceCodec>(
    module_path: &ModulePath,
) -> Result<ModuleInterface, StdlibInterfaceError<C::Error>> {
    let path = ensure_stdlib_module_interface_with_codec::<C>(module_path)?;
    read_module_interface_with_codec::<C>(&path).map_err(StdlibInterfaceError::from)
}

fn stdlib_interface_inputs<C: ModuleInterfaceCodec>(
    module_path: &ModulePath,
) -> Result<(ParsedModule, ResolvedContext, Vec<PathBuf>), StdlibInterfaceError<C::Error>> {
    let source_path = stdlib_source_path::<C>(module_path)?;
    let source = fs::read_to_string(&source_path)
        .map_err(|e| StdlibInterfaceError::Io(source_path.clone(), e))?;
    let loader = FsModuleLoader::new(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
    let capsule = capsule::discover_and_parse_capsule_with_loader_and_options(
        &source,
        &source_path,
        module_path.clone(),
        &loader,
        CapsuleParseOptions {
            inject_prelude_requires: false,
        },
    )?;
    let mut input_paths = capsule
        .modules
        .values()
        .map(|module| module.source.file_path.clone())
        .collect::<Vec<_>>();
    input_paths.push(prelude_path());

    let capsule = capsule_with_implicit_prelude::<C>(capsule)?;
    let program = CapsuleParsedContext::new(capsule);
    let resolved = resolve_program(program.clone())
        .map_err(|errs| StdlibInterfaceError::Resolve(format_resolve_errors(&errs)))?;
    let parsed_entry = program.entry_module().clone();
    let resolved_entry = resolved.entry_module().clone();
    Ok((parsed_entry, resolved_entry, input_paths))
}

fn capsule_with_implicit_prelude<C: ModuleInterfaceCodec>(
    mut capsule: crate::core::capsule::CapsuleParsed,
) -> Result<crate::core::capsule::CapsuleParsed, StdlibInterfaceError<C::Error>> {
    let prelude_source_path = prelude_path();
    let prelude_src = fs::read_to_string(&prelude_source_path)
        .map_err(|e| StdlibInterfaceError::Io(prelude_source_path.clone(), e))?;
    let (mut prelude_module, next_node_id_gen) =
        api::parse_module_with_id_gen(&prelude_src, capsule.next_node_id_gen.clone()).map_err(
            |err| {
                StdlibInterfaceError::PreludeParse(match err {
                    ParseModuleError::Lex(err) => err.to_string(),
                    ParseModuleError::Parse(err) => err.to_string(),
                })
            },
        )?;
    prelude_module.requires.clear();
    for parsed in capsule.modules.values_mut() {
        if is_std_prelude_module(parsed) {
            continue;
        }
        parsed.module = merge_modules(&prelude_module, &parsed.module);
    }
    capsule.next_node_id_gen = next_node_id_gen;
    Ok(capsule)
}

fn is_stdlib_module_path(module_path: &ModulePath) -> bool {
    module_path.segments().first().map(String::as_str) == Some("std")
}

fn is_std_prelude_module(parsed: &ParsedModule) -> bool {
    matches!(
        parsed.source.path.segments(),
        [std_seg, prelude_seg] if std_seg == "std" && prelude_seg == "prelude"
    )
}

fn stdlib_source_path<C: ModuleInterfaceCodec>(
    module_path: &ModulePath,
) -> Result<PathBuf, StdlibInterfaceError<C::Error>> {
    if !is_stdlib_module_path(module_path) {
        return Err(StdlibInterfaceError::NotStdModule(module_path.clone()));
    }
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std");
    for segment in module_path.segments().iter().skip(1) {
        path.push(segment);
    }
    path.set_extension("mc");
    Ok(path)
}

fn prelude_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("std")
        .join("prelude.mc")
}

fn stdlib_interface_artifact_root() -> Result<PathBuf, std::io::Error> {
    let root = interface_root_path();
    fs::create_dir_all(&root)?;
    Ok(root)
}

fn interface_root_path() -> PathBuf {
    let base = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target"));
    base.join("machina-support").join("interfaces")
}

fn artifact_is_stale(artifact: &Path, inputs: &[PathBuf]) -> Result<bool, std::io::Error> {
    let Ok(metadata) = fs::metadata(artifact) else {
        return Ok(true);
    };
    let artifact_mtime = metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH);
    for input in inputs {
        let input_mtime = fs::metadata(input)
            .and_then(|meta| meta.modified())
            .unwrap_or(SystemTime::UNIX_EPOCH);
        if input_mtime > artifact_mtime {
            return Ok(true);
        }
    }
    Ok(false)
}

fn format_resolve_errors(errors: &[crate::core::resolve::ResolveError]) -> String {
    errors
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join("\n")
}
