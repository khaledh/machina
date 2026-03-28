//! Stdlib object/archive helpers for object-backed stdlib modules.

use crate::core::capsule::ModulePath;
use crate::driver::compile::{CompileOptions, compile_with_path};
use crate::driver::support_utils::{
    archive_objects, artifact_is_stale, assemble_object, native_support_dir,
};

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct StdlibArtifacts {
    pub archive_path: PathBuf,
    pub object_backed_modules: HashSet<ModulePath>,
}

pub fn ensure_stdlib_archive_for_modules(
    module_paths: &HashSet<ModulePath>,
) -> Result<Option<StdlibArtifacts>, String> {
    let object_backed_modules = supported_stdlib_object_modules(module_paths);
    if object_backed_modules.is_empty() {
        return Ok(None);
    }

    let archive_dir = native_support_dir()?.join("stdlib");
    fs::create_dir_all(&archive_dir)
        .map_err(|e| format!("failed to create {}: {e}", archive_dir.display()))?;

    let object_path = ensure_flattened_stdlib_object(&object_backed_modules, &archive_dir)?;
    let archive_path = archive_dir.join("libmachina_std.a");
    if artifact_is_stale(&archive_path, std::slice::from_ref(&object_path))? {
        archive_objects(&archive_path, &[object_path.clone()])?;
    }

    Ok(Some(StdlibArtifacts {
        archive_path,
        object_backed_modules,
    }))
}

fn supported_stdlib_object_modules(module_paths: &HashSet<ModulePath>) -> HashSet<ModulePath> {
    let referenced_supported = module_paths
        .iter()
        .any(|path| is_object_backed_stdlib_module(path));
    if !referenced_supported {
        return HashSet::new();
    }

    let mut modules = HashSet::new();
    modules.insert(ModulePath::new(vec!["std".to_string(), "parse".to_string()]).unwrap());
    modules.insert(ModulePath::new(vec!["std".to_string(), "env".to_string()]).unwrap());
    modules.insert(ModulePath::new(vec!["std".to_string(), "io".to_string()]).unwrap());
    modules
}

fn is_object_backed_stdlib_module(module_path: &ModulePath) -> bool {
    matches!(
        module_path.segments(),
        [std_seg, name] if std_seg == "std" && (name == "parse" || name == "env" || name == "io")
    )
}

fn ensure_flattened_stdlib_object(
    object_backed_modules: &HashSet<ModulePath>,
    archive_dir: &PathBuf,
) -> Result<PathBuf, String> {
    let object_path = archive_dir.join("stdlib.o");
    let prelude_path = prelude_source_path();
    let mut sources = object_backed_modules
        .iter()
        .map(stdlib_source_path)
        .collect::<Result<Vec<_>, _>>()?;
    sources.push(prelude_path);
    if !artifact_is_stale(&object_path, &sources)? {
        return Ok(object_path);
    }

    let source = synthetic_stdlib_entry_source(object_backed_modules);
    let source_path = synthetic_stdlib_entry_path();
    let output = compile_with_path(
        &source,
        Some(&source_path),
        &CompileOptions {
            dump: None,
            emit_ir: false,
            verify_ir: false,
            trace_alloc: false,
            trace_drops: false,
            inject_prelude: true,
            use_stdlib_objects: false,
        },
    )
    .map_err(|errs| {
        errs.into_iter()
            .map(|err| err.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let asm_path = archive_dir.join("stdlib.s");
    fs::write(&asm_path, output.asm)
        .map_err(|e| format!("failed to write {}: {e}", asm_path.display()))?;
    let assemble_result = assemble_object(&asm_path, &object_path);
    let _ = fs::remove_file(&asm_path);
    assemble_result?;
    Ok(object_path)
}

fn stdlib_source_path(module_path: &ModulePath) -> Result<PathBuf, String> {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std");
    for segment in module_path.segments().iter().skip(1) {
        path.push(segment);
    }
    path.set_extension("mc");
    if path.exists() {
        Ok(path)
    } else {
        Err(format!(
            "stdlib source file not found at {}",
            path.display()
        ))
    }
}

fn prelude_source_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("std")
        .join("prelude.mc")
}

fn synthetic_stdlib_entry_source(modules: &HashSet<ModulePath>) -> String {
    let mut module_paths = modules
        .iter()
        .map(|path| path.segments().join("::"))
        .collect::<Vec<_>>();
    module_paths.sort();
    let requires = module_paths
        .into_iter()
        .map(|path| format!("    {path}"))
        .collect::<Vec<_>>()
        .join("\n");
    format!("requires {{\n{requires}\n}}\n")
}

fn synthetic_stdlib_entry_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("std")
        .join("__stdlib_archive_entry.mc")
}
