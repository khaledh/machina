//! Stdlib object/archive helpers for object-backed stdlib modules.

use crate::core::capsule::ModulePath;
use crate::core::interface::object_rel_path;
use crate::driver::compile::{CompileOptions, compile_with_path};
use crate::driver::support_utils::{
    archive_objects, artifact_is_stale, assemble_object, native_support_dir,
};

use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

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

    let object_root = native_support_dir()?.join("objects");
    let archive_dir = native_support_dir()?.join("stdlib");
    fs::create_dir_all(&object_root)
        .map_err(|e| format!("failed to create {}: {e}", object_root.display()))?;
    fs::create_dir_all(&archive_dir)
        .map_err(|e| format!("failed to create {}: {e}", archive_dir.display()))?;

    let mut objects = Vec::new();
    for module_path in &object_backed_modules {
        objects.push(ensure_stdlib_module_object(module_path, &object_root)?);
    }

    let archive_path = archive_dir.join("libmachina_std.a");
    archive_objects(&archive_path, &objects)?;

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
    modules
}

fn is_object_backed_stdlib_module(module_path: &ModulePath) -> bool {
    matches!(
        module_path.segments(),
        [std_seg, name] if std_seg == "std" && (name == "parse" || name == "env")
    )
}

fn ensure_stdlib_module_object(
    module_path: &ModulePath,
    object_root: &Path,
) -> Result<PathBuf, String> {
    let source_path = stdlib_source_path(module_path)?;
    let prelude_path = prelude_source_path();
    let object_path = object_root.join(object_rel_path(module_path));
    let object_parent = object_path
        .parent()
        .ok_or_else(|| format!("no parent for {}", object_path.display()))?;
    fs::create_dir_all(object_parent)
        .map_err(|e| format!("failed to create {}: {e}", object_parent.display()))?;

    if !artifact_is_stale(&object_path, &[source_path.clone(), prelude_path.clone()])? {
        return Ok(object_path);
    }

    let source = fs::read_to_string(&source_path)
        .map_err(|e| format!("failed to read {}: {e}", source_path.display()))?;
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

    let asm_path = object_parent.join(
        object_path
            .file_stem()
            .unwrap_or_else(|| OsStr::new("stdlib"))
            .to_string_lossy()
            .to_string()
            + ".s",
    );
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
