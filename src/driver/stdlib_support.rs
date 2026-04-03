//! Stdlib object/archive helpers for object-backed stdlib modules.

use crate::core::capsule::ModulePath;
use crate::driver::compile::{CompileOptions, compile_with_path};
use crate::driver::project_config::ProjectConfig;
use crate::driver::support_utils::{
    archive_objects, artifact_is_stale, assemble_object, native_support_dir, with_artifact_lock,
};
use crate::driver::target::SelectedTarget;

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
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<Option<StdlibArtifacts>, String> {
    let object_backed_modules = supported_stdlib_object_modules(module_paths);
    if object_backed_modules.is_empty() {
        return Ok(None);
    }

    let archive_dir = native_support_dir()?
        .join(target.kind.as_str())
        .join("stdlib");
    fs::create_dir_all(&archive_dir)
        .map_err(|e| format!("failed to create {}: {e}", archive_dir.display()))?;

    let subset_tag = stdlib_subset_tag(&object_backed_modules);
    let archive_path = archive_dir.join(format!("libmachina_std_{subset_tag}.a"));
    let lock_path = archive_dir.join(format!(".stdlib_{subset_tag}.lock"));
    with_artifact_lock(&lock_path, || {
        let object_path = ensure_flattened_stdlib_object(
            &object_backed_modules,
            &archive_dir,
            &subset_tag,
            target,
            project_config,
        )?;
        if artifact_is_stale(&archive_path, std::slice::from_ref(&object_path))? {
            archive_objects(&archive_path, &[object_path], target, project_config)?;
        }
        Ok(())
    })?;

    Ok(Some(StdlibArtifacts {
        archive_path,
        object_backed_modules,
    }))
}

fn supported_stdlib_object_modules(module_paths: &HashSet<ModulePath>) -> HashSet<ModulePath> {
    let mut modules = module_paths
        .iter()
        .filter(|path| is_object_backed_stdlib_module(path))
        .cloned()
        .collect::<HashSet<_>>();
    modules.insert(ModulePath::new(vec!["std".into(), "builtin".into()]).unwrap());
    modules
}

fn is_object_backed_stdlib_module(module_path: &ModulePath) -> bool {
    matches!(
        module_path.segments(),
        [std_seg, name]
            if std_seg == "std" && (name == "parse" || name == "env" || name == "io")
    )
}

fn ensure_flattened_stdlib_object(
    object_backed_modules: &HashSet<ModulePath>,
    archive_dir: &PathBuf,
    subset_tag: &str,
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<PathBuf, String> {
    let object_path = archive_dir.join(format!("stdlib_{subset_tag}.o"));
    let prelude_path = prelude_source_path();
    let mut sources = object_backed_modules
        .iter()
        .map(stdlib_source_path)
        .collect::<Result<Vec<_>, _>>()?;
    sources.push(prelude_path);
    if let Ok(current_exe) = std::env::current_exe() {
        sources.push(current_exe);
    }
    if !artifact_is_stale(&object_path, &sources)? {
        return Ok(object_path);
    }

    let source = synthetic_stdlib_entry_source(object_backed_modules);
    let source_path = synthetic_stdlib_entry_path();
    let output = compile_with_path(
        &source,
        Some(&source_path),
        &CompileOptions {
            target: target.clone(),
            dump: None,
            emit_ir: false,
            verify_ir: false,
            trace_alloc: false,
            trace_drops: false,
            inject_prelude: true,
            use_stdlib_objects: false,
            project_config: project_config.cloned(),
        },
    )
    .map_err(|errs| {
        errs.into_iter()
            .map(|err| err.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let asm_path = archive_dir.join(format!("stdlib_{subset_tag}.s"));
    fs::write(&asm_path, output.asm)
        .map_err(|e| format!("failed to write {}: {e}", asm_path.display()))?;
    let assemble_result = assemble_object(&asm_path, &object_path, target, project_config);
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

fn stdlib_subset_tag(modules: &HashSet<ModulePath>) -> String {
    let mut module_paths = modules
        .iter()
        .map(|path| path.segments().join("_"))
        .collect::<Vec<_>>();
    module_paths.sort();
    module_paths.join("__")
}

#[cfg(test)]
mod tests {
    use super::{stdlib_subset_tag, supported_stdlib_object_modules};
    use crate::core::capsule::ModulePath;
    use std::collections::HashSet;

    #[test]
    fn supported_stdlib_object_modules_keeps_only_referenced_supported_modules() {
        let mut referenced = HashSet::new();
        referenced.insert(ModulePath::new(vec!["std".into(), "parse".into()]).unwrap());
        referenced.insert(ModulePath::new(vec!["std".into(), "io".into()]).unwrap());
        referenced.insert(ModulePath::new(vec!["app".into(), "util".into()]).unwrap());

        let selected = supported_stdlib_object_modules(&referenced);

        assert_eq!(selected.len(), 3);
        assert!(selected.contains(&ModulePath::new(vec!["std".into(), "builtin".into()]).unwrap()));
        assert!(selected.contains(&ModulePath::new(vec!["std".into(), "parse".into()]).unwrap()));
        assert!(selected.contains(&ModulePath::new(vec!["std".into(), "io".into()]).unwrap()));
    }

    #[test]
    fn stdlib_subset_tag_is_stable_and_subset_specific() {
        let mut modules = HashSet::new();
        modules.insert(ModulePath::new(vec!["std".into(), "builtin".into()]).unwrap());
        modules.insert(ModulePath::new(vec!["std".into(), "io".into()]).unwrap());
        modules.insert(ModulePath::new(vec!["std".into(), "parse".into()]).unwrap());

        assert_eq!(
            stdlib_subset_tag(&modules),
            "std_builtin__std_io__std_parse"
        );
    }
}
