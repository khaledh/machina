//! Runtime archive artifact helpers.

use crate::driver::project_config::ProjectConfig;
use crate::driver::support_utils::{
    archive_objects, artifact_is_stale, compile_c_object, native_support_dir, with_artifact_lock,
};
use crate::driver::target::SelectedTarget;

use std::fs;
use std::path::{Path, PathBuf};

const RUNTIME_SOURCE_FILES: &[&str] = &[
    "alloc.c",
    "args.c",
    "conv.c",
    "dyn_array.c",
    "hash_table.c",
    "map_table.c",
    "machine/runtime.c",
    "machine/bridge.c",
    "machine/descriptor.c",
    "machine/hosted_instance.c",
    "machine/pending.c",
    "machine/emit.c",
    "mem.c",
    "io.c",
    "print.c",
    "set.c",
    "string.c",
    "trap.c",
];

pub fn runtime_source_paths() -> Vec<PathBuf> {
    let runtime_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("runtime");
    RUNTIME_SOURCE_FILES
        .iter()
        .map(|f| runtime_dir.join(f))
        .collect()
}

pub fn ensure_runtime_archive(
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<PathBuf, String> {
    let sources = runtime_source_paths();
    for source in &sources {
        if !source.exists() {
            return Err(format!(
                "runtime source file not found at {}",
                source.display()
            ));
        }
    }

    let build_dir = native_support_dir()?
        .join(target.kind.as_str())
        .join("runtime");
    fs::create_dir_all(&build_dir)
        .map_err(|e| format!("failed to create {}: {e}", build_dir.display()))?;

    let archive_path = build_dir.join("libmachina_rt.a");
    let lock_path = build_dir.join(".runtime-build.lock");
    with_artifact_lock(&lock_path, || {
        if !artifact_is_stale(&archive_path, &sources)? {
            return Ok(());
        }

        let mut objects = Vec::with_capacity(sources.len());
        for source in &sources {
            let object = build_dir.join(runtime_object_name(source));
            compile_c_object(source, &object, target, project_config)?;
            objects.push(object);
        }
        archive_objects(&archive_path, &objects, target, project_config)?;
        Ok(())
    })?;
    Ok(archive_path)
}

fn runtime_object_name(source: &Path) -> String {
    let relative = source
        .strip_prefix(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("runtime"))
        .unwrap_or(source);
    let mut name = String::new();
    for component in relative.components() {
        let piece = component.as_os_str().to_string_lossy();
        if !name.is_empty() {
            name.push('_');
        }
        name.push_str(piece.trim_end_matches(".c"));
    }
    name.push_str(".o");
    name
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_object_name_flattens_nested_paths() {
        let source = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("runtime")
            .join("machine/runtime.c");
        assert_eq!(runtime_object_name(&source), "machine_runtime.o");
    }
}
