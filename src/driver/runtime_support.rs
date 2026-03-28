//! Runtime and prelude-support artifact helpers.

use crate::driver::compile::{CompileOptions, compile_with_path};
use crate::driver::support_utils::{
    archive_objects, artifact_is_stale, assemble_object, compile_c_object, native_support_dir,
};

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

pub fn ensure_runtime_archive() -> Result<PathBuf, String> {
    let sources = runtime_source_paths();
    for source in &sources {
        if !source.exists() {
            return Err(format!(
                "runtime source file not found at {}",
                source.display()
            ));
        }
    }

    let build_dir = native_support_dir()?.join("runtime");
    fs::create_dir_all(&build_dir)
        .map_err(|e| format!("failed to create {}: {e}", build_dir.display()))?;

    let archive_path = build_dir.join("libmachina_rt.a");
    if !artifact_is_stale(&archive_path, &sources)? {
        return Ok(archive_path);
    }

    let mut objects = Vec::with_capacity(sources.len());
    for source in &sources {
        let object = build_dir.join(runtime_object_name(source));
        compile_c_object(source, &object)?;
        objects.push(object);
    }
    archive_objects(&archive_path, &objects)?;
    Ok(archive_path)
}

pub fn ensure_prelude_impl_object(
    opts: &CompileOptions,
    ir_dir: Option<&Path>,
) -> Result<PathBuf, String> {
    let source_path = prelude_impl_source_path();
    if !source_path.exists() {
        return Err(format!(
            "prelude support source file not found at {}",
            source_path.display()
        ));
    }

    let build_dir = native_support_dir()?.join("prelude");
    fs::create_dir_all(&build_dir)
        .map_err(|e| format!("failed to create {}: {e}", build_dir.display()))?;

    let object_path = build_dir.join("prelude_impl.o");
    let cached_ir_path = build_dir.join("prelude_impl.ir");
    let rebuild_needed = artifact_is_stale(&object_path, std::slice::from_ref(&source_path))?
        || (opts.emit_ir && !cached_ir_path.exists());

    if rebuild_needed {
        let prelude_src = fs::read_to_string(&source_path)
            .map_err(|e| format!("failed to read {}: {e}", source_path.display()))?;
        let compile_opts = CompileOptions {
            dump: None,
            emit_ir: true,
            verify_ir: opts.verify_ir,
            trace_alloc: opts.trace_alloc,
            trace_drops: opts.trace_drops,
            inject_prelude: true,
            use_stdlib_objects: false,
        };
        let output =
            compile_with_path(&prelude_src, Some(&source_path), &compile_opts).map_err(|errs| {
                let mut message = String::new();
                for err in errs {
                    message.push_str(&format!("{err}\n"));
                }
                message
            })?;

        let asm_path = build_dir.join("prelude_impl.s");
        fs::write(&asm_path, output.asm)
            .map_err(|e| format!("failed to write {}: {e}", asm_path.display()))?;
        assemble_object(&asm_path, &object_path)?;
        let _ = fs::remove_file(&asm_path);

        if let Some(ir) = output.ir.as_ref() {
            fs::write(&cached_ir_path, ir)
                .map_err(|e| format!("failed to write {}: {e}", cached_ir_path.display()))?;
        }
    }

    if opts.emit_ir {
        let Some(ir_dir) = ir_dir else {
            return Err("prelude support IR requested without output directory".to_string());
        };
        let ir_path = ir_dir.join("prelude_impl.ir");
        fs::copy(&cached_ir_path, &ir_path).map_err(|e| {
            format!(
                "failed to write cached prelude support IR to {}: {e}",
                ir_path.display()
            )
        })?;
    }

    Ok(object_path)
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

fn prelude_impl_source_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("std")
        .join("prelude_impl.mc")
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
