use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

use machina::driver::compile::{CompileOptions, compile_with_path};
use machina::driver::native_support::ensure_runtime_archive;
use std::fs;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(crate) fn run_program(name: &str, source: &str) -> Output {
    run_program_with_args(name, source, &[])
}

pub(crate) fn run_program_with_args(name: &str, source: &str, args: &[&str]) -> Output {
    run_program_with_opts(
        name,
        source,
        CompileOptions {
            dump: None,
            emit_ir: false,
            verify_ir: false,
            trace_alloc: false,
            trace_drops: false,
            inject_prelude: true,
            use_stdlib_objects: true,
        },
        args,
    )
}

pub(crate) fn run_program_with_opts(
    name: &str,
    source: &str,
    opts: CompileOptions,
    args: &[&str],
) -> Output {
    let run_id = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_runtime_test_{}_{}_{}",
        name,
        std::process::id(),
        run_id
    ));
    fs::create_dir_all(&temp_dir).expect("failed to create temp dir");

    let source_path = temp_dir.join(format!("{name}.mc"));
    fs::write(&source_path, source).expect("failed to write temp source");

    let output = compile_source_with_opts(&source_path, &opts);

    let asm_path = temp_dir.join(format!("{name}.s"));
    let exe_path = temp_dir.join(name);
    fs::write(&asm_path, output.asm).expect("failed to write asm");

    let runtime_archive = ensure_runtime_archive().expect("failed to build cached runtime archive");
    link_exe(
        &exe_path,
        &asm_path,
        &runtime_archive,
        &output.extra_link_paths,
    );

    let run = Command::new(&exe_path)
        .args(args)
        .output()
        .expect("failed to run executable");
    let _ = fs::remove_dir_all(&temp_dir);
    run
}

pub(crate) fn run_c_program(name: &str, source_path: &Path) -> Output {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let run_id = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_runtime_test_{}_{}_{}",
        name,
        std::process::id(),
        run_id
    ));
    fs::create_dir_all(&temp_dir).expect("failed to create temp dir");

    let exe_path = temp_dir.join(name);
    let runtime_dir = repo_root.join("runtime");
    let runtime_archive = ensure_runtime_archive().expect("failed to build cached runtime archive");

    let status = Command::new("cc")
        .arg("-std=c11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg("-o")
        .arg(&exe_path)
        .arg(source_path)
        .arg(&runtime_archive)
        .status()
        .expect("failed to invoke cc");
    assert!(status.success(), "cc failed with status {status}");

    let run = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");
    let _ = fs::remove_dir_all(&temp_dir);
    run
}

fn compile_source_with_opts(
    source_path: &Path,
    opts: &CompileOptions,
) -> machina::driver::compile::CompileOutput {
    let source = fs::read_to_string(source_path).expect("failed to read temp source");
    compile_with_path(&source, Some(source_path), opts).expect("compile failed")
}

fn link_exe(exe_path: &Path, asm_path: &Path, runtime_archive: &Path, extra_objs: &[PathBuf]) {
    let mut cmd = Command::new("cc");
    cmd.arg("-o").arg(exe_path).arg(asm_path);
    for obj in extra_objs {
        cmd.arg(obj);
    }
    cmd.arg(runtime_archive);
    let status = cmd.status().expect("failed to invoke cc");
    assert!(status.success(), "cc failed with status {status}");
}
