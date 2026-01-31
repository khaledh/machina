use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

use machina::compile::{BackendKind, CompileOptions, compile};
use machina::targets::TargetKind;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(crate) fn run_program(name: &str, source: &str) -> Output {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let run_id = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_runtime_test_{}_{}_{}",
        name,
        std::process::id(),
        run_id
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");

    let source_path = temp_dir.join(format!("{name}.mc"));
    std::fs::write(&source_path, source).expect("failed to write temp source");

    let output = compile_source(&source_path);

    let asm_path = temp_dir.join(format!("{name}.s"));
    let exe_path = temp_dir.join(name);
    std::fs::write(&asm_path, output.asm).expect("failed to write asm");

    let prelude_obj = compile_prelude_impl(&repo_root, &temp_dir);
    let runtime_sources = runtime_sources(&repo_root);
    link_exe(&exe_path, &asm_path, &runtime_sources, &[prelude_obj]);

    let run = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");
    let _ = std::fs::remove_dir_all(&temp_dir);
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
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");

    let exe_path = temp_dir.join(name);
    let runtime_dir = repo_root.join("runtime");
    let runtime_sources = runtime_sources(&repo_root);

    let status = Command::new("cc")
        .arg("-std=c11")
        .arg("-I")
        .arg(&runtime_dir)
        .arg("-o")
        .arg(&exe_path)
        .arg(source_path)
        .args(&runtime_sources)
        .status()
        .expect("failed to invoke cc");
    assert!(status.success(), "cc failed with status {status}");

    let run = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");
    let _ = std::fs::remove_dir_all(&temp_dir);
    run
}

fn compile_source(source_path: &Path) -> machina::compile::CompileOutput {
    let source = std::fs::read_to_string(source_path).expect("failed to read temp source");
    let opts = CompileOptions {
        dump: None,
        target: TargetKind::Arm64,
        backend: BackendKind::Ssa,
        emit_ir: false,
        trace_alloc: false,
        inject_prelude: true,
    };
    compile(&source, &opts).expect("compile failed")
}

fn compile_prelude_impl(repo_root: &Path, temp_dir: &Path) -> PathBuf {
    let prelude_path = repo_root.join("stdlib").join("prelude_impl.mc");
    let source = std::fs::read_to_string(&prelude_path).expect("failed to read prelude_impl");
    let opts = CompileOptions {
        dump: None,
        target: TargetKind::Arm64,
        backend: BackendKind::Ssa,
        emit_ir: false,
        trace_alloc: false,
        inject_prelude: false,
    };
    let prelude = compile(&source, &opts).expect("compile failed");

    let asm_path = temp_dir.join("prelude_impl.s");
    let obj_path = temp_dir.join("prelude_impl.o");
    std::fs::write(&asm_path, prelude.asm).expect("failed to write prelude asm");

    let status = Command::new("cc")
        .arg("-c")
        .arg("-o")
        .arg(&obj_path)
        .arg(&asm_path)
        .status()
        .expect("failed to invoke cc");
    assert!(status.success(), "cc failed with status {status}");

    obj_path
}

fn runtime_sources(repo_root: &Path) -> Vec<PathBuf> {
    vec![
        repo_root.join("runtime").join("alloc.c"),
        repo_root.join("runtime").join("conv.c"),
        repo_root.join("runtime").join("mem.c"),
        repo_root.join("runtime").join("print.c"),
        repo_root.join("runtime").join("string.c"),
        repo_root.join("runtime").join("trap.c"),
    ]
}

fn link_exe(exe_path: &Path, asm_path: &Path, runtime_sources: &[PathBuf], extra_objs: &[PathBuf]) {
    let mut cmd = Command::new("cc");
    cmd.arg("-o").arg(exe_path).arg(asm_path);
    for obj in extra_objs {
        cmd.arg(obj);
    }
    for runtime_path in runtime_sources {
        cmd.arg(runtime_path);
    }
    let status = cmd.status().expect("failed to invoke cc");
    assert!(status.success(), "cc failed with status {status}");
}
