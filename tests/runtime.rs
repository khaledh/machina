#![cfg(target_arch = "aarch64")]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

use machina::compile::{CompileOptions, compile};
use machina::targets::TargetKind;

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn run_program(name: &str, source: &str) -> Output {
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

    let runtime_sources = runtime_sources(&repo_root);
    link_exe(&exe_path, &asm_path, &runtime_sources);

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
        emit_mcir: false,
    };
    compile(&source, &opts).expect("compile failed")
}

fn runtime_sources(repo_root: &Path) -> Vec<PathBuf> {
    vec![
        repo_root.join("runtime").join("trap.c"),
        repo_root.join("runtime").join("print.c"),
        repo_root.join("runtime").join("conv.c"),
    ]
}

fn link_exe(exe_path: &Path, asm_path: &Path, runtime_sources: &[PathBuf]) {
    let mut cmd = Command::new("cc");
    cmd.arg("-o").arg(exe_path).arg(asm_path);
    for runtime_path in runtime_sources {
        cmd.arg(runtime_path);
    }
    let status = cmd.status().expect("failed to invoke cc");
    assert!(status.success(), "cc failed with status {status}");
}

#[test]
fn test_bounds_check_traps_with_message_and_exit_code() {
    let run = run_program(
        "bounds_check",
        r#"
            fn main() -> u64 {
                let arr = [1, 2, 3];
                arr[5]
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(101));

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Index out of bounds: index=5, len=3\n",
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_div_by_zero_traps_with_message_and_exit_code() {
    let run = run_program(
        "div_by_zero",
        r#"
            fn main() -> u64 {
                let z = 0;
                10 / z
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(100));

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Division by zero\n",
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_range_check_traps_with_message_and_exit_code() {
    let run = run_program(
        "range_check",
        r#"
            type MidRange = range(50, 100);

            fn main() -> u64 {
                let x = 42;
                let y: MidRange = x;
                y
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(102));

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Value out of range: value=42, min(incl)=50, max(excl)=100\n",
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_print_outputs_string() {
    let run = run_program(
        "print_str",
        r#"
            fn main() -> u64 {
                print("hello");
                println();
                0
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "hello\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_print_outputs_u64() {
    let run = run_program(
        "print_u64",
        r#"
            fn main() -> u64 {
                let x = 42;
                print("x=");
                print(x);
                println();
                0
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "x=42\n", "unexpected stdout: {stdout}");
}
