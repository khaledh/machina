#![cfg(target_arch = "aarch64")]

use std::path::PathBuf;
use std::process::Command;

use machina::compile::{CompileOptions, compile};
use machina::targets::TargetKind;

#[test]
fn test_bounds_check_traps_with_message_and_exit_code() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let temp_dir =
        std::env::temp_dir().join(format!("machina_runtime_test_{}", std::process::id()));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");

    let source_path = temp_dir.join("bounds_check.mc");
    let source = r#"
        fn main() -> u64 {
            let arr = [1, 2, 3];
            arr[5]
        }
    "#;
    std::fs::write(&source_path, source).expect("failed to write temp source");
    let source = std::fs::read_to_string(&source_path).expect("failed to read temp source");

    let opts = CompileOptions {
        dump: None,
        target: TargetKind::Arm64,
        emit_mcir: false,
    };
    let output = compile(&source, &opts).expect("compile failed");

    let asm_path = temp_dir.join("bounds_check.s");
    let exe_path = temp_dir.join("bounds_check");
    std::fs::write(&asm_path, output.asm).expect("failed to write asm");

    let runtime_path = repo_root.join("runtime").join("trap.c");

    let status = Command::new("cc")
        .arg("-o")
        .arg(&exe_path)
        .arg(&asm_path)
        .arg(&runtime_path)
        .status()
        .expect("failed to invoke cc");
    assert!(status.success(), "cc failed with status {status}");

    let run = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");
    assert_eq!(run.status.code(), Some(100));

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Machina runtime error: bounds check failed (index=5, len=3)\n",
        "unexpected stderr: {stderr}"
    );

    let _ = std::fs::remove_dir_all(&temp_dir);
}
