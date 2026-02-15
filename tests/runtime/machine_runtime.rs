use std::path::PathBuf;

use crate::common::run_c_program;

#[test]
fn test_machine_runtime_core() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_basic.c");

    let run = run_c_program("machine_runtime_basic", &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_transactional_commit_rollback() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_txn.c");

    let run = run_c_program("machine_runtime_txn", &source_path);
    assert_eq!(run.status.code(), Some(0));
}
