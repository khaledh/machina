use std::path::PathBuf;

use crate::common::run_c_program;

#[test]
fn test_alloc_realloc_free() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("alloc_basic.c");

    let run = run_c_program("alloc_basic", &source_path);

    assert_eq!(run.status.code(), Some(0));
}
