use std::path::PathBuf;

use crate::common::run_c_program;

#[test]
fn test_map_table_runtime_adapter() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("map_table_basic.c");

    let run = run_c_program("map_table_basic", &source_path);
    assert_eq!(run.status.code(), Some(0));
}
