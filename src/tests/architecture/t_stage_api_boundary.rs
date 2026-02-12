use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug)]
struct Violation {
    file: PathBuf,
    line: usize,
    pattern: &'static str,
    message: &'static str,
}

const SCAN_ROOTS: &[&str] = &["src/driver", "src/services"];

const BANNED_PATTERNS: &[(&str, &str)] = &[
    (
        "core::resolve::resolve_with_imports_and_symbols_partial",
        "use core::api resolve wrappers from orchestration code",
    ),
    (
        "core::resolve::resolve_with_imports_and_symbols",
        "use core::api resolve wrappers from orchestration code",
    ),
    (
        "core::resolve::resolve_with_imports_partial",
        "use core::api resolve wrappers from orchestration code",
    ),
    (
        "core::resolve::resolve_with_imports",
        "use core::api resolve wrappers from orchestration code",
    ),
    (
        "core::resolve::resolve_partial",
        "use core::api resolve wrappers from orchestration code",
    ),
    (
        "core::resolve::resolve",
        "use core::api resolve wrappers from orchestration code",
    ),
    (
        "core::typecheck::type_check_partial_with_imported_facts",
        "use core::api typecheck wrappers from orchestration code",
    ),
    (
        "core::typecheck::type_check_with_imported_facts",
        "use core::api typecheck wrappers from orchestration code",
    ),
    (
        "core::typecheck::type_check_partial",
        "use core::api typecheck wrappers from orchestration code",
    ),
    (
        "core::typecheck::type_check",
        "use core::api typecheck wrappers from orchestration code",
    ),
    (
        "core::normalize::normalize",
        "use core::api normalize wrapper from orchestration code",
    ),
    (
        "core::semck::sem_check_partial",
        "use core::api semcheck wrappers from orchestration code",
    ),
    (
        "core::semck::sem_check",
        "use core::api semcheck wrapper from orchestration code",
    ),
    (
        "core::elaborate::elaborate",
        "use core::api elaborate wrapper from orchestration code",
    ),
];

#[test]
fn orchestration_uses_core_api_stage_entrypoints() {
    let mut violations = Vec::new();
    for root in SCAN_ROOTS {
        let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join(root);
        scan_rs_files(&dir, &mut violations);
    }

    if !violations.is_empty() {
        let mut lines = Vec::new();
        lines.push(
            "stage API boundary violation: orchestration code must call crate::core::api wrappers"
                .to_string(),
        );
        for v in violations {
            lines.push(format!(
                "- {}:{} matched `{}` ({})",
                v.file.display(),
                v.line,
                v.pattern,
                v.message
            ));
        }
        panic!("{}", lines.join("\n"));
    }
}

fn scan_rs_files(dir: &Path, violations: &mut Vec<Violation>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            scan_rs_files(&path, violations);
            continue;
        }
        if path.extension().and_then(|ext| ext.to_str()) != Some("rs") {
            continue;
        }
        scan_file(&path, violations);
    }
}

fn scan_file(path: &Path, violations: &mut Vec<Violation>) {
    let Ok(src) = fs::read_to_string(path) else {
        return;
    };
    for (line_idx, line) in src.lines().enumerate() {
        for (pattern, message) in BANNED_PATTERNS {
            if line.contains(pattern) {
                violations.push(Violation {
                    file: path.to_path_buf(),
                    line: line_idx + 1,
                    pattern,
                    message,
                });
            }
        }
    }
}
