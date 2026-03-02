use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use super::*;

static CAPSULE_TMP_COUNTER: AtomicU64 = AtomicU64::new(1);

struct MockLoader {
    modules: HashMap<String, String>,
}

impl ModuleLoader for MockLoader {
    fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), CapsuleError> {
        let key = path.to_string();
        if let Some(src) = self.modules.get(&key) {
            Ok((PathBuf::from(format!("{key}.mc")), src.clone()))
        } else {
            Err(CapsuleError::UnknownModule(path.clone()))
        }
    }
}

#[test]
fn discover_capsule_collects_dependencies() {
    let entry_src = r#"
            requires {
                app::util
            }
            fn main() -> u64 { 0 }
        "#;
    let mut modules = HashMap::new();
    modules.insert("app.util".to_string(), "fn util() -> u64 { 1 }".to_string());
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    assert_eq!(program.modules.len(), 2);
    assert_eq!(program.edges.get(&program.entry).map(|v| v.len()), Some(1));
}

#[test]
fn discover_capsule_reports_cycle() {
    let entry_src = r#"
            requires {
                app::a
            }
            fn main() -> u64 { 0 }
        "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.a".to_string(),
        r#"
            requires {
                app::main
            }
            fn a() -> u64 { 1 }
        "#
        .to_string(),
    );
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let err = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect_err("cycle should be rejected");

    assert!(matches!(err, CapsuleError::ModuleDependencyCycle(_)));
}

#[test]
fn dependency_order_lists_deps_before_entry() {
    let entry_src = r#"
            requires {
                app::a
                app::b
            }
            fn main() -> u64 { 0 }
        "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.a".to_string(),
        r#"
            requires {
                app::c
            }
            fn a() -> u64 { 1 }
        "#
        .to_string(),
    );
    modules.insert("app.b".to_string(), "fn b() -> u64 { 2 }".to_string());
    modules.insert("app.c".to_string(), "fn c() -> u64 { 3 }".to_string());
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let order = program.dependency_order_from_entry();
    let pos = |m: &str| {
        let id = *program
            .by_path
            .get(&ModulePath::new(m.split("::").map(|s| s.to_string()).collect()).unwrap())
            .expect("module id should exist");
        order
            .iter()
            .position(|found| *found == id)
            .expect("module should be in order")
    };

    assert!(pos("app::c") < pos("app::a"));
    assert!(pos("app::a") < pos("app::main"));
    assert!(pos("app::b") < pos("app::main"));
}

#[test]
fn discover_capsule_resolves_symbol_import_to_parent_module() {
    let entry_src = r#"
            requires {
                app::util::answer
            }
            fn main() -> u64 { answer() }
        "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "@public fn answer() -> u64 { 7 }".to_string(),
    );
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let entry = program.entry_module();
    assert_eq!(entry.requires.len(), 1);
    let req = &entry.requires[0];
    assert_eq!(
        req.module_path,
        ModulePath::new(vec!["app".to_string(), "util".to_string()]).unwrap()
    );
    assert_eq!(req.member.as_deref(), Some("answer"));
    assert_eq!(req.kind, RequireKind::Symbol);
    assert_eq!(req.alias, "answer");
}

#[test]
fn discover_capsule_rejects_symbol_import_alias_for_now() {
    let entry_src = r#"
            requires {
                app::util::answer as a
            }
            fn main() -> u64 { 0 }
        "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "@public fn answer() -> u64 { 7 }".to_string(),
    );
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let err = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect_err("symbol import aliases should be rejected");
    assert!(matches!(
        err,
        CapsuleError::SymbolImportAliasUnsupported { .. }
    ));
}

#[test]
fn infer_capsule_root_uses_git_workspace_without_cargo() {
    let run_id = CAPSULE_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_root = std::env::temp_dir().join(format!(
        "machina_capsule_root_git_{}_{}",
        std::process::id(),
        run_id
    ));
    let src_dir = temp_root.join("app").join("src");
    fs::create_dir_all(&src_dir).expect("failed to create temp source tree");
    fs::create_dir_all(temp_root.join(".git")).expect("failed to create fake git dir");
    let entry = src_dir.join("main.mc");
    fs::write(&entry, "fn main() -> u64 { 0 }").expect("failed to write entry file");

    let inferred = infer_capsule_root(&entry);
    assert_eq!(inferred, temp_root);

    let _ = fs::remove_dir_all(&temp_root);
}

#[test]
fn infer_capsule_root_prefers_machina_toml_anchor() {
    let run_id = CAPSULE_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_root = std::env::temp_dir().join(format!(
        "machina_capsule_root_anchor_{}_{}",
        std::process::id(),
        run_id
    ));
    let nested = temp_root.join("workspace").join("pkg").join("src");
    fs::create_dir_all(&nested).expect("failed to create temp source tree");
    fs::write(temp_root.join("workspace").join("machina.toml"), "")
        .expect("failed to create capsule root config");
    let entry = nested.join("main.mc");
    fs::write(&entry, "fn main() -> u64 { 0 }").expect("failed to write entry file");

    let inferred = infer_capsule_root(&entry);
    assert_eq!(inferred, temp_root.join("workspace"));

    let _ = fs::remove_dir_all(&temp_root);
}
