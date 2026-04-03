use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::backend::TargetKind;
use crate::core::ast::visit::{self, Visitor};
use crate::core::ast::{
    Expr, ExprKind, MethodBlock, TopLevelItem, TypeExpr, TypeExprKind, TypeParam,
};
use crate::core::capsule::bind::CapsuleBindings;
use crate::core::capsule::compose::{flatten_capsule, flatten_capsule_module};
use crate::core::capsule::{
    CapsuleError, ModuleLoader, ModulePath, discover_and_parse_capsule_with_loader,
};
use crate::core::context::{CapsuleParsedContext, ParsedContext};
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::monomorphize::{build_retype_context, monomorphize_with_plan};
use crate::core::parse::Parser;
use crate::core::resolve::resolve;
use crate::core::typecheck::type_check;
use crate::driver::compile::{CompileOptions, compile, compile_with_path};
use crate::driver::native_support::{
    assemble_object, link_executable, native_toolchain_supports_target,
};
use crate::driver::project_config::ProjectConfig;
use crate::driver::target::{SelectedTarget, resolve_target};

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

#[derive(Default)]
struct CallRewriteStats {
    saw_rewritten_call: bool,
    saw_rewritten_var: bool,
    saw_alias_method_call: bool,
    saw_alias_struct_field: bool,
    saw_rewritten_type_ref: bool,
    saw_alias_type_ref: bool,
    saw_rewritten_trait_bound: bool,
    saw_alias_trait_bound: bool,
    saw_rewritten_trait_impl_name: bool,
    saw_alias_trait_impl_name: bool,
}

impl Visitor for CallRewriteStats {
    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Call { callee, .. } = &expr.kind
            && let ExprKind::Var { ident, .. } = &callee.kind
            && ident == "answer"
        {
            self.saw_rewritten_call = true;
        }
        if let ExprKind::Var { ident, .. } = &expr.kind
            && ident == "answer"
        {
            self.saw_rewritten_var = true;
        }
        if let ExprKind::MethodCall { callee, .. } = &expr.kind
            && let ExprKind::Var { ident, .. } = &callee.kind
            && ident == "util"
        {
            self.saw_alias_method_call = true;
        }
        if let ExprKind::StructField { target, .. } = &expr.kind
            && let ExprKind::Var { ident, .. } = &target.kind
            && ident == "util"
        {
            self.saw_alias_struct_field = true;
        }
        visit::walk_expr(self, expr);
    }

    fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
        if let TypeExprKind::Named { ident, .. } = &type_expr.kind {
            if ident == "Config" {
                self.saw_rewritten_type_ref = true;
            }
            if ident == "cfg::Config" {
                self.saw_alias_type_ref = true;
            }
        }
        visit::walk_type_expr(self, type_expr);
    }

    fn visit_type_param(&mut self, param: &TypeParam) {
        if let Some(bound) = &param.bound {
            if bound.name == "Runnable" {
                self.saw_rewritten_trait_bound = true;
            }
            if bound.name == "rt::Runnable" {
                self.saw_alias_trait_bound = true;
            }
        }
    }

    fn visit_method_block(&mut self, method_block: &MethodBlock) {
        if let Some(trait_name) = &method_block.trait_name {
            if trait_name == "Runnable" {
                self.saw_rewritten_trait_impl_name = true;
            }
            if trait_name == "rt::Runnable" {
                self.saw_alias_trait_impl_name = true;
            }
        }
        visit::walk_method_block(self, method_block);
    }
}

#[test]
fn flatten_capsule_rewrites_alias_method_call_to_plain_call() {
    let entry_src = r#"
        requires {
            app::util
        }

        fn main() -> u64 {
            util::answer()
        }
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

    let flattened = flatten_capsule_module(&CapsuleParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_call);
    assert!(!stats.saw_alias_method_call);
}

#[test]
fn flatten_capsule_rewrites_alias_member_access_to_var() {
    let entry_src = r#"
        requires {
            app::util
        }

        fn main() -> u64 {
            let f = util::answer;
            f()
        }
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

    let flattened = flatten_capsule_module(&CapsuleParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_var);
    assert!(!stats.saw_alias_struct_field);
}

#[test]
fn flatten_capsule_rewrites_alias_type_reference_to_plain_type_name() {
    let entry_src = r#"
        requires {
            app::config as cfg
        }

        fn use_config(c: cfg::Config) -> cfg::Config {
            c
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.config".to_string(),
        "@public type Config = { port: u64 }".to_string(),
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

    let flattened = flatten_capsule_module(&CapsuleParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_type_ref);
    assert!(!stats.saw_alias_type_ref);
}

#[test]
fn flatten_capsule_rewrites_alias_trait_bound_to_plain_trait_name() {
    let entry_src = r#"
        requires {
            app::runnable as rt
        }

        fn execute<T: rt::Runnable>(value: T) {
            ()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.runnable".to_string(),
        "@public trait Runnable { fn run(self); }".to_string(),
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

    let flattened = flatten_capsule_module(&CapsuleParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_trait_bound);
    assert!(!stats.saw_alias_trait_bound);
}

#[test]
fn flatten_capsule_rewrites_alias_trait_name_in_method_block() {
    let entry_src = r#"
        requires {
            app::runnable as rt
        }

        type Process = { id: u64 }

        Process :: rt::Runnable {
            fn run(self) {
                ()
            }
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.runnable".to_string(),
        "@public trait Runnable { fn run(self); }".to_string(),
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

    let flattened = flatten_capsule_module(&CapsuleParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_trait_impl_name);
    assert!(!stats.saw_alias_trait_impl_name);
}

#[test]
fn flatten_capsule_reports_unknown_alias_in_type_reference() {
    let entry_src = r#"
        fn use_config(c: cfg::Config) -> cfg::Config {
            c
        }
    "#;
    let loader = MockLoader {
        modules: HashMap::new(),
    };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();
    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let result = flatten_capsule_module(&CapsuleParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(
        errors.iter().any(
            |e| matches!(e, CapsuleError::UnknownRequireAlias { alias, .. } if alias == "cfg")
        )
    );
}

#[test]
fn flatten_capsule_reports_missing_trait_member_on_alias() {
    let entry_src = r#"
        requires {
            app::runnable as rt
        }

        fn execute<T: rt::Missing>(value: T) {
            ()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.runnable".to_string(),
        "@public trait Runnable { fn run(self); }".to_string(),
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

    let result = flatten_capsule_module(&CapsuleParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(errors.iter().any(|e| {
        matches!(
            e,
            CapsuleError::RequireMemberUndefined { alias, member, expected_kind, .. }
                if alias == "rt" && member == "Missing" && *expected_kind == "trait"
        )
    }));
}

#[test]
fn flatten_capsule_reports_private_function_on_alias() {
    let entry_src = r#"
        requires {
            app::util
        }

        fn main() -> u64 {
            util::secret()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "fn secret() -> u64 { 7 }".to_string(),
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

    let result = flatten_capsule_module(&CapsuleParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(errors.iter().any(|e| {
        matches!(
            e,
            CapsuleError::RequireMemberPrivate { alias, member, expected_kind, .. }
                if alias == "util" && member == "secret" && *expected_kind == "function"
        )
    }));
}

#[test]
fn flatten_capsule_reports_private_type_on_alias() {
    let entry_src = r#"
        requires {
            app::config as cfg
        }

        fn use_config(c: cfg::Config) -> cfg::Config {
            c
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.config".to_string(),
        "type Config = { port: u64 }".to_string(),
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

    let result = flatten_capsule_module(&CapsuleParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(errors.iter().any(|e| {
        matches!(
            e,
            CapsuleError::RequireMemberPrivate { alias, member, expected_kind, .. }
                if alias == "cfg" && member == "Config" && *expected_kind == "type"
        )
    }));
}

#[test]
fn flatten_capsule_reports_private_trait_on_alias() {
    let entry_src = r#"
        requires {
            app::runnable as rt
        }

        fn execute<T: rt::Runnable>(value: T) {
            ()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.runnable".to_string(),
        "trait Runnable { fn run(self); }".to_string(),
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

    let result = flatten_capsule_module(&CapsuleParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(errors.iter().any(|e| {
        matches!(
            e,
            CapsuleError::RequireMemberPrivate { alias, member, expected_kind, .. }
                if alias == "rt" && member == "Runnable" && *expected_kind == "trait"
        )
    }));
}

#[test]
fn flatten_capsule_allows_conflicting_public_export_names_via_module_qualification() {
    let entry_src = r#"
        requires {
            app::util as util
            app::math as math
        }

        fn main() -> u64 {
            util::answer() + math::answer()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "@public fn answer() -> u64 { 7 }".to_string(),
    );
    modules.insert(
        "app.math".to_string(),
        "@public fn answer() -> u64 { 11 }".to_string(),
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

    let flattened =
        flatten_capsule_module(&CapsuleParsedContext::new(program)).expect("flatten should pass");

    #[derive(Default)]
    struct CalledFnNames {
        names: Vec<String>,
    }

    impl Visitor for CalledFnNames {
        fn visit_expr(&mut self, expr: &Expr) {
            if let ExprKind::Call { callee, .. } = &expr.kind
                && let ExprKind::Var { ident, .. } = &callee.kind
            {
                self.names.push(ident.clone());
            }
            visit::walk_expr(self, expr);
        }
    }

    let mut called = CalledFnNames::default();
    called.visit_module(&flattened);
    assert!(
        called
            .names
            .iter()
            .any(|name| name.starts_with("__m$app$util$answer"))
    );
    assert!(
        called
            .names
            .iter()
            .any(|name| name.starts_with("__m$app$math$answer"))
    );
}

#[test]
fn flatten_capsule_mangles_private_dependency_function_names() {
    let entry_src = r#"
        requires {
            app::util
        }

        fn main() -> u64 {
            util::answer()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        r#"
            fn secret() -> u64 { 7 }
            @public fn answer() -> u64 { secret() }
        "#
        .to_string(),
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

    let flattened = flatten_capsule_module(&CapsuleParsedContext::new(program))
        .expect("flatten should succeed");

    let mut callable_names = Vec::new();
    for item in &flattened.top_level_items {
        match item {
            TopLevelItem::FuncDecl(func_decl) => {
                callable_names.push(func_decl.sig.name.clone());
            }
            TopLevelItem::FuncDef(func_def) => {
                callable_names.push(func_def.sig.name.clone());
            }
            _ => {}
        }
    }

    assert!(!callable_names.iter().any(|name| name == "secret"));
    assert!(callable_names.iter().any(|name| name == "answer"));
    assert!(
        callable_names
            .iter()
            .any(|name| name.starts_with("__m$app$util$secret")),
        "expected mangled private helper in flattened names: {callable_names:?}"
    );
}

#[test]
fn program_bindings_include_visibility_and_opaque_flags() {
    let entry_src = r#"
        requires {
            app::util as util
        }

        fn main() -> u64 {
            util::answer()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        r#"
            fn secret() -> u64 { 7 }
            @public fn answer() -> u64 { 7 }
            type Hidden = { x: u64 }
            @opaque type Buffer = { data: u64 }
            trait Internal { fn f(self); }
            @public trait Runnable { fn run(self); }
        "#
        .to_string(),
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
    let program_ctx = CapsuleParsedContext::new(program);

    let bindings = CapsuleBindings::build(&program_ctx);
    let aliases = bindings.alias_symbols_for(program_ctx.entry());
    let util = aliases.get("util").expect("util alias should be bound");

    assert!(!util.callables["secret"].public);
    assert!(util.callables["answer"].public);
    assert!(!util.types["Hidden"].public);
    assert!(!util.types["Hidden"].opaque);
    assert!(util.types["Buffer"].public);
    assert!(util.types["Buffer"].opaque);
    assert!(!util.traits["Internal"].public);
    assert!(util.traits["Runnable"].public);
}

#[test]
fn flatten_capsule_tracks_top_level_item_owners() {
    let entry_src = r#"
        requires {
            app::util
        }

        fn main() -> u64 {
            util::answer()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "@public fn answer() -> u64 { 7 }".to_string(),
    );
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();
    let util_path = ModulePath::new(vec!["app".to_string(), "util".to_string()]).unwrap();
    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path.clone(),
        &loader,
    )
    .expect("program should parse");
    let program_ctx = CapsuleParsedContext::new(program);
    let entry_id = *program_ctx
        .capsule
        .by_path
        .get(&entry_path)
        .expect("entry module id should exist");
    let util_id = *program_ctx
        .capsule
        .by_path
        .get(&util_path)
        .expect("util module id should exist");

    let flattened = flatten_capsule(&program_ctx).expect("flatten should succeed");
    let mut owner_by_func = HashMap::new();

    for item in &flattened.module.top_level_items {
        if let TopLevelItem::FuncDef(def) = item {
            let owner = flattened
                .top_level_owners
                .get(&def.id)
                .copied()
                .expect("owner must be recorded for each top-level item");
            owner_by_func.insert(def.sig.name.clone(), owner);
        }
    }

    assert_eq!(owner_by_func.get("answer"), Some(&util_id));
    assert_eq!(owner_by_func.get("main"), Some(&entry_id));
}

#[test]
fn flatten_capsule_accepts_public_symbol_import() {
    let entry_src = r#"
        requires {
            app::util::answer
        }

        fn main() -> u64 {
            answer()
        }
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

    let flattened = flatten_capsule_module(&CapsuleParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_call);
}

#[test]
fn flatten_capsule_rejects_private_symbol_import() {
    let entry_src = r#"
        requires {
            app::util::secret
        }

        fn main() -> u64 {
            secret()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "fn secret() -> u64 { 7 }".to_string(),
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

    let errors =
        flatten_capsule_module(&CapsuleParsedContext::new(program)).expect_err("flatten fails");
    assert!(errors.iter().any(|error| {
        matches!(
            error,
            CapsuleError::RequireMemberPrivate {
                alias,
                expected_kind,
                ..
            } if alias == "secret" && *expected_kind == "symbol"
        )
    }));
}

fn deterministic_compile_opts() -> CompileOptions {
    CompileOptions {
        target: SelectedTarget::builtin(TargetKind::Arm64Macos),
        dump: None,
        emit_ir: true,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: true,
        use_stdlib_objects: true,
        project_config: None,
    }
}

fn deterministic_x86_compile_opts() -> CompileOptions {
    CompileOptions {
        target: SelectedTarget::builtin(TargetKind::X86_64Macos),
        dump: None,
        emit_ir: false,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: true,
        use_stdlib_objects: false,
        project_config: None,
    }
}

fn deterministic_x86_archive_compile_opts() -> CompileOptions {
    CompileOptions {
        target: SelectedTarget::builtin(TargetKind::X86_64Macos),
        dump: None,
        emit_ir: false,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: true,
        use_stdlib_objects: true,
        project_config: None,
    }
}

fn deterministic_x86_linux_archive_compile_opts() -> CompileOptions {
    CompileOptions {
        target: SelectedTarget::builtin(TargetKind::X86_64Linux),
        dump: None,
        emit_ir: false,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: true,
        use_stdlib_objects: true,
        project_config: None,
    }
}

#[test]
fn compile_with_path_links_stdlib_parse_archive_and_suppresses_local_body() {
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_driver_parse_archive_{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let source_path = temp_dir.join("parse_archive.mc");
    let source = r#"
        requires {
            std::parse as parse
            std::parse::ParseError
        }

        fn main() -> u64 | ParseError {
            parse::parse_u64("42")
        }
    "#;
    std::fs::write(&source_path, source).expect("failed to write source");

    let output = compile_with_path(source, Some(&source_path), &deterministic_compile_opts())
        .expect("compile");

    assert_eq!(
        output.extra_link_paths.len(),
        1,
        "expected stdlib archive link path for std::parse"
    );
    assert!(
        output.extra_link_paths[0]
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name.starts_with("libmachina_std_") && name.ends_with(".a")),
        "expected stdlib archive path, got {}",
        output.extra_link_paths[0].display()
    );
    assert!(
        output.asm.contains("_parse_u64"),
        "expected callsite to reference external parse_u64 symbol"
    );
    assert!(
        !output.asm.contains("\n_parse_u64:\n"),
        "std::parse body should come from the cached stdlib archive, not local asm"
    );

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn compile_supports_x86_64_target_for_simple_program() {
    let source = r#"
        fn main() -> u64 {
            42
        }
    "#;

    let output = compile(source, &deterministic_x86_compile_opts())
        .expect("x86-64 compile should succeed for a simple program");
    assert!(output.asm.contains("retq"));
    assert!(output.asm.contains("movabsq $42"));
}

#[test]
fn compile_with_path_links_x86_64_stdlib_env_archive() {
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_driver_x86_env_archive_{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let source_path = temp_dir.join("env_archive_x86.mc");
    let source = r#"
        requires {
            std::env::args
        }

        fn main() -> u64 {
            let argv = args();
            argv.len
        }
    "#;
    std::fs::write(&source_path, source).expect("failed to write source");

    let output = compile_with_path(
        source,
        Some(&source_path),
        &deterministic_x86_archive_compile_opts(),
    )
    .expect("compile");

    assert_eq!(output.extra_link_paths.len(), 1);
    assert!(
        output.extra_link_paths[0]
            .to_string_lossy()
            .contains(&format!("{}stdlib", std::path::MAIN_SEPARATOR)),
        "expected stdlib archive path, got {}",
        output.extra_link_paths[0].display()
    );
    assert!(
        output.extra_link_paths[0]
            .to_string_lossy()
            .contains("x86_64"),
        "expected x86_64 stdlib archive path, got {}",
        output.extra_link_paths[0].display()
    );
    assert!(
        output.asm.contains("_args"),
        "expected callsite to reference external args symbol"
    );
    assert!(
        !output.asm.contains("\n_args:\n"),
        "expected std::env body to come from the x86_64 stdlib archive"
    );

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn compile_with_path_skips_x86_64_linux_stdlib_archive_without_toolchain() {
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_driver_x86_linux_env_inline_{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let source_path = temp_dir.join("env_archive_x86_linux.mc");
    let source = r#"
        requires {
            std::env::args
        }

        fn main() -> u64 {
            let argv = args();
            argv.len
        }
    "#;
    std::fs::write(&source_path, source).expect("failed to write source");

    let output = compile_with_path(
        source,
        Some(&source_path),
        &deterministic_x86_linux_archive_compile_opts(),
    )
    .expect("compile");

    assert!(
        output.extra_link_paths.is_empty(),
        "expected no stdlib archive path without a configured x86-64-linux toolchain"
    );
    assert!(
        output.asm.contains("\nargs:\n"),
        "expected std::env::args body to remain in local asm for x86-64-linux compile"
    );
    assert!(output.asm.contains("retq"));

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn compile_with_path_skips_hosted_entry_wrapper_for_none_platform_targets() {
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_driver_x86_bare_compile_{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    std::fs::write(
        temp_dir.join("machina.toml"),
        r#"
[target.x86-64-bare]
arch = "x86-64"
platform = "none"
"#,
    )
    .expect("failed to write machina.toml");
    let source_path = temp_dir.join("bare_main.mc");
    let source = r#"
        fn main() -> u64 {
            42
        }
    "#;
    std::fs::write(&source_path, source).expect("failed to write source");

    let project_config = ProjectConfig::load_for_root(&temp_dir)
        .expect("load config")
        .expect("present config");
    let target = resolve_target(Some("x86-64-bare"), Some(&project_config))
        .expect("resolve custom bare target");
    let output = compile_with_path(
        source,
        Some(&source_path),
        &CompileOptions {
            target,
            dump: None,
            emit_ir: false,
            verify_ir: false,
            trace_alloc: false,
            trace_drops: false,
            inject_prelude: true,
            use_stdlib_objects: true,
            project_config: Some(project_config),
        },
    )
    .expect("compile bare target");

    assert!(
        output.asm.contains("\nmain:\n"),
        "expected bare target to preserve user main symbol"
    );
    assert!(
        !output.asm.contains("__mc_entry_main_wrapper"),
        "did not expect hosted entry wrapper in bare target asm"
    );
    assert!(
        !output.asm.contains("__mc_user_main"),
        "did not expect hosted user-main rename in bare target asm"
    );
    assert!(
        !output.asm.contains("__rt_process_args_init"),
        "did not expect process-args runtime init in bare target asm"
    );

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn native_support_can_build_x86_64_simple_program() {
    if !native_toolchain_supports_target(TargetKind::X86_64Macos) {
        return;
    }

    let source = r#"
        fn main() -> u64 {
            42
        }
    "#;

    let output = compile(source, &deterministic_x86_compile_opts())
        .expect("x86-64 compile should succeed for a simple program");

    let temp_dir = std::env::temp_dir().join(format!(
        "machina_driver_x86_native_build_{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let asm_path = temp_dir.join("simple_x86.s");
    let obj_path = temp_dir.join("simple_x86.o");
    let exe_path = temp_dir.join("simple_x86");
    std::fs::write(&asm_path, output.asm).expect("failed to write asm");

    assemble_object(
        &asm_path,
        &obj_path,
        &SelectedTarget::builtin(TargetKind::X86_64Macos),
        None,
    )
    .expect("assemble x86_64 object");
    assert!(
        obj_path.exists(),
        "expected assembled object at {}",
        obj_path.display()
    );

    link_executable(
        &asm_path,
        &output.extra_link_paths,
        &exe_path,
        &SelectedTarget::builtin(TargetKind::X86_64Macos),
        None,
    )
    .expect("link x86_64 executable");
    assert!(
        exe_path.exists(),
        "expected linked executable at {}",
        exe_path.display()
    );

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn native_support_can_build_and_run_x86_64_print_program() {
    if !native_toolchain_supports_target(TargetKind::X86_64Macos) {
        return;
    }

    let temp_dir = std::env::temp_dir().join(format!(
        "machina_driver_x86_print_build_{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let source_path = temp_dir.join("print_x86.mc");
    let asm_path = temp_dir.join("print_x86.s");
    let exe_path = temp_dir.join("print_x86");
    let source = r#"
        requires { std::io::println }

        fn main() {
            println("hello from x86-64");
        }
    "#;
    std::fs::write(&source_path, source).expect("failed to write source");

    let output = compile_with_path(
        source,
        Some(&source_path),
        &deterministic_x86_archive_compile_opts(),
    )
    .expect("compile");
    std::fs::write(&asm_path, output.asm).expect("failed to write asm");
    link_executable(
        &asm_path,
        &output.extra_link_paths,
        &exe_path,
        &SelectedTarget::builtin(TargetKind::X86_64Macos),
        None,
    )
    .expect("link x86_64 executable");

    let run = std::process::Command::new(&exe_path)
        .output()
        .expect("failed to run x86_64 executable");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "hello from x86-64\n");

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn compile_with_path_links_stdlib_env_archive_and_suppresses_local_body() {
    let temp_dir =
        std::env::temp_dir().join(format!("machina_driver_env_archive_{}", std::process::id()));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let source_path = temp_dir.join("env_archive.mc");
    let source = r#"
        requires {
            std::env::args
        }

        fn main() -> u64 {
            let argv = args();
            argv.len
        }
    "#;
    std::fs::write(&source_path, source).expect("failed to write source");

    let output = compile_with_path(source, Some(&source_path), &deterministic_compile_opts())
        .expect("compile");

    assert_eq!(
        output.extra_link_paths.len(),
        1,
        "expected stdlib archive link path for std::env"
    );
    assert!(
        output.extra_link_paths[0]
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name.starts_with("libmachina_std_") && name.ends_with(".a")),
        "expected stdlib archive path, got {}",
        output.extra_link_paths[0].display()
    );
    assert!(
        output.asm.contains("_args"),
        "expected callsite to reference external args symbol"
    );
    assert!(
        !output.asm.contains("\n_args:\n"),
        "std::env body should come from the cached stdlib archive, not local asm"
    );

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn compile_with_path_links_stdlib_io_archive_and_suppresses_local_bodies() {
    let temp_dir =
        std::env::temp_dir().join(format!("machina_driver_io_archive_{}", std::process::id()));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let source_path = temp_dir.join("io_archive.mc");
    let output_path = temp_dir.join("out.txt");
    let source = format!(
        r#"
        requires {{
            std::io::open_write
            std::io::IoError
        }}

        fn main() -> () | IoError {{
            let raw = open_write("{output_path}")?;
            let writer = raw.text();
            writer.write_all("hi")?;
            writer.close()
        }}
    "#,
        output_path = output_path.display()
    );
    std::fs::write(&source_path, &source).expect("failed to write source");

    let output = compile_with_path(&source, Some(&source_path), &deterministic_compile_opts())
        .expect("compile");

    assert_eq!(
        output.extra_link_paths.len(),
        1,
        "expected stdlib archive link path for std::io"
    );
    assert!(
        output.extra_link_paths[0]
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name.starts_with("libmachina_std_") && name.ends_with(".a")),
        "expected stdlib archive path, got {}",
        output.extra_link_paths[0].display()
    );
    assert!(
        output.asm.contains("_open_write"),
        "expected callsite to reference external open_write symbol"
    );
    assert!(
        output.asm.contains("_TextWriter$write_all"),
        "expected callsite to reference external TextWriter::write_all symbol"
    );
    assert!(
        !output.asm.contains("\n_open_write:\n"),
        "std::io open_write body should come from the cached stdlib archive, not local asm"
    );
    assert!(
        !output.asm.contains("\n_TextWriter$write_all:\n"),
        "std::io TextWriter::write_all body should come from the cached stdlib archive, not local asm"
    );

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn compile_with_path_links_builtin_archive_for_prelude_string_methods() {
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_driver_builtin_archive_{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let source_path = temp_dir.join("builtin_archive.mc");
    let source = r#"
        fn main() -> () {
            let trimmed = "  hello  ".trim();
            let ok = trimmed.contains("ell");
            if ok { () } else { () }
        }
    "#;
    std::fs::write(&source_path, source).expect("failed to write source");

    let output = compile_with_path(source, Some(&source_path), &deterministic_compile_opts())
        .expect("compile");

    assert_eq!(
        output.extra_link_paths.len(),
        1,
        "expected stdlib archive link path for builtin support"
    );
    assert!(
        output.extra_link_paths[0]
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name.starts_with("libmachina_std_") && name.ends_with(".a")),
        "expected stdlib archive path, got {}",
        output.extra_link_paths[0].display()
    );
    assert!(
        output.asm.contains("_string_trim_impl"),
        "expected callsite to reference external string_trim_impl symbol"
    );
    assert!(
        output.asm.contains("_string_contains_impl"),
        "expected callsite to reference external string_contains_impl symbol"
    );
    assert!(
        !output.asm.contains("\n_string_trim_impl:\n"),
        "builtin string_trim_impl should come from the stdlib archive, not local asm"
    );
    assert!(
        !output.asm.contains("\n_string_contains_impl:\n"),
        "builtin string_contains_impl should come from the stdlib archive, not local asm"
    );

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn compile_with_path_emits_custom_section_for_static_global() {
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_driver_static_section_{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let source_path = temp_dir.join("static_section.mc");
    let source = r#"
        @layout(fixed, size: 24)
        type Header = {
            a: u64,
            b: u64,
            c: u64,
        }

        @section(".limine_requests")
        static var header = Header { a: 1, b: 2, c: 3 };

        fn main() -> u64 {
            header.a + header.b + header.c
        }
    "#;
    std::fs::write(&source_path, source).expect("failed to write source");

    let output = compile_with_path(source, Some(&source_path), &deterministic_compile_opts())
        .expect("compile");

    assert!(
        output.asm.contains(".section .limine_requests"),
        "expected custom section directive in asm:\n{}",
        output.asm
    );

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn compile_ir_dump_is_deterministic() {
    let source = r#"
type IoError = {
    code: u64,
}

fn ok(v: u64) -> u64 | IoError {
    v
}

fn fail() -> u64 | IoError {
    IoError { code: 7 }
}

fn choose(flag: bool, v: u64) -> u64 | IoError {
    if flag { ok(v) } else { fail() }
}

fn add_one(flag: bool, v: u64) -> u64 | IoError {
    let base = choose(flag, v)?;
    base + 1
}

fn main() -> () {
    let _x = add_one(true, 42);
    ()
}
"#;

    let opts = deterministic_compile_opts();
    let first = compile(source, &opts).expect("first compile should succeed");
    let first_ir = first.ir.expect("emit_ir should provide IR output");
    for run in 0..8 {
        let next = compile(source, &opts).expect("repeat compile should succeed");
        let next_ir = next.ir.expect("emit_ir should provide IR output");
        assert_eq!(
            first_ir, next_ir,
            "IR dump changed between deterministic runs (run={run})"
        );
    }
}

#[test]
fn compile_try_can_wrap_propagated_errors_into_return_enum_variant() {
    let source = r#"
type IoError = {
    code: u64,
}

type ParseError = {
    line: u64,
}

type AppError
  = Io(IoError)
  | Parse(ParseError)

fn read_io() -> u64 | IoError {
    IoError { code: 10 }
}

fn read_parse() -> u64 | ParseError {
    ParseError { line: 20 }
}

fn run(flag: bool) -> u64 | AppError {
    if flag {
        read_io()?
    } else {
        read_parse()?
    }
}

fn main() -> () | AppError {
    let _x = run(true)?;
    ()
}
"#;

    let opts = deterministic_compile_opts();
    compile(source, &opts).expect("compile should support wrapped try propagation");
}

#[test]
fn compile_asm_dump_is_deterministic() {
    let source = r#"
fn sum10(a0: u64, a1: u64, a2: u64, a3: u64, a4: u64, a5: u64, a6: u64, a7: u64, a8: u64, a9: u64) -> u64 {
    var acc = 0;
    acc = acc + a0;
    acc = acc + a1;
    acc = acc + a2;
    acc = acc + a3;
    acc = acc + a4;
    acc = acc + a5;
    acc = acc + a6;
    acc = acc + a7;
    acc = acc + a8;
    acc = acc + a9;
    acc
}

fn main() -> u64 {
    let x = sum10(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    x
}
"#;

    let opts = deterministic_compile_opts();
    let first = compile(source, &opts).expect("first compile should succeed");
    for run in 0..8 {
        let next = compile(source, &opts).expect("repeat compile should succeed");
        assert_eq!(
            first.asm, next.asm,
            "ASM dump changed between deterministic runs (run={run})"
        );
    }
}

#[test]
fn retype_context_skips_unchanged_function_bodies_after_monomorphization() {
    let source = r#"
fn id<T>(x: T) -> T { x }

fn uses_generic() -> u64 {
    id(7)
}

fn untouched() -> u64 {
    let a = 1;
    a + 2
}
"#;

    let tokens = Lexer::new(source)
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("tokenize");
    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("parse");
    let ast_context = ParsedContext::new(module, parser.into_id_gen());
    let resolved_context = resolve(ast_context).expect("resolve");
    let type_checked_context = type_check(resolved_context.clone()).expect("typecheck");

    assert!(
        !type_checked_context.generic_insts.is_empty(),
        "fixture should trigger generic instantiation"
    );

    let (monomorphized, _stats, plan) = monomorphize_with_plan(
        resolved_context,
        &type_checked_context.generic_insts,
        &type_checked_context.for_plans,
        &std::collections::HashSet::new(),
        &std::collections::HashMap::new(),
        &std::collections::HashMap::new(),
    )
    .expect("monomorphize with plan");
    let sparse = build_retype_context(&monomorphized, &plan.retype_def_ids);

    let mut saw_id_def = false;
    let mut saw_untouched_decl = false;
    for item in &sparse.module.top_level_items {
        match item {
            TopLevelItem::FuncDef(func_def) => {
                let name = sparse
                    .def_table
                    .lookup_def(sparse.def_table.def_id(func_def.id))
                    .map(|def| def.name.as_str());
                if name == Some("id") {
                    saw_id_def = true;
                }
            }
            TopLevelItem::FuncDecl(func_decl) => {
                let name = sparse
                    .def_table
                    .lookup_def(sparse.def_table.def_id(func_decl.id))
                    .map(|def| def.name.as_str());
                if name == Some("untouched") {
                    saw_untouched_decl = true;
                }
            }
            _ => {}
        }
    }

    assert!(
        saw_id_def,
        "instantiated generic def should remain in second-pass retype scope"
    );
    assert!(
        saw_untouched_decl,
        "unchanged function body should be downgraded to decl and skipped by second pass"
    );
}

#[test]
fn compile_linear_machine_program_injects_runtime_without_machines_attr() {
    let source = r#"
@linear
type Approval = {
    id: u64,

    states {
        Review,
        Approved,
    }

    actions {
        approve: Review -> Approved,
    }

    roles {
        Author { approve }
    }
}

Approval :: {
    fn approve(self) -> Approved {
        Approved {}
    }
}

machine ApprovalService hosts Approval(key: id) {
    fn new() -> Self { Self {} }
}

fn main() -> () | MachineError | SessionError {
    let service = ApprovalService::spawn()?;
    let review = service.create(Approval as Author)?;
    let _approved = review.approve()?;
    ()
}
"#;

    let out = compile(source, &deterministic_compile_opts())
        .expect("linear machine entrypoint should compile without @machines");
    let ir = out.ir.expect("emit_ir should include SSA dump");
    assert!(
        ir.contains("__mc_machine_runtime_managed_bootstrap_u64"),
        "expected managed bootstrap call in linear machine main without @machines"
    );
}
