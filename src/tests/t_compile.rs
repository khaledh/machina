use std::collections::HashMap;
use std::path::{Path, PathBuf};

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
use crate::core::tree::parsed::{Expr, ExprKind, MethodBlock, TypeExpr, TypeExprKind, TypeParam};
use crate::core::tree::resolved as res;
use crate::core::tree::visit::{self, Visitor};
use crate::core::typecheck::type_check;
use crate::driver::compile::{CompileOptions, compile};

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

impl Visitor<()> for CallRewriteStats {
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
        "@[public] fn answer() -> u64 { 7 }".to_string(),
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
        "@[public] fn answer() -> u64 { 7 }".to_string(),
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
        "@[public] type Config = { port: u64 }".to_string(),
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
        "@[public] trait Runnable { fn run(self); }".to_string(),
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
        "@[public] trait Runnable { fn run(self); }".to_string(),
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
        "@[public] trait Runnable { fn run(self); }".to_string(),
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
        "@[public] fn answer() -> u64 { 7 }".to_string(),
    );
    modules.insert(
        "app.math".to_string(),
        "@[public] fn answer() -> u64 { 11 }".to_string(),
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

    impl Visitor<()> for CalledFnNames {
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
            @[public] fn answer() -> u64 { secret() }
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
            crate::core::tree::parsed::TopLevelItem::FuncDecl(func_decl) => {
                callable_names.push(func_decl.sig.name.clone());
            }
            crate::core::tree::parsed::TopLevelItem::FuncDef(func_def) => {
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
            @[public] fn answer() -> u64 { 7 }
            type Hidden = { x: u64 }
            @[opaque] type Buffer = { data: u64 }
            trait Internal { fn f(self); }
            @[public] trait Runnable { fn run(self); }
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
        "@[public] fn answer() -> u64 { 7 }".to_string(),
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
        if let crate::core::tree::parsed::TopLevelItem::FuncDef(def) = item {
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
        "@[public] fn answer() -> u64 { 7 }".to_string(),
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
        dump: None,
        emit_ir: true,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: true,
        experimental_typestate: false,
    }
}

fn typestate_compile_opts() -> CompileOptions {
    CompileOptions {
        dump: None,
        emit_ir: false,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: false,
        experimental_typestate: true,
    }
}

fn typestate_emit_ir_compile_opts() -> CompileOptions {
    CompileOptions {
        dump: None,
        emit_ir: true,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: false,
        experimental_typestate: true,
    }
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

    let (monomorphized, _stats, plan) =
        monomorphize_with_plan(resolved_context, &type_checked_context.generic_insts)
            .expect("monomorphize with plan");
    let sparse = build_retype_context(&monomorphized, &plan.retype_def_ids);

    let mut saw_id_def = false;
    let mut saw_untouched_decl = false;
    for item in &sparse.module.top_level_items {
        match item {
            res::TopLevelItem::FuncDef(func_def) => {
                let name = sparse
                    .def_table
                    .lookup_def(func_def.def_id)
                    .map(|def| def.name.as_str());
                if name == Some("id") {
                    saw_id_def = true;
                }
            }
            res::TopLevelItem::FuncDecl(func_decl) => {
                let name = sparse
                    .def_table
                    .lookup_def(func_decl.def_id)
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
fn compile_typestate_desugars_before_resolve() {
    let source = r#"
typestate Connection {
    fields {
        addr: string,
        retries: u64,
    }

    fn new(addr: string) -> Disconnected {
        Disconnected { addr: addr, retries: 0 }
    }

    state Disconnected {
        fn connect(fd: u64) -> Connected {
            Connected { addr: self.addr, retries: self.retries, fd: fd }
        }
    }

    state Connected {
        fields {
            fd: u64,
        }

        fn close() -> Disconnected {
            Disconnected { addr: self.addr, retries: self.retries }
        }
    }
}

fn main() -> u64 {
    let c0 = Connection::new("localhost");
    let c1 = c0.connect(7);
    let c2 = c1.close();
    c2.retries
}
"#;

    compile(source, &typestate_compile_opts()).expect("typestate compile should succeed");
}

#[test]
fn compile_typestate_transition_auto_carries_shared_fields() {
    let source = r#"
typestate Connection {
    fields {
        addr: string,
        retries: u64,
    }

    fn new(addr: string) -> Disconnected {
        Disconnected { addr: addr, retries: 0 }
    }

    state Disconnected {
        fn connect() -> Connected {
            Connected { fd: 7 }
        }
    }

    state Connected {
        fields {
            fd: u64,
        }

        fn close() -> Disconnected {
            Disconnected
        }
    }
}

fn main() -> u64 {
    let c0 = Connection::new("localhost");
    let c1 = c0.connect();
    let c2 = c1.close();
    c2.retries
}
"#;

    compile(source, &typestate_compile_opts())
        .expect("typestate carried-field shorthand compile should succeed");
}

#[test]
fn compile_typestate_handler_keeps_machine_runtime_artifacts_in_ir() {
    let source = r#"
type Ping = {}

typestate M {
    fn new() -> S { S {} }

    state S {
        on Ping(e: Ping) -> S {
            e;
            S {}
        }
    }
}

fn main() -> u64 { 0 }
"#;

    let out = compile(source, &typestate_emit_ir_compile_opts())
        .expect("typestate handler compile should succeed");
    let ir = out.ir.expect("emit_ir should provide IR output");
    assert!(
        ir.contains("fn __mc_machine_dispatch_thunk_"),
        "expected managed machine dispatch thunk in emitted IR: {ir}"
    );
    assert!(
        ir.contains("global _g") && ir.contains("bytes [77, 67, 72, 68"),
        "expected managed machine descriptor blob in emitted IR: {ir}"
    );
    assert!(
        ir.contains("__mc_machine_bootstrap"),
        "expected managed machine bootstrap function in emitted IR: {ir}"
    );
    assert!(
        ir.contains("__mc_machine_runtime_register_thunk_u64"),
        "expected managed machine thunk registration call in emitted IR: {ir}"
    );
}
