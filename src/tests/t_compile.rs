use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::context::ProgramParsedContext;
use crate::frontend::program::flatten_program_module;
use crate::frontend::{
    FrontendError, ModuleLoader, ModulePath, discover_and_parse_program_with_loader,
};
use crate::tree::parsed::{Expr, ExprKind, MethodBlock, TypeExpr, TypeExprKind, TypeParam};
use crate::tree::visit::{self, Visitor};

struct MockLoader {
    modules: HashMap<String, String>,
}

impl ModuleLoader for MockLoader {
    fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), FrontendError> {
        let key = path.to_string();
        if let Some(src) = self.modules.get(&key) {
            Ok((PathBuf::from(format!("{key}.mc")), src.clone()))
        } else {
            Err(FrontendError::UnknownModule(path.clone()))
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
            if ident == "cfg.Config" {
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
            if bound.name == "rt.Runnable" {
                self.saw_alias_trait_bound = true;
            }
        }
    }

    fn visit_method_block(&mut self, method_block: &MethodBlock) {
        if let Some(trait_name) = &method_block.trait_name {
            if trait_name == "Runnable" {
                self.saw_rewritten_trait_impl_name = true;
            }
            if trait_name == "rt.Runnable" {
                self.saw_alias_trait_impl_name = true;
            }
        }
        visit::walk_method_block(self, method_block);
    }
}

#[test]
fn flatten_program_rewrites_alias_method_call_to_plain_call() {
    let entry_src = r#"
        requires {
            app.util
        }

        fn main() -> u64 {
            util.answer()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "@[public] fn answer() -> u64 { 7 }".to_string(),
    );
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let flattened = flatten_program_module(&ProgramParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_call);
    assert!(!stats.saw_alias_method_call);
}

#[test]
fn flatten_program_rewrites_alias_member_access_to_var() {
    let entry_src = r#"
        requires {
            app.util
        }

        fn main() -> u64 {
            let f = util.answer;
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

    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let flattened = flatten_program_module(&ProgramParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_var);
    assert!(!stats.saw_alias_struct_field);
}

#[test]
fn flatten_program_rewrites_alias_type_reference_to_plain_type_name() {
    let entry_src = r#"
        requires {
            app.config as cfg
        }

        fn use_config(c: cfg.Config) -> cfg.Config {
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

    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let flattened = flatten_program_module(&ProgramParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_type_ref);
    assert!(!stats.saw_alias_type_ref);
}

#[test]
fn flatten_program_rewrites_alias_trait_bound_to_plain_trait_name() {
    let entry_src = r#"
        requires {
            app.runnable as rt
        }

        fn execute<T: rt.Runnable>(value: T) {
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

    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let flattened = flatten_program_module(&ProgramParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_trait_bound);
    assert!(!stats.saw_alias_trait_bound);
}

#[test]
fn flatten_program_rewrites_alias_trait_name_in_method_block() {
    let entry_src = r#"
        requires {
            app.runnable as rt
        }

        type Process = { id: u64 }

        Process :: rt.Runnable {
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

    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let flattened = flatten_program_module(&ProgramParsedContext::new(program))
        .expect("flatten should succeed");
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_trait_impl_name);
    assert!(!stats.saw_alias_trait_impl_name);
}

#[test]
fn flatten_program_reports_unknown_alias_in_type_reference() {
    let entry_src = r#"
        fn use_config(c: cfg.Config) -> cfg.Config {
            c
        }
    "#;
    let loader = MockLoader {
        modules: HashMap::new(),
    };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();
    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let result = flatten_program_module(&ProgramParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(
        errors.iter().any(
            |e| matches!(e, FrontendError::UnknownRequireAlias { alias, .. } if alias == "cfg")
        )
    );
}

#[test]
fn flatten_program_reports_missing_trait_member_on_alias() {
    let entry_src = r#"
        requires {
            app.runnable as rt
        }

        fn execute<T: rt.Missing>(value: T) {
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
    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let result = flatten_program_module(&ProgramParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(errors.iter().any(|e| {
        matches!(
            e,
            FrontendError::RequireMemberUndefined { alias, member, expected_kind, .. }
                if alias == "rt" && member == "Missing" && *expected_kind == "trait"
        )
    }));
}

#[test]
fn flatten_program_reports_private_function_on_alias() {
    let entry_src = r#"
        requires {
            app.util
        }

        fn main() -> u64 {
            util.secret()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "fn secret() -> u64 { 7 }".to_string(),
    );
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();
    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let result = flatten_program_module(&ProgramParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(errors.iter().any(|e| {
        matches!(
            e,
            FrontendError::RequireMemberPrivate { alias, member, expected_kind, .. }
                if alias == "util" && member == "secret" && *expected_kind == "function"
        )
    }));
}

#[test]
fn flatten_program_reports_private_type_on_alias() {
    let entry_src = r#"
        requires {
            app.config as cfg
        }

        fn use_config(c: cfg.Config) -> cfg.Config {
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
    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let result = flatten_program_module(&ProgramParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(errors.iter().any(|e| {
        matches!(
            e,
            FrontendError::RequireMemberPrivate { alias, member, expected_kind, .. }
                if alias == "cfg" && member == "Config" && *expected_kind == "type"
        )
    }));
}

#[test]
fn flatten_program_reports_private_trait_on_alias() {
    let entry_src = r#"
        requires {
            app.runnable as rt
        }

        fn execute<T: rt.Runnable>(value: T) {
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
    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let result = flatten_program_module(&ProgramParsedContext::new(program));
    let errors = result.expect_err("flatten should fail");
    assert!(errors.iter().any(|e| {
        matches!(
            e,
            FrontendError::RequireMemberPrivate { alias, member, expected_kind, .. }
                if alias == "rt" && member == "Runnable" && *expected_kind == "trait"
        )
    }));
}

#[test]
fn flatten_program_mangles_private_dependency_function_names() {
    let entry_src = r#"
        requires {
            app.util
        }

        fn main() -> u64 {
            util.answer()
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
    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let flattened = flatten_program_module(&ProgramParsedContext::new(program))
        .expect("flatten should succeed");

    let mut callable_names = Vec::new();
    for item in &flattened.top_level_items {
        match item {
            crate::tree::parsed::TopLevelItem::FuncDecl(func_decl) => {
                callable_names.push(func_decl.sig.name.clone());
            }
            crate::tree::parsed::TopLevelItem::FuncDef(func_def) => {
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
