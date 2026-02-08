use super::*;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::context::{ParsedContext, ProgramParsedContext, ResolvedContext};
use crate::frontend::{
    FrontendError, ModuleLoader, ModulePath, discover_and_parse_program_with_loader,
};
use crate::lexer::{LexError, Lexer, Token};
use crate::parse::Parser;
use crate::resolve::{DefKind, Visibility, resolve, resolve_program};

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

fn resolve_source(source: &str) -> Result<ResolvedContext, Vec<ResolveError>> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
    resolve(ast_context)
}

#[test]
fn test_resolve_program_resolves_dependencies() {
    let entry_src = r#"
        requires {
            app::util
        }

        fn main() -> u64 {
            0
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert("app.util".to_string(), "fn util() -> u64 { 7 }".to_string());
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let resolved = resolve_program(ProgramParsedContext::new(program));
    assert!(resolved.is_ok());

    let resolved = resolved.expect("program resolve should succeed");
    assert_eq!(resolved.modules.len(), 2);
    assert!(resolved.module(resolved.entry).is_some());
    assert!(
        !resolved.top_level_owners.is_empty(),
        "program resolve should retain top-level ownership metadata"
    );
}

#[test]
fn test_resolve_enum_undefined() {
    let source = r#"
        fn main() -> u64 {
            let c = Color::Red;
            0
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            ResolveError::EnumUndefined(name, _) => {
                assert_eq!(name, "Color");
            }
            e => panic!("Expected EnumUndefined, got {:?}", e),
        }
    }
}

#[test]
fn test_resolve_enum_variant_undefined() {
    let source = r#"
        type Color = Red | Green

        fn main() -> u64 {
            let c = Color::Blue;
            0
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            ResolveError::EnumVariantUndefined(enum_name, variant, _) => {
                assert_eq!(enum_name, "Color");
                assert_eq!(variant, "Blue");
            }
            e => panic!("Expected EnumVariantUndefined, got {:?}", e),
        }
    }
}

#[test]
fn test_resolve_function_decl_conflicts_with_def() {
    let source = r#"
        fn foo(x: u64) -> u64;

        fn foo(x: u64) -> u64 {
            x
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, ResolveError::SymbolAlreadyDefined(_, _))),
            "Expected SymbolAlreadyDefined, got {errors:?}"
        );
    }
}

#[test]
fn test_resolve_unknown_attribute() {
    let source = "@[nope] fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::UnknownAttribute(name, _) if name == "nope"
        )));
    }
}

#[test]
fn test_resolve_attr_wrong_args_intrinsic() {
    let source = "@[intrinsic(\"x\")] fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::AttrWrongArgCount(name, 0, 1, _) if name == "intrinsic"
        )));
    }
}

#[test]
fn test_resolve_attr_wrong_args_link_name() {
    let source = "@[link_name] fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::AttrWrongArgCount(name, 1, 0, _) if name == "link_name"
        )));
    }
}

#[test]
fn test_resolve_attr_not_allowed_on_type() {
    let source = "@[link_name(\"x\")] type Foo = {}";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::AttrNotAllowed(name, _, _) if name == "link_name"
        )));
    }
}

#[test]
fn test_resolve_attr_duplicate() {
    let source = "@[intrinsic, intrinsic] fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::AttrDuplicate(name, _) if name == "intrinsic"
        )));
    }
}

#[test]
fn test_resolve_trait_undefined_in_method_block() {
    let source = r#"
        type Process = { name: string }

        Process :: Runnable {
            fn run(self) {
                ()
            }
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::TraitUndefined(name, _) if name == "Runnable"
        )));
    }
}

#[test]
fn test_resolve_expected_trait_in_method_block() {
    let source = r#"
        type Runnable = {}
        type Process = { name: string }

        Process :: Runnable {
            fn run(self) {
                ()
            }
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::ExpectedTrait(name, _, _) if name == "Runnable"
        )));
    }
}

#[test]
fn test_resolve_trait_bound_undefined() {
    let source = r#"
        fn execute<T: Runnable>(value: T) -> u64 {
            0
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::TraitUndefined(name, _) if name == "Runnable"
        )));
    }
}

#[test]
fn test_resolve_trait_bound_expected_trait() {
    let source = r#"
        type Runnable = {}

        fn execute<T: Runnable>(value: T) -> u64 {
            0
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::ExpectedTrait(name, _, _) if name == "Runnable"
        )));
    }
}

#[test]
fn test_resolve_requires_duplicate_alias_default() {
    let source = r#"
        requires {
            std::io
            app::io
        }

        fn main() -> u64 { 0 }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::DuplicateRequireAlias(alias, _) if alias == "io"
        )));
    }
}

#[test]
fn test_resolve_requires_duplicate_alias_explicit() {
    let source = r#"
        requires {
            std::io as net
            app::net as net
        }

        fn main() -> u64 { 0 }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::DuplicateRequireAlias(alias, _) if alias == "net"
        )));
    }
}

#[test]
fn test_resolve_module_qualified_access_reports_specific_error() {
    let source = r#"
        requires {
            std::io
        }

        fn main() -> u64 {
            io::read_file("foo");
            0
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::ModuleQualifiedAccessUnsupported(alias, member, _)
                if alias == "io" && member == "read_file"
        )));
        assert!(!errors.iter().any(|e| matches!(
            e,
            ResolveError::VarUndefined(name, _) if name == "io"
        )));
    }
}

#[test]
fn test_resolve_program_module_member_undefined() {
    let entry_src = r#"
        requires {
            app::util
        }

        fn main() -> u64 {
            util::missing();
            0
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "fn present() -> u64 { 7 }".to_string(),
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

    let resolved = resolve_program(ProgramParsedContext::new(program));
    assert!(resolved.is_err());

    if let Err(errors) = resolved {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::ModuleMemberUndefined(module, member, _)
                if module == "app.util" && member == "missing"
        )));
    }
}

#[test]
fn test_resolve_visibility_and_opacity_attrs_on_types() {
    let source = r#"
        @[public]
        type Config = { host: string }

        @[opaque]
        type Buffer = { data: ^u8[] }
    "#;

    let resolved = resolve_source(source).expect("resolve should succeed");
    let defs: Vec<_> = resolved.def_table.clone().into_iter().collect();

    let config = defs
        .iter()
        .find(|d| d.name == "Config")
        .expect("missing Config");
    match &config.kind {
        DefKind::TypeDef { attrs } => {
            assert_eq!(attrs.visibility, Visibility::Public);
            assert!(!attrs.opaque);
        }
        other => panic!("expected type def for Config, got {other:?}"),
    }

    let buffer = defs
        .iter()
        .find(|d| d.name == "Buffer")
        .expect("missing Buffer");
    match &buffer.kind {
        DefKind::TypeDef { attrs } => {
            assert_eq!(attrs.visibility, Visibility::Public);
            assert!(attrs.opaque);
        }
        other => panic!("expected type def for Buffer, got {other:?}"),
    }
}

#[test]
fn test_resolve_public_attrs_on_trait_and_function() {
    let source = r#"
        @[public]
        trait Runnable {
            fn run(self);
        }

        @[public]
        fn execute() -> u64 {
            0
        }
    "#;

    let resolved = resolve_source(source).expect("resolve should succeed");
    let defs: Vec<_> = resolved.def_table.clone().into_iter().collect();

    let runnable = defs
        .iter()
        .find(|d| d.name == "Runnable")
        .expect("missing Runnable");
    match &runnable.kind {
        DefKind::TraitDef { attrs } => {
            assert_eq!(attrs.visibility, Visibility::Public);
        }
        other => panic!("expected trait def for Runnable, got {other:?}"),
    }

    let execute = defs
        .iter()
        .find(|d| d.name == "execute")
        .expect("missing execute");
    match &execute.kind {
        DefKind::FuncDef { attrs } => {
            assert_eq!(attrs.visibility, Visibility::Public);
        }
        other => panic!("expected func def for execute, got {other:?}"),
    }
}

#[test]
fn test_resolve_opaque_attr_not_allowed_on_function() {
    let source = "@[opaque] fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e,
            ResolveError::AttrNotAllowed(name, where_, _)
                if name == "opaque" && *where_ == "function"
        )));
    }
}
