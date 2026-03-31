use super::*;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::core::capsule::{
    CapsuleError, CapsuleParseOptions, FsModuleLoader, ModuleLoader, ModulePath,
    discover_and_parse_capsule_with_loader, discover_and_parse_capsule_with_loader_and_options,
};
use crate::core::context::{CapsuleParsedContext, ParsedContext, ResolvedContext};
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::{
    DefKind, ResolveError, ResolveErrorKind, Visibility, resolve, resolve_partial, resolve_program,
};

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

fn resolve_source_partial(source: &str) -> ResolveOutput {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
    resolve_partial(ast_context)
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

    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let resolved = resolve_program(CapsuleParsedContext::new(program));
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
fn test_resolve_program_sets_def_table_source_paths() {
    let entry_src = r#"
        requires {
            app::util
        }

        fn main() -> u64 { 0 }
    "#;
    let mut modules = HashMap::new();
    modules.insert("app.util".to_string(), "fn util() -> u64 { 7 }".to_string());
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let resolved = resolve_program(CapsuleParsedContext::new(program))
        .expect("program resolve should succeed");
    let entry_source_path = resolved
        .entry_module()
        .def_table
        .source_path()
        .expect("entry def table should carry source path");
    assert_eq!(entry_source_path, Path::new("app/main.mc"));

    let dep_path = ModulePath::new(vec!["app".to_string(), "util".to_string()]).unwrap();
    let dep_id = *resolved
        .by_path
        .get(&dep_path)
        .expect("dependency module id should exist");
    let dep_source_path = resolved
        .module(dep_id)
        .expect("dependency module should resolve")
        .def_table
        .source_path()
        .expect("dependency def table should carry source path");
    assert_eq!(dep_source_path, Path::new("app.util.mc"));
}

#[test]
fn test_resolve_program_can_import_stdlib_from_interface_metadata() {
    let temp_dir =
        std::env::temp_dir().join(format!("machina_resolve_std_iface_{}", std::process::id()));
    std::fs::create_dir_all(&temp_dir).expect("temp dir should be created");
    let entry_path = temp_dir.join("main.mc");
    let entry_src = r#"
        requires {
            std::io::print
            std::io::IoError
        }

        fn main() -> u64 {
            print("hello");
            0
        }
    "#;
    let loader = FsModuleLoader::new(temp_dir.clone());
    let entry_module_path = ModulePath::new(vec!["main".to_string()]).unwrap();
    let mut program = discover_and_parse_capsule_with_loader_and_options(
        entry_src,
        &entry_path,
        entry_module_path,
        &loader,
        CapsuleParseOptions {
            inject_prelude_requires: false,
        },
    )
    .expect("program should parse");
    let std_io_path = ModulePath::new(vec!["std".to_string(), "io".to_string()]).unwrap();
    let std_io_id = *program
        .by_path
        .get(&std_io_path)
        .expect("stdlib dependency should exist");
    program
        .modules
        .get_mut(&std_io_id)
        .expect("stdlib dependency module should exist")
        .module
        .top_level_items
        .clear();

    let resolved = resolve_program(CapsuleParsedContext::new(program));
    assert!(
        resolved.is_ok(),
        "resolve should succeed via stdlib interface"
    );

    let resolved = resolved.expect("program resolve should succeed");
    let import_env = resolved
        .import_env(resolved.entry)
        .expect("entry import env should exist");
    assert!(import_env.symbol_aliases.contains_key("print"));
    assert!(import_env.symbol_aliases.contains_key("IoError"));

    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[test]
fn test_resolve_def_locations_cover_type_trait_and_method_defs() {
    let source = r#"
        type Point = { x: u64 }

        trait Runnable {
            fn run(self);
        }

        Point :: {
            fn run(self) {}
        }
    "#;
    let resolved = resolve_source(source).expect("resolve should succeed");

    let type_def = resolved
        .def_table
        .defs()
        .iter()
        .find(|def| def.name == "Point" && matches!(def.kind, DefKind::TypeDef { .. }))
        .expect("type def should exist");
    let trait_def = resolved
        .def_table
        .defs()
        .iter()
        .find(|def| def.name == "Runnable" && matches!(def.kind, DefKind::TraitDef { .. }))
        .expect("trait def should exist");
    let method_def = resolved
        .def_table
        .defs()
        .iter()
        .find(|def| def.name == "run" && matches!(def.kind, DefKind::FuncDef { .. }))
        .expect("method def should exist");

    for def in [type_def, trait_def, method_def] {
        let loc = resolved
            .def_table
            .lookup_def_location(def.id)
            .expect("def location should be available");
        assert!(
            loc.span.start.line > 0,
            "expected non-zero line for {}",
            def.name
        );
    }
}

#[test]
fn test_resolve_program_tracks_imported_symbol_origins() {
    let entry_src = r#"
        requires {
            app::dep::run
            app::dep::Config
            app::dep::Runnable
        }

        fn main() -> u64 {
            0
        }
    "#;
    let dep_src = r#"
        @public
        fn run() -> u64 { 1 }

        @public
        type Config = { value: u64 }

        @public
        trait Runnable {}
    "#;
    let mut modules = HashMap::new();
    modules.insert("app.dep".to_string(), dep_src.to_string());
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let resolved = resolve_program(CapsuleParsedContext::new(program))
        .expect("program resolve should succeed");
    let entry_id = resolved.entry;
    let dep_path = ModulePath::new(vec!["app".to_string(), "dep".to_string()]).unwrap();
    let dep_id = *resolved
        .by_path
        .get(&dep_path)
        .expect("dependency module id should exist");
    let dep_module = resolved
        .module(dep_id)
        .expect("dependency module should resolve");

    let run_def_id = dep_module
        .def_table
        .clone()
        .into_iter()
        .find(|def| def.name == "run")
        .expect("run def should exist")
        .id;
    let config_def_id = dep_module
        .def_table
        .clone()
        .into_iter()
        .find(|def| def.name == "Config")
        .expect("Config def should exist")
        .id;
    let runnable_def_id = dep_module
        .def_table
        .clone()
        .into_iter()
        .find(|def| def.name == "Runnable")
        .expect("Runnable def should exist")
        .id;

    let run_binding = resolved
        .imported_symbol_binding(entry_id, "run")
        .expect("run import binding should exist");
    assert_eq!(
        run_binding
            .callables
            .iter()
            .map(|item| item.source)
            .collect::<Vec<_>>(),
        vec![Some(resolved.global_def_id(dep_id, run_def_id))]
    );
    assert!(run_binding.type_def.is_none());
    assert!(run_binding.trait_def.is_none());

    let config_binding = resolved
        .imported_symbol_binding(entry_id, "Config")
        .expect("Config import binding should exist");
    assert_eq!(
        config_binding.type_def.as_ref().map(|item| item.source),
        Some(Some(resolved.global_def_id(dep_id, config_def_id)))
    );
    assert!(config_binding.callables.is_empty());
    assert!(config_binding.trait_def.is_none());

    let trait_binding = resolved
        .imported_symbol_binding(entry_id, "Runnable")
        .expect("Runnable import binding should exist");
    assert_eq!(
        trait_binding.trait_def.as_ref().map(|item| item.source),
        Some(Some(resolved.global_def_id(dep_id, runnable_def_id)))
    );
    assert!(trait_binding.callables.is_empty());
    assert!(trait_binding.type_def.is_none());
}

#[test]
fn test_resolve_program_builds_import_env_from_export_facts() {
    let entry_src = r#"
        requires {
            app::dep as dep
            app::dep::run
        }

        fn main() -> u64 {
            0
        }
    "#;
    let dep_src = r#"
        @public
        fn run() -> u64 { 1 }

        @public
        type Config = { value: u64 }

        type Hidden = { value: u64 }
    "#;
    let mut modules = HashMap::new();
    modules.insert("app.dep".to_string(), dep_src.to_string());
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let resolved = resolve_program(CapsuleParsedContext::new(program))
        .expect("program resolve should succeed");
    let entry_id = resolved.entry;
    let import_env = resolved
        .import_env(entry_id)
        .expect("entry import env exists");

    let dep_alias = import_env
        .module_aliases
        .get("dep")
        .expect("module alias binding should exist");
    assert!(
        dep_alias.exports.callables.contains_key("run"),
        "module export facts should include public function"
    );
    assert!(
        dep_alias.exports.types.contains_key("Config"),
        "module export facts should include public type"
    );
    assert!(
        !dep_alias.exports.types.contains_key("Hidden"),
        "module export facts should exclude private type"
    );

    let symbol_binding = import_env
        .symbol_aliases
        .get("run")
        .expect("symbol alias binding should exist");
    assert!(
        !symbol_binding.callables.is_empty(),
        "symbol binding should include callable target IDs"
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
        match errors[0].kind() {
            ResolveErrorKind::EnumUndefined(name) => {
                assert_eq!(name, "Color");
            }
            e => panic!("Expected EnumUndefined, got {:?}", e),
        }
    }
}

#[test]
fn test_resolve_partial_preserves_context_with_errors() {
    let source = r#"
        fn id(x: u64) -> u64 { x }
        fn main() -> u64 {
            let y = missing;
            id(1)
        }
    "#;

    let output = resolve_source_partial(source);
    assert!(
        !output.errors.is_empty(),
        "expected unresolved-name errors in partial resolve"
    );

    let defs: Vec<_> = output.context.def_table.clone().into_iter().collect();
    assert!(
        defs.iter().any(|d| d.name == "id"),
        "partial resolve should preserve healthy defs"
    );
    assert!(
        defs.iter().any(|d| d.name == "main"),
        "partial resolve should preserve function defs"
    );
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
        match errors[0].kind() {
            ResolveErrorKind::EnumVariantUndefined(enum_name, variant) => {
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
                .any(|e| matches!(e.kind(), ResolveErrorKind::SymbolAlreadyDefined(..))),
            "Expected SymbolAlreadyDefined, got {errors:?}"
        );
    }
}

#[test]
fn test_resolve_allows_same_scope_local_rebinding() {
    let source = r#"
        fn bump(x: u64) -> u64 {
            let x = x + 1;
            let x = x + 1;
            x
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_ok(), "expected rebinding to resolve");
}

#[test]
fn test_resolve_duplicate_names_in_single_bind_pattern_still_error() {
    let source = r#"
        fn main() {
            let (x, x) = (1, 2);
        }
    "#;

    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors.iter().any(
                |e| matches!(e.kind(), ResolveErrorKind::SymbolAlreadyDefined(name) if name == "x")
            ),
            "expected duplicate pattern binding error, got {errors:?}"
        );
    }
}

#[test]
fn test_resolve_unknown_attribute() {
    let source = "@nope fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::UnknownAttribute(name) if name == "nope"
            )
        ));
    }
}

#[test]
fn test_resolve_attr_wrong_args_intrinsic() {
    let source = "@intrinsic(\"x\") fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(e.kind(), ResolveErrorKind::AttrWrongArgCount(name, 0, 1) if name == "intrinsic"
        )));
    }
}

#[test]
fn test_resolve_attr_wrong_args_link_name() {
    let source = "@link_name fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(e.kind(), ResolveErrorKind::AttrWrongArgCount(name, 1, 0) if name == "link_name"
        )));
    }
}

#[test]
fn test_resolve_attr_not_allowed_on_type() {
    let source = "@link_name(\"x\") type Foo = {}";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::AttrNotAllowed(name, _) if name == "link_name"
            )
        ));
    }
}

#[test]
fn test_resolve_attr_duplicate() {
    let source = "@intrinsic @intrinsic fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::AttrDuplicate(name) if name == "intrinsic"
            )
        ));
    }
}

#[test]
fn test_resolve_layout_attr_records_fixed_layout_metadata() {
    let source = r#"
        @layout(fixed, size: 24)
        @align(8)
        type Foo = {
            a: u64,
            b: u64,
            c: u64,
        }
    "#;

    let resolved = resolve_source(source).expect("expected resolve success");
    let type_def = resolved.module.type_defs()[0];
    let def_id = resolved.def_table.def_id(type_def.id);
    let def = resolved
        .def_table
        .lookup_def(def_id)
        .expect("type def should be recorded");
    let DefKind::TypeDef { attrs } = &def.kind else {
        panic!("expected type def attrs");
    };

    assert!(attrs.fixed_layout);
    assert_eq!(attrs.fixed_size, Some(24));
    assert_eq!(attrs.fixed_align, Some(8));
}

#[test]
fn test_resolve_layout_attr_rejects_string_arg_form() {
    let source = r#"@layout("fixed") type Foo = { a: u64 }"#;
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::AttrWrongArgType(name) if name == "layout")
        ));
    }
}

#[test]
fn test_resolve_attr_machines_allowed_on_main() {
    let source = "@machines fn main() {}";
    let result = resolve_source(source);
    assert!(result.is_ok());
}

#[test]
fn test_resolve_attr_machines_is_tolerated_off_main_for_compatibility() {
    let source = "@machines fn foo() {}";
    let result = resolve_source(source);
    assert!(result.is_ok());
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
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::TraitUndefined(name) if name == "Runnable"
            )
        ));
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
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::ExpectedTrait(name, _) if name == "Runnable"
            )
        ));
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
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::TraitUndefined(name) if name == "Runnable"
            )
        ));
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
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::ExpectedTrait(name, _) if name == "Runnable"
            )
        ));
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
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::DuplicateRequireAlias(alias) if alias == "io"
            )
        ));
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
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::DuplicateRequireAlias(alias) if alias == "net"
            )
        ));
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
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::ModuleQualifiedAccessUnsupported(alias, member)
                    if alias == "io" && member == "read_file"
            )
        ));
        assert!(!errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::VarUndefined(name) if name == "io"
            )
        ));
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

    let program = discover_and_parse_capsule_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let resolved = resolve_program(CapsuleParsedContext::new(program));
    assert!(resolved.is_err());

    if let Err(errors) = resolved {
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::ModuleMemberUndefined(module, member)
                    if module == "app.util" && member == "missing"
            )
        ));
    }
}

#[test]
fn test_resolve_visibility_and_opacity_attrs_on_types() {
    let source = r#"
        @public
        type Config = { host: string }

        @opaque
        type Buffer = { data: u8^[] }
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
        @public
        trait Runnable {
            fn run(self);
        }

        @public
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
    let source = "@opaque fn foo() -> u64 { 0 }";
    let result = resolve_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(
            |e| matches!(e.kind(), ResolveErrorKind::AttrNotAllowed(name, where_)
                    if name == "opaque" && *where_ == "function"
            )
        ));
    }
}

#[test]
fn test_resolve_builtin_address_types() {
    let source = r#"
        type BootInfo = {
            mem_base: paddr,
            response: vaddr?,
        }
    "#;

    let resolved = resolve_source(source);
    assert!(resolved.is_ok(), "expected address builtins to resolve");
}

#[test]
fn test_resolve_builtin_nullable_view_type() {
    let source = r#"
        @layout(fixed)
        type Header = {
            magic: u64,
        }

        type BootInfo = {
            response: view<Header>?,
        }
    "#;

    let resolved = resolve_source(source);
    assert!(
        resolved.is_ok(),
        "expected nullable view builtin to resolve"
    );
}
