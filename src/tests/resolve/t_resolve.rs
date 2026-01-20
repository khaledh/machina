use super::*;
use crate::context::ParsedContext;
use crate::context::ResolvedContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::parse::Parser;
use crate::resolve::resolve;

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
