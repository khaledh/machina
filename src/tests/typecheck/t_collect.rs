use super::*;
use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::resolve;
use crate::core::typecheck::engine::TypecheckEngine;

fn resolve_source(source: &str) -> crate::core::context::ResolvedContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");
    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();
    let ast_context = ParsedContext::new(module, id_gen);
    resolve(ast_context).expect("Failed to resolve")
}

#[test]
fn test_collect_type_and_function_signatures() {
    let source = r#"
        type Pair<T> = { left: T, right: T }
        type Point = { x: u64, y: u64 }

        fn id<T>(x: T) -> T { x }
        fn add(x: u64, y: u64) -> u64 { x + y }
    "#;

    let resolved = resolve_source(source);
    let mut engine = TypecheckEngine::new(resolved);
    run(&mut engine).expect("collect pass failed");

    let env = engine.env();
    assert!(env.type_symbols.contains_key("Pair"));
    assert!(env.type_symbols.contains_key("Point"));
    assert!(env.type_defs.contains_key("Point"));
    assert!(!env.type_defs.contains_key("Pair"));

    let id_sigs = env.func_sigs.get("id").expect("missing id");
    assert_eq!(id_sigs.len(), 1);
    assert_eq!(id_sigs[0].type_param_count, 1);

    let add_sigs = env.func_sigs.get("add").expect("missing add");
    assert_eq!(add_sigs.len(), 1);
    assert_eq!(add_sigs[0].params.len(), 2);
    assert_eq!(add_sigs[0].ret_ty, Type::uint(64));
}

#[test]
fn test_collect_method_and_property_signatures() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point::{
            fn sum(self) -> u64 {
                self.x + self.y
            }

            prop y_val: u64 {
                get { self.y }
                set(v) { self.y = v; }
            }
        }
    "#;

    let resolved = resolve_source(source);
    let mut engine = TypecheckEngine::new(resolved);
    run(&mut engine).expect("collect pass failed");

    let env = engine.env();
    let point_methods = env.method_sigs.get("Point").expect("missing Point methods");
    assert!(point_methods.contains_key("sum"));

    let props = env
        .property_sigs
        .get("Point")
        .expect("missing Point properties");
    let y_val = props.get("y_val").expect("missing y_val property");
    assert_eq!(y_val.ty, Type::uint(64));
    assert!(y_val.getter.is_some());
    assert!(y_val.setter.is_some());
}

#[test]
fn test_collect_trait_contract_and_trait_impl_methods() {
    let source = r#"
        trait Runnable {
            fn run(self) -> u64;
        }

        type Process = { name: string }

        Process :: Runnable {
            fn run(self) -> u64 {
                1
            }
        }
    "#;

    let resolved = resolve_source(source);
    let mut engine = TypecheckEngine::new(resolved);
    run(&mut engine).expect("collect pass failed");

    let env = engine.env();
    let runnable = env
        .trait_sigs
        .get("Runnable")
        .expect("missing Runnable trait");
    assert!(runnable.methods.contains_key("run"));

    let process_methods = env
        .method_sigs
        .get("Process")
        .expect("missing Process methods");
    let run_methods = process_methods.get("run").expect("missing run overloads");
    assert_eq!(run_methods.len(), 1);
    assert_eq!(run_methods[0].impl_trait.as_deref(), Some("Runnable"));
}
