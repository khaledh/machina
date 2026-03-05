use std::collections::HashMap;

use crate::core::ast::NodeIdGen;
use crate::core::capsule::{ModuleId, ModulePath};
use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer};
use crate::core::parse::Parser;
use crate::core::resolve::{attach_def_owners, resolve};
use crate::core::symbol_id::SelectedCallable;
use crate::core::typecheck::type_check;
use crate::services::analysis::batch::{query_parse_resolve_typecheck, query_typecheck};
use crate::services::analysis::db::AnalysisDb;
use crate::services::analysis::results::{ResolvedModuleResult, TypeLookup};

fn parsed_context(source: &str) -> ParsedContext {
    let id_gen = NodeIdGen::new();
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<_>, LexError>>()
        .expect("lexing should succeed");
    let mut parser = Parser::new_with_id_gen(&tokens, id_gen);
    let module = parser.parse().expect("parsing should succeed");
    ParsedContext::new(module, parser.into_id_gen())
}

#[test]
fn query_batch_matches_direct_resolve_and_typecheck() {
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 { id(1) }
"#;

    let parsed = parsed_context(source);
    let direct_resolved = attach_def_owners(
        resolve(parsed.clone()).expect("resolve should succeed"),
        &HashMap::new(),
    );
    let direct_typed = type_check(direct_resolved.clone()).expect("typecheck should succeed");

    let mut db = AnalysisDb::new();
    let (query_resolved, query_typed) =
        query_parse_resolve_typecheck(&mut db, ModuleId(0), 1, parsed, HashMap::new())
            .expect("query pipeline should succeed");
    let query_resolved = query_resolved.into_context();
    let query_typed = query_typed.into_context();

    assert_eq!(
        direct_resolved.def_table.to_string(),
        query_resolved.def_table.to_string()
    );

    let call_node = *direct_typed
        .call_sigs
        .keys()
        .next()
        .expect("expected call signature");
    assert_eq!(
        direct_typed.lookup_node_type(call_node),
        query_typed.lookup_node_type(call_node)
    );
}

#[test]
fn query_typecheck_supports_second_pass_context() {
    let source = r#"
fn id<T>(x: T) -> T { x }
fn main() -> u64 { id(1) }
"#;

    let parsed = parsed_context(source);
    let resolved = attach_def_owners(
        resolve(parsed).expect("resolve should succeed"),
        &HashMap::new(),
    );
    let resolved_result = ResolvedModuleResult::from_context(ModuleId(11), resolved);

    let mut db = AnalysisDb::new();
    let typed = query_typecheck(&mut db, ModuleId(11), 3, resolved_result)
        .expect("query typecheck should succeed")
        .into_context();

    let main_call_node = *typed
        .call_sigs
        .keys()
        .next()
        .expect("main call should be present");
    assert!(typed.lookup_node_type(main_call_node).is_some());
}

#[test]
fn query_typecheck_records_canonical_selected_callable_for_local_overload() {
    let source = r#"
fn println(s: string) -> u64 { 1 }
fn println(n: u64) -> u64 { n }
fn main() -> u64 { println("hi") }
"#;

    let parsed = parsed_context(source).with_module_path(
        ModulePath::new(vec!["app".to_string(), "main".to_string()])
            .expect("module path should be valid"),
    );
    let resolved = attach_def_owners(
        resolve(parsed).expect("resolve should succeed"),
        &HashMap::new(),
    );
    let resolved_result = ResolvedModuleResult::from_context(ModuleId(17), resolved);

    let mut db = AnalysisDb::new();
    let typed = query_typecheck(&mut db, ModuleId(17), 5, resolved_result)
        .expect("query typecheck should succeed")
        .into_context();

    let selected = typed
        .call_sigs
        .values()
        .next()
        .and_then(|sig| sig.selected.clone())
        .expect("expected selected callable");

    match selected {
        SelectedCallable::Canonical(symbol_id) => {
            assert_eq!(symbol_id.to_string(), "app::main::println");
        }
        other => panic!("expected canonical selected callable, got {other:?}"),
    }
}
