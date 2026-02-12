use std::collections::HashMap;

use crate::core::capsule::ModuleId;
use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer};
use crate::core::parse::Parser;
use crate::core::resolve::{attach_def_owners, resolve};
use crate::core::tree::NodeIdGen;
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
