use std::collections::HashSet;

use crate::context::ParsedContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::{SemCheckError, sem_check_partial};
use crate::tree::NodeId;
use crate::typecheck::type_check;

fn normalized_context(source: &str) -> crate::context::NormalizedContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");
    normalize(type_checked_context)
}

#[test]
fn sem_check_partial_skips_when_upstream_is_root_poisoned() {
    let source = r#"
        fn test() -> u64 {
            for i in 5..5 { i; }
            0
        }
    "#;

    let ctx = normalized_context(source);
    let upstream_poisoned = HashSet::from([NodeId(0)]);
    let output = sem_check_partial(ctx, &upstream_poisoned);

    assert!(
        output.errors.is_empty(),
        "expected semck checks to be skipped under root poison"
    );
    assert!(
        output.poisoned_nodes.contains(&NodeId(0)),
        "upstream poison should be preserved"
    );
}

#[test]
fn sem_check_partial_reports_errors_without_upstream_poison() {
    let source = r#"
        fn test() -> u64 {
            for i in 5..5 { i; }
            0
        }
    "#;

    let ctx = normalized_context(source);
    let output = sem_check_partial(ctx, &HashSet::new());

    assert!(!output.errors.is_empty(), "expected semantic errors");
    assert!(
        matches!(output.errors[0], SemCheckError::InvalidRangeBounds(_, _, _)),
        "expected InvalidRangeBounds, got {:?}",
        output.errors
    );
    assert!(
        output.poisoned_nodes.contains(&NodeId(0)),
        "local semck errors should root-poison output"
    );
}
