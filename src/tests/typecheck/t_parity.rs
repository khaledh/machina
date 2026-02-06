use crate::context::ParsedContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::{typecheck, typeck};

fn resolve_source(source: &str) -> crate::context::ResolvedContext {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParityExpectation {
    Exact,
}

struct ParityCase<'a> {
    name: &'a str,
    source: &'a str,
    expectation: ParityExpectation,
}

fn parity_cases<'a>() -> Vec<ParityCase<'a>> {
    vec![
        ParityCase {
            name: "basic let + return",
            source: r#"
                fn test() -> u64 {
                    let x = 1;
                    x
                }
            "#,
            expectation: ParityExpectation::Exact,
        },
        ParityCase {
            name: "simple call",
            source: r#"
                fn id(x: u64) -> u64 { x }
                fn test() -> u64 {
                    id(7)
                }
            "#,
            expectation: ParityExpectation::Exact,
        },
        ParityCase {
            name: "method call",
            source: r#"
                type Point = { x: u64, y: u64 }
                Point::{
                    fn sum(self) -> u64 { self.x + self.y }
                }
                fn test() -> u64 {
                    let p = Point { x: 1, y: 2 };
                    p.sum()
                }
            "#,
            expectation: ParityExpectation::Exact,
        },
    ]
}

#[test]
fn test_typecheck_parity_core_cases() {
    for case in parity_cases() {
        let resolved = resolve_source(case.source);
        let legacy = typeck::type_check(resolved.clone());
        let rewrite = typecheck::type_check(resolved);

        match (legacy, rewrite, case.expectation) {
            (Ok(legacy_ctx), Ok(rewrite_ctx), ParityExpectation::Exact) => {
                let legacy_body = legacy_ctx.module.func_defs()[0].body.id;
                let rewrite_body = rewrite_ctx.module.func_defs()[0].body.id;
                assert_eq!(
                    legacy_body, rewrite_body,
                    "[{}] body node id mismatch",
                    case.name
                );

                let legacy_ty = legacy_ctx
                    .type_map
                    .lookup_node_type(legacy_body)
                    .expect("legacy body type missing");
                let rewrite_ty = rewrite_ctx
                    .type_map
                    .lookup_node_type(rewrite_body)
                    .expect("rewrite body type missing");
                assert_eq!(
                    legacy_ty, rewrite_ty,
                    "[{}] body type mismatch: legacy={} rewrite={}",
                    case.name, legacy_ty, rewrite_ty
                );
            }
            (Err(legacy_errs), Err(rewrite_errs), ParityExpectation::Exact) => {
                assert!(
                    !legacy_errs.is_empty(),
                    "[{}] expected legacy diagnostics",
                    case.name
                );
                assert!(
                    !rewrite_errs.is_empty(),
                    "[{}] expected rewrite diagnostics",
                    case.name
                );
            }
            (legacy, rewrite, ParityExpectation::Exact) => {
                panic!(
                    "[{}] parity mismatch: legacy={:?}, rewrite={:?}",
                    case.name,
                    legacy.as_ref().map(|_| "ok").unwrap_or("err"),
                    rewrite.as_ref().map(|_| "ok").unwrap_or("err")
                );
            }
        }
    }
}
