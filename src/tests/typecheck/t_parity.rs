use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::resolve;
use crate::core::typecheck;

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

struct ParityCase<'a> {
    name: &'a str,
    source: &'a str,
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
        },
        ParityCase {
            name: "simple call",
            source: r#"
                fn id(x: u64) -> u64 { x }
                fn test() -> u64 {
                    id(7)
                }
            "#,
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
        },
    ]
}

#[test]
fn test_typecheck_core_cases() {
    for case in parity_cases() {
        let resolved = resolve_source(case.source);
        let checked = typecheck::type_check(resolved)
            .unwrap_or_else(|errs| panic!("[{}] type check failed: {:?}", case.name, errs));
        let body_id = checked.module.func_defs()[0].body.id;
        checked
            .type_map
            .lookup_node_type(body_id)
            .unwrap_or_else(|| panic!("[{}] missing body type", case.name));
    }
}
