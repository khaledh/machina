use crate::context::ParsedContext;
use crate::elaborate::elaborate;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::ssa::lower::lower_const_return;
use crate::ssa::model::format::format_function;
use crate::typeck::type_check;
use indoc::indoc;

fn analyze(source: &str) -> crate::context::SemanticContext {
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
    let normalized_context = normalize(type_checked_context);
    let sem_checked_context = sem_check(normalized_context).expect("Failed to semantic check");
    elaborate(sem_checked_context)
}

#[test]
fn test_lower_const_return() {
    let ctx = analyze("fn main() -> u64 { 42 }");
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_const_return(func_def, &ctx.type_map);
    let text = format_function(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 42:u64
            ret %v0
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_binop_return() {
    let ctx = analyze("fn main() -> u64 { 1 + 2 }");
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_const_return(func_def, &ctx.type_map);
    let text = format_function(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = const 2:u64
            %v2: u64 = add %v0, %v1
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_unop_return() {
    let ctx = analyze("fn main() -> u64 { ~1 }");
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_const_return(func_def, &ctx.type_map);
    let text = format_function(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = bitnot %v0
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_unop_not_return() {
    let ctx = analyze("fn main() -> bool { !true }");
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_const_return(func_def, &ctx.type_map);
    let text = format_function(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> bool {
          bb0():
            %v0: bool = const true
            %v1: bool = not %v0
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}
