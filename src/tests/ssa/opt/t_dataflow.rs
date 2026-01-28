use crate::context::ParsedContext;
use crate::elaborate::elaborate;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::ssa::lower::lower_func;
use crate::ssa::model::format::formact_func;
use crate::ssa::opt::dataflow::PassManager;
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

fn lower_and_optimize(source: &str) -> String {
    let ctx = analyze(source);
    let func_def = ctx.module.func_defs()[0];
    let mut lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut lowered.func));
    formact_func(&lowered.func, &lowered.types)
}

#[test]
fn test_dce_removes_unused_binop() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> u64 {
            let x = 1 + 2;
            4
        }
    "});

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v3: u64 = const 4:u64
            ret %v3
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_dce_keeps_call() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            side();
            4
        }

        fn side() -> u64 {
            1
        }
    "});

    let main_def = ctx.module.func_defs()[0];
    let side_def = ctx.module.func_defs()[1];
    let side_id = side_def.def_id;

    let mut lowered = lower_func(
        main_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut lowered.func));
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main() -> u64 {{
              bb0():
                %v0: u64 = call @{}()
                %v1: u64 = const 4:u64
                ret %v1
            }}
        "},
        side_id
    );
    assert_eq!(text, expected);
}

#[test]
fn test_memops_to_runtime_calls() {
    let ctx = analyze(indoc! {"
        type Option = None | Some(u64)

        fn main() -> Option {
            Option::Some(42)
        }
    "});

    let func_def = ctx.module.func_defs()[0];
    let mut lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut lowered.func));
    let text = formact_func(&lowered.func, &lowered.types);

    assert!(text.contains("__rt_memcpy"));
}

#[test]
fn test_memops_to_runtime_calls_array() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> () {
            var a: u8[8];
            var b = u8[0; 8];
            a = b;
        }
    "});

    assert!(text.contains("__rt_memcpy"));
}
