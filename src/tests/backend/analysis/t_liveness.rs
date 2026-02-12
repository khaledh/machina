use crate::core::analysis::dataflow::DataflowGraph;
use crate::core::backend::analysis::cfg::Cfg;
use crate::core::backend::analysis::liveness::analyze as liveness_analyze;
use crate::core::backend::lower::lower_func;
use crate::core::context::{ParsedContext, SemanticContext};
use crate::core::elaborate::elaborate;
use crate::core::ir::Terminator;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::normalize::normalize;
use crate::core::parse::Parser;
use crate::core::resolve::resolve;
use crate::core::semck::sem_check;
use crate::core::typecheck::type_check;
use indoc::indoc;

fn analyze(source: &str) -> SemanticContext {
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
fn test_liveness_matches_block_count() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            if true { 1 } else { 2 }
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let live_map = liveness_analyze(&lowered.func);
    assert_eq!(live_map.len(), lowered.func.blocks.len());
}

#[test]
fn test_liveness_single_block_empty_sets() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            1
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let live_map = liveness_analyze(&lowered.func);
    assert_eq!(live_map.len(), 1);
    assert!(live_map[0].live_in.is_empty());
    assert!(live_map[0].live_out.is_empty());
}

#[test]
fn test_liveness_branch_args_live_out() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            if true { 1 } else { 2 }
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let cfg = Cfg::new(&lowered.func);
    let live_map = liveness_analyze(&lowered.func);

    let then_block = &lowered.func.blocks[1];
    let else_block = &lowered.func.blocks[2];

    let then_arg = match &then_block.term {
        Terminator::Br { args, .. } => args[0],
        other => panic!("expected then branch to end with br, got {:?}", other),
    };
    let else_arg = match &else_block.term {
        Terminator::Br { args, .. } => args[0],
        other => panic!("expected else branch to end with br, got {:?}", other),
    };

    let then_idx = cfg.index(then_block.id);
    let else_idx = cfg.index(else_block.id);
    assert!(live_map[then_idx].live_out.contains(&then_arg));
    assert!(live_map[else_idx].live_out.contains(&else_arg));
}
