use crate::core::backend::analysis::cfg::Cfg;
use crate::core::backend::lower::lower_func;
use crate::core::context::{ParsedContext, SemanticContext};
use crate::core::elaborate::elaborate;
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
fn test_cfg_if_diamond() {
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
    let blocks = cfg.blocks();
    assert_eq!(blocks.len(), 4);

    let entry = blocks[0];
    let then_bb = blocks[1];
    let else_bb = blocks[2];
    let join_bb = blocks[3];

    assert_eq!(cfg.entry(), entry);
    assert_eq!(cfg.succs(entry), &[then_bb, else_bb]);
    assert_eq!(cfg.succs(then_bb), &[join_bb]);
    assert_eq!(cfg.succs(else_bb), &[join_bb]);
    assert!(cfg.succs(join_bb).is_empty());

    assert_eq!(cfg.preds(then_bb), &[entry]);
    assert_eq!(cfg.preds(else_bb), &[entry]);
    assert_eq!(cfg.preds(join_bb), &[then_bb, else_bb]);

    let rpo = cfg.rpo();
    assert_eq!(rpo, vec![entry, else_bb, then_bb, join_bb]);
}

#[test]
fn test_cfg_while_loop_back_edge() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var i = 0;
            while i < 2 {
                i = i + 1;
            }
            i
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
    let blocks = cfg.blocks();
    assert_eq!(blocks.len(), 4);

    let entry = blocks[0];
    let header = blocks[1];
    let body = blocks[2];
    let exit = blocks[3];

    assert_eq!(cfg.entry(), entry);
    assert_eq!(cfg.succs(entry), &[header]);
    assert_eq!(cfg.succs(header), &[body, exit]);
    assert_eq!(cfg.succs(body), &[header]);
    assert!(cfg.succs(exit).is_empty());

    assert_eq!(cfg.preds(header), &[entry, body]);
    assert_eq!(cfg.preds(body), &[header]);
    assert_eq!(cfg.preds(exit), &[header]);

    let rpo = cfg.rpo();
    assert_eq!(rpo.first().copied(), Some(entry));
    assert_eq!(rpo.len(), blocks.len());
}
