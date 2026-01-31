use crate::context::ParsedContext;
use crate::elaborate::elaborate;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::ssa::lower::lower_func;
use crate::ssa::model::format::formact_func;
use crate::ssa::opt::cfg_free::PassManager as CfgPassManager;
use crate::ssa::opt::dataflow::PassManager;
use crate::typeck::type_check;

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

fn lower_and_optimize_all(source: &str) -> String {
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

    let mut cfg_free = CfgPassManager::new();
    cfg_free.run(std::slice::from_mut(&mut lowered.func));

    let mut dataflow = PassManager::new();
    dataflow.run(std::slice::from_mut(&mut lowered.func));

    formact_func(&lowered.func, &lowered.types)
}

mod t_byref_copy_elim;
mod t_dce;
mod t_local_memcpy_elim;
mod t_memops;
mod t_pipeline;
mod t_stack_temp_copy_elim;
