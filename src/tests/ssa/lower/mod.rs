use crate::context::ParsedContext;
use crate::elaborate::elaborate;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::typeck::type_check;

pub(super) fn analyze(source: &str) -> crate::context::SemanticContext {
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

pub(super) use crate::ssa::lower::lower_func;
pub(super) use crate::ssa::lower::lower_module;
pub(super) use crate::ssa::model::format::formact_func;
pub(super) use indoc::indoc;

#[path = "t_branching.rs"]
mod branching;
#[path = "t_calls.rs"]
mod calls;
#[path = "t_closure.rs"]
mod closure;
#[path = "t_linear.rs"]
mod linear;
#[path = "t_match.rs"]
mod r#match;
#[path = "t_module.rs"]
mod module;
#[path = "t_place.rs"]
mod place;
#[path = "t_types.rs"]
mod types;
