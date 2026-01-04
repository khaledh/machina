mod ast_liveness;
mod def_init;
mod errors;
mod lvalue_overlap;
mod move_check;
mod slice_borrow;
mod slice_escape;
mod structural;
mod util;
mod value;

pub use errors::SemCheckError;

use crate::context::{SemanticCheckedContext, TypeCheckedContext};

pub fn sem_check(ctx: TypeCheckedContext) -> Result<SemanticCheckedContext, Vec<SemCheckError>> {
    let mut errors = Vec::new();

    let move_result = move_check::check(&ctx);
    let def_init_result = def_init::check(&ctx);

    errors.extend(value::check(&ctx));
    errors.extend(structural::check(&ctx));
    errors.extend(lvalue_overlap::check(&ctx));
    errors.extend(slice_borrow::check(&ctx));
    errors.extend(def_init_result.errors);
    errors.extend(move_result.errors);
    errors.extend(slice_escape::check(&ctx));

    if errors.is_empty() {
        Ok(ctx.with_sem_results(
            move_result.implicit_moves,
            def_init_result.init_assigns,
            def_init_result.full_init_assigns,
        ))
    } else {
        Err(errors)
    }
}

#[cfg(test)]
#[path = "../tests/t_semck.rs"]
mod tests;

#[cfg(test)]
#[path = "../tests/t_semck_movecheck.rs"]
mod move_tests;
