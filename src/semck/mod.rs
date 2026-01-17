mod ast_liveness;
pub(crate) mod closure;
mod def_init;
mod errors;
mod liveness_util;
mod lvalue_overlap;
mod match_check;
mod move_check;
mod slice_borrow;
mod slice_escape;
mod structural;
mod value;

pub use errors::SemCheckError;

use crate::context::{NormalizedContext, SemanticCheckedContext};

pub fn sem_check(ctx: NormalizedContext) -> Result<SemanticCheckedContext, Vec<SemCheckError>> {
    let mut errors = Vec::new();

    let move_result = move_check::check(&ctx);
    let def_init_result = def_init::check(&ctx);
    let capture_result = closure::capture::check(&ctx);
    let closure_borrow_errors = closure::borrow::check(&ctx, &capture_result.captures);

    errors.extend(value::check(&ctx));
    errors.extend(structural::check(&ctx));
    errors.extend(lvalue_overlap::check(&ctx));
    errors.extend(slice_borrow::check(&ctx));
    errors.extend(def_init_result.errors);
    errors.extend(capture_result.errors);
    errors.extend(closure_borrow_errors);
    errors.extend(move_result.errors);
    errors.extend(slice_escape::check(&ctx));

    if errors.is_empty() {
        Ok(ctx.with_sem_results(
            move_result.implicit_moves,
            def_init_result.init_assigns,
            def_init_result.full_init_assigns,
            capture_result.captures,
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
