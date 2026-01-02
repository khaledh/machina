mod ast_liveness;
mod errors;
mod move_check;
mod slice_escape;
mod structural;
mod util;
mod value;

pub use errors::SemCheckError;

use crate::context::{SemanticCheckedContext, TypeCheckedContext};

pub fn sem_check(ctx: TypeCheckedContext) -> Result<SemanticCheckedContext, Vec<SemCheckError>> {
    let mut errors = Vec::new();

    let move_result = move_check::check(&ctx);

    errors.extend(value::check(&ctx));
    errors.extend(structural::check(&ctx));
    errors.extend(move_result.errors);
    errors.extend(slice_escape::check(&ctx));

    if errors.is_empty() {
        Ok(ctx.with_implicit_moves(move_result.implicit_moves))
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
