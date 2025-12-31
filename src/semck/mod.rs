mod errors;
mod move_check;
mod structural;
mod util;
mod value;

pub use errors::SemCheckError;

use crate::context::TypeCheckedContext;

pub fn sem_check(ctx: &TypeCheckedContext) -> Result<(), Vec<SemCheckError>> {
    let mut errors = Vec::new();

    errors.extend(value::check(ctx));
    errors.extend(structural::check(ctx));
    errors.extend(move_check::check(ctx));

    if errors.is_empty() {
        Ok(())
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
