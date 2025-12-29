mod errors;
mod structural;
mod value;

pub use errors::SemCheckError;

use crate::context::TypeCheckedContext;

pub fn sem_check(ctx: &TypeCheckedContext) -> Result<(), Vec<SemCheckError>> {
    let mut errors = Vec::new();

    errors.extend(value::check(ctx));
    errors.extend(structural::check(ctx));

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[cfg(test)]
#[path = "../tests/t_semck.rs"]
mod tests;
