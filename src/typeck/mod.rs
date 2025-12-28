pub mod type_map;

mod checker;
mod errors;
mod overloads;

pub use errors::TypeCheckError;

use crate::context::{ResolvedContext, TypeCheckedContext};
use crate::typeck::checker::TypeChecker;

pub fn type_check(context: ResolvedContext) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    let mut type_checker = TypeChecker::new(context.clone());
    let type_map = type_checker.check()?;
    let type_checked_context = context.with_type_map(type_map);
    Ok(type_checked_context)
}

#[cfg(test)]
#[path = "../tests/t_typeck.rs"]
mod tests;
