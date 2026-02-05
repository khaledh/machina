pub mod type_map;

mod checker;
mod errors;
mod overloads;
mod unify;

pub use errors::{TypeCheckError, TypeCheckErrorKind};
pub use unify::{Unifier, UnifyError};

use crate::context::{ResolvedContext, TypeCheckedContext};
use crate::tree::typed::build_module;
use crate::typeck::checker::TypeChecker;

pub fn type_check(context: ResolvedContext) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    let mut type_checker = TypeChecker::new(context.clone());
    let (type_map, call_sigs, generic_insts) = type_checker.check()?;
    let typed_module = build_module(&type_map, &context.module);
    let type_checked_context =
        context.with_type_map(type_map, call_sigs, generic_insts, typed_module);
    Ok(type_checked_context)
}

#[cfg(test)]
#[path = "../tests/typeck/t_typeck.rs"]
mod tests;

#[cfg(test)]
#[path = "../tests/typeck/t_unify.rs"]
mod tests_unify;
