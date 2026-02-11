//! Type checker pipeline entrypoint.

mod builtin_methods;
mod capability;
mod collect;
mod constraints;
mod engine;
mod errors;
mod finalize;
mod infer_unify;
pub(crate) mod nominal;
mod property_access;
mod solver;
pub mod type_map;
pub(crate) mod type_view;
mod typesys;
mod unify;
mod utils;
mod validate;

pub use engine::TypecheckOutput;
pub use errors::{TypeCheckError, TypeCheckErrorKind};
pub use infer_unify::{Unifier, UnifyError};

use crate::context::{ResolvedContext, TypeCheckedContext};
use crate::typecheck::engine::TypecheckEngine;

pub fn type_check(context: ResolvedContext) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    TypecheckEngine::new(context).run()
}

pub fn type_check_partial(context: ResolvedContext) -> TypecheckOutput {
    TypecheckEngine::new(context).run_partial()
}

#[cfg(test)]
#[path = "../tests/typecheck/t_parity.rs"]
mod tests_parity;

#[cfg(test)]
#[path = "../tests/typecheck/t_typeck.rs"]
mod tests_typecheck;

#[cfg(test)]
#[path = "../tests/typecheck/t_unify.rs"]
mod tests_unify;
