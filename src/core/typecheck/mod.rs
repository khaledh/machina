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

use crate::core::context::{ResolvedContext, TypeCheckedContext};
use crate::core::resolve::ImportedFacts;
use crate::core::typecheck::engine::TypecheckEngine;

pub fn type_check(context: ResolvedContext) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    TypecheckEngine::new(context, ImportedFacts::default()).run()
}

pub fn type_check_partial(context: ResolvedContext) -> TypecheckOutput {
    TypecheckEngine::new(context, ImportedFacts::default()).run_partial()
}

pub fn type_check_with_imported_facts(
    context: ResolvedContext,
    imported_facts: ImportedFacts,
) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    TypecheckEngine::new(context, imported_facts).run()
}

pub fn type_check_partial_with_imported_facts(
    context: ResolvedContext,
    imported_facts: ImportedFacts,
) -> TypecheckOutput {
    TypecheckEngine::new(context, imported_facts).run_partial()
}

#[cfg(test)]
#[path = "../../tests/typecheck/t_parity.rs"]
mod tests_parity;

#[cfg(test)]
#[path = "../../tests/typecheck/t_typeck.rs"]
mod tests_typecheck;

#[cfg(test)]
#[path = "../../tests/typecheck/t_unify.rs"]
mod tests_unify;
