//! Type checker pipeline entrypoint.

mod builtin_methods;
mod capability;
mod collect;
mod constraints;
mod engine;
mod errors;
mod finalize;
mod imported;
mod infer;
pub(crate) mod nominal;
mod nominal_infer;
mod property_access;
mod solver;
pub mod type_map;
pub(crate) mod type_view;
mod typesys;
mod unify;
mod utils;
mod validate;

pub use engine::TypecheckOutput;
pub use errors::{TEK, TypeCheckError, TypeCheckErrorKind};
pub use infer::{InferUnifier, InferUnifyError};

use crate::core::context::{TypecheckStageInput, TypecheckStageOutput};
use crate::core::resolve::ImportedFacts;
use crate::core::typecheck::engine::TypecheckEngine;

macro_rules! tc_push_error {
    ($errors:expr, $span:expr, $kind:expr) => {{
        $errors.push(($kind).at($span));
    }};
}

pub(crate) use tc_push_error;

/// Internal stage entrypoint.
///
/// Prefer `crate::core::api::typecheck_stage` from orchestration code.
pub fn type_check(
    context: TypecheckStageInput,
) -> Result<TypecheckStageOutput, Vec<TypeCheckError>> {
    TypecheckEngine::new(context, ImportedFacts::default()).run()
}

pub fn type_check_partial(context: TypecheckStageInput) -> TypecheckOutput {
    TypecheckEngine::new(context, ImportedFacts::default()).run_partial()
}

pub fn type_check_with_imported_facts(
    context: TypecheckStageInput,
    imported_facts: ImportedFacts,
) -> Result<TypecheckStageOutput, Vec<TypeCheckError>> {
    TypecheckEngine::new(context, imported_facts).run()
}

pub fn type_check_partial_with_imported_facts(
    context: TypecheckStageInput,
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
#[path = "../../tests/typecheck/t_infer_unify.rs"]
mod tests_infer_unify;
