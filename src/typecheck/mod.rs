//! New type checker pipeline scaffold.
//!
//! This module is a parallel implementation path for a clean-slate type checker.
//! It is intentionally non-default for now and mirrors the legacy public API.

mod collect;
mod constraints;
mod engine;
mod errors;
mod finalize;
mod solve;
mod typesys;
mod unify;
mod validate;

pub use errors::{TypeCheckError, TypeCheckErrorKind};

use crate::context::{ResolvedContext, TypeCheckedContext};
use crate::typecheck::engine::TypecheckEngine;

pub fn type_check(context: ResolvedContext) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    TypecheckEngine::new(context).run()
}
