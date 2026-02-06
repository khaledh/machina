//! Type checker pipeline entrypoint.

mod collect;
mod constraints;
mod engine;
mod errors;
mod finalize;
mod infer_unify;
mod legacy;
mod solve;
pub mod type_map;
mod typesys;
mod unify;
mod validate;

pub use errors::{TypeCheckError, TypeCheckErrorKind};
pub use infer_unify::{Unifier, UnifyError};

use crate::context::{ResolvedContext, TypeCheckedContext};
use crate::typecheck::engine::TypecheckEngine;

pub fn type_check(context: ResolvedContext) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    let primary_result = TypecheckEngine::new(context.clone()).run();
    if strict_mode_enabled() {
        return primary_result;
    }

    // Compatibility fallback path while strict mode is opt-in.
    let fallback_result = legacy::type_check_legacy(context);
    match (primary_result, fallback_result) {
        (_, Err(fallback_errs)) => Err(fallback_errs),
        (_, Ok(fallback_ctx)) => Ok(fallback_ctx),
    }
}

fn strict_mode_enabled() -> bool {
    std::env::var("MACHINA_TYPECHECK_REWRITE_STRICT")
        .ok()
        .is_some_and(|value| parse_bool_like(&value))
}

fn parse_bool_like(value: &str) -> bool {
    matches!(
        value.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on" | "strict"
    )
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
