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
    let rewrite_result = TypecheckEngine::new(context.clone()).run();
    if rewrite_strict_mode() {
        return rewrite_result;
    }

    let legacy_result = crate::typeck::type_check_legacy(context);
    match (rewrite_result, legacy_result) {
        // In compatibility mode, legacy diagnostics are canonical.
        (_, Err(legacy_errs)) => Err(legacy_errs),
        // Legacy succeeded, so return legacy context while rewrite is under development.
        (_, Ok(legacy_ctx)) => Ok(legacy_ctx),
    }
}

fn rewrite_strict_mode() -> bool {
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
