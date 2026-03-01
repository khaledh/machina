//! Pass 4 of the type checker: post-solve validation.
//!
//! This pass is intentionally narrow now: it covers checks that still belong
//! to typecheck because they depend on solved typing facts, but do not warrant
//! a heavier semcheck/dataflow pass.

use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::TypeCheckError;
#[cfg(test)]
use crate::core::typecheck::errors::TypeCheckErrorKind;

mod control;
mod stmt;

/// Pass 4: post-solve validation that remains part of typecheck.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let errors = collect_errors(engine);
    if errors.is_empty() {
        Ok(())
    } else {
        engine.state_mut().diags.extend(errors.clone());
        Err(errors)
    }
}

fn collect_errors(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let mut errors = Vec::new();
    errors.extend(control::check_control_facts(engine));
    // Stmt-level rules sit here because they depend on solved types, but do
    // not need the heavier semantic passes downstream.
    errors.extend(stmt::check_stmt_semantics(engine));
    errors
}

#[cfg(test)]
#[path = "../../../tests/typecheck/t_validate.rs"]
mod tests;
