//! Pass 4 of the type checker: validate control-flow and protocol semantics.

use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::TypeCheckError;
#[cfg(test)]
use crate::core::typecheck::errors::TypeCheckErrorKind;

mod control;
mod reply_cap;
mod stmt;

/// Pass 4: semantic checks that are not pure type equalities/assignability.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let mut errors = Vec::new();

    errors.extend(control::check_control_facts(engine));
    // Stmt-level rules sit here because they depend on solved types, but
    // do not need the heavier semantic passes downstream.
    errors.extend(stmt::check_stmt_semantics(engine));
    errors.extend(reply_cap::check_reply_cap_usage(engine));

    if errors.is_empty() {
        Ok(())
    } else {
        engine.state_mut().diags.extend(errors.clone());
        Err(errors)
    }
}

#[cfg(test)]
#[path = "../../../tests/typecheck/t_validate.rs"]
mod tests;
