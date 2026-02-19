//! Pass 4 of the type checker: validate control-flow and protocol semantics.

use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::TypeCheckError;
#[cfg(test)]
use crate::core::typecheck::errors::TypeCheckErrorKind;

mod control;
mod protocol;
mod reply_cap;

/// Pass 4: semantic checks that are not pure type equalities/assignability.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let mut errors = Vec::new();

    errors.extend(control::check_control_facts(engine));
    errors.extend(protocol::check_protocol_shape_conformance(engine));
    errors.extend(protocol::check_typestate_handler_overlap(engine));
    errors.extend(protocol::check_typestate_request_response_shape(engine));
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
