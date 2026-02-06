use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::TypeCheckError;

/// Pass 4: semantic checks that are not pure type equalities/assignability.
pub(crate) fn run(_engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    Ok(())
}
