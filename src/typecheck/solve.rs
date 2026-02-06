use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::TypeCheckError;

/// Pass 3: solve constraints and obligations.
pub(crate) fn run(_engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    Ok(())
}
