use crate::context::TypeCheckedContext;
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::TypeCheckError;

/// Pass 5: finalize side tables.
///
/// During migration this pass is intentionally a no-op. `materialize` delegates
/// to the legacy type checker to preserve current behavior.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    if engine.state().diags.is_empty() {
        Ok(())
    } else {
        Err(engine.state().diags.clone())
    }
}

pub(crate) fn materialize(
    engine: TypecheckEngine,
) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    crate::typeck::type_check(engine.context().clone())
}
