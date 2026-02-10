//! Pass 4 of the type checker: validate control-flow semantics.
//!
//! This pass handles semantic checks that are not pure type equalities or
//! assignability relations (e.g. `break`/`continue` scope and return rules).

use crate::typecheck::constraints::ControlFact;
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::types::Type;

/// Pass 4: semantic checks that are not pure type equalities/assignability.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let mut errors = Vec::new();
    for fact in &engine.state().constrain.control_facts {
        match fact {
            ControlFact::Break {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                errors.push(TypeCheckErrorKind::BreakOutsideLoop(*span).into())
            }
            ControlFact::Continue {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                errors.push(TypeCheckErrorKind::ContinueOutsideLoop(*span).into());
            }
            ControlFact::Return {
                has_value,
                expected_return_ty,
                span,
                ..
            } => match expected_return_ty {
                None => {
                    errors.push(TypeCheckErrorKind::ReturnOutsideFunction(*span).into());
                }
                Some(expected_ty) => {
                    let expected_ty = engine.type_vars().apply(expected_ty);
                    if *has_value && expected_ty == Type::Unit {
                        errors.push(TypeCheckErrorKind::ReturnValueUnexpected(*span).into());
                    } else if !*has_value && expected_ty != Type::Unit {
                        errors.push(
                            TypeCheckErrorKind::ReturnValueMissing(expected_ty, *span).into(),
                        );
                    }
                }
            },
            _ => {}
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        engine.state_mut().diags.extend(errors.clone());
        Err(errors)
    }
}

#[cfg(test)]
#[path = "../tests/typecheck/t_validate.rs"]
mod tests;
