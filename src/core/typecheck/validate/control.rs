//! Control-flow-only validation checks.

use crate::core::typecheck::constraints::ControlFact;
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::core::types::Type;

pub(super) fn check_control_facts(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let mut errors = Vec::new();

    for fact in &engine.state().constrain.control_facts {
        match fact {
            ControlFact::Break {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                errors.push(TypeCheckErrorKind::BreakOutsideLoop.at(*span).into())
            }
            ControlFact::Continue {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                errors.push(TypeCheckErrorKind::ContinueOutsideLoop.at(*span).into());
            }
            ControlFact::Return {
                has_value,
                expected_return_ty,
                span,
                ..
            } => match expected_return_ty {
                None => {
                    errors.push(TypeCheckErrorKind::ReturnOutsideFunction.at(*span).into());
                }
                Some(expected_ty) => {
                    let expected_ty = engine.type_vars().apply(expected_ty);
                    if *has_value && expected_ty == Type::Unit {
                        errors.push(TypeCheckErrorKind::ReturnValueUnexpected.at(*span).into());
                    } else if !*has_value && expected_ty != Type::Unit {
                        errors.push(
                            TypeCheckErrorKind::ReturnValueMissing(expected_ty)
                                .at(*span)
                                .into(),
                        );
                    }
                }
            },
            _ => {}
        }
    }

    errors
}
