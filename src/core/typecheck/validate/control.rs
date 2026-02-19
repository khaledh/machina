//! Control-flow-only validation checks.

use crate::core::typecheck::constraints::ControlFact;
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::{TypeCheckError, TEK};
use crate::core::types::Type;

pub(super) fn check_control_facts(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let mut errors = Vec::new();

    for fact in &engine.state().constrain.control_facts {
        match fact {
            ControlFact::Break {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                crate::core::typecheck::tc_push_error!(errors, *span, TEK::BreakOutsideLoop)
            }
            ControlFact::Continue {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                crate::core::typecheck::tc_push_error!(errors, *span, TEK::ContinueOutsideLoop);
            }
            ControlFact::Return {
                has_value,
                expected_return_ty,
                span,
                ..
            } => match expected_return_ty {
                None => {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::ReturnOutsideFunction);
                }
                Some(expected_ty) => {
                    let expected_ty = engine.type_vars().apply(expected_ty);
                    if *has_value && expected_ty == Type::Unit {
                        crate::core::typecheck::tc_push_error!(errors, *span, TEK::ReturnValueUnexpected);
                    } else if !*has_value && expected_ty != Type::Unit {
                        crate::core::typecheck::tc_push_error!(errors, *span, TEK::ReturnValueMissing(expected_ty));
                    }
                }
            },
            _ => {}
        }
    }

    errors
}
