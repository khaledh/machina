//! Control-flow-only validation checks.

use crate::core::diag::Span;
use crate::core::typecheck::constraints::ControlFact;
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::{TEK, TypeCheckError};
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
            } => {
                if let Some(error) =
                    return_fact_error(engine, *has_value, expected_return_ty.as_ref(), *span)
                {
                    errors.push(error);
                }
            }
            _ => {}
        }
    }

    errors
}

fn return_fact_error(
    engine: &TypecheckEngine,
    has_value: bool,
    expected_return_ty: Option<&Type>,
    span: Span,
) -> Option<TypeCheckError> {
    let expected_ty = match expected_return_ty {
        None => return Some(TEK::ReturnOutsideFunction.at(span)),
        Some(expected_ty) => engine.type_vars().apply(expected_ty),
    };

    if has_value && expected_ty == Type::Unit {
        Some(TEK::ReturnValueUnexpected.at(span))
    } else if !has_value && expected_ty != Type::Unit {
        Some(TEK::ReturnValueMissing(expected_ty).at(span))
    } else {
        None
    }
}
