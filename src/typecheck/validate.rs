use crate::typecheck::constraints::{ControlFact, TyTerm};
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
                Some(expected_term) => {
                    let expected_ty = resolve_term(expected_term, engine);
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

fn resolve_term(term: &TyTerm, engine: &TypecheckEngine) -> Type {
    match term {
        TyTerm::Concrete(ty) => ty.clone(),
        TyTerm::Var(var) => engine.type_vars().apply(&Type::Var(*var)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::ParsedContext;
    use crate::lexer::{LexError, Lexer, Token};
    use crate::parse::Parser;
    use crate::resolve::resolve;
    use crate::typecheck::{collect, constraints, engine::TypecheckEngine, solve};

    fn resolve_source(source: &str) -> crate::context::ResolvedContext {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .expect("Failed to tokenize");
        let mut parser = Parser::new(&tokens);
        let module = parser.parse().expect("Failed to parse");
        let id_gen = parser.into_id_gen();
        let ast_context = ParsedContext::new(module, id_gen);
        resolve(ast_context).expect("Failed to resolve")
    }

    fn run_validate(source: &str) -> Result<(), Vec<TypeCheckError>> {
        let resolved = resolve_source(source);
        let mut engine = TypecheckEngine::new(resolved);
        collect::run(&mut engine).expect("collect pass failed");
        constraints::run(&mut engine).expect("constrain pass failed");
        solve::run(&mut engine).expect("solve pass failed");
        run(&mut engine)
    }

    #[test]
    fn test_validate_break_outside_loop() {
        let source = r#"
            fn test() -> u64 {
                break;
                0
            }
        "#;

        let result = run_validate(source);
        assert!(result.is_err());
        let errors = result.expect_err("expected error");
        assert!(
            errors
                .iter()
                .any(|err| matches!(err.kind(), TypeCheckErrorKind::BreakOutsideLoop(_)))
        );
    }

    #[test]
    fn test_validate_return_value_unexpected() {
        let source = r#"
            fn test() -> () {
                return 1;
            }
        "#;

        let result = run_validate(source);
        assert!(result.is_err());
        let errors = result.expect_err("expected error");
        assert!(
            errors
                .iter()
                .any(|err| matches!(err.kind(), TypeCheckErrorKind::ReturnValueUnexpected(_)))
        );
    }

    #[test]
    fn test_validate_return_value_missing() {
        let source = r#"
            fn test() -> u64 {
                return;
            }
        "#;

        let result = run_validate(source);
        assert!(result.is_err());
        let errors = result.expect_err("expected error");
        assert!(
            errors
                .iter()
                .any(|err| matches!(err.kind(), TypeCheckErrorKind::ReturnValueMissing(_, _)))
        );
    }
}
