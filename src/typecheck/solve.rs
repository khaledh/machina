use std::collections::HashMap;

use crate::diag::Span;
use crate::resolve::DefId;
use crate::tree::NodeId;
use crate::typecheck::constraints::{Constraint, ConstraintReason, TyTerm};
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typecheck::unify::{TcUnifier, TcUnifyError};
use crate::types::Type;

#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub(crate) struct SolveOutput {
    pub(crate) resolved_node_types: HashMap<NodeId, Type>,
    pub(crate) resolved_def_types: HashMap<DefId, Type>,
    pub(crate) failed_constraints: usize,
}

/// Pass 3: solve constraints and obligations.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let constrain = engine.state().constrain.clone();
    let vars = std::mem::take(engine.type_vars_mut());
    let mut unifier = TcUnifier::new(vars);

    let mut failed_constraints = 0usize;
    let mut errors = Vec::new();
    for constraint in &constrain.constraints {
        let result = match constraint {
            Constraint::Eq { left, right, .. } => {
                unifier.unify(&term_as_type(left), &term_as_type(right))
            }
            Constraint::Assignable { from, to, .. } => {
                // Assignability is intentionally modeled as equality in the
                // first solver iteration; structural checks move to `validate`.
                unifier.unify(&term_as_type(from), &term_as_type(to))
            }
        };
        if let Err(err) = result {
            failed_constraints += 1;
            errors.push(unify_error_to_diag(err, reason_span(constraint)));
        }
    }

    let mut output = SolveOutput {
        resolved_node_types: HashMap::new(),
        resolved_def_types: HashMap::new(),
        failed_constraints,
    };

    for (node_id, term) in &constrain.node_terms {
        output
            .resolved_node_types
            .insert(*node_id, unifier.apply(&term_as_type(term)));
    }
    for (def_id, term) in &constrain.def_terms {
        output
            .resolved_def_types
            .insert(*def_id, unifier.apply(&term_as_type(term)));
    }

    *engine.type_vars_mut() = unifier.vars().clone();
    engine.state_mut().solve = output;

    if errors.is_empty() {
        Ok(())
    } else {
        engine.state_mut().diags.extend(errors.clone());
        Err(errors)
    }
}

fn term_as_type(term: &TyTerm) -> Type {
    match term {
        TyTerm::Concrete(ty) => ty.clone(),
        TyTerm::Var(var) => Type::Var(*var),
    }
}

fn reason_span(constraint: &Constraint) -> Span {
    let reason = match constraint {
        Constraint::Eq { reason, .. } => reason,
        Constraint::Assignable { reason, .. } => reason,
    };
    match reason {
        ConstraintReason::Expr(_, span)
        | ConstraintReason::Stmt(_, span)
        | ConstraintReason::Pattern(_, span)
        | ConstraintReason::Decl(_, span)
        | ConstraintReason::Return(_, span) => *span,
    }
}

fn unify_error_to_diag(err: TcUnifyError, span: Span) -> TypeCheckError {
    match err {
        TcUnifyError::Mismatch(expected, found) => {
            TypeCheckErrorKind::DeclTypeMismatch(expected, found, span).into()
        }
        TcUnifyError::CannotBindRigid(var, found) => {
            TypeCheckErrorKind::DeclTypeMismatch(Type::Var(var), found, span).into()
        }
        TcUnifyError::OccursCheckFailed(_, _) => TypeCheckErrorKind::UnknownType(span).into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::ParsedContext;
    use crate::lexer::{LexError, Lexer, Token};
    use crate::parse::Parser;
    use crate::resolve::resolve;
    use crate::typecheck::{collect, constraints, engine::TypecheckEngine};

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

    #[test]
    fn test_solver_resolves_basic_local_types() {
        let source = r#"
            fn test() -> u64 {
                let x = 1;
                x
            }
        "#;

        let resolved = resolve_source(source);
        let mut engine = TypecheckEngine::new(resolved);
        collect::run(&mut engine).expect("collect pass failed");
        constraints::run(&mut engine).expect("constrain pass failed");
        run(&mut engine).expect("solve pass failed");

        assert!(!engine.state().solve.resolved_node_types.is_empty());
        assert!(!engine.state().solve.resolved_def_types.is_empty());
        assert_eq!(engine.state().solve.failed_constraints, 0);
        assert!(
            engine
                .state()
                .solve
                .resolved_def_types
                .values()
                .any(|ty| *ty == Type::uint(64))
        );
    }
}
