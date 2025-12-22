use thiserror::Error;

use crate::ast::ExprKind;
use crate::diagnostics::Span;
use crate::resolve::symbols::SymbolKind;

#[derive(Clone, Debug, Error)]
pub enum ResolveError {
    #[error("Variable already defined in current scope: {0}")]
    VarAlreadyDefined(String, Span),

    #[error("Undefined variable: {0}")]
    VarUndefined(String, Span),

    #[error("Cannot assign to immutable variable: {0}")]
    VarImmutable(String, Span),

    #[error("Undefined function: {0}")]
    FuncUndefined(String, Span),

    #[error("Invalid assignment target. Expected an l-value, found: {0:?}")]
    InvalidAssignmentTarget(ExprKind, Span),

    #[error("Invalid callee. Expected a function name, found: {0:?}")]
    InvalidCallee(ExprKind, Span),

    #[error("Expected '{0}' to be a type, found {1}")]
    ExpectedType(String, SymbolKind, Span),

    #[error("Undefined type: {0}")]
    TypeUndefined(String, Span),

    #[error("Undefined struct: {0}")]
    StructUndefined(String, Span),

    #[error("Undefined enum: {0}")]
    EnumUndefined(String, Span),

    #[error("Undefined enum variant: {0}::{1}")]
    EnumVariantUndefined(String, String, Span),
}

impl ResolveError {
    pub fn span(&self) -> Span {
        match self {
            ResolveError::VarAlreadyDefined(_, span) => *span,
            ResolveError::VarUndefined(_, span) => *span,
            ResolveError::VarImmutable(_, span) => *span,
            ResolveError::FuncUndefined(_, span) => *span,
            ResolveError::InvalidAssignmentTarget(_, span) => *span,
            ResolveError::InvalidCallee(_, span) => *span,
            ResolveError::ExpectedType(_, _, span) => *span,
            ResolveError::TypeUndefined(_, span) => *span,
            ResolveError::StructUndefined(_, span) => *span,
            ResolveError::EnumUndefined(_, span) => *span,
            ResolveError::EnumVariantUndefined(_, _, span) => *span,
        }
    }
}
