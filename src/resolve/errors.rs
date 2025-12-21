use thiserror::Error;

use crate::ast::{ExprKind, StructField, TypeExpr};
use crate::diagnostics::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    Var { is_mutable: bool },
    Func,
    TypeAlias { ty_expr: TypeExpr },
    StructDef { fields: Vec<StructField> },
}

impl std::fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolKind::Var { .. } => write!(f, "var"),
            SymbolKind::Func => write!(f, "func"),
            SymbolKind::TypeAlias { ty_expr } => write!(f, "type_alias[{}]", ty_expr),
            SymbolKind::StructDef { fields } => {
                let field_names = fields
                    .iter()
                    .map(|field| field.name.as_str())
                    .collect::<Vec<_>>();
                write!(f, "struct_def[{}]", field_names.join(", "))
            }
        }
    }
}

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
        }
    }
}
