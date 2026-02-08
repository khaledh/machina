use thiserror::Error;

use crate::diag::Span;
use crate::resolve::symbols::SymbolKind;
use crate::tree::parsed::ExprKind;

#[derive(Clone, Debug, Error)]
pub enum ResolveError {
    #[error("Symbol already defined in current scope: {0}")]
    SymbolAlreadyDefined(String, Span),

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

    #[error("Expected '{0}' to be a trait, found {1}")]
    ExpectedTrait(String, SymbolKind, Span),

    #[error("Undefined type: {0}")]
    TypeUndefined(String, Span),

    #[error("Undefined trait: {0}")]
    TraitUndefined(String, Span),

    #[error("Undefined struct: {0}")]
    StructUndefined(String, Span),

    #[error("Undefined enum: {0}")]
    EnumUndefined(String, Span),

    #[error("Undefined enum variant: {0}::{1}")]
    EnumVariantUndefined(String, String, Span),

    #[error("Method declarations are only allowed on intrinsic types: {0}")]
    MethodDeclOnNonIntrinsicType(String, Span),

    #[error("Method declaration '{0}' must be marked @intrinsic")]
    MethodDeclMissingIntrinsic(String, Span),

    #[error("Unknown attribute `{0}`")]
    UnknownAttribute(String, Span),

    #[error("Duplicate attribute `{0}`")]
    AttrDuplicate(String, Span),

    #[error("Attribute `{0}` expects {1} args, found {2}")]
    AttrWrongArgCount(String, usize, usize, Span),

    #[error("Attribute `{0}` has invalid argument type")]
    AttrWrongArgType(String, Span),

    #[error("Attribute `{0}` is not allowed on {1}")]
    AttrNotAllowed(String, &'static str, Span),

    #[error("Duplicate requires alias: {0}")]
    DuplicateRequireAlias(String, Span),

    #[error("Module-qualified access is not implemented yet: {0}.{1}")]
    ModuleQualifiedAccessUnsupported(String, String, Span),
}

impl ResolveError {
    pub fn span(&self) -> Span {
        match self {
            ResolveError::SymbolAlreadyDefined(_, span) => *span,
            ResolveError::VarUndefined(_, span) => *span,
            ResolveError::VarImmutable(_, span) => *span,
            ResolveError::FuncUndefined(_, span) => *span,
            ResolveError::InvalidAssignmentTarget(_, span) => *span,
            ResolveError::InvalidCallee(_, span) => *span,
            ResolveError::ExpectedType(_, _, span) => *span,
            ResolveError::ExpectedTrait(_, _, span) => *span,
            ResolveError::TypeUndefined(_, span) => *span,
            ResolveError::TraitUndefined(_, span) => *span,
            ResolveError::StructUndefined(_, span) => *span,
            ResolveError::EnumUndefined(_, span) => *span,
            ResolveError::EnumVariantUndefined(_, _, span) => *span,
            ResolveError::MethodDeclOnNonIntrinsicType(_, span) => *span,
            ResolveError::MethodDeclMissingIntrinsic(_, span) => *span,
            ResolveError::UnknownAttribute(_, span) => *span,
            ResolveError::AttrDuplicate(_, span) => *span,
            ResolveError::AttrWrongArgCount(_, _, _, span) => *span,
            ResolveError::AttrWrongArgType(_, span) => *span,
            ResolveError::AttrNotAllowed(_, _, span) => *span,
            ResolveError::DuplicateRequireAlias(_, span) => *span,
            ResolveError::ModuleQualifiedAccessUnsupported(_, _, span) => *span,
        }
    }
}
