use thiserror::Error;

use crate::ast::ExprKind;
use crate::diag::Span;
use crate::types::Type;

#[derive(Debug, Clone, Error)]
pub enum SemCheckError {
    #[error("Value out of range: {0} not in range [{1},{2})")]
    ValueOutOfRange(u64, u64, u64, Span),

    #[error("Invalid range bounds: {0} >= {1}")]
    InvalidRangeBounds(u64, u64, Span),

    #[error("Division by zero")]
    DivisionByZero(Span),

    #[error("String index only supported on ASCII literals")]
    StringIndexNonAscii(Span),

    #[error("Invalid callee. Expected a function name, found: {0:?}")]
    InvalidCallee(ExprKind, Span),

    #[error("Unknown struct type: {0}")]
    UnknownStructType(String, Span),

    #[error("Duplicate struct field: {0}")]
    DuplicateStructField(String, Span),

    #[error("Unknown struct field: {0}")]
    UnknownStructField(String, Span),

    #[error("Struct fields missing: {0}")]
    StructFieldsMissing(String, Span),

    #[error("Unknown enum type: {0}")]
    UnknownEnumType(String, Span),

    #[error("Unknown enum variant: {0}::{1}")]
    UnknownEnumVariant(String, String, Span),

    #[error("Enum variant payload arity mismatch: variant {0} expected {1} elements, found {2}")]
    EnumVariantPayloadArityMismatch(String, usize, usize, Span),

    #[error("Match target is not an enum: {0}")]
    MatchTargetNotEnum(Type, Span),

    #[error("Match pattern enum mismatch: expected {0}, found {1}")]
    MatchPatternEnumMismatch(String, String, Span),

    #[error("Match is not exhaustive: {0}")]
    NonExhaustiveMatch(Span),

    #[error("Duplicate match arm variant: {0}")]
    DuplicateMatchVariant(String, Span),

    #[error("inout requires an aggregate parameter type, found {0}")]
    InoutParamNotAggregate(Type, Span),

    #[error("inout argument must be an assignable lvalue")]
    InoutArgNotLvalue(Span),

    #[error("inout argument must be mutable")]
    InoutArgNotMutable(Span),
}

impl SemCheckError {
    pub fn span(&self) -> Span {
        match self {
            SemCheckError::ValueOutOfRange(_, _, _, span) => *span,
            SemCheckError::InvalidRangeBounds(_, _, span) => *span,
            SemCheckError::DivisionByZero(span) => *span,
            SemCheckError::StringIndexNonAscii(span) => *span,
            SemCheckError::InvalidCallee(_, span) => *span,
            SemCheckError::UnknownStructType(_, span) => *span,
            SemCheckError::DuplicateStructField(_, span) => *span,
            SemCheckError::UnknownStructField(_, span) => *span,
            SemCheckError::StructFieldsMissing(_, span) => *span,
            SemCheckError::UnknownEnumType(_, span) => *span,
            SemCheckError::UnknownEnumVariant(_, _, span) => *span,
            SemCheckError::EnumVariantPayloadArityMismatch(_, _, _, span) => *span,
            SemCheckError::MatchTargetNotEnum(_, span) => *span,
            SemCheckError::MatchPatternEnumMismatch(_, _, span) => *span,
            SemCheckError::NonExhaustiveMatch(span) => *span,
            SemCheckError::DuplicateMatchVariant(_, span) => *span,
            SemCheckError::InoutParamNotAggregate(_, span) => *span,
            SemCheckError::InoutArgNotLvalue(span) => *span,
            SemCheckError::InoutArgNotMutable(span) => *span,
        }
    }
}
