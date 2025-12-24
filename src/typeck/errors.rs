use thiserror::Error;

use crate::ast::{ExprKind, Pattern};
use crate::diagnostics::Span;
use crate::types::Type;

#[derive(Debug, Clone, Error)]
pub enum TypeCheckError {
    #[error("Function return type mismatch: expected {0}, found {1}")]
    FuncReturnTypeMismatch(Type, Type, Span),

    #[error("Invalid types for arithmetic operation: {0}, {1}")]
    ArithTypeMismatch(Type, Type, Span),

    #[error("Invalid types for comparison operation: {0} != {1}")]
    CmpTypeMismatch(Type, Type, Span),

    #[error("Comparison of non-scalars types is not supported: {0}")]
    CmpNonScalar(Type, Span),

    #[error("Condition must be a boolean, found {0}")]
    CondNotBoolean(Type, Span),

    #[error("Then and else branches have different types: {0} != {1}")]
    ThenElseTypeMismatch(Type, Type, Span),

    #[error("Type mismatch in assignment: lhs type {0} != rhs type {1}")]
    AssignTypeMismatch(Type, Type, Span),

    #[error("Invalid argument count for function {0}: expected {1}, found {2}")]
    ArgCountMismatch(String, usize, usize, Span),

    #[error("Type mismatch in arg {0}: expected {1}, found {2}")]
    ArgTypeMismatch(usize, Type, Type, Span),

    #[error("Invalid callee. Expected a function name, found: {0:?}")]
    InvalidCallee(ExprKind, Span),

    #[error("Empty array literals are unsupported: {0}")]
    EmptyArrayLiteral(Span),

    #[error("Too many indices for array: expected {0}, found {1}")]
    TooManyIndices(usize, usize, Span),

    #[error("Array element type mismatch: expected {0}, found {1}")]
    ArrayElementTypeMismatch(Type, Type, Span),

    #[error("Index must be an integer, found {0}")]
    IndexTypeNotInt(Type, Span),

    #[error("Index on non-array type: {0}")]
    InvalidIndexTargetType(Type, Span),

    #[error("Type cannot be inferred")]
    UnknownType(Span),

    #[error("Pattern type mismatch: expected {0}, found {1}")]
    PatternTypeMismatch(Pattern, Type, Span),

    #[error("Declaration type mismatch: declared type {0}, found type {1}")]
    DeclTypeMismatch(Type, Type, Span),

    #[error("Array pattern length mismatch: expected {0}, found {1}")]
    ArrayPatternLengthMismatch(usize, usize, Span),

    #[error("Empty tuple literals are unsupported: {0}")]
    EmptyTupleLiteral(Span),

    #[error("Tuple field out of bounds: tuple has {0} fields, index is {1}")]
    TupleFieldOutOfBounds(usize, usize, Span),

    #[error("Invalid tuple field access target: expected tuple, found {0}")]
    InvalidTupleFieldTarget(Type, Span),

    #[error("Tuple pattern length mismatch: expected {0}, found {1}")]
    TuplePatternLengthMismatch(usize, usize, Span),

    #[error("Uknown struct type: {0}")]
    UnknownStructType(String, Span),

    #[error("Duplicate struct field: {0}")]
    DuplicateStructField(String, Span),

    #[error("Struct field type mismatch: field {0} expected {1}, found {2}")]
    StructFieldTypeMismatch(String, Type, Type, Span),

    #[error("Unknown struct field: {0}")]
    UnknownStructField(String, Span),

    #[error("Struct fields missing: {0}")]
    StructFieldsMissing(String, Span),

    #[error("Invalid struct field target: expected struct, found {0}")]
    InvalidStructFieldTarget(Type, Span),

    #[error("Unknown enum type: {0}")]
    UnknownEnumType(String, Span),

    #[error("Unknown enum variant: {0}::{1}")]
    UnknownEnumVariant(String, String, Span),

    #[error("Invalid struct update target: expected struct, found {0}")]
    InvalidStructUpdateTarget(Type, Span),

    #[error("Enum variant payload arity mismatch: variant {0} expected {1} elements, found {2}")]
    EnumVariantPayloadArityMismatch(String, usize, usize, Span),

    #[error("Enum variant payload type mismatch: variant {0} index {1} expected {2}, found {3}")]
    EnumVariantPayloadTypeMismatch(String, usize, Type, Type, Span),

    #[error("Match target is not an enum: {0}")]
    MatchTargetNotEnum(Type, Span),

    #[error("Match arm type mismatch: expected {0}, found {1}")]
    MatchArmTypeMismatch(Type, Type, Span),

    #[error("Match pattern enum mismatch: expected {0}, found {1}")]
    MatchPatternEnumMismatch(String, String, Span),

    #[error("Match is not exhaustive: {0}")]
    NonExhaustiveMatch(Span),

    #[error("Duplicate match arm variant: {0}")]
    DuplicateMatchVariant(String, Span),

    #[error("String index operation on non-ASCII string: {0}")]
    StringIndexNonAscii(Span),
}

impl TypeCheckError {
    pub fn span(&self) -> Span {
        match self {
            TypeCheckError::FuncReturnTypeMismatch(_, _, span) => *span,
            TypeCheckError::ArithTypeMismatch(_, _, span) => *span,
            TypeCheckError::CmpTypeMismatch(_, _, span) => *span,
            TypeCheckError::CmpNonScalar(_, span) => *span,
            TypeCheckError::CondNotBoolean(_, span) => *span,
            TypeCheckError::ThenElseTypeMismatch(_, _, span) => *span,
            TypeCheckError::AssignTypeMismatch(_, _, span) => *span,
            TypeCheckError::ArgCountMismatch(_, _, _, span) => *span,
            TypeCheckError::ArgTypeMismatch(_, _, _, span) => *span,
            TypeCheckError::InvalidCallee(_, span) => *span,
            TypeCheckError::EmptyArrayLiteral(span) => *span,
            TypeCheckError::TooManyIndices(_, _, span) => *span,
            TypeCheckError::ArrayElementTypeMismatch(_, _, span) => *span,
            TypeCheckError::IndexTypeNotInt(_, span) => *span,
            TypeCheckError::InvalidIndexTargetType(_, span) => *span,
            TypeCheckError::UnknownType(span) => *span,
            TypeCheckError::PatternTypeMismatch(_, _, span) => *span,
            TypeCheckError::DeclTypeMismatch(_, _, span) => *span,
            TypeCheckError::ArrayPatternLengthMismatch(_, _, span) => *span,
            TypeCheckError::EmptyTupleLiteral(span) => *span,
            TypeCheckError::TupleFieldOutOfBounds(_, _, span) => *span,
            TypeCheckError::InvalidTupleFieldTarget(_, span) => *span,
            TypeCheckError::TuplePatternLengthMismatch(_, _, span) => *span,
            TypeCheckError::UnknownStructType(_, span) => *span,
            TypeCheckError::DuplicateStructField(_, span) => *span,
            TypeCheckError::StructFieldTypeMismatch(_, _, _, span) => *span,
            TypeCheckError::UnknownStructField(_, span) => *span,
            TypeCheckError::StructFieldsMissing(_, span) => *span,
            TypeCheckError::InvalidStructFieldTarget(_, span) => *span,
            TypeCheckError::UnknownEnumType(_, span) => *span,
            TypeCheckError::UnknownEnumVariant(_, _, span) => *span,
            TypeCheckError::InvalidStructUpdateTarget(_, span) => *span,
            TypeCheckError::EnumVariantPayloadArityMismatch(_, _, _, span) => *span,
            TypeCheckError::EnumVariantPayloadTypeMismatch(_, _, _, _, span) => *span,
            TypeCheckError::MatchTargetNotEnum(_, span) => *span,
            TypeCheckError::MatchArmTypeMismatch(_, _, span) => *span,
            TypeCheckError::MatchPatternEnumMismatch(_, _, span) => *span,
            TypeCheckError::NonExhaustiveMatch(span) => *span,
            TypeCheckError::DuplicateMatchVariant(_, span) => *span,
            TypeCheckError::StringIndexNonAscii(span) => *span,
        }
    }
}
