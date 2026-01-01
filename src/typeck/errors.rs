use thiserror::Error;

use crate::ast::{ExprKind, Pattern};
use crate::diag::Span;
use crate::types::Type;

#[derive(Debug, Clone, Error)]
#[error(transparent)]
pub struct TypeCheckError(Box<TypeCheckErrorKind>);

#[derive(Debug, Clone, Error)]
pub enum TypeCheckErrorKind {
    #[error("Invalid types for arithmetic operation: {0}, {1}")]
    ArithTypeMismatch(Type, Type, Span),

    #[error("Arithmetic operand must be an integer, found {0}")]
    ArithOperandNotInt(Type, Span),

    #[error("Comparison operand must be an integer, found {0}")]
    CmpOperandNotInt(Type, Span),

    #[error("Negation operand must be an integer, found {0}")]
    NegationOperandNotInt(Type, Span),

    #[error("Logical operand must be a boolean, found {0}")]
    LogicalOperandNotBoolean(Type, Span),

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

    #[error("Declaration type mismatch: expected one of {0:?}, found type {1}")]
    DeclTypeMismatchMulti(Vec<Type>, Type, Span),

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

    #[error("Invalid range bounds: {0} >= {1}")]
    InvalidRangeBounds(u64, u64, Span),

    #[error("Value out of range: {0} not in range [{1},{2})")]
    ValueOutOfRange(i128, i128, i128, Span),

    #[error("For iterator is not iterable: {0}")]
    ForIterNotIterable(Type, Span),

    #[error("Division by zero: {0}")]
    DivisionByZero(Span),

    #[error("Function overload not found: {0}")]
    FuncOverloadNoMatch(String, Span),

    #[error("Function overload is ambiguous: {0}")]
    FuncOverloadAmbiguous(String, Span),

    #[error("Slice target is not an array: {0}")]
    SliceTargetNotArray(Type, Span),

    #[error("Slice target is not a 1-D array: {0}")]
    SliceTargetNot1DArray(Type, Span),

    #[error("Format expression must be string or integer, found {0}")]
    StringFmtExprUnsupportedType(Type, Span),
}

impl TypeCheckError {
    pub fn new(kind: TypeCheckErrorKind) -> Self {
        Self(Box::new(kind))
    }

    pub fn kind(&self) -> &TypeCheckErrorKind {
        &self.0
    }

    pub fn span(&self) -> Span {
        match &*self.0 {
            TypeCheckErrorKind::ArithTypeMismatch(_, _, span) => *span,
            TypeCheckErrorKind::ArithOperandNotInt(_, span) => *span,
            TypeCheckErrorKind::CmpOperandNotInt(_, span) => *span,
            TypeCheckErrorKind::NegationOperandNotInt(_, span) => *span,
            TypeCheckErrorKind::LogicalOperandNotBoolean(_, span) => *span,
            TypeCheckErrorKind::CondNotBoolean(_, span) => *span,
            TypeCheckErrorKind::ThenElseTypeMismatch(_, _, span) => *span,
            TypeCheckErrorKind::AssignTypeMismatch(_, _, span) => *span,
            TypeCheckErrorKind::ArgCountMismatch(_, _, _, span) => *span,
            TypeCheckErrorKind::ArgTypeMismatch(_, _, _, span) => *span,
            TypeCheckErrorKind::InvalidCallee(_, span) => *span,
            TypeCheckErrorKind::EmptyArrayLiteral(span) => *span,
            TypeCheckErrorKind::TooManyIndices(_, _, span) => *span,
            TypeCheckErrorKind::ArrayElementTypeMismatch(_, _, span) => *span,
            TypeCheckErrorKind::IndexTypeNotInt(_, span) => *span,
            TypeCheckErrorKind::InvalidIndexTargetType(_, span) => *span,
            TypeCheckErrorKind::UnknownType(span) => *span,
            TypeCheckErrorKind::PatternTypeMismatch(_, _, span) => *span,
            TypeCheckErrorKind::DeclTypeMismatch(_, _, span) => *span,
            TypeCheckErrorKind::DeclTypeMismatchMulti(_, _, span) => *span,
            TypeCheckErrorKind::ArrayPatternLengthMismatch(_, _, span) => *span,
            TypeCheckErrorKind::EmptyTupleLiteral(span) => *span,
            TypeCheckErrorKind::TupleFieldOutOfBounds(_, _, span) => *span,
            TypeCheckErrorKind::InvalidTupleFieldTarget(_, span) => *span,
            TypeCheckErrorKind::TuplePatternLengthMismatch(_, _, span) => *span,
            TypeCheckErrorKind::UnknownStructType(_, span) => *span,
            TypeCheckErrorKind::DuplicateStructField(_, span) => *span,
            TypeCheckErrorKind::StructFieldTypeMismatch(_, _, _, span) => *span,
            TypeCheckErrorKind::UnknownStructField(_, span) => *span,
            TypeCheckErrorKind::StructFieldsMissing(_, span) => *span,
            TypeCheckErrorKind::InvalidStructFieldTarget(_, span) => *span,
            TypeCheckErrorKind::UnknownEnumType(_, span) => *span,
            TypeCheckErrorKind::UnknownEnumVariant(_, _, span) => *span,
            TypeCheckErrorKind::InvalidStructUpdateTarget(_, span) => *span,
            TypeCheckErrorKind::EnumVariantPayloadArityMismatch(_, _, _, span) => *span,
            TypeCheckErrorKind::EnumVariantPayloadTypeMismatch(_, _, _, _, span) => *span,
            TypeCheckErrorKind::MatchTargetNotEnum(_, span) => *span,
            TypeCheckErrorKind::MatchArmTypeMismatch(_, _, span) => *span,
            TypeCheckErrorKind::MatchPatternEnumMismatch(_, _, span) => *span,
            TypeCheckErrorKind::NonExhaustiveMatch(span) => *span,
            TypeCheckErrorKind::DuplicateMatchVariant(_, span) => *span,
            TypeCheckErrorKind::StringIndexNonAscii(span) => *span,
            TypeCheckErrorKind::InvalidRangeBounds(_, _, span) => *span,
            TypeCheckErrorKind::ValueOutOfRange(_, _, _, span) => *span,
            TypeCheckErrorKind::ForIterNotIterable(_, span) => *span,
            TypeCheckErrorKind::DivisionByZero(span) => *span,
            TypeCheckErrorKind::FuncOverloadNoMatch(_, span) => *span,
            TypeCheckErrorKind::FuncOverloadAmbiguous(_, span) => *span,
            TypeCheckErrorKind::SliceTargetNotArray(_, span) => *span,
            TypeCheckErrorKind::SliceTargetNot1DArray(_, span) => *span,
            TypeCheckErrorKind::StringFmtExprUnsupportedType(_, span) => *span,
        }
    }
}

impl From<TypeCheckErrorKind> for TypeCheckError {
    fn from(kind: TypeCheckErrorKind) -> Self {
        Self::new(kind)
    }
}
