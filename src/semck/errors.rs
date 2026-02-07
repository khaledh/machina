use thiserror::Error;

use crate::diag::Span;
use crate::tree::normalized::ExprKind;
use crate::types::Type;

#[derive(Debug, Clone, Error)]
pub enum SemCheckError {
    #[error("Value out of range: {0} not in range [{1},{2})")]
    ValueOutOfRange(i128, i128, i128, Span),

    #[error("Value must be nonzero, found {0}")]
    ValueNotNonZero(i128, Span),

    #[error("Invalid range bounds: {0} >= {1}")]
    InvalidRangeBounds(i128, i128, Span),

    #[error("Division by zero")]
    DivisionByZero(Span),

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

    #[error(
        "Match on error union is not exhaustive; missing variants: {missing}",
        missing = .0.join(" | ")
    )]
    NonExhaustiveUnionMatch(Vec<String>, Span),

    #[error("Duplicate match arm variant: {0}")]
    DuplicateMatchVariant(String, Span),

    #[error("Invalid match pattern for type {0}")]
    InvalidMatchPattern(Type, Span),

    #[error("Wildcard match arm must be last")]
    WildcardArmNotLast(Span),

    #[error("Tuple match requires a single arm for now")]
    TupleMatchRequiresSingleArm(Span),

    #[error("Tuple pattern arity mismatch: expected {0}, found {1}")]
    TuplePatternArityMismatch(usize, usize, Span),

    #[error("inout requires an aggregate parameter type, found {0}")]
    InOutParamNotAggregate(Type, Span),

    #[error("inout argument must be an assignable lvalue")]
    InOutArgNotLvalue(Span),

    #[error("inout argument must be mutable")]
    InOutArgNotMutable(Span),

    #[error("inout parameter requires `inout` argument")]
    InOutArgMissingMode(Span),

    #[error("inout argument provided for non-inout parameter")]
    InOutArgUnexpected(Span),

    #[error("out requires an aggregate parameter type, found {0}")]
    OutParamNotAggregate(Type, Span),

    #[error("out argument must be an assignable lvalue")]
    OutArgNotLvalue(Span),

    #[error("out argument must be mutable")]
    OutArgNotMutable(Span),

    #[error("out parameter requires `out` argument")]
    OutArgMissingMode(Span),

    #[error("out argument provided for non-out parameter")]
    OutArgUnexpected(Span),

    #[error("out self is not allowed")]
    OutSelfNotAllowed(Span),

    #[error("out parameter `{0}` is not initialized on all paths")]
    OutParamNotInitialized(String, Span),

    #[error("sink parameter requires `move` argument")]
    SinkArgMissingMove(Span),

    #[error("move argument provided for non-sink parameter")]
    MoveArgUnexpected(Span),

    #[error("Partially initialized local `{0}` must be fully initialized")]
    PartialInitNotAllowed(String, Span),

    #[error("lvalue arguments may overlap")]
    OverlappingLvalueArgs(Span),

    #[error("sink requires an owned parameter type, found {0}")]
    SinkParamNotOwned(Type, Span),

    #[error("Use before initialization: {0}")]
    UseBeforeInit(String, Span),

    #[error("Use after move: {0}")]
    UseAfterMove(String, Span),

    #[error("Invalid move target")]
    InvalidMoveTarget(Span),

    #[error("Cannot move from parameter")]
    MoveFromParam(Span),

    #[error("Owned value must be moved")]
    OwnedMoveRequired(Span),

    #[error("Slice value cannot be returned in v1")]
    SliceEscapeReturn(Span),

    #[error("Slice value cannot be stored in v1")]
    SliceEscapeStore(Span),

    #[error("Slice borrow conflicts with mutation")]
    SliceBorrowConflict(Span),

    #[error("Slice target must be an lvalue")]
    SliceTargetNotLvalue(Span),

    #[error("Cannot move captured variable `{0}` inside closure")]
    ClosureCaptureMove(String, Span),

    #[error("Capture `{0}` is not used inside closure")]
    ClosureCaptureUnused(String, Span),

    #[error("Closure borrow conflicts with use of `{0}`")]
    ClosureBorrowConflict(String, Span),

    #[error("Captured closure cannot be returned")]
    ClosureEscapeReturn(Span),

    #[error("Captured closure cannot be stored")]
    ClosureEscapeStore(Span),

    #[error("Captured closure cannot be passed as an argument")]
    ClosureEscapeArg(Span),
}

impl SemCheckError {
    pub fn span(&self) -> Span {
        match self {
            SemCheckError::ValueOutOfRange(_, _, _, span) => *span,
            SemCheckError::ValueNotNonZero(_, span) => *span,
            SemCheckError::InvalidRangeBounds(_, _, span) => *span,
            SemCheckError::DivisionByZero(span) => *span,
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
            SemCheckError::NonExhaustiveUnionMatch(_, span) => *span,
            SemCheckError::DuplicateMatchVariant(_, span) => *span,
            SemCheckError::InvalidMatchPattern(_, span) => *span,
            SemCheckError::WildcardArmNotLast(span) => *span,
            SemCheckError::TupleMatchRequiresSingleArm(span) => *span,
            SemCheckError::TuplePatternArityMismatch(_, _, span) => *span,
            SemCheckError::InOutParamNotAggregate(_, span) => *span,
            SemCheckError::InOutArgNotLvalue(span) => *span,
            SemCheckError::InOutArgNotMutable(span) => *span,
            SemCheckError::InOutArgMissingMode(span) => *span,
            SemCheckError::InOutArgUnexpected(span) => *span,
            SemCheckError::OutParamNotAggregate(_, span) => *span,
            SemCheckError::OutArgNotLvalue(span) => *span,
            SemCheckError::OutArgNotMutable(span) => *span,
            SemCheckError::OutArgMissingMode(span) => *span,
            SemCheckError::OutArgUnexpected(span) => *span,
            SemCheckError::OutSelfNotAllowed(span) => *span,
            SemCheckError::OutParamNotInitialized(_, span) => *span,
            SemCheckError::SinkArgMissingMove(span) => *span,
            SemCheckError::MoveArgUnexpected(span) => *span,
            SemCheckError::PartialInitNotAllowed(_, span) => *span,
            SemCheckError::OverlappingLvalueArgs(span) => *span,
            SemCheckError::SinkParamNotOwned(_, span) => *span,
            SemCheckError::UseBeforeInit(_, span) => *span,
            SemCheckError::UseAfterMove(_, span) => *span,
            SemCheckError::InvalidMoveTarget(span) => *span,
            SemCheckError::MoveFromParam(span) => *span,
            SemCheckError::OwnedMoveRequired(span) => *span,
            SemCheckError::SliceEscapeReturn(span) => *span,
            SemCheckError::SliceEscapeStore(span) => *span,
            SemCheckError::SliceBorrowConflict(span) => *span,
            SemCheckError::SliceTargetNotLvalue(span) => *span,
            SemCheckError::ClosureCaptureMove(_, span) => *span,
            SemCheckError::ClosureCaptureUnused(_, span) => *span,
            SemCheckError::ClosureBorrowConflict(_, span) => *span,
            SemCheckError::ClosureEscapeReturn(span) => *span,
            SemCheckError::ClosureEscapeStore(span) => *span,
            SemCheckError::ClosureEscapeArg(span) => *span,
        }
    }
}
