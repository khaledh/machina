use thiserror::Error;

use crate::core::diag::{Span, SpannedError};
use crate::core::tree::normalized::ExprKind;
use crate::core::types::Type;

#[derive(Debug, Clone, Error)]
pub enum SemCheckErrorKind {
    #[error("Value out of range: {0} not in range [{1},{2})")]
    ValueOutOfRange(i128, i128, i128),

    #[error("Value must be nonzero, found {0}")]
    ValueNotNonZero(i128),

    #[error("Invalid range bounds: {0} >= {1}")]
    InvalidRangeBounds(i128, i128),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Invalid callee. Expected a function name, found: {0:?}")]
    InvalidCallee(ExprKind),

    #[error("Unknown struct type: {0}")]
    UnknownStructType(String),

    #[error("Duplicate struct field: {0}")]
    DuplicateStructField(String),

    #[error("Unknown struct field: {0}")]
    UnknownStructField(String),

    #[error("Struct fields missing: {0}")]
    StructFieldsMissing(String),

    #[error("Unknown enum type: {0}")]
    UnknownEnumType(String),

    #[error("Unknown enum variant: {0}::{1}")]
    UnknownEnumVariant(String, String),

    #[error("Enum variant payload arity mismatch: variant {0} expected {1} elements, found {2}")]
    EnumVariantPayloadArityMismatch(String, usize, usize),

    #[error("Match target is not an enum: {0}")]
    MatchTargetNotEnum(Type),

    #[error("Match pattern enum mismatch: expected {0}, found {1}")]
    MatchPatternEnumMismatch(String, String),

    #[error("Match is not exhaustive")]
    NonExhaustiveMatch,

    #[error(
        "Match on error union is not exhaustive; missing variants: {missing}",
        missing = .0.join(" | ")
    )]
    NonExhaustiveUnionMatch(Vec<String>),

    #[error("Duplicate match arm variant: {0}")]
    DuplicateMatchVariant(String),

    #[error("Invalid match pattern for type {0}")]
    InvalidMatchPattern(Type),

    #[error("Wildcard match arm must be last")]
    WildcardArmNotLast,

    #[error("Tuple match requires a single arm for now")]
    TupleMatchRequiresSingleArm,

    #[error("Tuple pattern arity mismatch: expected {0}, found {1}")]
    TuplePatternArityMismatch(usize, usize),

    #[error("inout requires an aggregate parameter type, found {0}")]
    InOutParamNotAggregate(Type),

    #[error("inout argument must be an assignable lvalue")]
    InOutArgNotLvalue,

    #[error("inout argument must be mutable")]
    InOutArgNotMutable,

    #[error("inout parameter requires `inout` argument")]
    InOutArgMissingMode,

    #[error("inout argument provided for non-inout parameter")]
    InOutArgUnexpected,

    #[error("out requires an aggregate parameter type, found {0}")]
    OutParamNotAggregate(Type),

    #[error("out argument must be an assignable lvalue")]
    OutArgNotLvalue,

    #[error("out argument must be mutable")]
    OutArgNotMutable,

    #[error("out parameter requires `out` argument")]
    OutArgMissingMode,

    #[error("out argument provided for non-out parameter")]
    OutArgUnexpected,

    #[error("out self is not allowed")]
    OutSelfNotAllowed,

    #[error("out parameter `{0}` is not initialized on all paths")]
    OutParamNotInitialized(String),

    #[error("sink parameter requires `move` argument")]
    SinkArgMissingMove,

    #[error("move argument provided for non-sink parameter")]
    MoveArgUnexpected,

    #[error("Partially initialized local `{0}` must be fully initialized")]
    PartialInitNotAllowed(String),

    #[error("lvalue arguments may overlap")]
    OverlappingLvalueArgs,

    #[error("sink requires an owned parameter type, found {0}")]
    SinkParamNotOwned(Type),

    #[error("Use before initialization: {0}")]
    UseBeforeInit(String),

    #[error("Use after move: {0}")]
    UseAfterMove(String),

    #[error("Invalid move target")]
    InvalidMoveTarget,

    #[error("Cannot move from parameter")]
    MoveFromParam,

    #[error("Owned value must be moved")]
    OwnedMoveRequired,

    #[error("Slice value cannot be returned in v1")]
    SliceEscapeReturn,

    #[error("Slice value cannot be stored in v1")]
    SliceEscapeStore,

    #[error("Slice borrow conflicts with mutation")]
    SliceBorrowConflict,

    #[error("Slice target must be an lvalue")]
    SliceTargetNotLvalue,

    #[error("Cannot move captured variable `{0}` inside closure")]
    ClosureCaptureMove(String),

    #[error("Capture `{0}` is not used inside closure")]
    ClosureCaptureUnused(String),

    #[error("Closure borrow conflicts with use of `{0}`")]
    ClosureBorrowConflict(String),

    #[error("Captured closure cannot be returned")]
    ClosureEscapeReturn,

    #[error("Captured closure cannot be stored")]
    ClosureEscapeStore,

    #[error("Captured closure cannot be passed as an argument")]
    ClosureEscapeArg,

    #[error(
        "Typestate {0} ({1}::{2} state {3}) has handler trigger {4} with no matching protocol transition"
    )]
    ProtocolProgressionMissingTriggerTransition(String, String, String, String, Type),

    #[error(
        "Typestate {0} ({1}::{2} state {3}) handler trigger {4} emits {5} to role {6}, but no protocol transition edge allows it"
    )]
    ProtocolProgressionImpossibleEmit(String, String, String, String, Type, Type, String),

    #[error(
        "Typestate {0} ({1}::{2} state {3}) handler trigger {4} returns state {5}, but no protocol transition allows it"
    )]
    ProtocolProgressionImpossibleReturnState(String, String, String, String, Type, String),
}

pub type SemCheckError = SpannedError<SemCheckErrorKind>;

impl SemCheckErrorKind {
    pub fn at(self, span: Span) -> SemCheckError {
        SemCheckError::new(self, span)
    }
}
