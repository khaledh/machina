//! Type-check diagnostics.
//!
//! The checker carries errors as structured `TypeCheckErrorKind` and converts
//! them to spans/messages at reporting boundaries.

use thiserror::Error;

use crate::core::diag::{Span, SpannedError};
use crate::core::tree::resolved::{BindPattern, ExprKind};
use crate::core::types::Type;

#[derive(Debug, Clone, Error)]
pub enum TypeCheckErrorKind {
    #[error("Invalid types for arithmetic operation: {0}, {1}")]
    ArithTypeMismatch(Type, Type),

    #[error("Arithmetic operand must be an integer, found {0}")]
    ArithOperandNotInt(Type),

    #[error("Comparison operand must be an integer, found {0}")]
    CmpOperandNotInt(Type),

    #[error("Type {0} is not equatable: {1} has non-equatable type {2}")]
    TypeNotEquatable(Type, String, Type),

    #[error("Negation operand must be an integer, found {0}")]
    NegationOperandNotInt(Type),

    #[error("Logical operand must be a boolean, found {0}")]
    LogicalOperandNotBoolean(Type),

    #[error("`?` operand must be an error union, found {0}")]
    TryOperandNotErrorUnion(Type),

    #[error("`?` used outside of a function or closure")]
    TryOutsideFunction,

    #[error("`?` requires an error-union return type, found {0}")]
    TryReturnTypeNotErrorUnion(Type),

    #[error("`or` handler must be callable, found {0}")]
    TryHandlerNotCallable(Type),

    #[error("`or` handler must accept exactly one argument, found {0}")]
    TryHandlerArity(usize),

    #[error("`or` handler argument type mismatch: expected {0}, found {1}")]
    TryHandlerArgTypeMismatch(Type, Type),

    #[error(
        "`?` cannot propagate errors {missing} into return union {ret}; add them to the return union or handle them with match",
        missing = .0.join(" | "),
        ret = .1.join(" | ")
    )]
    TryErrorNotInReturn(Vec<String>, Vec<String>),

    #[error("Join arm type mismatch: expected join type {0}, found arm type {1}")]
    JoinArmTypeMismatch(Type, Type),

    #[error(
        "Join arm type mismatch: expected one of {expected}, found arm type {1}",
        expected = .0.join(" | ")
    )]
    JoinArmNotInErrorUnion(Vec<String>, Type),

    #[error("Condition must be a boolean, found {0}")]
    CondNotBoolean(Type),

    #[error("break used outside of a loop")]
    BreakOutsideLoop,

    #[error("continue used outside of a loop")]
    ContinueOutsideLoop,

    #[error("return used outside of a function or closure")]
    ReturnOutsideFunction,

    #[error("return value missing: expected {0}")]
    ReturnValueMissing(Type),

    #[error("unexpected return value")]
    ReturnValueUnexpected,

    #[error("return type mismatch: expected {0}, found {1}")]
    ReturnTypeMismatch(Type, Type),

    #[error(
        "return value must be one of {expected}, found {1}",
        expected = .0.join(" | ")
    )]
    ReturnNotInErrorUnion(Vec<String>, Type),

    #[error("Then and else branches have different types: {0} != {1}")]
    ThenElseTypeMismatch(Type, Type),

    #[error("Type mismatch in assignment: lhs type {0} != rhs type {1}")]
    AssignTypeMismatch(Type, Type),

    #[error("Invalid argument count for function {0}: expected {1}, found {2}")]
    ArgCountMismatch(String, usize, usize),

    #[error("Type mismatch in arg {0}: expected {1}, found {2}")]
    ArgTypeMismatch(usize, Type, Type),

    #[error("Invalid type argument count for type {0}: expected {1}, found {2}")]
    TypeArgCountMismatch(String, usize, usize),

    #[error("Invalid callee. Expected a function name, found: {0:?}")]
    InvalidCallee(ExprKind),

    #[error("Empty array literals are unsupported")]
    EmptyArrayLiteral,

    #[error("Too many indices for array: expected {0}, found {1}")]
    TooManyIndices(usize, usize),

    #[error("Array element type mismatch: expected {0}, found {1}")]
    ArrayElementTypeMismatch(Type, Type),

    #[error("Type {0} is not hashable: {1} has non-hashable type {2}")]
    TypeNotHashable(Type, String, Type),

    #[error("Index must be an integer, found {0}")]
    IndexTypeNotInt(Type),

    #[error("Index on non-array type: {0}")]
    InvalidIndexTargetType(Type),

    #[error("Map index key type mismatch: expected {0}, found {1}")]
    MapKeyTypeMismatch(Type, Type),

    #[error("Map indexing requires copy-safe value type, found {0}")]
    MapIndexValueNotCopySafe(Type),

    #[error("Map index assignment is not supported")]
    MapIndexAssignUnsupported,

    #[error("Type cannot be inferred")]
    UnknownType,

    #[error("Error union types are only allowed in return type position")]
    UnionNotAllowedHere,

    #[error("Pattern type mismatch: expected {0}, found {1}")]
    PatternTypeMismatch(BindPattern, Type),

    #[error("Declaration type mismatch: declared type {0}, found type {1}")]
    DeclTypeMismatch(Type, Type),

    #[error("Declaration type mismatch: expected one of {0:?}, found type {1}")]
    DeclTypeMismatchMulti(Vec<Type>, Type),

    #[error("Array pattern length mismatch: expected {0}, found {1}")]
    ArrayPatternLengthMismatch(usize, usize),

    #[error("Empty tuple literals are unsupported")]
    EmptyTupleLiteral,

    #[error("Tuple field out of bounds: tuple has {0} fields, index is {1}")]
    TupleFieldOutOfBounds(usize, usize),

    #[error("Invalid tuple field access target: expected tuple, found {0}")]
    InvalidTupleFieldTarget(Type),

    #[error("Tuple pattern length mismatch: expected {0}, found {1}")]
    TuplePatternLengthMismatch(usize, usize),

    #[error("Uknown struct type: {0}")]
    UnknownStructType(String),

    #[error("Duplicate struct field: {0}")]
    DuplicateStructField(String),

    #[error("Struct field type mismatch: field {0} expected {1}, found {2}")]
    StructFieldTypeMismatch(String, Type, Type),

    #[error("Unknown struct field: {0}")]
    UnknownStructField(String),

    #[error("Struct fields missing: {0}")]
    StructFieldsMissing(String),

    #[error("Invalid struct field target: expected struct, found {0}")]
    InvalidStructFieldTarget(Type),

    #[error("Cannot construct opaque type {0} outside its defining module")]
    OpaqueTypeConstruction(String),

    #[error("Cannot access field {1} of opaque type {0} outside its defining module")]
    OpaqueFieldAccess(String, String),

    #[error("Property {0} is read-only")]
    PropertyNotWritable(String),

    #[error("Property {0} is write-only")]
    PropertyNotReadable(String),

    #[error("Property getter {0} must not take parameters")]
    PropertyGetterHasParams(String),

    #[error("Property setter {0} must take exactly one parameter, found {1}")]
    PropertySetterParamCount(String, usize),

    #[error("Property setter {0} must return unit, found {1}")]
    PropertySetterReturnType(String, Type),

    #[error("Property {0} getter/setter type mismatch: {1} vs {2}")]
    PropertyAccessorTypeMismatch(String, Type, Type),

    #[error("Property {0} accessor already defined")]
    PropertyAccessorDuplicate(String),

    #[error("Property {0} cannot be called as a method")]
    PropertyCalledAsMethod(String),

    #[error("Property {0} conflicts with struct field {1}")]
    PropertyConflictsWithField(String, String),

    #[error("Duplicate trait implementation block: {0} :: {1}")]
    TraitImplDuplicate(String, String),

    #[error("Duplicate method {1} in trait {0}")]
    TraitMethodDuplicate(String, String),

    #[error("Method {2} in {0} :: {1} is not declared in trait")]
    TraitMethodNotInTrait(String, String, String),

    #[error("Duplicate method implementation {2} in {0} :: {1}")]
    TraitMethodImplDuplicate(String, String, String),

    #[error("Missing trait method implementation {2} in {0} :: {1}")]
    TraitMethodMissingImpl(String, String, String),

    #[error("Trait method signature mismatch for {0} :: {1}::{2}")]
    TraitMethodSignatureMismatch(String, String, String),

    #[error("Duplicate trait property {1} in trait {0}")]
    TraitPropertyDuplicate(String, String),

    #[error("Property {2} in {0} :: {1} is not declared in trait")]
    TraitPropertyNotInTrait(String, String, String),

    #[error("Missing trait property implementation {2} in {0} :: {1}")]
    TraitPropertyMissingImpl(String, String, String),

    #[error("Trait property type mismatch for {0} :: {1}.{2}: expected {3}, found {4}")]
    TraitPropertyTypeMismatch(String, String, String, Type, Type),

    #[error("Missing required getter for trait property {2} in {0} :: {1}")]
    TraitPropertyMissingGetter(String, String, String),

    #[error("Missing required setter for trait property {2} in {0} :: {1}")]
    TraitPropertyMissingSetter(String, String, String),

    #[error("Unknown enum type: {0}")]
    UnknownEnumType(String),

    #[error("Unknown enum variant: {0}::{1}")]
    UnknownEnumVariant(String, String),

    #[error("Invalid struct update target: expected struct, found {0}")]
    InvalidStructUpdateTarget(Type),

    #[error("Enum variant payload arity mismatch: variant {0} expected {1} elements, found {2}")]
    EnumVariantPayloadArityMismatch(String, usize, usize),

    #[error("Enum variant payload type mismatch: variant {0} index {1} expected {2}, found {3}")]
    EnumVariantPayloadTypeMismatch(String, usize, Type, Type),

    #[error("Match target is not an enum: {0}")]
    MatchTargetNotEnum(Type),

    #[error("Match arm type mismatch: expected {0}, found {1}")]
    MatchArmTypeMismatch(Type, Type),

    #[error("Match pattern enum mismatch: expected {0}, found {1}")]
    MatchPatternEnumMismatch(String, String),

    #[error("Match typed binding expects one of {0:?}, found {1}")]
    MatchTypedBindingTypeMismatch(Vec<String>, Type),

    #[error("Match is not exhaustive")]
    NonExhaustiveMatch,

    #[error("Duplicate match arm variant: {0}")]
    DuplicateMatchVariant(String),

    #[error("String index assignment is not supported")]
    StringIndexAssign,

    #[error("Invalid range bounds: {0} >= {1}")]
    InvalidRangeBounds(i128, i128),

    #[error("Refinement base type must be an integer, found {0}")]
    RefinementBaseNotInt(Type),

    #[error("Bounds [{0},{1}) are outside integer range [{2},{3})")]
    BoundsOutOfRange(i128, i128, i128, i128),

    #[error("Value out of range: {0} not in range [{1},{2})")]
    ValueOutOfRange(i128, i128, i128),

    #[error("Value must be nonzero, found {0}")]
    ValueNotNonZero(i128),

    #[error("nonzero is redundant: bounds [{0},{1}) already exclude 0")]
    RedundantNonZero(i128, i128),

    #[error("For iterator is not iterable: {0}")]
    ForIterNotIterable(Type),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Function overload not found: {0}")]
    OverloadNoMatch(String),

    #[error("Function overload is ambiguous: {0}")]
    OverloadAmbiguous(String),

    #[error("Cannot access private callable: {0}")]
    CallableNotAccessible(String),

    #[error("Cannot access private property: {0}")]
    PropertyNotAccessible(String),

    #[error("Type {1} does not satisfy trait bound {0}")]
    TraitBoundNotSatisfied(String, Type),

    #[error("Slice target is not an array or string: {0}")]
    SliceTargetNotArrayOrString(Type),

    #[error("Slice target is a zero-dimension array: {0}")]
    SliceTargetZeroDimArray(Type),

    #[error("Format expression must be string, integer, or bool, found {0}")]
    StringFmtExprUnsupportedType(Type),

    #[error("len() target must be an lvalue")]
    LenTargetNotLvalue,

    #[error("Cannot destructure opaque type {0} outside its defining module")]
    OpaquePatternDestructure(String),

    #[error(
        "Typestate {0} implementing role {1} state {2} is missing a handler for incoming payload {3}"
    )]
    ProtocolStateHandlerMissing(String, String, String, Type),

    #[error(
        "Typestate {0} implementing role {1} state {2} emits payload {3} which is not allowed by protocol transitions"
    )]
    ProtocolStateOutgoingPayloadNotAllowed(String, String, String, Type),

    #[error(
        "Typestate {0} implementing role {1} state {2} emits payload {3} to `{5}` (bound role `{6}`), expected peer role `{4}`"
    )]
    ProtocolStateEmitDestinationRoleMismatch(String, String, String, Type, String, String, String),

    #[error(
        "Typestate {0} implementing role {1} state {2} emits payload {3} to an unbound destination; expected peer role `{4}`"
    )]
    ProtocolStateEmitDestinationRoleUnbound(String, String, String, Type, String),

    #[error(
        "Typestate {0} implementing role {1} issues request payload {2} to peer role `{3}` without a matching request contract"
    )]
    ProtocolRequestContractMissing(String, String, Type, String),

    #[error(
        "Typestate {0} implementing role {1} issues request payload {2} to peer role `{3}` with ambiguous request contracts"
    )]
    ProtocolRequestContractAmbiguous(String, String, Type, String),

    #[error(
        "Typestate {0} implementing role {1} request payload {2} to peer role `{3}` expects response {4} outside contract responses"
    )]
    ProtocolRequestResponseNotInContract(String, String, Type, String, Type),

    #[error(
        "Typestate {0} state {1} has overlapping `on` handlers for selector {2} on response variants {3:?}"
    )]
    TypestateOverlappingOnHandlers(String, String, Type, Vec<Type>),
    #[error(
        "Typestate {0} state {1} has ambiguous response provenance for selector {2} on variants {3:?}; use request-site labels (for RequestType:label(...)) to disambiguate"
    )]
    TypestateAmbiguousResponseProvenance(String, String, Type, Vec<Type>),
    #[error("Typestate {0} request {1}{2} has no handler for expected response variant {3}")]
    TypestateRequestMissingResponseHandler(String, Type, String, Type),
    #[error("Typestate {0} handler for request {1}{2} uses unsupported response variant {3}")]
    TypestateHandlerUnsupportedResponseVariant(String, Type, String, Type),

    #[error("`reply` can only be used inside typestate `on` handlers")]
    ReplyOutsideHandler,

    #[error("`reply` expects a ReplyCap as its first argument, found {0}")]
    ReplyCapExpected(Type),

    #[error("`reply` payload type {0} is not in reply capability response set {1:?}")]
    ReplyPayloadNotAllowed(Type, Vec<Type>),

    #[error("reply capability must be consumed exactly once on all paths: {0}")]
    ReplyCapMustBeConsumed(String),

    #[error("reply capability consumed multiple times: {0}")]
    ReplyCapConsumedMultipleTimes(String),

    #[error("`reply` capability argument must be a handler ReplyCap parameter")]
    ReplyCapParamRequired,
}

pub type TypeCheckError = SpannedError<TypeCheckErrorKind>;
pub use TypeCheckErrorKind as TEK;

impl TypeCheckErrorKind {
    pub fn at(self, span: Span) -> TypeCheckError {
        TypeCheckError::new(self, span)
    }
}
