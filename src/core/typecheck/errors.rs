//! Type-check diagnostics.
//!
//! The checker carries errors as structured `TypeCheckErrorKind` and converts
//! them to spans/messages at reporting boundaries.

use thiserror::Error;

use crate::core::ast::{BindPattern, ExprKind};
use crate::core::diag::{Span, SpannedError};
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

    #[error("`defer` expression must be non-fallible, found {0}")]
    DeferExprFallible(Type),

    #[error("`defer` cannot use bare `?`; handle the error inside the deferred expression")]
    DeferBareTry,

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

    #[error("Argument for parameter '{0}' has type {1}, expected {2}")]
    ArgTypeMismatchForParam(String, Type, Type),

    #[error("No parameter named '{0}' in function '{1}'")]
    NoParameterNamed(String, String),

    #[error("Argument for parameter '{0}' provided more than once")]
    ArgProvidedMoreThanOnce(String),

    #[error("Missing argument for parameter '{0}'")]
    MissingArgumentForParameter(String),

    #[error("Named arguments not supported for function values")]
    NamedArgsNotSupportedForFunctionValues,

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

    #[error("Foreign view indexing is read-only")]
    ForeignViewIndexAssignUnsupported,

    #[error("Type cannot be inferred")]
    UnknownType,

    #[error("Error union types are only allowed in return type position")]
    UnionNotAllowedHere,

    #[error("`Iterable<T>` is only allowed in function and method parameter or return position")]
    IterableNotAllowedHere,

    #[error("Fixed layout is only supported on struct definitions, found {0}")]
    FixedLayoutRequiresStruct(String),

    #[error("Type `{0}` uses `@align` but is not marked `@layout(fixed)`")]
    FixedLayoutAlignRequiresFixedLayout(String),

    #[error(
        "Struct field `{field}` uses `@align` but type `{type_name}` is not marked `@layout(fixed)`"
    )]
    FixedLayoutFieldAlignRequiresFixedLayout { type_name: String, field: String },

    #[error("Unknown fixed-layout field attribute `{attr}` on field `{field}`")]
    FixedLayoutUnknownFieldAttr { field: String, attr: String },

    #[error("Alignment must be a non-zero power of two, found {0}")]
    FixedLayoutInvalidAlign(u64),

    #[error("Type `{type_name}` alignment {actual} is smaller than required alignment {required}")]
    FixedLayoutTypeAlignTooSmall {
        type_name: String,
        actual: u64,
        required: u64,
    },

    #[error("Field `{field}` alignment {actual} is smaller than natural alignment {required}")]
    FixedLayoutFieldAlignTooSmall {
        field: String,
        actual: u64,
        required: u64,
    },

    #[error(
        "Field `{field}` requires {padding} bytes of implicit padding before it in fixed-layout type `{type_name}`"
    )]
    FixedLayoutImplicitPadding {
        type_name: String,
        field: String,
        padding: u64,
    },

    #[error("Fixed-layout type `{type_name}` has size {actual}, expected {expected}")]
    FixedLayoutSizeMismatch {
        type_name: String,
        expected: u64,
        actual: u64,
    },

    #[error(
        "Field `{field}` in fixed-layout type `{type_name}` cannot use abstract handle type `{field_ty}`"
    )]
    FixedLayoutAbstractFieldType {
        type_name: String,
        field: String,
        field_ty: Type,
    },

    #[error("`{view_name}` requires a `@layout(fixed)` struct type argument, found `{elem_ty}`")]
    ForeignViewElementMustBeFixedLayout { view_name: String, elem_ty: Type },

    #[error(
        "opaque return {0} requires a single concrete witness type across all paths, found {1:?}"
    )]
    OpaqueIterableReturnMultipleWitnesses(Type, Vec<Type>),

    #[error("Pattern type mismatch: expected {0}, found {1}")]
    PatternTypeMismatch(BindPattern, Type),

    #[error("Declaration type mismatch: declared type {0}, found type {1}")]
    DeclTypeMismatch(Type, Type),

    #[error("Declaration type mismatch: expected one of {0:?}, found type {1}")]
    DeclTypeMismatchMulti(Vec<Type>, Type),

    #[error("Array pattern length mismatch: expected {0}, found {1}")]
    ArrayPatternLengthMismatch(usize, usize),

    #[error("Array pattern requires at least {0} elements, found {1}")]
    ArrayPatternMinLengthMismatch(usize, usize),

    #[error("Empty tuple literals are unsupported")]
    EmptyTupleLiteral,

    #[error("Tuple field out of bounds: tuple has {0} fields, index is {1}")]
    TupleFieldOutOfBounds(usize, usize),

    #[error("Invalid tuple field access target: expected tuple, found {0}")]
    InvalidTupleFieldTarget(Type),

    #[error("Tuple pattern length mismatch: expected {0}, found {1}")]
    TuplePatternLengthMismatch(usize, usize),

    #[error("Unknown struct type: {0}")]
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

    #[error("Method {1} is not available on opaque iterable type {0}")]
    OpaqueMethodAccess(Type, String),

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

    #[error("`for` protocol requires `{0}.iter(self) -> Iter`")]
    ForIterProtocolMissingIter(Type),

    #[error("`for` protocol requires iterator type `{0}` to define `next(inout self)`")]
    ForIterProtocolMissingNext(Type),

    #[error(
        "`for` protocol `next` must return `Item | IterDone` or `Item | Error | IterDone`, found {0}"
    )]
    ForIterProtocolInvalidNextReturn(Type),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Function overload not found: {0}")]
    OverloadNoMatch(String),

    #[error("Function overload is ambiguous: {0}")]
    OverloadAmbiguous(String),

    #[error("Overloads of '{0}' have overlapping arity ranges")]
    OverloadArityOverlap(String),

    #[error("Machine {0} hosts linear type {1}, not {2}")]
    LinearSessionHostMismatch(String, String, String),

    #[error("Linear type {0} does not define role {1}")]
    LinearSessionUnknownRole(String, String),

    #[error("`{0}` is not available on {1}::{2} (session role: {3})")]
    LinearSessionActionNotAllowed(String, String, String, String),

    #[error("Machine {0} hosting linear type {1} cannot deliver undeclared event {2}")]
    LinearMachineDeliverUnknownTrigger(String, String, String),

    #[error("Machine {0} hosting linear type {1} expects key type {2} for deliver, found {3}")]
    LinearMachineDeliverKeyTypeMismatch(String, String, String, String),

    #[error("send target must be Machine<T>, found {0}")]
    LinearMachineSendInvalidTarget(String),

    #[error("Machine {0} does not define an `on` handler for {1}")]
    LinearMachineSendUnknownMessage(String, String),

    #[error("Machine {0} hosting linear type {1} expects key type {2} for lookup, found {3}")]
    LinearMachineLookupKeyTypeMismatch(String, String, String, String),

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
}

pub type TypeCheckError = SpannedError<TypeCheckErrorKind>;
pub use TypeCheckErrorKind as TEK;

impl TypeCheckErrorKind {
    pub fn at(self, span: Span) -> TypeCheckError {
        TypeCheckError::new(self, span)
    }
}
