use thiserror::Error;

use crate::core::ast::ExprKind;
use crate::core::diag::{Span, SpannedError};
use crate::core::resolve::symbols::SymbolKind;

#[derive(Clone, Debug, Error)]
pub enum ResolveErrorKind {
    #[error("Symbol already defined in current scope: {0}")]
    SymbolAlreadyDefined(String),

    #[error("Undefined variable: {0}")]
    VarUndefined(String),

    #[error("Cannot assign to immutable variable: {0}")]
    VarImmutable(String),

    #[error("Undefined function: {0}")]
    FuncUndefined(String),

    #[error("Invalid assignment target. Expected an l-value, found: {0:?}")]
    InvalidAssignmentTarget(ExprKind),

    #[error("Invalid callee. Expected a function name, found: {0:?}")]
    InvalidCallee(ExprKind),

    #[error("Expected '{0}' to be a type, found {1}")]
    ExpectedType(String, SymbolKind),

    #[error("Expected '{0}' to be a trait, found {1}")]
    ExpectedTrait(String, SymbolKind),

    #[error("Undefined type: {0}")]
    TypeUndefined(String),

    #[error("Undefined trait: {0}")]
    TraitUndefined(String),

    #[error("Undefined struct: {0}")]
    StructUndefined(String),

    #[error("Undefined enum: {0}")]
    EnumUndefined(String),

    #[error("Undefined enum variant: {0}::{1}")]
    EnumVariantUndefined(String, String),

    #[error("Method declarations are only allowed on intrinsic types: {0}")]
    MethodDeclOnNonIntrinsicType(String),

    #[error("Method declaration '{0}' must be marked @intrinsic")]
    MethodDeclMissingIntrinsic(String),

    #[error("Unknown attribute `{0}`")]
    UnknownAttribute(String),

    #[error("Duplicate attribute `{0}`")]
    AttrDuplicate(String),

    #[error("Attribute `{0}` expects {1} args, found {2}")]
    AttrWrongArgCount(String, usize, usize),

    #[error("Attribute `{0}` has invalid argument type")]
    AttrWrongArgType(String),

    #[error("Attribute `{0}` is not allowed on {1}")]
    AttrNotAllowed(String, &'static str),

    #[error("Attribute `machines` is only allowed on top-level `fn main` definitions")]
    AttrMachinesRequiresMain,

    #[error("Duplicate requires alias: {0}")]
    DuplicateRequireAlias(String),

    #[error("Module-qualified access is not implemented yet: {0}.{1}")]
    ModuleQualifiedAccessUnsupported(String, String),

    #[error("Module `{0}` has no member `{1}`")]
    ModuleMemberUndefined(String, String),

    #[error("Typestate `{0}` must declare at least one state")]
    TypestateMissingState(String),

    #[error("Typestate `{0}` has duplicate state `{1}`")]
    TypestateDuplicateState(String, String),

    #[error("Typestate `{0}` has multiple `fields` blocks")]
    TypestateDuplicateFieldsBlock(String),

    #[error("State `{0}.{1}` has multiple `fields` blocks")]
    TypestateDuplicateStateFieldsBlock(String, String),

    #[error("State field `{2}` in `{0}.{1}` shadows a typestate field")]
    TypestateStateFieldShadowsCarriedField(String, String, String),

    #[error("Typestate `{0}` is missing required `fn new(...)` constructor")]
    TypestateMissingNew(String),

    #[error("Typestate `{0}` has duplicate `new` constructors")]
    TypestateDuplicateNew(String),

    #[error(
        "Typestate `{0}` constructor return type must be a declared state (or `State | Error...`)"
    )]
    TypestateInvalidNewReturn(String),

    #[error("Transition `{0}.{1}::{2}` must not declare explicit `self` parameter")]
    TypestateExplicitSelfNotAllowed(String, String, String),

    #[error(
        "Transition `{0}.{1}::{2}` return type must be a declared state (or `State | Error...`)"
    )]
    TypestateInvalidTransitionReturn(String, String, String),

    #[error(
        "Typestate `{0}` `on` handler return type must be a declared state, `stay`, or `State | Error...`"
    )]
    TypestateInvalidOnHandlerReturn(String),

    #[error(
        "State `{0}.{1}` `on` handler return type must be a declared state, `stay`, or `State | Error...`"
    )]
    TypestateInvalidStateOnHandlerReturn(String, String),

    #[error("State `{0}.{1}` has duplicate transition `{2}`")]
    TypestateDuplicateTransition(String, String, String),

    #[error("Unknown typestate state attribute: `{2}` on state `{0}.{1}`")]
    TypestateUnknownStateAttribute(String, String, String),

    #[error("Final state `{0}.{1}` must not declare transition methods")]
    TypestateFinalStateHasTransition(String, String),

    #[error("Final state `{0}.{1}` must not declare `on` handlers")]
    TypestateFinalStateHasHandler(String, String),

    #[error("State literal `{0}` is only allowed inside typestate constructor/transition methods")]
    TypestateStateLiteralOutsideTypestate(String),

    #[error("Managed typestate spawn requires `@machines fn main(...)` entrypoint opt-in")]
    TypestateSpawnRequiresMachinesOptIn,

    #[error("Linear type `{0}` must declare at least one state")]
    LinearNoStates(String),

    #[error("Linear type `{0}` must declare at least one non-`@final` state")]
    LinearNoNonFinalStates(String),

    #[error("Linear type `{0}` has duplicate state `{1}`")]
    LinearDuplicateState(String, String),

    #[error("Unknown state `{2}` referenced by action `{1}` in linear type `{0}`")]
    LinearUnknownStateInAction(String, String, String),

    #[error("Unknown state `{2}` referenced by trigger `{1}` in linear type `{0}`")]
    LinearUnknownStateInTrigger(String, String, String),

    #[error("Linear type `{0}` has duplicate action `{1}` for source state `{2}`")]
    LinearDuplicateAction(String, String, String),

    #[error("Linear type `{0}` has duplicate trigger `{1}` for source state `{2}`")]
    LinearDuplicateTrigger(String, String, String),

    #[error("Transition `{1}` in linear type `{0}` uses final state `{2}` as a source")]
    LinearFinalStateAsSource(String, String, String),

    #[error("Role `{1}` in linear type `{0}` references unknown action `{2}`")]
    LinearUnknownActionInRole(String, String, String),

    #[error("Unknown state attribute `{2}` on linear type `{0}` state `{1}`")]
    LinearUnknownStateAttribute(String, String, String),

    #[error(
        "Declared action `{1}` for source state `{2}` in linear type `{0}` has no matching method"
    )]
    LinearMethodMissingAction(String, String, String),

    #[error(
        "Methods implementing overloaded action `{1}` in linear type `{0}` must annotate the receiver state"
    )]
    LinearMethodAmbiguousReceiver(String, String),

    #[error("Method `{1}` in linear type `{0}` expects receiver state `{2}`, found `{3}`")]
    LinearMethodSourceStateMismatch(String, String, String, String),

    #[error("Method `{1}` in linear type `{0}` must return `{2}`, found `{3}`")]
    LinearMethodTargetStateMismatch(String, String, String, String),

    #[error("Method `{1}` in linear type `{0}` does not match the declared action parameters")]
    LinearMethodParamMismatch(String, String),

    #[error("Use after consume of linear value `{0}`")]
    LinearUseAfterConsume(String),

    #[error("Machine `{0}` hosts undefined type `{1}`")]
    MachineHostedTypeUndefined(String, String),

    #[error("Machine `{0}` can only host `@linear type` `{1}`")]
    MachineHostedTypeNotLinear(String, String),

    #[error("Machine `{0}` key field `{2}` is not declared on hosted type `{1}`")]
    MachineInvalidKeyField(String, String, String),

    #[error("Machine `{0}` must implement trigger handler `{1}`")]
    MachineMissingTriggerHandler(String, String),

    #[error(
        "Machine `{0}` declares {1} handler `{2}` that is not declared on the hosted linear type"
    )]
    MachineExtraHandler(String, &'static str, String),

    #[error("Machine `{0}` {1} handler `{2}` does not match the declared transition signature")]
    MachineHandlerTypeMismatch(String, &'static str, String),

    #[error("Machine `{0}` action override `{1}` must preserve the base action's error cases")]
    MachineOverrideErrorSubset(String, String),

    #[error(
        "Machine `{0}` action override `{1}` cannot use `emit request` or `reply`; hosted action overrides currently support only `emit Send(...)`"
    )]
    MachineHostedActionEmitUnsupported(String, String),

    #[error(
        "Machine `{0}` on-handler payload `{1}` is not supported by hosted mailbox ingress; supported payloads are `struct {{}}`, `struct {{ field: u64 }}`, or `struct {{ a: u64, b: u64 }}`"
    )]
    MachineHostedOnPayloadUnsupported(String, String),
}

pub type ResolveError = SpannedError<ResolveErrorKind>;

impl ResolveErrorKind {
    pub fn at(self, span: Span) -> ResolveError {
        ResolveError::new(self, span)
    }
}
