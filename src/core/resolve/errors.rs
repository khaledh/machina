use thiserror::Error;

use crate::core::diag::{Span, SpannedError};
use crate::core::resolve::symbols::SymbolKind;
use crate::core::tree::parsed::ExprKind;

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

    #[error("Undefined protocol role: {0}")]
    ProtocolRoleUndefined(String),

    #[error("Expected '{0}' to be a protocol role, found {1}")]
    ExpectedProtocolRole(String, SymbolKind),

    #[error("Undefined protocol role `{1}` in request contract of protocol `{0}`")]
    ProtocolRequestContractRoleUndefined(String, String),

    #[error(
        "Undefined trigger source role `{3}` in protocol `{0}` role `{1}` state `{2}` transition"
    )]
    ProtocolTransitionSourceRoleUndefined(String, String, String, String),

    #[error("Undefined effect destination role `{3}` in protocol `{0}` role `{1}` state `{2}`")]
    ProtocolTransitionEffectRoleUndefined(String, String, String, String),

    #[error("Undefined next state `{3}` in protocol `{0}` role `{1}` state `{2}` transition")]
    ProtocolTransitionNextStateUndefined(String, String, String, String),

    #[error("Ambiguous transition trigger `{3}@{4}` in protocol `{0}` role `{1}` state `{2}`")]
    ProtocolTransitionTriggerConflict(String, String, String, String, String),

    #[error("Typestate `{0}` role implementation path must be `<Protocol>::<Role>`, found `{1}`")]
    TypestateRoleImplMalformedPath(String, String),

    #[error("Typestate `{0}` references undefined protocol role `{1}`")]
    TypestateRoleImplRoleUndefined(String, String),

    #[error("Typestate `{0}` expected `{1}` to resolve to a protocol role, found {2}")]
    TypestateRoleImplExpectedRole(String, String, SymbolKind),

    #[error("Typestate `{0}` field `{1}` role binding must use `Machine<...>` type")]
    TypestateRoleBindingInvalidType(String, String),

    #[error("Typestate `{0}` field `{1}` binds undefined protocol role `{2}`")]
    TypestateRoleBindingRoleUndefined(String, String, String),

    #[error("Typestate `{0}` binds protocol role `{1}` more than once")]
    TypestateRoleBindingDuplicateRole(String, String),

    #[error("Typestate `{0}` is missing protocol peer-role binding for `{1}`")]
    TypestateRoleBindingMissing(String, String),

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
}

pub type ResolveError = SpannedError<ResolveErrorKind>;

impl ResolveErrorKind {
    pub fn at(self, span: Span) -> ResolveError {
        ResolveError::new(self, span)
    }
}
