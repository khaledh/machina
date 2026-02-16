use thiserror::Error;

use crate::core::diag::Span;
use crate::core::resolve::symbols::SymbolKind;
use crate::core::tree::parsed::ExprKind;

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

    #[error("Attribute `machines` is only allowed on top-level `fn main` definitions")]
    AttrMachinesRequiresMain(Span),

    #[error("Duplicate requires alias: {0}")]
    DuplicateRequireAlias(String, Span),

    #[error("Module-qualified access is not implemented yet: {0}.{1}")]
    ModuleQualifiedAccessUnsupported(String, String, Span),

    #[error("Module `{0}` has no member `{1}`")]
    ModuleMemberUndefined(String, String, Span),

    #[error("Undefined protocol role: {0}")]
    ProtocolRoleUndefined(String, Span),

    #[error("Expected '{0}' to be a protocol role, found {1}")]
    ExpectedProtocolRole(String, SymbolKind, Span),

    #[error("Protocol `{0}` flow references undefined role `{1}`")]
    ProtocolFlowRoleUndefined(String, String, Span),

    #[error("Typestate `{0}` role implementation path must be `<Protocol>::<Role>`, found `{1}`")]
    TypestateRoleImplMalformedPath(String, String, Span),

    #[error("Typestate `{0}` references undefined protocol role `{1}`")]
    TypestateRoleImplRoleUndefined(String, String, Span),

    #[error("Typestate `{0}` expected `{1}` to resolve to a protocol role, found {2}")]
    TypestateRoleImplExpectedRole(String, String, SymbolKind, Span),

    #[error("Typestate `{0}` must declare at least one state")]
    TypestateMissingState(String, Span),

    #[error("Typestate `{0}` has duplicate state `{1}`")]
    TypestateDuplicateState(String, String, Span),

    #[error("Typestate `{0}` has multiple `fields` blocks")]
    TypestateDuplicateFieldsBlock(String, Span),

    #[error("State `{0}.{1}` has multiple `fields` blocks")]
    TypestateDuplicateStateFieldsBlock(String, String, Span),

    #[error("State field `{2}` in `{0}.{1}` shadows a typestate field")]
    TypestateStateFieldShadowsCarriedField(String, String, String, Span),

    #[error("Typestate `{0}` is missing required `fn new(...)` constructor")]
    TypestateMissingNew(String, Span),

    #[error("Typestate `{0}` has duplicate `new` constructors")]
    TypestateDuplicateNew(String, Span),

    #[error(
        "Typestate `{0}` constructor return type must be a declared state (or `State | Error...`)"
    )]
    TypestateInvalidNewReturn(String, Span),

    #[error("Transition `{0}.{1}::{2}` must not declare explicit `self` parameter")]
    TypestateExplicitSelfNotAllowed(String, String, String, Span),

    #[error(
        "Transition `{0}.{1}::{2}` return type must be a declared state (or `State | Error...`)"
    )]
    TypestateInvalidTransitionReturn(String, String, String, Span),

    #[error(
        "Typestate `{0}` `on` handler return type must be a declared state, `stay`, or `State | Error...`"
    )]
    TypestateInvalidOnHandlerReturn(String, Span),

    #[error(
        "State `{0}.{1}` `on` handler return type must be a declared state, `stay`, or `State | Error...`"
    )]
    TypestateInvalidStateOnHandlerReturn(String, String, Span),

    #[error("State `{0}.{1}` has duplicate transition `{2}`")]
    TypestateDuplicateTransition(String, String, String, Span),

    #[error("State literal `{0}` is only allowed inside typestate constructor/transition methods")]
    TypestateStateLiteralOutsideTypestate(String, Span),

    #[error("Managed typestate spawn requires `@machines fn main(...)` entrypoint opt-in")]
    TypestateSpawnRequiresMachinesOptIn(Span),
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
            ResolveError::AttrMachinesRequiresMain(span) => *span,
            ResolveError::DuplicateRequireAlias(_, span) => *span,
            ResolveError::ModuleQualifiedAccessUnsupported(_, _, span) => *span,
            ResolveError::ModuleMemberUndefined(_, _, span) => *span,
            ResolveError::ProtocolRoleUndefined(_, span) => *span,
            ResolveError::ExpectedProtocolRole(_, _, span) => *span,
            ResolveError::ProtocolFlowRoleUndefined(_, _, span) => *span,
            ResolveError::TypestateRoleImplMalformedPath(_, _, span) => *span,
            ResolveError::TypestateRoleImplRoleUndefined(_, _, span) => *span,
            ResolveError::TypestateRoleImplExpectedRole(_, _, _, span) => *span,
            ResolveError::TypestateMissingState(_, span) => *span,
            ResolveError::TypestateDuplicateState(_, _, span) => *span,
            ResolveError::TypestateDuplicateFieldsBlock(_, span) => *span,
            ResolveError::TypestateDuplicateStateFieldsBlock(_, _, span) => *span,
            ResolveError::TypestateStateFieldShadowsCarriedField(_, _, _, span) => *span,
            ResolveError::TypestateMissingNew(_, span) => *span,
            ResolveError::TypestateDuplicateNew(_, span) => *span,
            ResolveError::TypestateInvalidNewReturn(_, span) => *span,
            ResolveError::TypestateExplicitSelfNotAllowed(_, _, _, span) => *span,
            ResolveError::TypestateInvalidTransitionReturn(_, _, _, span) => *span,
            ResolveError::TypestateInvalidOnHandlerReturn(_, span) => *span,
            ResolveError::TypestateInvalidStateOnHandlerReturn(_, _, span) => *span,
            ResolveError::TypestateDuplicateTransition(_, _, _, span) => *span,
            ResolveError::TypestateStateLiteralOutsideTypestate(_, span) => *span,
            ResolveError::TypestateSpawnRequiresMachinesOptIn(span) => *span,
        }
    }
}
