//! Protocol semantic side tables.
//!
//! This module centralizes protocol facts extracted from resolved trees so
//! downstream stages (typecheck, analysis) can consume one canonical view
//! instead of re-scanning protocol AST nodes ad hoc.

pub(crate) mod event_extract;
pub mod index;

pub use index::{
    ProtocolEffectFact, ProtocolFact, ProtocolIndex, ProtocolRequestContractFact, ProtocolRoleFact,
    ProtocolRoleShape, ProtocolStateFact, ProtocolStateShape, ProtocolTransitionFact,
    TypestateProtocolBindingFact, TypestateProtocolPeerBindingFact, build_protocol_index,
};
