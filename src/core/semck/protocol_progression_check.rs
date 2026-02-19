//! Starter protocol progression/coherence checks (Tier C seed).
//!
//! This pass validates that each typestate handler's observed progression
//! events (emits + return-state edges) can be explained by at least one
//! protocol transition for `(role, state, selector)`.
//!
//! It intentionally stays narrow in v1:
//! - no global deadlock/progress proofs,
//! - no full path theorem checking,
//! - local transition-coherence checks only.

use std::collections::HashSet;

use crate::core::context::{
    ProtocolProgressionEvent, ProtocolProgressionFacts, SemCheckNormalizedContext,
};
use crate::core::protocol::{ProtocolStateFact, ProtocolTransitionFact};
use crate::core::semck::{push_error, SemCheckError, SEK};
use crate::core::types::Type;

pub(super) fn check(
    ctx: &SemCheckNormalizedContext,
    facts: &ProtocolProgressionFacts,
) -> Vec<SemCheckError> {
    let mut errors = Vec::new();

    for fact in &facts.handlers {
        let Some(state_fact) = lookup_protocol_state(
            ctx,
            &fact.entry_state.protocol_name,
            &fact.entry_state.role_name,
            &fact.entry_state.state_name,
        ) else {
            // Upstream resolve/typecheck guarantees protocol facts for valid
            // role impls. If missing, skip to avoid noisy symptom diagnostics.
            continue;
        };

        let candidates = matching_trigger_transitions(state_fact, &fact.selector_ty);
        if candidates.is_empty() {
            push_error(&mut errors, fact.span, SEK::ProtocolProgressionMissingTriggerTransition(
                    fact.typestate_name.clone(),
                    fact.entry_state.protocol_name.clone(),
                    fact.entry_state.role_name.clone(),
                    fact.entry_state.state_name.clone(),
                    fact.selector_ty.clone(),
                ));
            // Follow-on emit/return diagnostics would only be symptoms.
            continue;
        }

        // Build a compact transition-space summary once per handler to avoid
        // repeated nested scans for each observed event.
        let transition_summary = summarize_transition_space(&candidates);
        let mut seen_impossible_emits = HashSet::<(Type, String)>::new();
        let mut seen_impossible_returns = HashSet::<String>::new();

        for events in fact.cfg.node_events.values() {
            for event in events {
                match event {
                    ProtocolProgressionEvent::Emit(emit) => {
                        let Some(to_role_name) = emit.to_role_name.as_deref() else {
                            // Unbound destination is already handled by
                            // typecheck's destination-role validation.
                            continue;
                        };

                        if !transition_summary
                            .allowed_emits
                            .contains(&(emit.payload_ty.clone(), to_role_name.to_string()))
                            && seen_impossible_emits
                                .insert((emit.payload_ty.clone(), to_role_name.to_string()))
                        {
                            push_error(&mut errors, emit.span, SEK::ProtocolProgressionImpossibleEmit(
                                    fact.typestate_name.clone(),
                                    fact.entry_state.protocol_name.clone(),
                                    fact.entry_state.role_name.clone(),
                                    fact.entry_state.state_name.clone(),
                                    fact.selector_ty.clone(),
                                    emit.payload_ty.clone(),
                                    to_role_name.to_string(),
                                ));
                        }
                    }
                    ProtocolProgressionEvent::ReturnState(ret) => {
                        let Some(to_state_name) = ret.to_state_name.as_deref() else {
                            continue;
                        };
                        if !transition_summary
                            .allowed_next_states
                            .contains(to_state_name)
                            && seen_impossible_returns.insert(to_state_name.to_string())
                        {
                            push_error(&mut errors, ret.span, SEK::ProtocolProgressionImpossibleReturnState(
                                    fact.typestate_name.clone(),
                                    fact.entry_state.protocol_name.clone(),
                                    fact.entry_state.role_name.clone(),
                                    fact.entry_state.state_name.clone(),
                                    fact.selector_ty.clone(),
                                    to_state_name.to_string(),
                                ));
                        }
                    }
                }
            }
        }
    }

    errors
}

#[derive(Default)]
struct TransitionSummary {
    allowed_emits: HashSet<(Type, String)>,
    allowed_next_states: HashSet<String>,
}

fn summarize_transition_space(candidates: &[&ProtocolTransitionFact]) -> TransitionSummary {
    let mut summary = TransitionSummary::default();
    for transition in candidates {
        summary
            .allowed_next_states
            .insert(transition.next_state.clone());
        for effect in &transition.effects {
            if let Some(payload_ty) = &effect.payload_ty {
                summary
                    .allowed_emits
                    .insert((payload_ty.clone(), effect.to_role.clone()));
            }
        }
    }
    summary
}

fn lookup_protocol_state<'a>(
    ctx: &'a SemCheckNormalizedContext,
    protocol_name: &str,
    role_name: &str,
    state_name: &str,
) -> Option<&'a ProtocolStateFact> {
    ctx.protocol_index
        .protocols
        .get(protocol_name)?
        .roles
        .get(role_name)?
        .states
        .get(state_name)
}

fn matching_trigger_transitions<'a>(
    state: &'a ProtocolStateFact,
    selector_ty: &Type,
) -> Vec<&'a ProtocolTransitionFact> {
    state
        .transitions
        .iter()
        .filter(|transition| transition.trigger_payload_ty.as_ref() == Some(selector_ty))
        .collect()
}
