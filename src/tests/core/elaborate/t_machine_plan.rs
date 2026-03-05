use std::collections::HashMap;

use super::*;
use crate::core::resolve::DefId;
use crate::core::types::TypeCache;

fn dummy_type_id() -> TypeId {
    let mut cache = TypeCache::new();
    cache.intern(Type::Bool)
}

fn named_payload(name: &str) -> Type {
    Type::Struct {
        name: name.to_string(),
        fields: Vec::new(),
    }
}

#[test]
fn fallback_prefix_len_stops_at_first_state_divergence() {
    let make_handler = |ordinal, event| HandlerPlanSeed {
        def_id: DefId(ordinal as u32),
        ordinal,
        state_name: "S".to_string(),
        state_layout_ty: dummy_type_id(),
        event_key: MachineEventKeyPlan::Payload {
            payload_ty: named_payload(event),
        },
        request_site_key_match: None,
        provenance_param_index: None,
        payload_layout_ty: dummy_type_id(),
        next_state_layout_ty: dummy_type_id(),
    };

    let state_a = StatePlanSeed {
        state_name: "A".to_string(),
        state_type_def_id: DefId(1),
        state_layout_ty: dummy_type_id(),
        is_final: false,
        handlers: vec![make_handler(0, "Ping"), make_handler(1, "Pong")],
    };
    let state_b = StatePlanSeed {
        state_name: "B".to_string(),
        state_type_def_id: DefId(2),
        state_layout_ty: dummy_type_id(),
        is_final: false,
        handlers: vec![make_handler(0, "Ping")],
    };

    assert_eq!(shared_fallback_prefix_len(&[&state_a, &state_b]), 1);
}

#[test]
fn dispatch_rows_include_local_and_fallback_channels() {
    let state_tag_by_name = HashMap::from([("A".to_string(), 1), ("B".to_string(), 2)]);
    let event_kinds = vec![
        MachineEventKindPlan {
            key: MachineEventKeyPlan::Payload {
                payload_ty: named_payload("Ping"),
            },
            payload_layout_ty: dummy_type_id(),
            kind: 1,
        },
        MachineEventKindPlan {
            key: MachineEventKeyPlan::Payload {
                payload_ty: named_payload("Pong"),
            },
            payload_layout_ty: dummy_type_id(),
            kind: 2,
        },
    ];

    let ping = HandlerPlanSeed {
        def_id: DefId(100),
        ordinal: 0,
        state_name: "A".to_string(),
        state_layout_ty: dummy_type_id(),
        event_key: event_kinds[0].key.clone(),
        request_site_key_match: None,
        provenance_param_index: None,
        payload_layout_ty: dummy_type_id(),
        next_state_layout_ty: dummy_type_id(),
    };
    let pong_local = HandlerPlanSeed {
        def_id: DefId(101),
        ordinal: 1,
        state_name: "A".to_string(),
        state_layout_ty: dummy_type_id(),
        event_key: event_kinds[1].key.clone(),
        request_site_key_match: None,
        provenance_param_index: None,
        payload_layout_ty: dummy_type_id(),
        next_state_layout_ty: dummy_type_id(),
    };

    let state_a = StatePlanSeed {
        state_name: "A".to_string(),
        state_type_def_id: DefId(1),
        state_layout_ty: dummy_type_id(),
        is_final: false,
        handlers: vec![ping.clone(), pong_local],
    };
    let state_b = StatePlanSeed {
        state_name: "B".to_string(),
        state_type_def_id: DefId(2),
        state_layout_ty: dummy_type_id(),
        is_final: false,
        handlers: vec![HandlerPlanSeed {
            state_name: "B".to_string(),
            ..ping
        }],
    };

    let fallback_map = HashMap::from([(
        DispatchKey {
            event_stable_key: event_kinds[0].key.stable_key(),
            site: None,
        },
        DefId(100),
    )]);
    let rows = build_dispatch_table(
        &[&state_a, &state_b],
        &state_tag_by_name,
        &event_kinds,
        &fallback_map,
        1,
    );

    // A/Ping: fallback only; A/Pong: local only.
    assert_eq!(
        rows.iter()
            .find(|row| row.state_tag == 1 && row.event_kind == 1)
            .and_then(|row| row.typestate_fallback_thunk),
        Some(DefId(100))
    );
    assert_eq!(
        rows.iter()
            .find(|row| row.state_tag == 1 && row.event_kind == 2)
            .and_then(|row| row.state_local_thunk),
        Some(DefId(101))
    );
    // B/Pong: no local and no fallback.
    let b_pong = rows
        .iter()
        .find(|row| row.state_tag == 2 && row.event_kind == 2)
        .expect("expected B/Pong row");
    assert!(b_pong.state_local_thunk.is_none());
    assert!(b_pong.typestate_fallback_thunk.is_none());
}

#[test]
fn fallback_prefix_len_requires_shared_handler_identity() {
    let event_key = MachineEventKeyPlan::Payload {
        payload_ty: named_payload("Ping"),
    };
    let state_a = StatePlanSeed {
        state_name: "A".to_string(),
        state_type_def_id: DefId(1),
        state_layout_ty: dummy_type_id(),
        is_final: false,
        handlers: vec![HandlerPlanSeed {
            def_id: DefId(100),
            ordinal: 0,
            state_name: "A".to_string(),
            state_layout_ty: dummy_type_id(),
            event_key: event_key.clone(),
            request_site_key_match: None,
            provenance_param_index: None,
            payload_layout_ty: dummy_type_id(),
            next_state_layout_ty: dummy_type_id(),
        }],
    };
    let state_b = StatePlanSeed {
        state_name: "B".to_string(),
        state_type_def_id: DefId(2),
        state_layout_ty: dummy_type_id(),
        is_final: false,
        handlers: vec![HandlerPlanSeed {
            def_id: DefId(200),
            ordinal: 0,
            state_name: "B".to_string(),
            state_layout_ty: dummy_type_id(),
            event_key,
            request_site_key_match: None,
            provenance_param_index: None,
            payload_layout_ty: dummy_type_id(),
            next_state_layout_ty: dummy_type_id(),
        }],
    };

    assert_eq!(shared_fallback_prefix_len(&[&state_a, &state_b]), 0);
}
