use std::collections::HashMap;
use std::sync::Arc;

use crate::analysis::facts::{
    DefTableOverlay, FactOrigin, FactOverlayMap, SyntheticReason, TypeMapOverlay,
};
use crate::resolve::def_table::DefTable;
use crate::resolve::{Def, DefId, DefKind};
use crate::tree::NodeId;
use crate::types::Type;

#[test]
fn overlay_returns_base_when_no_synthetic_value_exists() {
    let mut base = HashMap::new();
    base.insert("x".to_string(), 1u64);
    let facts = FactOverlayMap::new(base);

    assert_eq!(facts.get(&"x".to_string()), Some(1));
    assert_eq!(facts.origin(&"x".to_string()), Some(FactOrigin::Source));
    assert_eq!(facts.synthetic_len(), 0);
}

#[test]
fn synthetic_overlay_overrides_base_value_and_origin() {
    let mut base = HashMap::new();
    base.insert("x".to_string(), 1u64);
    let mut facts = FactOverlayMap::new(base);

    facts.insert_synthetic(
        "x".to_string(),
        99,
        "elaborate",
        SyntheticReason::ElaborateSyntheticNode,
    );

    assert_eq!(facts.get(&"x".to_string()), Some(99));
    assert_eq!(
        facts.origin(&"x".to_string()),
        Some(FactOrigin::Synthetic {
            stage: "elaborate".to_string(),
            reason: SyntheticReason::ElaborateSyntheticNode
        })
    );
    assert_eq!(facts.synthetic_len(), 1);
}

#[test]
fn shared_base_can_be_reused_with_independent_overlays() {
    let mut base = HashMap::new();
    base.insert(1u32, "a".to_string());
    let shared = Arc::new(base);

    let mut facts_a = FactOverlayMap::with_shared_base(Arc::clone(&shared));
    let facts_b = FactOverlayMap::with_shared_base(Arc::clone(&shared));

    facts_a.insert_synthetic(
        2,
        "b".to_string(),
        "normalize",
        SyntheticReason::NormalizeCoercion,
    );

    assert_eq!(facts_a.get(&1), Some("a".to_string()));
    assert_eq!(facts_a.get(&2), Some("b".to_string()));
    assert_eq!(facts_b.get(&1), Some("a".to_string()));
    assert_eq!(facts_b.get(&2), None);
}

#[test]
fn clearing_overlay_restores_base_visibility() {
    let mut base = HashMap::new();
    base.insert("k".to_string(), 10i32);
    let mut facts = FactOverlayMap::new(base);
    facts.insert_synthetic(
        "k".to_string(),
        20i32,
        "closure",
        SyntheticReason::ClosureLowering,
    );
    assert_eq!(facts.get(&"k".to_string()), Some(20));

    facts.clear_overlay();
    assert_eq!(facts.get(&"k".to_string()), Some(10));
    assert_eq!(facts.origin(&"k".to_string()), Some(FactOrigin::Source));
}

#[test]
fn type_map_overlay_tracks_source_and_synthetic_origins() {
    let source_def = Def {
        id: DefId(0),
        name: "x".to_string(),
        kind: DefKind::LocalVar {
            nrvo_eligible: false,
            is_mutable: false,
        },
    };
    let mut base = crate::typecheck::type_map::TypeMapBuilder::new();
    base.record_def_type(source_def.clone(), Type::uint(64));
    let base = base.finish().0;

    let mut overlay = TypeMapOverlay::new(base);
    assert_eq!(
        overlay.origin_for_def(source_def.id),
        Some(FactOrigin::Source)
    );

    let synth_node = NodeId(42);
    let _ = overlay.insert_node_type(
        synth_node,
        Type::Bool,
        "normalize",
        SyntheticReason::NormalizeCoercion,
    );
    assert_eq!(
        overlay.origin_for_node(synth_node),
        Some(FactOrigin::Synthetic {
            stage: "normalize".to_string(),
            reason: SyntheticReason::NormalizeCoercion
        })
    );
}

#[test]
fn def_table_overlay_tracks_synthetic_defs() {
    let mut table = DefTable::new(vec![Def {
        id: DefId(0),
        name: "Base".to_string(),
        kind: DefKind::TypeDef {
            attrs: Default::default(),
        },
    }]);
    let base_id = table.next_def_id();
    let _ = table.add_def(
        "f".to_string(),
        DefKind::FuncDef {
            attrs: Default::default(),
        },
    );

    let mut overlay = DefTableOverlay::new(table);
    let synth = overlay.add_def(
        "Synth".to_string(),
        DefKind::TypeDef {
            attrs: Default::default(),
        },
        "elaborate",
        SyntheticReason::ElaborateSyntheticNode,
    );

    assert_eq!(overlay.origin_for_def(base_id), Some(FactOrigin::Source));
    assert_eq!(
        overlay.origin_for_def(synth),
        Some(FactOrigin::Synthetic {
            stage: "elaborate".to_string(),
            reason: SyntheticReason::ElaborateSyntheticNode
        })
    );
}
