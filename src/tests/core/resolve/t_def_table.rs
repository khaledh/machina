use super::*;
use crate::core::resolve::FuncAttrs;
use std::path::PathBuf;

#[test]
fn def_location_uses_override_when_present() {
    let def_id = DefId(0);
    let mut builder = DefTableBuilder::new();
    builder.record_def(
        Def {
            id: def_id,
            name: "f".to_string(),
            kind: DefKind::FuncDef {
                attrs: FuncAttrs::default(),
            },
        },
        NodeId(1),
        Span::default(),
    );
    let mut table = builder.finish();
    let source_path = PathBuf::from("main.mc");
    table.set_source_path(Some(source_path.clone()));

    let default_loc = table
        .lookup_def_location(def_id)
        .expect("expected default location");
    assert_eq!(default_loc.path.as_deref(), Some(source_path.as_path()));

    let override_loc = DefLocation {
        path: Some(PathBuf::from("std/prelude.mc")),
        span: Span::default(),
    };
    table.set_def_location(def_id, override_loc.clone());

    let resolved_loc = table
        .lookup_def_location(def_id)
        .expect("expected override location");
    assert_eq!(resolved_loc, override_loc);
}

#[test]
fn synthetic_def_without_span_has_no_location() {
    let mut table = DefTable::new(Vec::new());
    let def_id = table.add_def(
        "__synthetic".to_string(),
        DefKind::FuncDef {
            attrs: FuncAttrs::default(),
        },
    );
    table.set_source_path(Some(PathBuf::from("main.mc")));
    assert!(table.lookup_def_location(def_id).is_none());
}
