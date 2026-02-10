use std::path::Path;

use crate::analysis::snapshot::SourceStore;

#[test]
fn overlay_text_shadows_disk_text_in_snapshot() {
    let mut store = SourceStore::new();
    let file_id = store.upsert_disk_text("src/main.mc", "fn main() {}");
    let rev_after_disk = store.revision();

    store.set_overlay(file_id, "fn main() { let x = 1; }");
    assert!(store.revision() > rev_after_disk);

    let snap = store.snapshot();
    assert_eq!(snap.revision(), store.revision());
    assert_eq!(
        snap.text(file_id).as_deref(),
        Some("fn main() { let x = 1; }")
    );
}

#[test]
fn clear_overlay_reveals_disk_text() {
    let mut store = SourceStore::new();
    let file_id = store.upsert_disk_text("src/lib.mc", "fn a() {}");
    store.set_overlay(file_id, "fn b() {}");
    let rev_with_overlay = store.revision();

    store.clear_overlay(file_id);
    assert!(store.revision() > rev_with_overlay);
    assert_eq!(store.text(file_id).as_deref(), Some("fn a() {}"));
}

#[test]
fn unchanged_updates_do_not_bump_revision() {
    let mut store = SourceStore::new();
    let file_id = store.upsert_disk_text("src/mod.mc", "fn c() {}");
    let rev1 = store.revision();

    store.upsert_disk_text("src/mod.mc", "fn c() {}");
    assert_eq!(store.revision(), rev1);

    store.set_overlay(file_id, "fn d() {}");
    let rev2 = store.revision();
    store.set_overlay(file_id, "fn d() {}");
    assert_eq!(store.revision(), rev2);

    store.clear_overlay(file_id);
    let rev3 = store.revision();
    store.clear_overlay(file_id);
    assert_eq!(store.revision(), rev3);
}

#[test]
fn path_and_file_id_lookup_round_trip() {
    let mut store = SourceStore::new();
    let file_id = store.upsert_disk_text("examples/demo.mc", "fn main() -> u64 { 0 }");
    let snap = store.snapshot();

    let got_id = snap
        .file_id(Path::new("examples/demo.mc"))
        .expect("file id should exist");
    assert_eq!(got_id, file_id);
    let got_path = snap.path(file_id).expect("path should exist");
    assert_eq!(got_path, Path::new("examples/demo.mc"));
}
