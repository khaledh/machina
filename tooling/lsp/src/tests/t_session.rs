use super::{ANALYSIS_FILE_ID_KEY, AnalysisSession, DiagnosticValue, SessionError, uri_to_path};
use std::fs;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

#[test]
fn uri_to_path_accepts_file_uri() {
    let path = uri_to_path("file:///tmp/main.mc").expect("file uri should parse");
    assert_eq!(path, PathBuf::from("/tmp/main.mc"));
}

#[test]
fn uri_to_path_rejects_non_file_uri() {
    let err = uri_to_path("untitled:main.mc").expect_err("non-file uri should fail");
    assert_eq!(
        err,
        SessionError::UnsupportedUri("untitled:main.mc".to_string())
    );
}

#[test]
fn open_change_close_updates_document_state() {
    let mut session = AnalysisSession::new();
    let uri = "file:///tmp/session-open-change-close.mc";

    let file_id = session
        .open_document(uri, 1, "fn main() {}")
        .expect("open should succeed");
    let doc = session.lookup_document(uri).expect("doc should be tracked");
    assert_eq!(doc.file_id, file_id);
    assert_eq!(doc.version, 1);

    session
        .change_document(uri, 2, "fn main() { let x = 1; }")
        .expect("change should succeed");
    let doc = session
        .lookup_document(uri)
        .expect("doc should still be tracked");
    assert_eq!(doc.version, 2);

    session.close_document(uri).expect("close should succeed");
    assert!(matches!(
        session.lookup_document(uri),
        Err(SessionError::UnknownUri(_))
    ));
}

#[test]
fn snapshot_revision_increases_across_edits() {
    let mut session = AnalysisSession::new();
    let uri = "file:///tmp/session-revision.mc";
    session
        .open_document(uri, 1, "fn main() {}")
        .expect("open should succeed");
    let rev1 = session.snapshot().revision();
    session
        .change_document(uri, 2, "fn main() { let x = 1; }")
        .expect("change should succeed");
    let rev2 = session.snapshot().revision();
    assert!(rev2 > rev1);
}

#[test]
fn diagnostics_are_overlay_aware() {
    let mut session = AnalysisSession::new();
    let uri = "file:///tmp/session-diagnostics.mc";
    session
        .open_document(uri, 1, "fn main() {}")
        .expect("open should succeed");
    let clean = session
        .diagnostics_for_uri(uri)
        .expect("diagnostics query should succeed");
    assert!(clean.is_empty());

    session
        .change_document(uri, 2, "fn main(")
        .expect("change should succeed");
    let broken = session
        .diagnostics_for_uri(uri)
        .expect("diagnostics query should succeed");
    assert!(!broken.is_empty());
}

#[test]
fn close_clears_overlay_and_stops_tracking() {
    let mut session = AnalysisSession::new();
    let uri = "file:///tmp/session-close.mc";
    session
        .open_document(uri, 1, "fn main() {")
        .expect("open should succeed");
    session.close_document(uri).expect("close should succeed");
    assert!(matches!(
        session.diagnostics_for_uri(uri),
        Err(SessionError::UnknownUri(_))
    ));
}

#[test]
fn change_document_rejects_stale_version() {
    let mut session = AnalysisSession::new();
    let uri = "file:///tmp/session-stale-version.mc";
    session
        .open_document(uri, 4, "fn main() {}")
        .expect("open should succeed");
    let err = session
        .change_document(uri, 3, "fn main() { let x = 1; }")
        .expect_err("stale version should be rejected");
    assert!(matches!(
        err,
        SessionError::StaleVersion {
            expected: 4,
            found: 3
        }
    ));
}

#[test]
fn file_id_for_uri_tracks_open_documents() {
    let mut session = AnalysisSession::new();
    let uri = "file:///tmp/session-file-id.mc";
    let file_id = session
        .open_document(uri, 1, "fn main() {}")
        .expect("open should succeed");
    let mapped = session
        .file_id_for_uri(uri)
        .expect("file id should be available");
    assert_eq!(mapped, file_id);
}

#[test]
fn diagnostics_for_uri_if_version_drops_stale_results() {
    let mut session = AnalysisSession::new();
    let uri = "file:///tmp/session-stale-diag.mc";
    session
        .open_document(uri, 1, "fn main() {}")
        .expect("open should succeed");
    session
        .change_document(uri, 2, "fn main(")
        .expect("change should succeed");
    let stale = session
        .diagnostics_for_uri_if_version(uri, 1)
        .expect("query should succeed");
    assert!(stale.is_none(), "stale version should be dropped");
}

#[test]
fn diagnostics_for_uri_filters_dependency_module_diagnostics() {
    let run_id = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be monotonic")
        .as_nanos();
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_lsp_diag_filter_{}_{}",
        std::process::id(),
        run_id
    ));
    let app_dir = temp_dir.join("app");
    fs::create_dir_all(&app_dir).expect("failed to create temp module ast");

    let entry_path = temp_dir.join("main.mc");
    let dep_path = app_dir.join("dep.mc");
    let entry_source = r#"
requires {
    app::dep as dep
}

fn main() -> u64 {
    dep::value()
}
"#;
    let dep_source = r#"
fn value( {
"#;
    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

    let mut session = AnalysisSession::new();
    let uri = format!("file://{}", entry_path.to_string_lossy());
    session
        .open_document(&uri, 1, entry_source)
        .expect("open should succeed");
    let entry_file_id = session
        .lookup_document(&uri)
        .expect("entry doc should be tracked")
        .file_id;
    let diagnostics = session
        .diagnostics_for_uri(&uri)
        .expect("diagnostics query should succeed");

    for diag in diagnostics {
        let source_file_id = diag
            .metadata
            .get(ANALYSIS_FILE_ID_KEY)
            .expect("program diagnostics should include source file id metadata");
        assert_eq!(
            source_file_id,
            &DiagnosticValue::Number(entry_file_id.0 as i64),
            "diagnostic from dependency leaked into entry file publish list"
        );
    }

    let _ = fs::remove_dir_all(&temp_dir);
}

#[test]
fn diagnostics_for_uri_resolves_symbol_imports() {
    let run_id = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be monotonic")
        .as_nanos();
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_lsp_symbol_import_{}_{}",
        std::process::id(),
        run_id
    ));
    fs::create_dir_all(&temp_dir).expect("failed to create temp dir");
    let entry_path = temp_dir.join("hello.mc");
    let source = r#"
requires {
    std::io::println
}

fn main() {
    println("hello");
}
"#;
    fs::write(&entry_path, source).expect("failed to write source file");

    let mut session = AnalysisSession::new();
    let uri = format!("file://{}", entry_path.to_string_lossy());
    session
        .open_document(&uri, 1, source)
        .expect("open should succeed");
    let diagnostics = session
        .diagnostics_for_uri(&uri)
        .expect("diagnostics query should succeed");
    assert!(
        diagnostics.is_empty(),
        "symbol import should resolve; diagnostics: {diagnostics:#?}"
    );

    let _ = fs::remove_dir_all(&temp_dir);
}
