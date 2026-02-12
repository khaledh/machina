//! Editor-agnostic analysis session service.
//!
//! This layer owns open-document lifecycle, overlay state, and query-facing
//! snapshot access. It intentionally has no JSON-RPC/LSP dependency.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use machina::analysis::db::AnalysisDb;
use machina::analysis::diagnostics::{ANALYSIS_FILE_PATH_KEY, Diagnostic, DiagnosticValue};
use machina::analysis::module_graph::ModuleGraph;
use machina::analysis::query::{CancellationToken, QueryCancelled, QueryResult};
use machina::analysis::snapshot::{AnalysisSnapshot, FileId};
use machina::capsule::ModuleId;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentState {
    pub file_id: FileId,
    pub version: i32,
    pub path: PathBuf,
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum SessionError {
    #[error("unsupported uri: {0}")]
    UnsupportedUri(String),
    #[error("unknown uri: {0}")]
    UnknownUri(String),
    #[error("query cancelled")]
    Cancelled,
    #[error("stale document version: expected >= {expected}, found {found}")]
    StaleVersion { expected: i32, found: i32 },
}

impl From<QueryCancelled> for SessionError {
    fn from(_: QueryCancelled) -> Self {
        SessionError::Cancelled
    }
}

pub type SessionResult<T> = Result<T, SessionError>;

pub struct AnalysisSession {
    db: AnalysisDb,
    docs: HashMap<String, DocumentState>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StaleResultPolicy {
    LatestOnly,
}

impl Default for AnalysisSession {
    fn default() -> Self {
        Self::new()
    }
}

impl AnalysisSession {
    pub fn new() -> Self {
        Self {
            db: AnalysisDb::new(),
            docs: HashMap::new(),
        }
    }

    pub fn with_cancellation_token(token: CancellationToken) -> Self {
        Self {
            db: AnalysisDb::with_cancellation_token(token),
            docs: HashMap::new(),
        }
    }

    pub fn set_module_graph(&mut self, graph: ModuleGraph) {
        self.db.set_module_graph(graph);
    }

    pub fn invalidate_changed_modules(&mut self, changed: &HashSet<ModuleId>) {
        self.db.invalidate_changed_modules(changed);
    }

    pub fn open_document(&mut self, uri: &str, version: i32, text: &str) -> SessionResult<FileId> {
        let path = uri_to_path(uri)?;
        let file_id = self.db.upsert_disk_text(path.clone(), text);
        self.db.set_overlay(file_id, text);
        self.docs.insert(
            uri.to_string(),
            DocumentState {
                file_id,
                version,
                path,
            },
        );
        Ok(file_id)
    }

    pub fn change_document(
        &mut self,
        uri: &str,
        version: i32,
        text: &str,
    ) -> SessionResult<FileId> {
        let state = self
            .docs
            .get_mut(uri)
            .ok_or_else(|| SessionError::UnknownUri(uri.to_string()))?;
        if version < state.version {
            return Err(SessionError::StaleVersion {
                expected: state.version,
                found: version,
            });
        }
        state.version = version;
        self.db.set_overlay(state.file_id, text);
        Ok(state.file_id)
    }

    pub fn close_document(&mut self, uri: &str) -> SessionResult<()> {
        let state = self
            .docs
            .remove(uri)
            .ok_or_else(|| SessionError::UnknownUri(uri.to_string()))?;
        self.db.clear_overlay(state.file_id);
        Ok(())
    }

    pub fn lookup_document(&self, uri: &str) -> SessionResult<&DocumentState> {
        self.docs
            .get(uri)
            .ok_or_else(|| SessionError::UnknownUri(uri.to_string()))
    }

    pub fn snapshot(&self) -> AnalysisSnapshot {
        self.db.snapshot()
    }

    pub fn diagnostics_for_uri(
        &mut self,
        uri: &str,
    ) -> SessionResult<Vec<machina::analysis::diagnostics::Diagnostic>> {
        let state = self.lookup_document(uri)?.clone();
        let diagnostics = self
            .db
            .diagnostics_for_program_file(state.file_id)
            .map_err(SessionError::from)?;
        Ok(filter_diagnostics_for_path(&diagnostics, &state.path))
    }

    pub fn diagnostics_for_uri_if_version(
        &mut self,
        uri: &str,
        expected_version: i32,
    ) -> SessionResult<Option<Vec<machina::analysis::diagnostics::Diagnostic>>> {
        let state = self.lookup_document(uri)?.clone();
        if state.version != expected_version {
            return Ok(None);
        }
        let diagnostics = self
            .db
            .diagnostics_for_program_file(state.file_id)
            .map_err(SessionError::from)?;
        let latest = self.lookup_document(uri)?;
        if latest.version != expected_version {
            return Ok(None);
        }
        Ok(Some(filter_diagnostics_for_path(&diagnostics, &state.path)))
    }

    pub fn is_current_version(&self, uri: &str, expected_version: i32) -> SessionResult<bool> {
        Ok(self.lookup_document(uri)?.version == expected_version)
    }

    pub fn execute_query<T, F>(&mut self, query: F) -> SessionResult<T>
    where
        F: FnOnce(&mut AnalysisDb) -> QueryResult<T>,
    {
        query(&mut self.db).map_err(SessionError::from)
    }

    pub fn file_id_for_path(&self, path: &Path) -> Option<FileId> {
        self.snapshot().file_id(path)
    }

    pub fn file_id_for_uri(&self, uri: &str) -> SessionResult<FileId> {
        Ok(self.lookup_document(uri)?.file_id)
    }

    pub fn stale_policy(&self) -> StaleResultPolicy {
        StaleResultPolicy::LatestOnly
    }
}

fn filter_diagnostics_for_path(diagnostics: &[Diagnostic], path: &Path) -> Vec<Diagnostic> {
    let expected = path.to_string_lossy();
    diagnostics
        .iter()
        .filter(|diag| match diag.metadata.get(ANALYSIS_FILE_PATH_KEY) {
            Some(DiagnosticValue::String(file_path)) => file_path == expected.as_ref(),
            _ => false,
        })
        .cloned()
        .collect()
}

pub fn uri_to_path(uri: &str) -> SessionResult<PathBuf> {
    if let Some(path) = uri.strip_prefix("file://") {
        if path.is_empty() {
            return Err(SessionError::UnsupportedUri(uri.to_string()));
        }
        return Ok(PathBuf::from(path));
    }
    Err(SessionError::UnsupportedUri(uri.to_string()))
}

#[cfg(test)]
mod tests {
    use super::{
        ANALYSIS_FILE_PATH_KEY, AnalysisSession, DiagnosticValue, SessionError, uri_to_path,
    };
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
        fs::create_dir_all(&app_dir).expect("failed to create temp module tree");

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
        let diagnostics = session
            .diagnostics_for_uri(&uri)
            .expect("diagnostics query should succeed");

        let expected_path = entry_path.to_string_lossy();
        for diag in diagnostics {
            let source_path = diag
                .metadata
                .get(ANALYSIS_FILE_PATH_KEY)
                .expect("program diagnostics should include source path metadata");
            assert_eq!(
                source_path,
                &DiagnosticValue::String(expected_path.to_string()),
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
}
