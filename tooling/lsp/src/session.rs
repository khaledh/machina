//! Editor-agnostic analysis session service.
//!
//! This layer owns open-document lifecycle, overlay state, and query-facing
//! snapshot access. It intentionally has no JSON-RPC/LSP dependency.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use thiserror::Error;

use machina::core::capsule::ModuleId;
use machina::services::analysis::db::AnalysisDb;
use machina::services::analysis::diagnostics::{
    ANALYSIS_FILE_ID_KEY, ANALYSIS_FILE_PATH_KEY, Diagnostic, DiagnosticValue,
};
use machina::services::analysis::module_graph::ModuleGraph;
use machina::services::analysis::query::{CancellationToken, QueryCancelled, QueryResult};
use machina::services::analysis::snapshot::{AnalysisSnapshot, FileId};

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

    pub fn set_legacy_typestate_enabled(&mut self, enabled: bool) {
        self.db.set_experimental_typestate(enabled);
    }

    pub fn legacy_typestate_enabled(&self) -> bool {
        self.db.experimental_typestate()
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
    ) -> SessionResult<Vec<machina::services::analysis::diagnostics::Diagnostic>> {
        let state = self.lookup_document(uri)?.clone();
        let diagnostics = self
            .db
            .diagnostics_for_program_file(state.file_id)
            .map_err(SessionError::from)?;
        let snapshot = self.snapshot();
        Ok(filter_diagnostics_for_file(
            &diagnostics,
            &snapshot,
            state.file_id,
        ))
    }

    pub fn diagnostics_for_uri_if_version(
        &mut self,
        uri: &str,
        expected_version: i32,
    ) -> SessionResult<Option<Vec<machina::services::analysis::diagnostics::Diagnostic>>> {
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
        let snapshot = self.snapshot();
        Ok(Some(filter_diagnostics_for_file(
            &diagnostics,
            &snapshot,
            state.file_id,
        )))
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

    /// Check whether the given 0-based position falls inside a `//` line
    /// comment. Used by hover/definition/signatureHelp handlers to suppress
    /// analysis results when the cursor is over commented-out code.
    pub fn is_position_in_line_comment(&self, file_id: FileId, line0: usize, col0: usize) -> bool {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id) else {
            return false;
        };
        let Some(line) = source.lines().nth(line0) else {
            return false;
        };
        is_column_in_line_comment(line, col0)
    }
}

/// Scan a single source line left-to-right, tracking string and char literal
/// state, to determine whether `col0` sits inside a `//` comment. We need
/// the literal tracking to avoid false positives on `"//"` in strings.
fn is_column_in_line_comment(line: &str, col0: usize) -> bool {
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0usize;
    let mut in_string = false;
    let mut in_char = false;
    let mut escaped = false;

    while i + 1 < chars.len() {
        let ch = chars[i];
        if in_string {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        if in_char {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '\'' {
                in_char = false;
            }
            i += 1;
            continue;
        }

        if ch == '"' {
            in_string = true;
            i += 1;
            continue;
        }
        if ch == '\'' {
            in_char = true;
            i += 1;
            continue;
        }
        if ch == '/' && chars[i + 1] == '/' {
            return col0 >= i;
        }
        i += 1;
    }
    false
}

fn filter_diagnostics_for_file(
    diagnostics: &[Diagnostic],
    snapshot: &AnalysisSnapshot,
    target_file_id: FileId,
) -> Vec<Diagnostic> {
    diagnostics
        .iter()
        .filter(|diag| diagnostic_file_id(diag, snapshot) == Some(target_file_id))
        .cloned()
        .collect()
}

fn diagnostic_file_id(diag: &Diagnostic, snapshot: &AnalysisSnapshot) -> Option<FileId> {
    if let Some(DiagnosticValue::Number(n)) = diag.metadata.get(ANALYSIS_FILE_ID_KEY)
        && *n >= 0
        && *n <= u32::MAX as i64
    {
        return Some(FileId(*n as u32));
    }
    let file_path = match diag.metadata.get(ANALYSIS_FILE_PATH_KEY) {
        Some(DiagnosticValue::String(file_path)) => PathBuf::from(file_path),
        _ => return None,
    };
    if let Some(file_id) = snapshot.file_id(&file_path) {
        return Some(file_id);
    }
    if let Ok(canon) = file_path.canonicalize() {
        return snapshot.file_id(&canon);
    }
    None
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
#[path = "./tests/t_session.rs"]
mod tests;
