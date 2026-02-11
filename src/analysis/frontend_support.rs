//! Frontend/program helper utilities for analysis workflows.
//!
//! These helpers are used by analysis queries that need whole-program loading
//! or frontend-originated diagnostics.

use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use crate::analysis::diagnostics::{ANALYSIS_FILE_PATH_KEY, Diagnostic, DiagnosticValue};
use crate::analysis::snapshot::AnalysisSnapshot;
use crate::diag::Span;
use crate::frontend::{self, FrontendError, ModuleLoader, ModulePath};

pub(crate) struct SnapshotOverlayLoader {
    snapshot: AnalysisSnapshot,
    fs_loader: frontend::FsModuleLoader,
}

impl SnapshotOverlayLoader {
    pub(crate) fn new(snapshot: AnalysisSnapshot, project_root: PathBuf) -> Self {
        Self {
            snapshot,
            fs_loader: frontend::FsModuleLoader::new(project_root),
        }
    }
}

impl ModuleLoader for SnapshotOverlayLoader {
    fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), FrontendError> {
        let (file_path, disk_source) = self.fs_loader.load(path)?;
        if let Some(overlay) = snapshot_text_for_path(&self.snapshot, &file_path) {
            return Ok((file_path, overlay));
        }
        Ok((file_path, disk_source))
    }
}

pub(crate) fn snapshot_text_for_path(snapshot: &AnalysisSnapshot, path: &Path) -> Option<String> {
    if let Some(file_id) = snapshot.file_id(path) {
        return snapshot.text(file_id).map(|s| s.to_string());
    }
    if let Ok(canon) = path.canonicalize()
        && let Some(file_id) = snapshot.file_id(&canon)
    {
        return snapshot.text(file_id).map(|s| s.to_string());
    }
    None
}

pub(crate) fn infer_project_root(entry_file: &Path) -> PathBuf {
    for ancestor in entry_file.ancestors() {
        if ancestor.join("Cargo.toml").exists() {
            return ancestor.to_path_buf();
        }
    }
    entry_file
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf()
}

pub(crate) fn stable_source_revision(source: &str) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    source.hash(&mut hasher);
    hasher.finish()
}

pub(crate) fn frontend_error_diagnostics(error: FrontendError) -> Vec<Diagnostic> {
    fn frontend_diag(message: String, span: Span) -> Diagnostic {
        Diagnostic {
            phase: crate::analysis::diagnostics::DiagnosticPhase::Resolve,
            code: "MC-FRONTEND".to_string(),
            severity: crate::analysis::diagnostics::DiagnosticSeverity::Error,
            span,
            message,
            metadata: Default::default(),
        }
    }

    fn with_path_metadata(mut diag: Diagnostic, path: Option<&Path>) -> Diagnostic {
        if let Some(path) = path {
            diag.metadata.insert(
                ANALYSIS_FILE_PATH_KEY.to_string(),
                DiagnosticValue::String(path.to_string_lossy().to_string()),
            );
        }
        diag
    }

    match error {
        FrontendError::Lex { path, error } => {
            vec![with_path_metadata(
                Diagnostic::from_lex_error(&error),
                Some(&path),
            )]
        }
        FrontendError::Parse { path, error } => {
            vec![with_path_metadata(
                Diagnostic::from_parse_error(&error),
                Some(&path),
            )]
        }
        FrontendError::Io(path, io_error) => {
            vec![with_path_metadata(
                frontend_diag(
                    format!(
                        "failed to read module file {}: {}",
                        path.display(),
                        io_error
                    ),
                    Span::default(),
                ),
                Some(&path),
            )]
        }
        FrontendError::UnknownRequireAlias { span, .. }
        | FrontendError::RequireMemberUndefined { span, .. }
        | FrontendError::RequireMemberPrivate { span, .. }
        | FrontendError::DuplicateRequireAlias { span, .. }
        | FrontendError::SymbolImportAliasUnsupported { span, .. } => {
            vec![frontend_diag(error.to_string(), span)]
        }
        _ => vec![frontend_diag(error.to_string(), Span::default())],
    }
}
