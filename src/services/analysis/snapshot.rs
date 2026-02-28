//! Revisioned source snapshots with unsaved overlay support.
//!
//! IDE features operate on in-memory edits that are not yet written to disk.
//! This module keeps canonical file identity and lets callers materialize
//! immutable snapshots keyed by revision.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Stable file identity used by analysis queries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub u32);

/// Immutable source view used by query evaluation.
#[derive(Debug, Clone)]
pub struct AnalysisSnapshot {
    revision: u64,
    path_to_file: Arc<HashMap<PathBuf, FileId>>,
    file_to_path: Arc<HashMap<FileId, PathBuf>>,
    file_text: Arc<HashMap<FileId, Arc<str>>>,
}

impl AnalysisSnapshot {
    pub fn revision(&self) -> u64 {
        self.revision
    }

    pub fn file_id(&self, path: &Path) -> Option<FileId> {
        self.path_to_file.get(path).copied()
    }

    pub fn path(&self, file_id: FileId) -> Option<&Path> {
        self.file_to_path.get(&file_id).map(|p| p.as_path())
    }

    pub fn text(&self, file_id: FileId) -> Option<Arc<str>> {
        self.file_text.get(&file_id).cloned()
    }

    pub fn file_ids(&self) -> Vec<FileId> {
        let mut ids: Vec<_> = self.file_text.keys().copied().collect();
        ids.sort_unstable();
        ids
    }
}

/// Mutable source state that tracks disk text and unsaved overlays.
#[derive(Debug, Default)]
pub struct SourceStore {
    next_file_id: u32,
    revision: u64,
    path_to_file: HashMap<PathBuf, FileId>,
    file_to_path: HashMap<FileId, PathBuf>,
    disk_text: HashMap<FileId, Arc<str>>,
    overlays: HashMap<FileId, Arc<str>>,
}

impl SourceStore {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn revision(&self) -> u64 {
        self.revision
    }

    /// Intern a file path if missing and update its on-disk text.
    ///
    /// Returns the stable `FileId` for the path.
    pub fn upsert_disk_text<P, S>(&mut self, path: P, text: S) -> FileId
    where
        P: Into<PathBuf>,
        S: Into<Arc<str>>,
    {
        let path = path.into();
        let file_id = if let Some(id) = self.path_to_file.get(&path).copied() {
            id
        } else {
            let id = FileId(self.next_file_id);
            self.next_file_id = self.next_file_id.checked_add(1).expect("file id overflow");
            self.path_to_file.insert(path.clone(), id);
            self.file_to_path.insert(id, path.clone());
            id
        };

        let text = text.into();
        let changed = self.disk_text.get(&file_id) != Some(&text);
        if changed {
            self.disk_text.insert(file_id, text);
            self.bump_revision();
        } else if let std::collections::hash_map::Entry::Vacant(e) = self.disk_text.entry(file_id) {
            // First insertion may compare equal only if map was empty.
            e.insert(text);
            self.bump_revision();
        }
        file_id
    }

    pub fn file_id(&self, path: &Path) -> Option<FileId> {
        self.path_to_file.get(path).copied()
    }

    pub fn path(&self, file_id: FileId) -> Option<&Path> {
        self.file_to_path.get(&file_id).map(|p| p.as_path())
    }

    pub fn set_overlay<S>(&mut self, file_id: FileId, text: S)
    where
        S: Into<Arc<str>>,
    {
        let text = text.into();
        let changed = self.overlays.get(&file_id) != Some(&text);
        if changed {
            self.overlays.insert(file_id, text);
            self.bump_revision();
        }
    }

    pub fn clear_overlay(&mut self, file_id: FileId) {
        if self.overlays.remove(&file_id).is_some() {
            self.bump_revision();
        }
    }

    pub fn text(&self, file_id: FileId) -> Option<Arc<str>> {
        self.overlays
            .get(&file_id)
            .cloned()
            .or_else(|| self.disk_text.get(&file_id).cloned())
    }

    /// Returns `true` when strict single-file frontend fallback is safe for
    /// `file_id`, i.e. every active overlay belongs to that same file.
    pub fn supports_isolated_file_frontend(&self, file_id: FileId) -> bool {
        self.overlays
            .keys()
            .all(|overlay_id| *overlay_id == file_id)
    }

    /// Materialize an immutable snapshot where overlays shadow disk text.
    pub fn snapshot(&self) -> AnalysisSnapshot {
        let mut file_text = self.disk_text.clone();
        for (file_id, overlay) in &self.overlays {
            file_text.insert(*file_id, overlay.clone());
        }
        AnalysisSnapshot {
            revision: self.revision,
            path_to_file: Arc::new(self.path_to_file.clone()),
            file_to_path: Arc::new(self.file_to_path.clone()),
            file_text: Arc::new(file_text),
        }
    }

    fn bump_revision(&mut self) {
        self.revision = self
            .revision
            .checked_add(1)
            .expect("analysis source revision overflow");
    }
}

#[cfg(test)]
#[path = "../../tests/analysis/t_snapshot.rs"]
mod tests;
