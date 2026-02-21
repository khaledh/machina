//! Editor-facing helper methods for `AnalysisDb`.

use std::path::Path;

use crate::core::diag::Span;
use crate::core::resolve::DefId;
use crate::services::analysis::diagnostics::Diagnostic;
use crate::services::analysis::lookups::{
    code_actions_for_range, document_symbols, semantic_tokens,
};
use crate::services::analysis::query::QueryResult;
use crate::services::analysis::rename::{
    references as collect_references, rename_plan as build_rename_plan,
};
use crate::services::analysis::results::{
    CodeAction, DocumentSymbol, Location, RenamePlan, SemanticToken,
};
use crate::services::analysis::snapshot::FileId;

impl super::AnalysisDb {
    pub fn document_symbols_at_path(&mut self, path: &Path) -> QueryResult<Vec<DocumentSymbol>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.document_symbols_at_file(file_id)
        })
    }

    pub fn document_symbols_at_file(
        &mut self,
        file_id: FileId,
    ) -> QueryResult<Vec<DocumentSymbol>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(document_symbols(&state))
    }

    pub fn semantic_tokens_at_path(&mut self, path: &Path) -> QueryResult<Vec<SemanticToken>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.semantic_tokens_at_file(file_id)
        })
    }

    pub fn semantic_tokens_at_file(&mut self, file_id: FileId) -> QueryResult<Vec<SemanticToken>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(semantic_tokens(&state))
    }

    pub fn code_actions_at_path(
        &mut self,
        path: &Path,
        range: Span,
    ) -> QueryResult<Vec<CodeAction>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.code_actions_at_file(file_id, range)
        })
    }

    pub fn code_actions_at_file(
        &mut self,
        file_id: FileId,
        range: Span,
    ) -> QueryResult<Vec<CodeAction>> {
        let diagnostics = self.diagnostics_for_file(file_id)?;
        self.code_actions_for_diagnostics_at_file(file_id, range, diagnostics)
    }

    pub fn code_actions_for_diagnostics_at_file(
        &mut self,
        file_id: FileId,
        range: Span,
        diagnostics: Vec<Diagnostic>,
    ) -> QueryResult<Vec<CodeAction>> {
        let source = self.snapshot().text(file_id);
        Ok(code_actions_for_range(
            &diagnostics,
            range,
            source.as_deref(),
        ))
    }

    pub fn references(&mut self, def_id: DefId) -> QueryResult<Vec<Location>> {
        let snapshot = self.snapshot();
        collect_references(&snapshot, def_id, |file_id| {
            self.lookup_state_for_file(file_id)
        })
    }

    pub fn rename_plan(&mut self, def_id: DefId, new_name: &str) -> QueryResult<RenamePlan> {
        let snapshot = self.snapshot();
        build_rename_plan(&snapshot, def_id, new_name, |file_id| {
            self.lookup_state_for_file(file_id)
        })
    }
}
