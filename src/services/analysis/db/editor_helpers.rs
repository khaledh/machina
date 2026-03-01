//! Editor-facing helper methods for `AnalysisDb`.

use std::path::Path;

use crate::core::diag::Span;
use crate::services::analysis::diagnostics::Diagnostic;
use crate::services::analysis::lookups::{
    code_actions_for_range, document_symbols, semantic_tokens,
};
use crate::services::analysis::query::QueryResult;
use crate::services::analysis::rename::{
    references as collect_references, rename_plan as build_rename_plan,
};
use crate::services::analysis::results::{
    CodeAction, DefTarget, DocumentSymbol, Location, RenamePlan, SemanticToken,
};
use crate::services::analysis::snapshot::FileId;

impl super::AnalysisDb {
    fn references_for_target(&mut self, target: &DefTarget) -> QueryResult<Vec<Location>> {
        let snapshot = self.snapshot();
        collect_references(&snapshot, target, |file_id| {
            if target.program_scoped {
                self.best_lookup_state_for_navigation(target.file_id, file_id)
            } else {
                self.lookup_state_for_file(file_id)
            }
        })
    }

    fn rename_plan_for_target(
        &mut self,
        target: &DefTarget,
        new_name: &str,
    ) -> QueryResult<RenamePlan> {
        let snapshot = self.snapshot();
        build_rename_plan(&snapshot, target, new_name, |file_id| {
            if target.program_scoped {
                self.best_lookup_state_for_navigation(target.file_id, file_id)
            } else {
                self.lookup_state_for_file(file_id)
            }
        })
    }

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

    pub fn references(&mut self, target: &DefTarget) -> QueryResult<Vec<Location>> {
        self.references_for_target(target)
    }

    pub fn references_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Vec<Location>> {
        let Some(target) = self.def_target_at_file(file_id, query_span)? else {
            return Ok(Vec::new());
        };
        self.references_for_target(&target)
    }

    pub fn references_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Vec<Location>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.references_at_file(file_id, query_span)
        })
    }

    pub fn references_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Vec<Location>> {
        let Some(target) = self.def_target_at_program_file(file_id, query_span)? else {
            return Ok(Vec::new());
        };
        self.references_for_target(&target)
    }

    pub fn references_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Vec<Location>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.references_at_program_file(file_id, query_span)
        })
    }

    pub fn rename_plan(&mut self, target: &DefTarget, new_name: &str) -> QueryResult<RenamePlan> {
        self.rename_plan_for_target(target, new_name)
    }

    pub fn rename_plan_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
        new_name: &str,
    ) -> QueryResult<Option<RenamePlan>> {
        let Some(target) = self.def_target_at_file(file_id, query_span)? else {
            return Ok(None);
        };
        self.rename_plan_for_target(&target, new_name).map(Some)
    }

    pub fn rename_plan_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
        new_name: &str,
    ) -> QueryResult<Option<RenamePlan>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.rename_plan_at_file(file_id, query_span, new_name),
        )
    }

    pub fn rename_plan_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
        new_name: &str,
    ) -> QueryResult<Option<RenamePlan>> {
        let Some(target) = self.def_target_at_program_file(file_id, query_span)? else {
            return Ok(None);
        };
        self.rename_plan_for_target(&target, new_name).map(Some)
    }

    pub fn rename_plan_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
        new_name: &str,
    ) -> QueryResult<Option<RenamePlan>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.rename_plan_at_program_file(file_id, query_span, new_name),
        )
    }
}
