//! File/path lookup endpoint helpers for `AnalysisDb`.

use std::path::Path;

use crate::core::capsule::ModuleId;
use crate::core::diag::Span;
use crate::core::resolve::DefId;
use crate::core::types::Type;
use crate::services::analysis::lookups::{
    def_at_span, def_location_at_span, hover_at_span_in_file, type_at_span,
};
use crate::services::analysis::query::QueryResult;
use crate::services::analysis::results::{
    CompletionItem, DefTarget, HoverInfo, Location, SignatureHelp,
};
use crate::services::analysis::snapshot::FileId;

impl super::AnalysisDb {
    pub fn def_at_path(&mut self, path: &Path, query_span: Span) -> QueryResult<Option<DefId>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.def_at_file(file_id, query_span),
        )
    }

    pub fn def_at_file(&mut self, file_id: FileId, query_span: Span) -> QueryResult<Option<DefId>> {
        let state = self.lookup_state_for_file(file_id)?;
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id);
        Ok(def_at_span(&state, query_span, source.as_deref()))
    }

    pub fn def_target_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<DefTarget>> {
        let state = self.lookup_state_for_file(file_id)?;
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id);
        Ok(
            def_at_span(&state, query_span, source.as_deref()).map(|def_id| DefTarget {
                file_id,
                module_id: Some(ModuleId(file_id.0)),
                def_id,
                symbol_id: state
                    .resolved
                    .and_then(|resolved| resolved.symbol_ids.lookup_symbol_id(def_id).cloned()),
                program_scoped: false,
            }),
        )
    }

    pub fn def_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<DefId>> {
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id);
        let state = if let Some(state) = self.entry_lookup_state_for_program_file(file_id)? {
            state
        } else {
            self.lookup_state_for_file(file_id)?
        };
        Ok(def_at_span(&state, query_span, source.as_deref()))
    }

    pub fn def_target_at_program_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<DefTarget>> {
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id);
        let (state, module_id) = if let Some(program) =
            self.program_pipeline_for_file(file_id)?.entry_module_id
            && let Some(state) = self.entry_lookup_state_for_program_file(file_id)?
        {
            (state, Some(program))
        } else {
            (
                self.lookup_state_for_file(file_id)?,
                Some(ModuleId(file_id.0)),
            )
        };
        Ok(
            def_at_span(&state, query_span, source.as_deref()).map(|def_id| DefTarget {
                file_id,
                module_id,
                def_id,
                symbol_id: state
                    .resolved
                    .and_then(|resolved| resolved.symbol_ids.lookup_symbol_id(def_id).cloned()),
                program_scoped: true,
            }),
        )
    }

    pub fn def_location_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<Location>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.def_location_at_file(file_id, query_span),
        )
    }

    pub fn def_location_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<Location>> {
        let snapshot = self.snapshot();
        let state = self.lookup_state_for_file(file_id)?;
        let source = snapshot.text(file_id);
        Ok(def_location_at_span(
            &snapshot,
            file_id,
            &state,
            query_span,
            source.as_deref(),
        ))
    }

    pub fn type_at_path(&mut self, path: &Path, query_span: Span) -> QueryResult<Option<Type>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.type_at_file(file_id, query_span),
        )
    }

    pub fn type_at_file(&mut self, file_id: FileId, query_span: Span) -> QueryResult<Option<Type>> {
        let state = self.lookup_state_for_file(file_id)?;
        Ok(type_at_span(&state, query_span))
    }

    pub fn type_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<Type>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.type_at_program_file(file_id, query_span),
        )
    }

    pub fn hover_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.hover_at_file(file_id, query_span),
        )
    }

    pub fn hover_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        let state = self.lookup_state_for_file(file_id)?;
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id);
        Ok(hover_at_span_in_file(
            &state,
            query_span,
            snapshot.path(file_id),
            source.as_deref(),
            Some(self.tracer()),
        ))
    }

    pub fn hover_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<HoverInfo>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.hover_at_program_file(file_id, query_span),
        )
    }

    pub fn completions_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.completions_at_file(file_id, query_span)
        })
    }

    pub fn completions_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        let snapshot = self.snapshot();
        let Some(source) = snapshot.text(file_id).map(|s| s.to_string()) else {
            return Ok(Vec::new());
        };
        let state = self.lookup_state_for_file(file_id)?;
        self.completions_for_state(file_id, query_span, source, state)
    }

    pub fn completions_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Vec<CompletionItem>> {
        self.with_file_id(path, Vec::new, |db, file_id| {
            db.completions_at_program_file(file_id, query_span)
        })
    }

    pub fn signature_help_at_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.signature_help_at_file(file_id, query_span),
        )
    }

    pub fn signature_help_at_file(
        &mut self,
        file_id: FileId,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        let snapshot = self.snapshot();
        let source = snapshot.text(file_id).map(|s| s.to_string());
        let state = self.lookup_state_for_file(file_id)?;
        if let Some(sig) = self.signature_help_for_state(&state, query_span, source.as_deref()) {
            return Ok(Some(sig));
        }
        self.signature_help_with_synthetic_fallback(file_id, query_span)
    }

    pub fn signature_help_at_program_path(
        &mut self,
        path: &Path,
        query_span: Span,
    ) -> QueryResult<Option<SignatureHelp>> {
        self.with_file_id(
            path,
            || None,
            |db, file_id| db.signature_help_at_program_file(file_id, query_span),
        )
    }
}
