//! Unified diagnostics model for analysis queries and IDE adapters.
//!
//! This layer normalizes parse/resolve/typecheck/semcheck diagnostics into one
//! phase-tagged representation with stable codes and structured metadata.

mod parse;
mod resolve;
mod semcheck;
mod typecheck;

use std::collections::BTreeMap;

use crate::core::diag::Span;
use crate::core::lexer::LexError;
use crate::core::parse::ParseError;
use crate::core::resolve::ResolveError;
use crate::core::semck::SemCheckError;
use crate::core::typecheck::TypeCheckError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticPhase {
    Parse,
    Resolve,
    Typecheck,
    Semcheck,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticSeverity {
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticValue {
    String(String),
    Number(i64),
    Bool(bool),
    StringList(Vec<String>),
}

pub type DiagnosticMetadata = BTreeMap<String, DiagnosticValue>;
pub const ANALYSIS_FILE_PATH_KEY: &str = "analysis_file_path";
pub const ANALYSIS_FILE_ID_KEY: &str = "analysis_file_id";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub phase: DiagnosticPhase,
    pub code: String,
    pub severity: DiagnosticSeverity,
    pub span: Span,
    pub message: String,
    pub metadata: DiagnosticMetadata,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WireDiagnostic {
    pub phase: DiagnosticPhase,
    pub code: String,
    pub severity: DiagnosticSeverity,
    pub span: Span,
    pub message: String,
    pub metadata: Vec<(String, DiagnosticValue)>,
}

impl Diagnostic {
    pub fn from_lex_error(error: &LexError) -> Self {
        parse::from_lex_error(error)
    }

    pub fn to_wire(&self) -> WireDiagnostic {
        WireDiagnostic {
            phase: self.phase,
            code: self.code.clone(),
            severity: self.severity,
            span: self.span,
            message: self.message.clone(),
            metadata: self
                .metadata
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        }
    }

    pub fn from_parse_error(error: &ParseError) -> Self {
        parse::from_parse_error(error)
    }

    pub fn from_resolve_error(error: &ResolveError) -> Self {
        resolve::from_resolve_error(error)
    }

    pub fn from_semcheck_error(error: &SemCheckError) -> Self {
        semcheck::from_semcheck_error(error)
    }

    pub fn from_typecheck_error(error: &TypeCheckError) -> Self {
        typecheck::from_typecheck_error(error)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PartialDiagnostics {
    pub parse: Vec<Diagnostic>,
    pub resolve: Vec<Diagnostic>,
    pub typecheck: Vec<Diagnostic>,
}

impl PartialDiagnostics {
    pub fn all(&self) -> Vec<&Diagnostic> {
        self.parse
            .iter()
            .chain(self.resolve.iter())
            .chain(self.typecheck.iter())
            .collect()
    }
}

pub fn collect_partial_diagnostics(
    parse_errors: Option<&[ParseError]>,
    resolve_errors: Option<&[ResolveError]>,
    typecheck_errors: Option<&[TypeCheckError]>,
) -> PartialDiagnostics {
    PartialDiagnostics {
        parse: parse_errors
            .unwrap_or(&[])
            .iter()
            .map(Diagnostic::from_parse_error)
            .collect(),
        resolve: resolve_errors
            .unwrap_or(&[])
            .iter()
            .map(Diagnostic::from_resolve_error)
            .collect(),
        typecheck: typecheck_errors
            .unwrap_or(&[])
            .iter()
            .map(Diagnostic::from_typecheck_error)
            .collect(),
    }
}

#[cfg(test)]
#[path = "../../../tests/analysis/t_diagnostics.rs"]
mod tests;
