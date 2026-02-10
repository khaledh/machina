use crate::analysis::diagnostics::{
    Diagnostic, DiagnosticPhase, DiagnosticSeverity, DiagnosticValue, collect_partial_diagnostics,
};
use crate::diag::Span;
use crate::lexer::{Token, TokenKind};
use crate::parse::ParseError;
use crate::resolve::ResolveError;
use crate::resolve::symbols::SymbolKind;
use crate::typecheck::{TypeCheckError, TypeCheckErrorKind};
use crate::types::Type;

fn token(kind: TokenKind, span: Span) -> Token {
    Token { kind, span }
}

#[test]
fn parse_diagnostic_is_phase_tagged_and_stable_coded() {
    let span = Span::default();
    let err = ParseError::ExpectedToken(TokenKind::KwFn, token(TokenKind::KwType, span));
    let diag = Diagnostic::from_parse_error(&err);

    assert_eq!(diag.phase, DiagnosticPhase::Parse);
    assert_eq!(diag.severity, DiagnosticSeverity::Error);
    assert_eq!(diag.code, "MC-PARSE-EXPECTED-TOKEN");
    assert_eq!(
        diag.metadata.get("expected"),
        Some(&DiagnosticValue::String("fn".to_string()))
    );
    assert_eq!(
        diag.metadata.get("found"),
        Some(&DiagnosticValue::String("type".to_string()))
    );
}

#[test]
fn resolve_diagnostic_keeps_structured_metadata() {
    let err = ResolveError::ExpectedType(
        "HasTickCount".to_string(),
        SymbolKind::TraitDef {
            def_id: crate::resolve::DefId(7),
        },
        Span::default(),
    );
    let diag = Diagnostic::from_resolve_error(&err);

    assert_eq!(diag.phase, DiagnosticPhase::Resolve);
    assert_eq!(diag.code, "MC-RESOLVE-EXPECTED-TYPE");
    assert_eq!(
        diag.metadata.get("name"),
        Some(&DiagnosticValue::String("HasTickCount".to_string()))
    );
    assert_eq!(
        diag.metadata.get("found_kind"),
        Some(&DiagnosticValue::String("trait_def".to_string()))
    );
}

#[test]
fn typecheck_diagnostic_includes_quick_fix_metadata() {
    let err: TypeCheckError = TypeCheckErrorKind::TryErrorNotInReturn(
        vec!["IoError".to_string(), "ParseError".to_string()],
        vec!["Config".to_string(), "ValidationError".to_string()],
        Span::default(),
    )
    .into();
    let diag = Diagnostic::from_typecheck_error(&err);

    assert_eq!(diag.phase, DiagnosticPhase::Typecheck);
    assert_eq!(diag.code, "MC-TYPECHECK-TryErrorNotInReturn");
    assert_eq!(
        diag.metadata.get("missing"),
        Some(&DiagnosticValue::StringList(vec![
            "IoError".to_string(),
            "ParseError".to_string()
        ]))
    );
    assert_eq!(
        diag.metadata.get("quick_fixable"),
        Some(&DiagnosticValue::Bool(true))
    );
}

#[test]
fn partial_diagnostics_support_missing_stages() {
    let parse_errors = vec![ParseError::ExpectedPrimary(token(
        TokenKind::Eof,
        Span::default(),
    ))];
    let resolve_errors: Vec<ResolveError> = Vec::new();
    let typecheck_errors: Vec<TypeCheckError> = Vec::new();

    let only_parse = collect_partial_diagnostics(
        Some(&parse_errors),
        Some(&resolve_errors),
        Some(&typecheck_errors),
    );
    assert_eq!(only_parse.parse.len(), 1);
    assert!(only_parse.resolve.is_empty());
    assert!(only_parse.typecheck.is_empty());
    assert_eq!(only_parse.all().len(), 1);

    let none = collect_partial_diagnostics(None, None, None);
    assert!(none.all().is_empty());
}

#[test]
fn wire_conversion_is_deterministic() {
    let err: TypeCheckError =
        TypeCheckErrorKind::ArgCountMismatch("run".to_string(), 1, 2, Span::default()).into();
    let diag = Diagnostic::from_typecheck_error(&err);
    let wire = diag.to_wire();

    assert_eq!(wire.phase, DiagnosticPhase::Typecheck);
    assert_eq!(wire.code, "MC-TYPECHECK-ArgCountMismatch");
    // BTreeMap-backed metadata ordering is deterministic in wire form.
    assert_eq!(wire.metadata[0].0, "callable");
    assert_eq!(wire.metadata[1].0, "expected");
    assert_eq!(wire.metadata[2].0, "found");
    assert_eq!(
        wire.metadata[0].1,
        DiagnosticValue::String("run".to_string())
    );
}

#[test]
fn typecheck_stable_code_exists_for_newer_variants() {
    let err: TypeCheckError = TypeCheckErrorKind::MapIndexAssignUnsupported(Span::default()).into();
    let diag = Diagnostic::from_typecheck_error(&err);
    assert_eq!(diag.code, "MC-TYPECHECK-MapIndexAssignUnsupported");

    let err: TypeCheckError =
        TypeCheckErrorKind::StringFmtExprUnsupportedType(Type::Bool, Span::default()).into();
    let diag = Diagnostic::from_typecheck_error(&err);
    assert_eq!(diag.code, "MC-TYPECHECK-StringFmtExprUnsupportedType");
}
