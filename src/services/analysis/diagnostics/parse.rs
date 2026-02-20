//! Parse-phase diagnostic mapping.

use crate::core::lexer::{LexError, LexErrorKind};
use crate::core::parse::{ParseError, ParseErrorKind};

use super::{Diagnostic, DiagnosticMetadata, DiagnosticPhase, DiagnosticSeverity, DiagnosticValue};

pub(super) fn from_lex_error(error: &LexError) -> Diagnostic {
    let mut metadata = DiagnosticMetadata::new();
    let code = match error.kind() {
        LexErrorKind::UnexpectedCharacter(ch) => {
            metadata.insert(
                "character".to_string(),
                DiagnosticValue::String(ch.to_string()),
            );
            "MC-LEX-UNEXPECTED-CHARACTER"
        }
        LexErrorKind::InvalidInteger(_) => "MC-LEX-INVALID-INTEGER",
        LexErrorKind::InvalidEscapeSequence(seq) => {
            metadata.insert("escape".to_string(), DiagnosticValue::String(seq.clone()));
            "MC-LEX-INVALID-ESCAPE-SEQUENCE"
        }
        LexErrorKind::UnterminatedString => "MC-LEX-UNTERMINATED-STRING",
    };
    Diagnostic {
        phase: DiagnosticPhase::Parse,
        code: code.to_string(),
        severity: DiagnosticSeverity::Error,
        span: error.span(),
        message: error.to_string(),
        metadata,
    }
}

pub(super) fn from_parse_error(error: &ParseError) -> Diagnostic {
    let mut metadata = DiagnosticMetadata::new();
    let code = match error.kind() {
        ParseErrorKind::ExpectedDecl(_) => "MC-PARSE-EXPECTED-DECL",
        ParseErrorKind::ExpectedToken(expected, found) => {
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::String(expected.to_string()),
            );
            metadata.insert(
                "found".to_string(),
                DiagnosticValue::String(found.kind.to_string()),
            );
            "MC-PARSE-EXPECTED-TOKEN"
        }
        ParseErrorKind::ExpectedIdent(_) => "MC-PARSE-EXPECTED-IDENT",
        ParseErrorKind::ExpectedSelf(_) => "MC-PARSE-EXPECTED-SELF",
        ParseErrorKind::ExpectedType(_) => "MC-PARSE-EXPECTED-TYPE",
        ParseErrorKind::ExpectedPrimary(_) => "MC-PARSE-EXPECTED-PRIMARY",
        ParseErrorKind::ExpectedIntLit(_) => "MC-PARSE-EXPECTED-INT-LIT",
        ParseErrorKind::ExpectedStringLit(_) => "MC-PARSE-EXPECTED-STRING-LIT",
        ParseErrorKind::ExpectedPattern(_) => "MC-PARSE-EXPECTED-PATTERN",
        ParseErrorKind::SingleFieldTupleMissingComma(_) => "MC-PARSE-TUPLE-MISSING-COMMA",
        ParseErrorKind::SingleElementSetMissingComma(_) => "MC-PARSE-SET-MISSING-COMMA",
        ParseErrorKind::ExpectedStructField(_) => "MC-PARSE-EXPECTED-STRUCT-FIELD",
        ParseErrorKind::ExpectedMatchArm(_) => "MC-PARSE-EXPECTED-MATCH-ARM",
        ParseErrorKind::ExpectedMatchPattern(_) => "MC-PARSE-EXPECTED-MATCH-PATTERN",
        ParseErrorKind::ExpectedArrayIndexOrRange(_) => "MC-PARSE-EXPECTED-INDEX-OR-RANGE",
        ParseErrorKind::ExpectedRefinement(_) => "MC-PARSE-EXPECTED-REFINEMENT",
        ParseErrorKind::UnknownAttribute(name) => {
            metadata.insert(
                "attribute".to_string(),
                DiagnosticValue::String(name.clone()),
            );
            "MC-PARSE-UNKNOWN-ATTRIBUTE"
        }
        ParseErrorKind::AttributeNotAllowed => "MC-PARSE-ATTRIBUTE-NOT-ALLOWED",
        ParseErrorKind::FeatureDisabled { feature } => {
            metadata.insert(
                "feature".to_string(),
                DiagnosticValue::String((*feature).to_string()),
            );
            "MC-PARSE-FEATURE-DISABLED"
        }
        ParseErrorKind::UnmatchedFormatBrace => "MC-PARSE-UNMATCHED-FORMAT-BRACE",
        ParseErrorKind::InvalidFormatExpr => "MC-PARSE-INVALID-FORMAT-EXPR",
        ParseErrorKind::EmptyFormatExpr => "MC-PARSE-EMPTY-FORMAT-EXPR",
        ParseErrorKind::UnterminatedFormatExpr => "MC-PARSE-UNTERMINATED-FORMAT-EXPR",
    };
    Diagnostic {
        phase: DiagnosticPhase::Parse,
        code: code.to_string(),
        severity: DiagnosticSeverity::Error,
        span: error.span(),
        message: error.to_string(),
        metadata,
    }
}
