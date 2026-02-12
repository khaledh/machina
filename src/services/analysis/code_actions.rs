//! Diagnostic-driven code action generation.
//!
//! This keeps quick-fix mapping separate from `AnalysisDb` orchestration so new
//! actions can be added without growing query plumbing code.

use crate::core::diag::Span;
use crate::services::analysis::diagnostics::{Diagnostic, DiagnosticValue};
use crate::services::analysis::results::{CodeAction, CodeActionKind, TextEdit};

pub(crate) fn code_actions_for_diagnostic(diag: &Diagnostic) -> Vec<CodeAction> {
    match diag.code.as_str() {
        "MC-PARSE-SET-MISSING-COMMA" | "MC-PARSE-TUPLE-MISSING-COMMA" => vec![CodeAction {
            title: "Insert missing comma".to_string(),
            kind: CodeActionKind::QuickFix,
            diagnostic_code: diag.code.clone(),
            edits: vec![TextEdit {
                span: Span {
                    start: diag.span.end,
                    end: diag.span.end,
                },
                new_text: ",".to_string(),
            }],
        }],
        "MC-TYPECHECK-TryErrorNotInReturn" => {
            let missing = diag
                .metadata
                .get("missing")
                .and_then(|v| match v {
                    DiagnosticValue::StringList(items) => Some(items.join(" | ")),
                    _ => None,
                })
                .unwrap_or_else(|| "missing error variants".to_string());
            vec![CodeAction {
                title: format!("Add `{missing}` to function return error union"),
                kind: CodeActionKind::QuickFix,
                diagnostic_code: diag.code.clone(),
                edits: Vec::new(),
            }]
        }
        "MC-TYPECHECK-TryReturnTypeNotErrorUnion" => vec![CodeAction {
            title: "Change function return type to an error union (or remove `?`)".to_string(),
            kind: CodeActionKind::QuickFix,
            diagnostic_code: diag.code.clone(),
            edits: Vec::new(),
        }],
        "MC-SEMCK-NonExhaustiveUnionMatch" => {
            let missing = diag
                .metadata
                .get("missing")
                .and_then(|v| match v {
                    DiagnosticValue::StringList(items) => Some(items.join(" | ")),
                    _ => None,
                })
                .unwrap_or_else(|| "missing union variants".to_string());
            vec![CodeAction {
                title: format!("Add match arms for missing union variants: {missing}"),
                kind: CodeActionKind::QuickFix,
                diagnostic_code: diag.code.clone(),
                edits: Vec::new(),
            }]
        }
        _ => Vec::new(),
    }
}
