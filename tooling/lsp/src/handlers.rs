//! JSON-RPC request handlers for the machina language server.
//!
//! Each LSP method is dispatched from [`handle_message`]. Position-based read
//! requests (hover, definition, completion, signatureHelp) share a common
//! `dispatch_read_request` helper that handles param parsing, version lookup,
//! and response framing. Response functions use `resolve_versioned_file` and
//! `version_guarded_query` to bracket the actual analysis query with document-
//! version staleness checks — ensuring the editor never sees results computed
//! from an outdated snapshot.

use serde_json::{Map, Value, json};
use std::path::Path;

use machina::services::analysis::db::AnalysisDb;
use machina::services::analysis::query::QueryResult;
use machina::services::analysis::snapshot::FileId;

use crate::session::{AnalysisSession, SessionError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HandlerAction {
    Continue,
    Exit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DidOpenParams {
    uri: String,
    version: i32,
    text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DidChangeParams {
    uri: String,
    version: i32,
    text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DidCloseParams {
    uri: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TextDocumentRequestParams {
    uri: String,
    version: Option<i32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ReadRequestParams {
    uri: String,
    line0: usize,
    col0: usize,
    version: Option<i32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CodeActionRequestParams {
    uri: String,
    range: machina::core::diag::Span,
    version: Option<i32>,
    context_diagnostics: Vec<Value>,
}

pub fn handle_message(
    session: &mut AnalysisSession,
    message: Value,
) -> (HandlerAction, Option<Value>) {
    let method = message.get("method").and_then(Value::as_str);
    let id = message.get("id").cloned();
    let params = message.get("params");

    match method {
        Some("initialize") => {
            let init_features = parse_initialize_feature_flags(params);
            session.set_legacy_typestate_enabled(init_features.legacy_typestate);
            let signature_retrigger_characters = signature_help_retrigger_characters();
            let response = success_response(
                id.unwrap_or(Value::Null),
                json!({
                    "capabilities": {
                        "textDocumentSync": {
                            "openClose": true,
                            "change": 1
                        },
                        "hoverProvider": true,
                        "completionProvider": {
                            "resolveProvider": false,
                            "triggerCharacters": [".", ":"]
                        },
                        "definitionProvider": true,
                        "referencesProvider": false,
                        "renameProvider": false,
                        "signatureHelpProvider": {
                            "triggerCharacters": ["(", ","],
                            "retriggerCharacters": signature_retrigger_characters
                        },
                        "semanticTokensProvider": {
                            "legend": {
                                "tokenTypes": semantic_token_legend_types(),
                                "tokenModifiers": []
                            },
                            "full": true
                        },
                        "documentSymbolProvider": true,
                        "codeActionProvider": true
                    },
                    "serverInfo": {
                        "name": "machina-lsp",
                        "version": env!("CARGO_PKG_VERSION")
                    }
                }),
            );
            (HandlerAction::Continue, Some(response))
        }
        Some("initialized") => (HandlerAction::Continue, None),
        Some("textDocument/didOpen") => {
            let Some(params) = params.and_then(parse_did_open_params) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            if let Err(error) = session.open_document(&params.uri, params.version, &params.text) {
                return (
                    HandlerAction::Continue,
                    id.map(|id| session_error_response(id, &error)),
                );
            }
            publish_diagnostics_if_current(session, &params.uri, params.version)
        }
        Some("textDocument/didChange") => {
            let Some(params) = params.and_then(parse_did_change_params) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            if let Err(error) = session.change_document(&params.uri, params.version, &params.text) {
                return (
                    HandlerAction::Continue,
                    id.map(|id| session_error_response(id, &error)),
                );
            }
            publish_diagnostics_if_current(session, &params.uri, params.version)
        }
        Some("textDocument/didClose") => {
            let Some(params) = params.and_then(parse_did_close_params) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            if let Err(error) = session.close_document(&params.uri) {
                return (
                    HandlerAction::Continue,
                    id.map(|id| session_error_response(id, &error)),
                );
            }
            let notification = json!({
                "jsonrpc": "2.0",
                "method": "textDocument/publishDiagnostics",
                "params": {
                    "uri": params.uri,
                    "version": Value::Null,
                    "diagnostics": []
                }
            });
            (HandlerAction::Continue, Some(notification))
        }
        Some("textDocument/hover") => dispatch_read_request(session, params, id, hover_response),
        Some("textDocument/definition") => {
            dispatch_read_request(session, params, id, definition_response)
        }
        Some("textDocument/completion") => {
            dispatch_read_request(session, params, id, completion_response)
        }
        Some("textDocument/signatureHelp") => {
            dispatch_read_request(session, params, id, signature_help_response)
        }
        Some("textDocument/documentSymbol") => {
            let Some(params) = params.and_then(parse_text_document_request_params) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(id) = id else {
                return (HandlerAction::Continue, None);
            };
            let version = match text_document_request_version(session, &params) {
                Ok(version) => version,
                Err(error) => {
                    return (
                        HandlerAction::Continue,
                        Some(session_error_response(id, &error)),
                    );
                }
            };
            let response = document_symbol_response(session, id, &params.uri, version);
            (HandlerAction::Continue, Some(response))
        }
        Some("textDocument/semanticTokens/full") => {
            let Some(params) = params.and_then(parse_text_document_request_params) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(id) = id else {
                return (HandlerAction::Continue, None);
            };
            let version = match text_document_request_version(session, &params) {
                Ok(version) => version,
                Err(error) => {
                    return (
                        HandlerAction::Continue,
                        Some(session_error_response(id, &error)),
                    );
                }
            };
            let response = semantic_tokens_response(session, id, &params.uri, version);
            (HandlerAction::Continue, Some(response))
        }
        Some("textDocument/codeAction") => {
            let Some(params) = params.and_then(parse_code_action_request_params) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(id) = id else {
                return (HandlerAction::Continue, None);
            };
            let version = match params.version {
                Some(version) => version,
                None => match session.lookup_document(&params.uri) {
                    Ok(doc) => doc.version,
                    Err(error) => {
                        return (
                            HandlerAction::Continue,
                            Some(session_error_response(id, &error)),
                        );
                    }
                },
            };
            let response = code_action_response(
                session,
                id,
                &params.uri,
                params.range,
                version,
                params.context_diagnostics,
            );
            (HandlerAction::Continue, Some(response))
        }
        Some("shutdown") => {
            let response = success_response(id.unwrap_or(Value::Null), Value::Null);
            (HandlerAction::Continue, Some(response))
        }
        Some("exit") => (HandlerAction::Exit, None),
        Some(_) => {
            let response = id.map(|id| error_response(id, -32601, "method not found"));
            (HandlerAction::Continue, response)
        }
        None => {
            let response = id.map(|id| error_response(id, -32600, "invalid request"));
            (HandlerAction::Continue, response)
        }
    }
}

/// Characters that should keep parameter hints alive while editing inside
/// argument lists. VS Code re-requests signatureHelp when these are typed and
/// a hint session is already active.
fn signature_help_retrigger_characters() -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for ch in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".chars() {
        out.push(ch.to_string());
    }
    for ch in [
        '_', '"', '\'', '.', '-', '+', '*', '/', '%', '&', '|', '^', '!', '~', '=',
    ] {
        out.push(ch.to_string());
    }
    out
}

struct InitializeFeatureFlags {
    legacy_typestate: bool,
}

fn parse_initialize_feature_flags(params: Option<&Value>) -> InitializeFeatureFlags {
    let Some(init_opts) = params.and_then(|value| value.get("initializationOptions")) else {
        return InitializeFeatureFlags {
            legacy_typestate: false,
        };
    };

    // Preferred shape: initializationOptions.legacyFeatures = ["typestate", ...]
    let legacy_typestate = init_opts
        .get("legacyFeatures")
        .and_then(Value::as_array)
        .map(|features| {
            features
                .iter()
                .filter_map(Value::as_str)
                .any(|feature| feature == "typestate")
        })
        .unwrap_or(false);
    if legacy_typestate {
        return InitializeFeatureFlags { legacy_typestate };
    }

    // Backward-compat: initializationOptions.experimentalFeatures = ["typestate", ...]
    let legacy_typestate = init_opts
        .get("experimentalFeatures")
        .and_then(Value::as_array)
        .map(|features| {
            features
                .iter()
                .filter_map(Value::as_str)
                .any(|feature| feature == "typestate")
        })
        .unwrap_or(false);
    if legacy_typestate {
        return InitializeFeatureFlags { legacy_typestate };
    }

    // Backward-compat: initializationOptions.experimentalTypestate = true
    InitializeFeatureFlags {
        legacy_typestate: init_opts
            .get("experimentalTypestate")
            .and_then(Value::as_bool)
            .unwrap_or(false),
    }
}

fn parse_did_open_params(params: &Value) -> Option<DidOpenParams> {
    let text_doc = params.get("textDocument")?;
    let uri = text_doc.get("uri")?.as_str()?.to_string();
    let version = i32::try_from(text_doc.get("version")?.as_i64()?).ok()?;
    let text = text_doc.get("text")?.as_str()?.to_string();
    Some(DidOpenParams { uri, version, text })
}

fn parse_did_change_params(params: &Value) -> Option<DidChangeParams> {
    let text_doc = params.get("textDocument")?;
    let uri = text_doc.get("uri")?.as_str()?.to_string();
    let version = i32::try_from(text_doc.get("version")?.as_i64()?).ok()?;
    let content_changes = params.get("contentChanges")?.as_array()?;
    let text = content_changes
        .last()
        .and_then(|change| change.get("text"))
        .and_then(Value::as_str)?
        .to_string();
    Some(DidChangeParams { uri, version, text })
}

fn parse_did_close_params(params: &Value) -> Option<DidCloseParams> {
    let text_doc = params.get("textDocument")?;
    let uri = text_doc.get("uri")?.as_str()?.to_string();
    Some(DidCloseParams { uri })
}

fn parse_read_request_params(params: &Value) -> Option<ReadRequestParams> {
    let text_doc = params.get("textDocument")?;
    let uri = text_doc.get("uri")?.as_str()?.to_string();
    let position = params.get("position")?;
    let line0 = usize::try_from(position.get("line")?.as_u64()?).ok()?;
    let col0 = usize::try_from(position.get("character")?.as_u64()?).ok()?;
    let version = params
        .get("mcDocVersion")
        .and_then(Value::as_i64)
        .and_then(|v| i32::try_from(v).ok());
    Some(ReadRequestParams {
        uri,
        line0,
        col0,
        version,
    })
}

fn parse_text_document_request_params(params: &Value) -> Option<TextDocumentRequestParams> {
    let text_doc = params.get("textDocument")?;
    let uri = text_doc.get("uri")?.as_str()?.to_string();
    let version = params
        .get("mcDocVersion")
        .and_then(Value::as_i64)
        .and_then(|v| i32::try_from(v).ok());
    Some(TextDocumentRequestParams { uri, version })
}

fn parse_code_action_request_params(params: &Value) -> Option<CodeActionRequestParams> {
    let text_doc = params.get("textDocument")?;
    let uri = text_doc.get("uri")?.as_str()?.to_string();
    let range = params.get("range")?;
    let start = range.get("start")?;
    let end = range.get("end")?;
    let start_line = usize::try_from(start.get("line")?.as_u64()?).ok()?;
    let start_col = usize::try_from(start.get("character")?.as_u64()?).ok()?;
    let end_line = usize::try_from(end.get("line")?.as_u64()?).ok()?;
    let end_col = usize::try_from(end.get("character")?.as_u64()?).ok()?;
    let version = params
        .get("mcDocVersion")
        .and_then(Value::as_i64)
        .and_then(|v| i32::try_from(v).ok());
    let context_diagnostics = params
        .get("context")
        .and_then(|ctx| ctx.get("diagnostics"))
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    let range = machina::core::diag::Span {
        start: machina::core::diag::Position {
            offset: 0,
            line: start_line.saturating_add(1),
            column: start_col.saturating_add(1),
        },
        end: machina::core::diag::Position {
            offset: 0,
            line: end_line.saturating_add(1),
            column: end_col.saturating_add(1),
        },
    };
    Some(CodeActionRequestParams {
        uri,
        range,
        version,
        context_diagnostics,
    })
}

fn read_request_version(
    session: &AnalysisSession,
    params: &ReadRequestParams,
) -> Result<i32, SessionError> {
    if let Some(version) = params.version {
        return Ok(version);
    }
    Ok(session.lookup_document(&params.uri)?.version)
}

fn text_document_request_version(
    session: &AnalysisSession,
    params: &TextDocumentRequestParams,
) -> Result<i32, SessionError> {
    if let Some(version) = params.version {
        return Ok(version);
    }
    Ok(session.lookup_document(&params.uri)?.version)
}

fn publish_diagnostics_if_current(
    session: &mut AnalysisSession,
    uri: &str,
    version: i32,
) -> (HandlerAction, Option<Value>) {
    let diagnostics = match session.diagnostics_for_uri_if_version(uri, version) {
        Ok(Some(diag)) => diag,
        Ok(None) => return (HandlerAction::Continue, None),
        Err(_) => Vec::new(),
    };
    let notification = publish_diagnostics_notification(uri, version, diagnostics);
    (HandlerAction::Continue, Some(notification))
}

/// Convert LSP 0-based line/column to a 1-based point span used internally.
fn span_from_lsp_position(line0: usize, col0: usize) -> machina::core::diag::Span {
    let line = line0.saturating_add(1);
    let col = col0.saturating_add(1);
    machina::core::diag::Span {
        start: machina::core::diag::Position {
            offset: 0,
            line,
            column: col,
        },
        end: machina::core::diag::Position {
            offset: 0,
            line,
            column: col,
        },
    }
}

/// Build a JSON-RPC 2.0 success envelope.
fn success_response(id: Value, result: Value) -> Value {
    json!({"jsonrpc": "2.0", "id": id, "result": result})
}

/// Build a JSON-RPC 2.0 error envelope.
fn error_response(id: Value, code: i32, message: &str) -> Value {
    json!({"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message}})
}

/// Shared dispatch for position-based read requests (hover, definition,
/// completion, signatureHelp). Parses params, resolves the document version,
/// and delegates to the given `handler` to build the response.
fn dispatch_read_request(
    session: &mut AnalysisSession,
    params: Option<&Value>,
    id: Option<Value>,
    handler: impl FnOnce(&mut AnalysisSession, Value, &str, usize, usize, i32) -> Value,
) -> (HandlerAction, Option<Value>) {
    let Some(params) = params.and_then(parse_read_request_params) else {
        return (HandlerAction::Continue, invalid_params_response(id));
    };
    let Some(id) = id else {
        return (HandlerAction::Continue, None);
    };
    let version = match read_request_version(session, &params) {
        Ok(version) => version,
        Err(error) => {
            return (
                HandlerAction::Continue,
                Some(session_error_response(id, &error)),
            );
        }
    };
    let response = handler(session, id, &params.uri, params.line0, params.col0, version);
    (HandlerAction::Continue, Some(response))
}

/// Pre-query staleness gate: verify the document version is still current,
/// then resolve the URI to a `FileId`. Returns `Err(response)` if stale or
/// unknown, so callers can early-return with the response directly.
fn resolve_versioned_file(
    session: &AnalysisSession,
    id: &Value,
    uri: &str,
    version: i32,
) -> Result<FileId, Value> {
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return Err(stale_result_response(id.clone()));
    }
    session
        .file_id_for_uri(uri)
        .map_err(|e| session_error_response(id.clone(), &e))
}

/// Execute an analysis query, then re-check document version afterwards.
/// A didChange may arrive while the query runs; the post-check discards
/// results computed against an outdated snapshot.
fn version_guarded_query<T>(
    session: &mut AnalysisSession,
    id: &Value,
    uri: &str,
    version: i32,
    query: impl FnOnce(&mut AnalysisDb) -> QueryResult<T>,
) -> Result<T, Value> {
    let result = session
        .execute_query(query)
        .map_err(|_| session_error_response(id.clone(), &SessionError::Cancelled))?;
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return Err(stale_result_response(id.clone()));
    }
    Ok(result)
}

fn hover_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    line0: usize,
    col0: usize,
    version: i32,
) -> Value {
    let file_id = match resolve_versioned_file(session, &id, uri, version) {
        Ok(fid) => fid,
        Err(r) => return r,
    };
    if session.is_position_in_line_comment(file_id, line0, col0) {
        return success_response(id, Value::Null);
    }
    let span = span_from_lsp_position(line0, col0);
    let result = match version_guarded_query(session, &id, uri, version, |db| {
        db.hover_at_program_file(file_id, span)
    }) {
        Ok(r) => r,
        Err(r) => return r,
    };
    match result {
        Some(hover) => {
            let hover_range = match version_guarded_query(session, &id, uri, version, |db| {
                db.def_location_at_program_file(file_id, span)
            }) {
                Ok(Some(location))
                    if location.path.as_deref().map(path_to_file_uri).as_deref() == Some(uri) =>
                {
                    hover.span
                }
                _ => span,
            };
            success_response(
                id,
                json!({
                    "contents": {
                        "kind": "markdown",
                        "value": hover_markdown_value(&hover.display)
                    },
                    // LSP hover ranges are interpreted in the current
                    // document, so only reuse the source definition span when
                    // the definition is in the same file.
                    "range": {
                        "start": {"line": hover_range.start.line.saturating_sub(1), "character": hover_range.start.column.saturating_sub(1)},
                        "end": {"line": hover_range.end.line.saturating_sub(1), "character": hover_range.end.column.saturating_sub(1)}
                    }
                }),
            )
        }
        None => success_response(id, Value::Null),
    }
}

fn hover_markdown_value(display: &str) -> String {
    let escaped = display.replace("```", "\\`\\`\\`");
    format!("```machina\n{escaped}\n```")
}

fn definition_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    line0: usize,
    col0: usize,
    version: i32,
) -> Value {
    let file_id = match resolve_versioned_file(session, &id, uri, version) {
        Ok(fid) => fid,
        Err(r) => return r,
    };
    if session.is_position_in_line_comment(file_id, line0, col0) {
        return success_response(id, json!([]));
    }
    let span = span_from_lsp_position(line0, col0);
    let result = match version_guarded_query(session, &id, uri, version, |db| {
        db.def_location_at_program_file(file_id, span)
    }) {
        Ok(r) => r,
        Err(r) => return r,
    };
    match result {
        Some(location) => {
            let uri = location
                .path
                .as_deref()
                .map(path_to_file_uri)
                .unwrap_or_else(|| uri.to_string());
            success_response(
                id,
                json!([{
                    "uri": uri,
                    "range": {
                        "start": {
                            "line": location.span.start.line.saturating_sub(1),
                            "character": location.span.start.column.saturating_sub(1)
                        },
                        "end": {
                            "line": location.span.end.line.saturating_sub(1),
                            "character": location.span.end.column.saturating_sub(1)
                        }
                    }
                }]),
            )
        }
        None => success_response(id, json!([])),
    }
}

fn path_to_file_uri(path: &Path) -> String {
    format!("file://{}", path.to_string_lossy())
}

fn completion_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    line0: usize,
    col0: usize,
    version: i32,
) -> Value {
    let file_id = match resolve_versioned_file(session, &id, uri, version) {
        Ok(fid) => fid,
        Err(r) => return r,
    };
    let span = span_from_lsp_position(line0, col0);
    let items = match version_guarded_query(session, &id, uri, version, |db| {
        db.completions_at_program_file(file_id, span)
    }) {
        Ok(r) => r,
        Err(r) => return r,
    };
    let items: Vec<Value> = items
        .into_iter()
        .map(|item| {
            let kind = match item.kind {
                machina::services::analysis::results::CompletionKind::Function => 3,
                machina::services::analysis::results::CompletionKind::Type => 7,
                machina::services::analysis::results::CompletionKind::Trait => 7,
                machina::services::analysis::results::CompletionKind::Variable => 6,
                machina::services::analysis::results::CompletionKind::Parameter => 6,
                machina::services::analysis::results::CompletionKind::TypeParameter => 25,
                machina::services::analysis::results::CompletionKind::EnumVariant => 20,
            };
            json!({
                "label": item.label,
                "kind": kind,
                "detail": item.detail
            })
        })
        .collect();
    success_response(
        id,
        json!({
            "isIncomplete": false,
            "items": items
        }),
    )
}

fn signature_help_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    line0: usize,
    col0: usize,
    _version: i32,
) -> Value {
    // Signature help is highly interactive while typing. Be permissive about
    // transient version skew so we can still show useful parameter hints
    // instead of returning stale-result errors during fast edits.
    let file_id = match session.file_id_for_uri(uri) {
        Ok(fid) => fid,
        Err(error) => return session_error_response(id, &error),
    };
    if session.is_position_in_line_comment(file_id, line0, col0) {
        return success_response(id, Value::Null);
    }
    let span = span_from_lsp_position(line0, col0);
    let result = match session.execute_query(|db| db.signature_help_at_program_file(file_id, span))
    {
        Ok(r) => r,
        Err(_) => return session_error_response(id, &SessionError::Cancelled),
    };
    match result {
        Some(sig) => {
            let parameters: Vec<Value> = sig
                .parameters
                .iter()
                .map(|param| json!({ "label": param }))
                .collect();
            success_response(
                id,
                json!({
                    "signatures": [{
                        "label": sig.label,
                        "parameters": parameters
                    }],
                    "activeSignature": 0,
                    "activeParameter": sig.active_parameter
                }),
            )
        }
        None => success_response(id, Value::Null),
    }
}

fn document_symbol_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    version: i32,
) -> Value {
    let file_id = match resolve_versioned_file(session, &id, uri, version) {
        Ok(fid) => fid,
        Err(r) => return r,
    };
    let symbols = match version_guarded_query(session, &id, uri, version, |db| {
        db.document_symbols_at_file(file_id)
    }) {
        Ok(r) => r,
        Err(r) => return r,
    };
    let items: Vec<Value> = symbols
        .into_iter()
        .map(|symbol| {
            json!({
                "name": symbol.name,
                "detail": symbol.detail,
                "kind": lsp_document_symbol_kind(symbol.kind),
                "range": {
                    "start": {
                        "line": symbol.span.start.line.saturating_sub(1),
                        "character": symbol.span.start.column.saturating_sub(1)
                    },
                    "end": {
                        "line": symbol.span.end.line.saturating_sub(1),
                        "character": symbol.span.end.column.saturating_sub(1)
                    }
                },
                "selectionRange": {
                    "start": {
                        "line": symbol.span.start.line.saturating_sub(1),
                        "character": symbol.span.start.column.saturating_sub(1)
                    },
                    "end": {
                        "line": symbol.span.end.line.saturating_sub(1),
                        "character": symbol.span.end.column.saturating_sub(1)
                    }
                },
                "children": []
            })
        })
        .collect();
    success_response(id, json!(items))
}

fn lsp_document_symbol_kind(kind: machina::services::analysis::results::DocumentSymbolKind) -> u32 {
    match kind {
        machina::services::analysis::results::DocumentSymbolKind::Type => 5,
        machina::services::analysis::results::DocumentSymbolKind::Trait => 11,
        machina::services::analysis::results::DocumentSymbolKind::Function => 12,
        machina::services::analysis::results::DocumentSymbolKind::Method => 6,
        machina::services::analysis::results::DocumentSymbolKind::Property => 7,
    }
}

fn semantic_tokens_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    version: i32,
) -> Value {
    let file_id = match resolve_versioned_file(session, &id, uri, version) {
        Ok(fid) => fid,
        Err(r) => return r,
    };
    let tokens = match version_guarded_query(session, &id, uri, version, |db| {
        db.semantic_tokens_at_file(file_id)
    }) {
        Ok(r) => r,
        Err(r) => return r,
    };
    success_response(
        id,
        json!({
            "data": encode_semantic_tokens(tokens)
        }),
    )
}

fn semantic_token_legend_types() -> Vec<&'static str> {
    vec![
        "type",
        "class",
        "function",
        "method",
        "property",
        "variable",
        "parameter",
        "typeParameter",
        "enumMember",
    ]
}

fn semantic_token_kind_index(kind: machina::services::analysis::results::SemanticTokenKind) -> u32 {
    match kind {
        machina::services::analysis::results::SemanticTokenKind::Type => 0,
        machina::services::analysis::results::SemanticTokenKind::Trait => 1,
        machina::services::analysis::results::SemanticTokenKind::Function => 2,
        machina::services::analysis::results::SemanticTokenKind::Method => 3,
        machina::services::analysis::results::SemanticTokenKind::Property => 4,
        machina::services::analysis::results::SemanticTokenKind::Variable => 5,
        machina::services::analysis::results::SemanticTokenKind::Parameter => 6,
        machina::services::analysis::results::SemanticTokenKind::TypeParameter => 7,
        machina::services::analysis::results::SemanticTokenKind::EnumVariant => 8,
    }
}

fn encode_semantic_tokens(
    tokens: Vec<machina::services::analysis::results::SemanticToken>,
) -> Vec<u32> {
    let mut out = Vec::with_capacity(tokens.len() * 5);
    let mut prev_line0 = 0usize;
    let mut prev_col0 = 0usize;
    let mut first = true;
    for token in tokens {
        let line0 = token.span.start.line.saturating_sub(1);
        let col0 = token.span.start.column.saturating_sub(1);
        let delta_line = if first {
            line0
        } else {
            line0.saturating_sub(prev_line0)
        };
        let delta_start = if first || delta_line > 0 {
            col0
        } else {
            col0.saturating_sub(prev_col0)
        };
        let length = if token.span.start.line == token.span.end.line {
            token
                .span
                .end
                .column
                .saturating_sub(token.span.start.column)
                .max(1)
        } else {
            1
        };
        out.push(delta_line as u32);
        out.push(delta_start as u32);
        out.push(length as u32);
        out.push(semantic_token_kind_index(token.kind));
        out.push(0);
        prev_line0 = line0;
        prev_col0 = col0;
        first = false;
    }
    out
}

fn code_action_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    range: machina::core::diag::Span,
    version: i32,
    context_diagnostics: Vec<Value>,
) -> Value {
    let file_id = match resolve_versioned_file(session, &id, uri, version) {
        Ok(fid) => fid,
        Err(r) => return r,
    };
    let diagnostics = match session.diagnostics_for_uri_if_version(uri, version) {
        Ok(Some(diag)) => diag,
        Ok(None) => return stale_result_response(id),
        Err(error) => return session_error_response(id, &error),
    };
    let actions = match version_guarded_query(session, &id, uri, version, move |db| {
        db.code_actions_for_diagnostics_at_file(file_id, range, diagnostics)
    }) {
        Ok(r) => r,
        Err(r) => return r,
    };
    let items: Vec<Value> = actions
        .into_iter()
        .map(|action| {
            let edits: Vec<Value> = action
                .edits
                .into_iter()
                .map(|edit| {
                    json!({
                        "range": {
                            "start": {
                                "line": edit.span.start.line.saturating_sub(1),
                                "character": edit.span.start.column.saturating_sub(1)
                            },
                            "end": {
                                "line": edit.span.end.line.saturating_sub(1),
                                "character": edit.span.end.column.saturating_sub(1)
                            }
                        },
                        "newText": edit.new_text
                    })
                })
                .collect();
            let mut changes = Map::new();
            changes.insert(uri.to_string(), Value::Array(edits));
            let action_diagnostics: Vec<Value> = context_diagnostics
                .iter()
                .filter(|diag| {
                    diag.get("code")
                        .and_then(Value::as_str)
                        .is_some_and(|code| code == action.diagnostic_code)
                })
                .cloned()
                .collect();
            json!({
                "title": action.title,
                "kind": "quickfix",
                "diagnostics": action_diagnostics,
                "edit": {
                    "changes": changes
                },
                "data": {
                    "diagnosticCode": action.diagnostic_code
                }
            })
        })
        .collect();
    success_response(id, json!(items))
}

/// Stale results are returned as `null` (a valid "no info" response) rather
/// than an error, since staleness is a normal race condition, not a fault.
fn stale_result_response(id: Value) -> Value {
    success_response(id, Value::Null)
}

fn publish_diagnostics_notification(
    uri: &str,
    version: i32,
    diagnostics: Vec<machina::services::analysis::diagnostics::Diagnostic>,
) -> Value {
    let diagnostics: Vec<Value> = diagnostics
        .into_iter()
        .map(|diag| {
            json!({
                "range": {
                    "start": {
                        "line": diag.span.start.line.saturating_sub(1),
                        "character": diag.span.start.column.saturating_sub(1)
                    },
                    "end": {
                        "line": diag.span.end.line.saturating_sub(1),
                        "character": diag.span.end.column.saturating_sub(1)
                    }
                },
                "severity": 1,
                "code": diag.code,
                "message": diag.message,
                "source": "machina"
            })
        })
        .collect();
    json!({
        "jsonrpc": "2.0",
        "method": "textDocument/publishDiagnostics",
        "params": {
            "uri": uri,
            "version": version,
            "diagnostics": diagnostics
        }
    })
}

fn session_error_response(id: Value, error: &SessionError) -> Value {
    let message = match error {
        SessionError::UnsupportedUri(_) => "unsupported uri",
        SessionError::UnknownUri(_) => "unknown document",
        SessionError::Cancelled => "analysis query cancelled",
        SessionError::StaleVersion { .. } => "stale document version",
    };
    error_response(id, -32001, message)
}

fn invalid_params_response(id: Option<Value>) -> Option<Value> {
    id.map(|id| error_response(id, -32602, "invalid params"))
}

#[cfg(test)]
#[path = "./tests/t_handlers.rs"]
mod tests;
