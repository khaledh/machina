//! Minimal JSON-RPC request handlers for LSP bootstrap methods.

use serde_json::{Map, Value, json};
use std::path::Path;

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
            session.set_experimental_typestate(parse_initialize_typestate_flag(params));
            let response = json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": {
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
                            "retriggerCharacters": [","]
                        },
                        "semanticTokensProvider": Value::Null,
                        "documentSymbolProvider": false,
                        "codeActionProvider": true
                    },
                    "serverInfo": {
                        "name": "machina-lsp",
                        "version": env!("CARGO_PKG_VERSION")
                    }
                }
            });
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
        Some("textDocument/hover") => {
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
            let response =
                hover_response(session, id, &params.uri, params.line0, params.col0, version);
            (HandlerAction::Continue, Some(response))
        }
        Some("textDocument/definition") => {
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
            let response =
                definition_response(session, id, &params.uri, params.line0, params.col0, version);
            (HandlerAction::Continue, Some(response))
        }
        Some("textDocument/completion") => {
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
            let response =
                completion_response(session, id, &params.uri, params.line0, params.col0, version);
            (HandlerAction::Continue, Some(response))
        }
        Some("textDocument/signatureHelp") => {
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
            let response = signature_help_response(
                session,
                id,
                &params.uri,
                params.line0,
                params.col0,
                version,
            );
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
            let response = json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": Value::Null
            });
            (HandlerAction::Continue, Some(response))
        }
        Some("exit") => (HandlerAction::Exit, None),
        Some(_) => {
            let response = id.map(|id| {
                json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "error": {
                        "code": -32601,
                        "message": "method not found"
                    }
                })
            });
            (HandlerAction::Continue, response)
        }
        None => {
            let response = id.map(|id| {
                json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "error": {
                        "code": -32600,
                        "message": "invalid request"
                    }
                })
            });
            (HandlerAction::Continue, response)
        }
    }
}

fn parse_initialize_typestate_flag(params: Option<&Value>) -> bool {
    let Some(init_opts) = params.and_then(|value| value.get("initializationOptions")) else {
        return false;
    };
    // Preferred shape: initializationOptions.experimentalFeatures = ["typestate", ...]
    let list_enabled = init_opts
        .get("experimentalFeatures")
        .and_then(Value::as_array)
        .map(|features| {
            features
                .iter()
                .filter_map(Value::as_str)
                .any(|feature| feature == "typestate")
        })
        .unwrap_or(false);
    if list_enabled {
        return true;
    }
    // Backward-compat: initializationOptions.experimentalTypestate = true
    init_opts
        .get("experimentalTypestate")
        .and_then(Value::as_bool)
        .unwrap_or(false)
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

fn hover_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    line0: usize,
    col0: usize,
    version: i32,
) -> Value {
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    let file_id = match session.file_id_for_uri(uri) {
        Ok(file_id) => file_id,
        Err(error) => return session_error_response(id, &error),
    };
    if is_position_in_line_comment(session, file_id, line0, col0) {
        return json!({"jsonrpc":"2.0","id":id,"result": Value::Null});
    }
    let span = span_from_lsp_position(line0, col0);
    let result = session.execute_query(|db| db.hover_at_program_file(file_id, span));
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    match result {
        Ok(Some(hover)) => json!({
            "jsonrpc": "2.0",
            "id": id,
            "result": {
                "contents": {
                    "kind": "markdown",
                    "value": hover_markdown_value(&hover.display)
                },
                "range": {
                    "start": {"line": hover.span.start.line.saturating_sub(1), "character": hover.span.start.column.saturating_sub(1)},
                    "end": {"line": hover.span.end.line.saturating_sub(1), "character": hover.span.end.column.saturating_sub(1)}
                }
            }
        }),
        Ok(None) => json!({"jsonrpc":"2.0","id":id,"result": Value::Null}),
        Err(_) => session_error_response(id, &SessionError::Cancelled),
    }
}

fn hover_markdown_value(display: &str) -> String {
    let escaped = display.replace("```", "\\`\\`\\`");
    format!("```machina\n{escaped}\n```")
}

fn is_position_in_line_comment(
    session: &AnalysisSession,
    file_id: FileId,
    line0: usize,
    col0: usize,
) -> bool {
    let snapshot = session.snapshot();
    let Some(source) = snapshot.text(file_id) else {
        return false;
    };
    let Some(line) = source.lines().nth(line0) else {
        return false;
    };
    is_column_in_line_comment(line, col0)
}

fn is_column_in_line_comment(line: &str, col0: usize) -> bool {
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0usize;
    let mut in_string = false;
    let mut in_char = false;
    let mut escaped = false;

    while i + 1 < chars.len() {
        let ch = chars[i];
        if in_string {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        if in_char {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '\'' {
                in_char = false;
            }
            i += 1;
            continue;
        }

        if ch == '"' {
            in_string = true;
            i += 1;
            continue;
        }
        if ch == '\'' {
            in_char = true;
            i += 1;
            continue;
        }
        if ch == '/' && chars[i + 1] == '/' {
            return col0 >= i;
        }
        i += 1;
    }
    false
}

fn definition_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    line0: usize,
    col0: usize,
    version: i32,
) -> Value {
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    let file_id = match session.file_id_for_uri(uri) {
        Ok(file_id) => file_id,
        Err(error) => return session_error_response(id, &error),
    };
    if is_position_in_line_comment(session, file_id, line0, col0) {
        return json!({"jsonrpc":"2.0","id":id,"result": []});
    }
    let span = span_from_lsp_position(line0, col0);
    let result = session.execute_query(|db| db.def_location_at_program_file(file_id, span));
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    match result {
        Ok(Some(location)) => {
            let uri = location
                .path
                .as_deref()
                .map(path_to_file_uri)
                .unwrap_or_else(|| uri.to_string());
            json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": [
                    {
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
                    }
                ]
            })
        }
        Ok(None) => json!({"jsonrpc":"2.0","id":id,"result": []}),
        Err(_) => session_error_response(id, &SessionError::Cancelled),
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
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    let file_id = match session.file_id_for_uri(uri) {
        Ok(file_id) => file_id,
        Err(error) => return session_error_response(id, &error),
    };
    let span = span_from_lsp_position(line0, col0);
    let result = session.execute_query(|db| db.completions_at_program_file(file_id, span));
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    match result {
        Ok(items) => {
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
            json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": {
                    "isIncomplete": false,
                    "items": items
                }
            })
        }
        Err(_) => session_error_response(id, &SessionError::Cancelled),
    }
}

fn signature_help_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    line0: usize,
    col0: usize,
    version: i32,
) -> Value {
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    let file_id = match session.file_id_for_uri(uri) {
        Ok(file_id) => file_id,
        Err(error) => return session_error_response(id, &error),
    };
    if is_position_in_line_comment(session, file_id, line0, col0) {
        return json!({"jsonrpc":"2.0","id":id,"result": Value::Null});
    }
    let span = span_from_lsp_position(line0, col0);
    let result = session.execute_query(|db| db.signature_help_at_program_file(file_id, span));
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    match result {
        Ok(Some(sig)) => {
            let parameters: Vec<Value> = sig
                .parameters
                .iter()
                .map(|param| json!({ "label": param }))
                .collect();
            json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": {
                    "signatures": [{
                        "label": sig.label,
                        "parameters": parameters
                    }],
                    "activeSignature": 0,
                    "activeParameter": sig.active_parameter
                }
            })
        }
        Ok(None) => json!({"jsonrpc":"2.0","id":id,"result": Value::Null}),
        Err(_) => session_error_response(id, &SessionError::Cancelled),
    }
}

fn code_action_response(
    session: &mut AnalysisSession,
    id: Value,
    uri: &str,
    range: machina::core::diag::Span,
    version: i32,
    context_diagnostics: Vec<Value>,
) -> Value {
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    let file_id = match session.file_id_for_uri(uri) {
        Ok(file_id) => file_id,
        Err(error) => return session_error_response(id, &error),
    };
    let diagnostics = match session.diagnostics_for_uri_if_version(uri, version) {
        Ok(Some(diag)) => diag,
        Ok(None) => return stale_result_response(id),
        Err(error) => return session_error_response(id, &error),
    };
    let result = session.execute_query(move |db| {
        db.code_actions_for_diagnostics_at_file(file_id, range, diagnostics)
    });
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    match result {
        Ok(actions) => {
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
            json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": items
            })
        }
        Err(_) => session_error_response(id, &SessionError::Cancelled),
    }
}

fn stale_result_response(id: Value) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": Value::Null
    })
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
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "error": {
            "code": -32001,
            "message": message
        }
    })
}

fn invalid_params_response(id: Option<Value>) -> Option<Value> {
    id.map(|id| {
        json!({
            "jsonrpc": "2.0",
            "id": id,
            "error": {
                "code": -32602,
                "message": "invalid params"
            }
        })
    })
}

#[cfg(test)]
mod tests {
    use super::{HandlerAction, handle_message};
    use crate::session::AnalysisSession;
    use serde_json::{Value, json};

    #[test]
    fn initialize_returns_capabilities() {
        let mut session = AnalysisSession::new();
        let (action, response) = handle_message(
            &mut session,
            json!({ "jsonrpc": "2.0", "id": 1, "method": "initialize" }),
        );
        assert_eq!(action, HandlerAction::Continue);
        let response = response.expect("expected initialize response");
        assert_eq!(response["id"], 1);
        assert_eq!(response["result"]["serverInfo"]["name"], "machina-lsp");
    }

    #[test]
    fn initialize_enables_typestate_when_requested() {
        let mut session = AnalysisSession::new();
        let (action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 11,
                "method": "initialize",
                "params": {
                    "initializationOptions": {
                        "experimentalTypestate": true
                    }
                }
            }),
        );
        assert_eq!(action, HandlerAction::Continue);
        assert!(response.is_some(), "expected initialize response");
        assert!(
            session.experimental_typestate(),
            "initialize option should enable typestate in analysis session"
        );
    }

    #[test]
    fn initialize_enables_typestate_from_experimental_features_list() {
        let mut session = AnalysisSession::new();
        let (action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 13,
                "method": "initialize",
                "params": {
                    "initializationOptions": {
                        "experimentalFeatures": ["typestate"]
                    }
                }
            }),
        );
        assert_eq!(action, HandlerAction::Continue);
        assert!(response.is_some(), "expected initialize response");
        assert!(
            session.experimental_typestate(),
            "experimentalFeatures should enable typestate in analysis session"
        );
    }

    #[test]
    fn did_open_typestate_reports_feature_disabled_by_default() {
        let mut session = AnalysisSession::new();
        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-typestate-disabled.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": "typestate Connection { fn new() -> Disconnected { Disconnected } state Disconnected {} }"
                    }
                }
            }),
        );
        let response = response.expect("expected diagnostics notification");
        let diagnostics = response["params"]["diagnostics"]
            .as_array()
            .expect("diagnostics should be an array");
        assert!(
            diagnostics.iter().any(|diag| diag.get("code")
                == Some(&Value::String("MC-PARSE-FEATURE-DISABLED".to_string()))),
            "expected feature-disabled diagnostic, got: {diagnostics:#?}"
        );
    }

    #[test]
    fn did_open_typestate_parses_when_initialize_enables_feature() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 12,
                "method": "initialize",
                "params": {
                    "initializationOptions": {
                        "experimentalFeatures": ["typestate"]
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-typestate-enabled.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": "typestate Connection { fn new() -> Disconnected { Disconnected } state Disconnected {} }"
                    }
                }
            }),
        );
        let response = response.expect("expected diagnostics notification");
        let diagnostics = response["params"]["diagnostics"]
            .as_array()
            .expect("diagnostics should be an array");
        assert!(
            diagnostics.iter().all(|diag| diag.get("code")
                != Some(&Value::String("MC-PARSE-FEATURE-DISABLED".to_string()))),
            "did not expect feature-disabled diagnostic after enabling typestate, got: {diagnostics:#?}"
        );
    }

    #[test]
    fn hover_over_typestate_fields_block_returns_field_hover() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 13,
                "method": "initialize",
                "params": {
                    "initializationOptions": {
                        "experimentalFeatures": ["typestate"]
                    }
                }
            }),
        );
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-typestate-hover-field.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": "typestate Connection {\n    fields {\n        retries: u64,\n    }\n\n    fn new() -> Disconnected {\n        Disconnected { retries: 0 }\n    }\n\n    state Disconnected {}\n}\n"
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 14,
                "method": "textDocument/hover",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-typestate-hover-field.mc" },
                    "position": { "line": 2, "character": 11 },
                    "mcDocVersion": 1
                }
            }),
        );
        let response = response.expect("expected hover response");
        let value = response["result"]["contents"]["value"]
            .as_str()
            .expect("hover markdown value should be a string");
        assert!(
            value.contains("retries"),
            "expected typestate field hover, got: {value}"
        );
        assert!(
            !value.contains("__rt_print"),
            "typestate field hover should not bind runtime intrinsics, got: {value}"
        );
    }

    #[test]
    fn unknown_method_returns_method_not_found() {
        let mut session = AnalysisSession::new();
        let (action, response) = handle_message(
            &mut session,
            json!({ "jsonrpc": "2.0", "id": 10, "method": "foo/bar" }),
        );
        assert_eq!(action, HandlerAction::Continue);
        let response = response.expect("expected error response");
        assert_eq!(response["error"]["code"], -32601);
    }

    #[test]
    fn notifications_without_id_do_not_emit_error_response() {
        let mut session = AnalysisSession::new();
        let (action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "workspace/didChangeConfiguration",
                "params": {}
            }),
        );
        assert_eq!(action, HandlerAction::Continue);
        assert!(response.is_none());
    }

    #[test]
    fn did_open_publishes_diagnostics_notification() {
        let mut session = AnalysisSession::new();
        let (action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-open.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": "fn main() {}"
                    }
                }
            }),
        );
        assert_eq!(action, HandlerAction::Continue);
        let response = response.expect("expected diagnostics notification");
        assert_eq!(response["method"], "textDocument/publishDiagnostics");
        assert_eq!(response["params"]["version"], 1);
    }

    #[test]
    fn did_change_publishes_diagnostics_for_new_version() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-change.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": "fn main() {}"
                    }
                }
            }),
        );
        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didChange",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-change.mc",
                        "version": 2
                    },
                    "contentChanges": [
                        {
                            "text": "fn main("
                        }
                    ]
                }
            }),
        );
        let response = response.expect("expected diagnostics notification");
        assert_eq!(response["method"], "textDocument/publishDiagnostics");
        assert_eq!(response["params"]["version"], 2);
        assert!(
            response["params"]["diagnostics"]
                .as_array()
                .expect("diagnostics must be array")
                .len()
                >= 1
        );
    }

    #[test]
    fn did_change_with_stale_version_returns_session_error() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-change-stale.mc",
                        "version": 5,
                        "languageId": "machina",
                        "text": "fn main() {}"
                    }
                }
            }),
        );
        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 42,
                "method": "textDocument/didChange",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-change-stale.mc",
                        "version": 4
                    },
                    "contentChanges": [
                        { "text": "fn main() { let x = 1; }" }
                    ]
                }
            }),
        );
        let response = response.expect("expected error response");
        assert_eq!(response["id"], 42);
        assert_eq!(response["error"]["code"], -32001);
    }

    #[test]
    fn did_close_clears_published_diagnostics() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-close.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": "fn main("
                    }
                }
            }),
        );
        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didClose",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-close.mc"
                    }
                }
            }),
        );
        let response = response.expect("expected clear diagnostics notification");
        assert_eq!(response["method"], "textDocument/publishDiagnostics");
        assert_eq!(
            response["params"]["diagnostics"]
                .as_array()
                .expect("diagnostics must be array")
                .len(),
            0
        );
    }

    #[test]
    fn hover_request_returns_response_when_version_matches() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-hover.mc",
                        "version": 4,
                        "languageId": "machina",
                        "text": "fn main() {}"
                    }
                }
            }),
        );
        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 50,
                "method": "textDocument/hover",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-hover.mc" },
                    "position": { "line": 0, "character": 4 }
                }
            }),
        );
        let response = response.expect("expected hover response");
        assert_eq!(response["id"], 50);
        assert_eq!(response["result"]["contents"]["kind"], "markdown");
        let value = response["result"]["contents"]["value"]
            .as_str()
            .expect("hover markdown value should be a string");
        assert!(value.starts_with("```machina\n"));
    }

    #[test]
    fn hover_request_returns_null_when_version_is_stale() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-hover-stale.mc",
                        "version": 10,
                        "languageId": "machina",
                        "text": "fn main() {}"
                    }
                }
            }),
        );
        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 51,
                "method": "textDocument/hover",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-hover-stale.mc" },
                    "position": { "line": 0, "character": 1 },
                    "mcDocVersion": 9
                }
            }),
        );
        let response = response.expect("expected hover response");
        assert_eq!(response["result"], Value::Null);
    }

    #[test]
    fn hover_request_returns_null_over_line_comment() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-hover-comment.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": "fn main() {\n    // comment text\n    let x = 1;\n}\n"
                    }
                }
            }),
        );
        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 511,
                "method": "textDocument/hover",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-hover-comment.mc" },
                    "position": { "line": 1, "character": 8 }
                }
            }),
        );
        let response = response.expect("expected hover response");
        assert_eq!(response["result"], Value::Null);
    }

    #[test]
    fn completion_and_definition_requests_are_version_gated() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-comp-def.mc",
                        "version": 2,
                        "languageId": "machina",
                        "text": "fn id(x: u64) -> u64 { x }\nfn main() -> u64 { id(1) }"
                    }
                }
            }),
        );
        let (_a, completion) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 60,
                "method": "textDocument/completion",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-comp-def.mc" },
                    "position": { "line": 1, "character": 5 },
                    "mcDocVersion": 2
                }
            }),
        );
        assert!(completion.is_some());
        let (_b, definition) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 61,
                "method": "textDocument/definition",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-comp-def.mc" },
                    "position": { "line": 1, "character": 18 },
                    "mcDocVersion": 1
                }
            }),
        );
        let definition = definition.expect("expected definition response");
        assert_eq!(definition["result"], Value::Null);
    }

    #[test]
    fn definition_request_returns_location_when_version_matches() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-definition.mc",
                        "version": 2,
                        "languageId": "machina",
                        "text": "fn id(x: u64) -> u64 { x }\nfn main() -> u64 { id(1) }"
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 90,
                "method": "textDocument/definition",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-definition.mc" },
                    "position": { "line": 1, "character": 19 },
                    "mcDocVersion": 2
                }
            }),
        );

        let response = response.expect("expected definition response");
        assert_eq!(response["id"], 90);
        let locations = response["result"]
            .as_array()
            .expect("expected location array");
        assert!(
            !locations.is_empty(),
            "expected at least one definition location"
        );
        assert_eq!(locations[0]["uri"], "file:///tmp/lsp-definition.mc");
    }

    #[test]
    fn definition_request_returns_empty_over_line_comment() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-definition-comment.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": "fn id(x: u64) -> u64 { x }\nfn main() -> u64 {\n    // id(1)\n    id(1)\n}\n"
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 905,
                "method": "textDocument/definition",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-definition-comment.mc" },
                    "position": { "line": 2, "character": 8 },
                    "mcDocVersion": 1
                }
            }),
        );

        let response = response.expect("expected definition response");
        assert_eq!(response["id"], 905);
        assert_eq!(response["result"], json!([]));
    }

    #[test]
    fn definition_request_resolves_imported_symbol_location_across_modules() {
        let run_id = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system time should be valid")
            .as_nanos();
        let temp_dir = std::env::temp_dir().join(format!(
            "machina_lsp_def_import_{}_{}",
            std::process::id(),
            run_id
        ));
        let app_dir = temp_dir.join("app");
        std::fs::create_dir_all(&app_dir).expect("failed to create temp module tree");

        let entry_path = temp_dir.join("main.mc");
        let dep_path = app_dir.join("dep.mc");
        let entry_source = r#"requires {
    app::dep::run
}

fn main() -> u64 {
    run()
}"#;
        let dep_source = r#"@public
fn run() -> u64 { 1 }
"#;
        std::fs::write(&entry_path, entry_source).expect("failed to write entry source");
        std::fs::write(&dep_path, dep_source).expect("failed to write dependency source");

        let entry_uri = format!("file://{}", entry_path.to_string_lossy());
        let dep_uri = format!("file://{}", dep_path.to_string_lossy());

        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": entry_uri,
                        "version": 1,
                        "languageId": "machina",
                        "text": entry_source
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 91,
                "method": "textDocument/definition",
                "params": {
                    "textDocument": { "uri": entry_uri },
                    "position": { "line": 5, "character": 5 },
                    "mcDocVersion": 1
                }
            }),
        );

        let response = response.expect("expected definition response");
        assert_eq!(response["id"], 91);
        let locations = response["result"]
            .as_array()
            .expect("expected location array");
        assert_eq!(locations.len(), 1, "expected one definition location");
        assert_eq!(locations[0]["uri"], dep_uri);
        assert_eq!(locations[0]["range"]["start"]["line"], 1);

        let _ = std::fs::remove_dir_all(&temp_dir);
    }

    #[test]
    fn read_requests_without_version_use_current_document_version() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-read-no-version.mc",
                        "version": 3,
                        "languageId": "machina",
                        "text": "fn main() {}"
                    }
                }
            }),
        );

        let (_hover_action, hover) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 71,
                "method": "textDocument/hover",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-read-no-version.mc" },
                    "position": { "line": 0, "character": 1 }
                }
            }),
        );
        let hover = hover.expect("expected hover response");
        assert_eq!(hover["id"], 71);
        assert!(hover.get("error").is_none());

        let (_completion_action, completion) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 72,
                "method": "textDocument/completion",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-read-no-version.mc" },
                    "position": { "line": 0, "character": 1 }
                }
            }),
        );
        let completion = completion.expect("expected completion response");
        assert_eq!(completion["id"], 72);
        assert!(completion.get("error").is_none());

        let (_signature_action, signature) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 73,
                "method": "textDocument/signatureHelp",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-read-no-version.mc" },
                    "position": { "line": 0, "character": 1 }
                }
            }),
        );
        let signature = signature.expect("expected signature-help response");
        assert_eq!(signature["id"], 73);
        assert!(signature.get("error").is_none());
    }

    #[test]
    fn signature_help_request_uses_program_aware_imported_callable_signature() {
        let run_id = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system time should be valid")
            .as_nanos();
        let temp_dir = std::env::temp_dir().join(format!(
            "machina_lsp_signature_import_{}_{}",
            std::process::id(),
            run_id
        ));
        let app_dir = temp_dir.join("app");
        std::fs::create_dir_all(&app_dir).expect("failed to create temp module tree");

        let entry_path = temp_dir.join("main.mc");
        let dep_path = app_dir.join("dep.mc");
        let entry_source = r#"requires {
    app::dep::run
}

fn main() -> u64 {
    run(1, true)
}"#;
        let dep_source = r#"@public
fn run(x: u64, y: bool) -> u64 { x }
"#;
        std::fs::write(&entry_path, entry_source).expect("failed to write entry source");
        std::fs::write(&dep_path, dep_source).expect("failed to write dependency source");
        let entry_uri = format!("file://{}", entry_path.to_string_lossy());

        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": entry_uri,
                        "version": 1,
                        "languageId": "machina",
                        "text": entry_source
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 92,
                "method": "textDocument/signatureHelp",
                "params": {
                    "textDocument": { "uri": entry_uri },
                    "position": { "line": 5, "character": 11 },
                    "mcDocVersion": 1
                }
            }),
        );

        let response = response.expect("expected signature-help response");
        assert_eq!(response["id"], 92);
        assert!(
            response["result"]["signatures"][0]["label"]
                .as_str()
                .unwrap_or_default()
                .contains("run("),
            "expected imported callable signature label, got: {}",
            response
        );
        assert_eq!(response["result"]["activeParameter"], 1);

        let _ = std::fs::remove_dir_all(&temp_dir);
    }

    #[test]
    fn signature_help_request_returns_null_over_line_comment() {
        let mut session = AnalysisSession::new();
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-signature-comment.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": "fn add(a: u64, b: u64) -> u64 { a + b }\nfn main() {\n    // add(\n    add(1, 2);\n}\n"
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 906,
                "method": "textDocument/signatureHelp",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-signature-comment.mc" },
                    "position": { "line": 2, "character": 9 },
                    "mcDocVersion": 1
                }
            }),
        );

        let response = response.expect("expected signature-help response");
        assert_eq!(response["id"], 906);
        assert_eq!(response["result"], Value::Null);
    }

    #[test]
    fn code_action_request_returns_union_match_quickfix_with_edits() {
        let mut session = AnalysisSession::new();
        let source = r#"type ParseErr = {}
type IoErr = {}
fn load() -> u64 | ParseErr | IoErr { IoErr{} }
fn main() -> u64 {
    match load() {
        v: u64 => v,
    }
}"#;
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-code-action.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": source
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 100,
                "method": "textDocument/codeAction",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-code-action.mc" },
                    "range": {
                        "start": { "line": 4, "character": 4 },
                        "end": { "line": 6, "character": 5 }
                    },
                    "mcDocVersion": 1,
                    "context": { "diagnostics": [] }
                }
            }),
        );

        let response = response.expect("expected code action response");
        assert_eq!(response["id"], 100);
        let actions = response["result"]
            .as_array()
            .expect("expected code action result array");
        assert!(
            actions.iter().any(|a| a["kind"] == "quickfix"),
            "expected quick fix action"
        );
        let union_fix = actions
            .iter()
            .find(|a| {
                a["title"]
                    .as_str()
                    .unwrap_or_default()
                    .contains("Add match arms for missing union variants")
            })
            .expect("expected union match quick fix");
        let edit_text =
            union_fix["edit"]["changes"]["file:///tmp/lsp-code-action.mc"][0]["newText"]
                .as_str()
                .expect("expected generated edit text");
        assert!(edit_text.contains("v0: ParseErr =>"));
        assert!(edit_text.contains("v1: IoErr =>"));
    }

    #[test]
    fn code_action_request_attaches_matching_context_diagnostics() {
        let mut session = AnalysisSession::new();
        let source = r#"type ParseErr = {}
type IoErr = {}
fn load() -> u64 | ParseErr | IoErr { IoErr{} }
fn main() -> u64 {
    match load() {
        v: u64 => v,
    }
}"#;
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-code-action-diag.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": source
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 101,
                "method": "textDocument/codeAction",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-code-action-diag.mc" },
                    "range": {
                        "start": { "line": 4, "character": 4 },
                        "end": { "line": 6, "character": 5 }
                    },
                    "mcDocVersion": 1,
                    "context": {
                        "diagnostics": [
                            {
                                "range": {
                                    "start": { "line": 4, "character": 4 },
                                    "end": { "line": 6, "character": 5 }
                                },
                                "code": "MC-SEMCK-NonExhaustiveUnionMatch",
                                "message": "missing",
                                "source": "machina"
                            }
                        ]
                    }
                }
            }),
        );

        let response = response.expect("expected code action response");
        let actions = response["result"]
            .as_array()
            .expect("expected code action result array");
        let union_fix = actions
            .iter()
            .find(|a| a["data"]["diagnosticCode"] == "MC-SEMCK-NonExhaustiveUnionMatch")
            .expect("expected union fix action");
        let attached = union_fix["diagnostics"]
            .as_array()
            .expect("expected diagnostics array on action");
        assert_eq!(attached.len(), 1);
        assert_eq!(attached[0]["code"], "MC-SEMCK-NonExhaustiveUnionMatch");
    }

    #[test]
    fn code_action_request_returns_non_exhaustive_match_wildcard_fix() {
        let mut session = AnalysisSession::new();
        let source = r#"type Flag = On | Off

fn describe_flag(f: Flag) -> u64 {
    match f {
        Flag::On => 1,
    }
}"#;
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-code-action-match.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": source
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 102,
                "method": "textDocument/codeAction",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-code-action-match.mc" },
                    "range": {
                        "start": { "line": 3, "character": 4 },
                        "end": { "line": 5, "character": 5 }
                    },
                    "mcDocVersion": 1,
                    "context": {
                        "diagnostics": [
                            {
                                "range": {
                                    "start": { "line": 3, "character": 4 },
                                    "end": { "line": 5, "character": 5 }
                                },
                                "code": "MC-SEMCK-NonExhaustiveMatch",
                                "message": "Match is not exhaustive",
                                "source": "machina"
                            }
                        ]
                    }
                }
            }),
        );
        let response = response.expect("expected code action response");
        let actions = response["result"]
            .as_array()
            .expect("expected code action result array");
        let wildcard_fix = actions
            .iter()
            .find(|a| a["data"]["diagnosticCode"] == "MC-SEMCK-NonExhaustiveMatch")
            .expect("expected non-exhaustive match quick fix");
        let edit_text = wildcard_fix["edit"]["changes"]["file:///tmp/lsp-code-action-match.mc"][0]
            ["newText"]
            .as_str()
            .expect("expected generated wildcard edit text");
        assert!(edit_text.contains("_ => {"));
    }

    #[test]
    fn did_open_publishes_semcheck_diagnostic_for_non_exhaustive_match() {
        let mut session = AnalysisSession::new();
        let source = r#"requires {
    std::io::println
}

type Flag = On | Off

fn describe_flag(f: Flag) -> u64 {
    match f {
        Flag::On => 1,
    }
}

fn main() {
    let d = describe_flag(Flag::Off);
    println(d);
}"#;
        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-semcheck-match.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": source
                    }
                }
            }),
        );

        let response = response.expect("expected diagnostics notification");
        let diagnostics = response["params"]["diagnostics"]
            .as_array()
            .expect("diagnostics must be array");
        assert!(
            diagnostics
                .iter()
                .any(|d| d["code"] == "MC-SEMCK-NonExhaustiveMatch"),
            "expected semcheck non-exhaustive match diagnostic, got: {diagnostics:?}"
        );
    }

    #[test]
    fn code_action_uses_program_diagnostics_for_non_exhaustive_match() {
        let mut session = AnalysisSession::new();
        let source = r#"requires {
    std::io::println
}

type Flag = On | Off

fn describe_flag(f: Flag) -> u64 {
    match f {
        Flag::On => 1,
        // Flag::Off => 0,
    }
}

fn main() {
    let d = describe_flag(Flag::Off);
    println(d);
}"#;
        let _ = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///tmp/lsp-code-action-program-match.mc",
                        "version": 1,
                        "languageId": "machina",
                        "text": source
                    }
                }
            }),
        );

        let (_action, response) = handle_message(
            &mut session,
            json!({
                "jsonrpc": "2.0",
                "id": 103,
                "method": "textDocument/codeAction",
                "params": {
                    "textDocument": { "uri": "file:///tmp/lsp-code-action-program-match.mc" },
                    "range": {
                        "start": { "line": 7, "character": 4 },
                        "end": { "line": 9, "character": 5 }
                    },
                    "mcDocVersion": 1,
                    "context": {
                        "diagnostics": [
                            {
                                "range": {
                                    "start": { "line": 7, "character": 4 },
                                    "end": { "line": 9, "character": 5 }
                                },
                                "code": "MC-SEMCK-NonExhaustiveMatch",
                                "message": "Match is not exhaustive",
                                "source": "machina"
                            }
                        ]
                    }
                }
            }),
        );

        let response = response.expect("expected code action response");
        let actions = response["result"]
            .as_array()
            .expect("expected code action result array");
        let wildcard_fix = actions
            .iter()
            .find(|a| a["data"]["diagnosticCode"] == "MC-SEMCK-NonExhaustiveMatch")
            .expect("expected non-exhaustive match quick fix");
        assert_eq!(
            wildcard_fix["edit"]["changes"]["file:///tmp/lsp-code-action-program-match.mc"][0]["range"]
                ["start"]["line"],
            10
        );
        let edit_text =
            wildcard_fix["edit"]["changes"]["file:///tmp/lsp-code-action-program-match.mc"][0]
                ["newText"]
                .as_str()
                .expect("expected generated wildcard edit text");
        assert!(edit_text.contains("_ => {"));
    }
}
