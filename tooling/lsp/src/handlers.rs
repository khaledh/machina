//! Minimal JSON-RPC request handlers for LSP bootstrap methods.

use serde_json::{Value, json};
use std::path::Path;

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

pub fn handle_message(
    session: &mut AnalysisSession,
    message: Value,
) -> (HandlerAction, Option<Value>) {
    let method = message.get("method").and_then(Value::as_str);
    let id = message.get("id").cloned();
    let params = message.get("params");

    match method {
        Some("initialize") => {
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
                        "signatureHelpProvider": Value::Null,
                        "semanticTokensProvider": Value::Null,
                        "documentSymbolProvider": false,
                        "codeActionProvider": false
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

fn span_from_lsp_position(line0: usize, col0: usize) -> machina::diag::Span {
    let line = line0.saturating_add(1);
    let col = col0.saturating_add(1);
    machina::diag::Span {
        start: machina::diag::Position {
            offset: 0,
            line,
            column: col,
        },
        end: machina::diag::Position {
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
    let span = span_from_lsp_position(line0, col0);
    let result = session.execute_query(|db| db.hover_at_file(file_id, span));
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    match result {
        Ok(Some(hover)) => json!({
            "jsonrpc": "2.0",
            "id": id,
            "result": {
                "contents": {
                    "kind": "plaintext",
                    "value": hover.display
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
    let span = span_from_lsp_position(line0, col0);
    let result = session.execute_query(|db| db.def_location_at_file(file_id, span));
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
    let result = session.execute_query(|db| db.completions_at_file(file_id, span));
    if !matches!(session.is_current_version(uri, version), Ok(true)) {
        return stale_result_response(id);
    }
    match result {
        Ok(items) => {
            let items: Vec<Value> = items
                .into_iter()
                .map(|item| {
                    let kind = match item.kind {
                        machina::analysis::results::CompletionKind::Function => 3,
                        machina::analysis::results::CompletionKind::Type => 7,
                        machina::analysis::results::CompletionKind::Trait => 7,
                        machina::analysis::results::CompletionKind::Variable => 6,
                        machina::analysis::results::CompletionKind::Parameter => 6,
                        machina::analysis::results::CompletionKind::TypeParameter => 25,
                        machina::analysis::results::CompletionKind::EnumVariant => 20,
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
    diagnostics: Vec<machina::analysis::diagnostics::Diagnostic>,
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
                    "position": { "line": 0, "character": 1 }
                }
            }),
        );
        let response = response.expect("expected hover response");
        assert_eq!(response["id"], 50);
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
        assert!(!locations.is_empty(), "expected at least one definition location");
        assert_eq!(locations[0]["uri"], "file:///tmp/lsp-definition.mc");
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
    }
}
