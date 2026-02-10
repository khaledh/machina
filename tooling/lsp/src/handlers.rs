//! Minimal JSON-RPC request handlers for LSP bootstrap methods.

use serde_json::{Value, json};

use crate::session::{AnalysisSession, SessionError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HandlerAction {
    Continue,
    Exit,
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
                        "hoverProvider": false,
                        "completionProvider": Value::Null,
                        "definitionProvider": false,
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
            let Some(params) = params else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(text_doc) = params.get("textDocument") else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(uri) = text_doc.get("uri").and_then(Value::as_str) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(version) = text_doc.get("version").and_then(Value::as_i64) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(text) = text_doc.get("text").and_then(Value::as_str) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            if let Err(error) = session.open_document(uri, version as i32, text) {
                return (
                    HandlerAction::Continue,
                    id.map(|id| session_error_response(id, &error)),
                );
            }
            let diagnostics = session
                .diagnostics_for_uri(uri)
                .unwrap_or_else(|_| Vec::new());
            let notification = publish_diagnostics_notification(uri, version as i32, diagnostics);
            (HandlerAction::Continue, Some(notification))
        }
        Some("textDocument/didChange") => {
            let Some(params) = params else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(text_doc) = params.get("textDocument") else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(uri) = text_doc.get("uri").and_then(Value::as_str) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(version) = text_doc.get("version").and_then(Value::as_i64) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(changes) = params.get("contentChanges").and_then(Value::as_array) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(last_change_text) = changes
                .last()
                .and_then(|c| c.get("text"))
                .and_then(Value::as_str)
            else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            if let Err(error) = session.change_document(uri, version as i32, last_change_text) {
                return (
                    HandlerAction::Continue,
                    id.map(|id| session_error_response(id, &error)),
                );
            }
            let diagnostics = session
                .diagnostics_for_uri(uri)
                .unwrap_or_else(|_| Vec::new());
            let notification = publish_diagnostics_notification(uri, version as i32, diagnostics);
            (HandlerAction::Continue, Some(notification))
        }
        Some("textDocument/didClose") => {
            let Some(params) = params else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(text_doc) = params.get("textDocument") else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            let Some(uri) = text_doc.get("uri").and_then(Value::as_str) else {
                return (HandlerAction::Continue, invalid_params_response(id));
            };
            if let Err(error) = session.close_document(uri) {
                return (
                    HandlerAction::Continue,
                    id.map(|id| session_error_response(id, &error)),
                );
            }
            let notification = json!({
                "jsonrpc": "2.0",
                "method": "textDocument/publishDiagnostics",
                "params": {
                    "uri": uri,
                    "version": Value::Null,
                    "diagnostics": []
                }
            });
            (HandlerAction::Continue, Some(notification))
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
    use serde_json::json;

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
}
