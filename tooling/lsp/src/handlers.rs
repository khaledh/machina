//! Minimal JSON-RPC request handlers for LSP bootstrap methods.

use serde_json::{Value, json};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HandlerAction {
    Continue,
    Exit,
}

pub fn handle_message(message: Value) -> (HandlerAction, Option<Value>) {
    let method = message.get("method").and_then(Value::as_str);
    let id = message.get("id").cloned();

    match method {
        Some("initialize") => {
            let response = json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": {
                    "capabilities": {
                        "textDocumentSync": 2,
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

#[cfg(test)]
mod tests {
    use super::{HandlerAction, handle_message};
    use serde_json::json;

    #[test]
    fn initialize_returns_capabilities() {
        let (action, response) =
            handle_message(json!({ "jsonrpc": "2.0", "id": 1, "method": "initialize" }));
        assert_eq!(action, HandlerAction::Continue);
        let response = response.expect("expected initialize response");
        assert_eq!(response["id"], 1);
        assert_eq!(response["result"]["serverInfo"]["name"], "machina-lsp");
    }

    #[test]
    fn unknown_method_returns_method_not_found() {
        let (action, response) =
            handle_message(json!({ "jsonrpc": "2.0", "id": 10, "method": "foo/bar" }));
        assert_eq!(action, HandlerAction::Continue);
        let response = response.expect("expected error response");
        assert_eq!(response["error"]["code"], -32601);
    }

    #[test]
    fn notifications_without_id_do_not_emit_error_response() {
        let (action, response) = handle_message(json!({
            "jsonrpc": "2.0",
            "method": "workspace/didChangeConfiguration",
            "params": {}
        }));
        assert_eq!(action, HandlerAction::Continue);
        assert!(response.is_none());
    }
}
