//! In-process LSP client harness for driving the server handlers as if from an
//! editor. This keeps session state across requests and is shared by the
//! standalone client binary and focused tests.

use std::fs;
use std::io;
use std::path::Path;

use serde_json::{Value, json};
use thiserror::Error;

use crate::handlers::{HandlerAction, handle_message};
use crate::session::{AnalysisSession, SessionError};
use machina::services::analysis::trace::AnalysisTracer;

#[derive(Debug, Error)]
pub enum ClientError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Session(#[from] SessionError),
    #[error("expected response for `{method}`")]
    MissingResponse { method: &'static str },
    #[error("unexpected exit while handling `{method}`")]
    UnexpectedExit { method: &'static str },
    #[error("position must be formatted as line:col")]
    InvalidPosition,
    #[error("unknown trace category `{0}`")]
    InvalidTraceCategory(String),
}

pub type ClientResult<T> = Result<T, ClientError>;

pub struct LspClient {
    session: AnalysisSession,
    next_request_id: u64,
    initialized: bool,
}

impl Default for LspClient {
    fn default() -> Self {
        Self::new()
    }
}

impl LspClient {
    pub fn new() -> Self {
        Self {
            session: AnalysisSession::new(),
            next_request_id: 1,
            initialized: false,
        }
    }

    pub fn initialize(&mut self) -> ClientResult<Value> {
        self.ensure_initialized_response()
    }

    pub fn set_tracer(&mut self, tracer: AnalysisTracer) {
        self.session.set_tracer(tracer);
    }

    pub fn open_text(&mut self, uri: &str, text: &str, version: i32) -> ClientResult<Value> {
        self.ensure_initialized()?;
        self.send_notification(
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": uri,
                    "version": version,
                    "languageId": "machina",
                    "text": text,
                }
            }),
        )
        .and_then(expect_response("textDocument/didOpen"))
    }

    pub fn open_path(&mut self, path: &Path) -> ClientResult<Value> {
        let text = fs::read_to_string(path)?;
        let uri = path_to_file_uri(path);
        self.open_text(&uri, &text, 1)
    }

    pub fn diagnostics(&mut self, path: &Path) -> ClientResult<Value> {
        let uri = path_to_file_uri(path);
        if self.session.lookup_document(&uri).is_ok() {
            self.change_path(path)
        } else {
            self.open_path(path)
        }
    }

    pub fn change_text(&mut self, uri: &str, text: &str, version: i32) -> ClientResult<Value> {
        self.ensure_initialized()?;
        self.send_notification(
            "textDocument/didChange",
            json!({
                "textDocument": {
                    "uri": uri,
                    "version": version,
                },
                "contentChanges": [{
                    "text": text,
                }]
            }),
        )
        .and_then(expect_response("textDocument/didChange"))
    }

    pub fn change_path(&mut self, path: &Path) -> ClientResult<Value> {
        let text = fs::read_to_string(path)?;
        let uri = path_to_file_uri(path);
        let version = self
            .session
            .lookup_document(&uri)
            .map(|doc| doc.version.saturating_add(1))
            .unwrap_or(1);
        if version == 1 {
            self.open_text(&uri, &text, version)
        } else {
            self.change_text(&uri, &text, version)
        }
    }

    pub fn close_path(&mut self, path: &Path) -> ClientResult<Value> {
        self.ensure_initialized()?;
        let uri = path_to_file_uri(path);
        self.send_notification(
            "textDocument/didClose",
            json!({
                "textDocument": {
                    "uri": uri,
                }
            }),
        )
        .and_then(expect_response("textDocument/didClose"))
    }

    pub fn hover(&mut self, path: &Path, line1: usize, col1: usize) -> ClientResult<Value> {
        self.read_request("textDocument/hover", path, line1, col1)
    }

    pub fn definition(&mut self, path: &Path, line1: usize, col1: usize) -> ClientResult<Value> {
        self.read_request("textDocument/definition", path, line1, col1)
    }

    pub fn completion(&mut self, path: &Path, line1: usize, col1: usize) -> ClientResult<Value> {
        self.read_request("textDocument/completion", path, line1, col1)
    }

    pub fn signature(&mut self, path: &Path, line1: usize, col1: usize) -> ClientResult<Value> {
        self.read_request("textDocument/signatureHelp", path, line1, col1)
    }

    fn read_request(
        &mut self,
        method: &'static str,
        path: &Path,
        line1: usize,
        col1: usize,
    ) -> ClientResult<Value> {
        self.ensure_initialized()?;
        self.change_path(path)?;
        let uri = path_to_file_uri(path);
        let version = self.session.lookup_document(&uri)?.version;
        self.send_request(
            method,
            json!({
                "textDocument": {
                    "uri": uri,
                },
                "position": {
                    "line": line1.saturating_sub(1),
                    "character": col1.saturating_sub(1),
                },
                "mcDocVersion": version,
            }),
        )
    }

    fn ensure_initialized(&mut self) -> ClientResult<()> {
        let _ = self.ensure_initialized_response()?;
        Ok(())
    }

    fn ensure_initialized_response(&mut self) -> ClientResult<Value> {
        if self.initialized {
            return Ok(json!({
                "jsonrpc": "2.0",
                "result": {
                    "alreadyInitialized": true
                }
            }));
        }
        let response = self.send_request("initialize", json!({}))?;
        self.initialized = true;
        let _ = self.send_notification("initialized", json!({}))?;
        Ok(response)
    }

    fn send_request(&mut self, method: &'static str, params: Value) -> ClientResult<Value> {
        let id = self.next_request_id;
        self.next_request_id += 1;
        let message = json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
            "params": params,
        });
        let (action, response) = handle_message(&mut self.session, message);
        if action == HandlerAction::Exit {
            return Err(ClientError::UnexpectedExit { method });
        }
        response.ok_or(ClientError::MissingResponse { method })
    }

    fn send_notification(
        &mut self,
        method: &'static str,
        params: Value,
    ) -> ClientResult<Option<Value>> {
        let message = json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        });
        let (action, response) = handle_message(&mut self.session, message);
        if action == HandlerAction::Exit {
            return Err(ClientError::UnexpectedExit { method });
        }
        Ok(response)
    }
}

fn expect_response(
    method: &'static str,
) -> impl FnOnce(Option<Value>) -> ClientResult<Value> + 'static {
    move |response| response.ok_or(ClientError::MissingResponse { method })
}

fn path_to_file_uri(path: &Path) -> String {
    format!("file://{}", path.to_string_lossy())
}

#[cfg(test)]
mod tests {
    use super::LspClient;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_path(stem: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time should be after epoch")
            .as_nanos();
        std::env::temp_dir().join(format!("machina_lsp_client_{stem}_{nanos}.mc"))
    }

    #[test]
    fn open_text_emits_publish_diagnostics_notification() {
        let mut client = LspClient::new();
        let response = client
            .open_text(
                "file:///tmp/lsp-client-open.mc",
                "fn main() -> u64 { 0 }\n",
                1,
            )
            .expect("open should succeed");
        assert_eq!(response["method"], "textDocument/publishDiagnostics");
        assert!(
            response["params"]["diagnostics"]
                .as_array()
                .is_some_and(|items| items.is_empty())
        );
    }

    #[test]
    fn hover_request_returns_result_after_opening_path() {
        let path = temp_path("hover");
        fs::write(
            &path,
            "fn helper(x: u64) -> u64 { x }\nfn main() -> u64 { helper(1) }\n",
        )
        .expect("temp source should write");

        let mut client = LspClient::new();
        let response = client.hover(&path, 2, 22).expect("hover should succeed");
        assert_eq!(response["jsonrpc"], "2.0");
        assert!(response.get("result").is_some());

        let _ = fs::remove_file(path);
    }
}
