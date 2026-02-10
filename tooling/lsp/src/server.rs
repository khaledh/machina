//! Stdio JSON-RPC server loop.

use std::io::{self, BufReader, BufWriter, Read, Write};

use serde_json::json;
use thiserror::Error;

use crate::handlers::{HandlerAction, handle_message};
use crate::session::AnalysisSession;
use crate::transport::{self, TransportError};

#[derive(Debug, Error)]
pub enum ServerError {
    #[error(transparent)]
    Transport(#[from] TransportError),
    #[error("io error: {0}")]
    Io(#[from] io::Error),
}

pub type ServerResult<T> = Result<T, ServerError>;

pub fn run_stdio<R: Read, W: Write>(input: R, output: W) -> ServerResult<()> {
    let mut reader = BufReader::new(input);
    let mut writer = BufWriter::new(output);
    let mut session = AnalysisSession::new();

    loop {
        let message = match transport::read_message(&mut reader) {
            Ok(Some(value)) => value,
            Ok(None) => return Ok(()),
            Err(TransportError::InvalidJson(_)) => {
                let payload = json!({
                    "jsonrpc": "2.0",
                    "error": {
                        "code": -32700,
                        "message": "parse error"
                    }
                });
                transport::write_message(&mut writer, &payload)?;
                continue;
            }
            Err(error) => return Err(error.into()),
        };

        let (action, response) = handle_message(&mut session, message);
        if let Some(payload) = response {
            transport::write_message(&mut writer, &payload)?;
        }
        if action == HandlerAction::Exit {
            return Ok(());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::run_stdio;
    use crate::transport::read_message;
    use std::io::Cursor;

    fn frame(body: &str) -> String {
        format!("Content-Length: {}\r\n\r\n{}", body.len(), body)
    }

    #[test]
    fn server_handles_initialize_then_exit() {
        let initialize = r#"{"jsonrpc":"2.0","id":1,"method":"initialize"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;
        let input = format!("{}{}", frame(initialize), frame(exit));

        let mut out = Vec::new();
        run_stdio(Cursor::new(input.into_bytes()), &mut out).expect("server should run");

        let mut reader = Cursor::new(out);
        let response = read_message(&mut reader)
            .expect("framed response should parse")
            .expect("expected one response");
        assert_eq!(response["id"], 1);
        assert_eq!(response["result"]["serverInfo"]["name"], "machina-lsp");
    }

    #[test]
    fn server_returns_parse_error_for_invalid_json() {
        let bad = "not-json";
        let input = format!(
            "{}{}",
            frame(bad),
            frame(r#"{"jsonrpc":"2.0","method":"exit"}"#)
        );
        let mut out = Vec::new();
        run_stdio(Cursor::new(input.into_bytes()), &mut out).expect("server should continue");

        let mut reader = Cursor::new(out);
        let response = read_message(&mut reader)
            .expect("response parse should succeed")
            .expect("expected parse-error response");
        assert_eq!(response["error"]["code"], -32700);
    }

    #[test]
    fn unknown_request_returns_method_not_found() {
        let unknown = r#"{"jsonrpc":"2.0","id":3,"method":"workspace/unknownMethod"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;
        let input = format!("{}{}", frame(unknown), frame(exit));
        let mut out = Vec::new();

        run_stdio(Cursor::new(input.into_bytes()), &mut out).expect("server should run");

        let mut reader = Cursor::new(out);
        let response = read_message(&mut reader)
            .expect("response parse should succeed")
            .expect("expected method-not-found response");
        assert_eq!(response["id"], 3);
        assert_eq!(response["error"]["code"], -32601);
    }

    #[test]
    fn initialized_notification_emits_no_response() {
        let initialized = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;
        let input = format!("{}{}", frame(initialized), frame(exit));
        let mut out = Vec::new();

        run_stdio(Cursor::new(input.into_bytes()), &mut out).expect("server should run");
        assert!(out.is_empty());
    }
}
