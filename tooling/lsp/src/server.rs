//! Stdio JSON-RPC server loop.

use std::collections::HashSet;
use std::io::{self, BufReader, BufWriter, Read, Write};

use serde_json::{Value, json};
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
    let mut cancelled_requests = HashSet::new();

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

        if is_cancel_notification(&message) {
            if let Some(cancelled_id) = cancelled_request_id(&message) {
                cancelled_requests.insert(cancelled_id);
            }
            continue;
        }

        if let Some(request_id) = request_id_key(&message)
            && cancelled_requests.remove(&request_id)
        {
            if let Some(id) = message.get("id") {
                let payload = json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "error": {
                        "code": -32800,
                        "message": "request cancelled"
                    }
                });
                transport::write_message(&mut writer, &payload)?;
            }
            continue;
        }

        let (action, response) = handle_message(&mut session, message);
        if let Some(payload) = response {
            transport::write_message(&mut writer, &payload)?;
        }
        if action == HandlerAction::Exit {
            return Ok(());
        }
    }
}

fn is_cancel_notification(message: &Value) -> bool {
    matches!(
        message.get("method").and_then(Value::as_str),
        Some("$/cancelRequest")
    )
}

fn cancelled_request_id(message: &Value) -> Option<String> {
    let params = message.get("params")?;
    let id = params.get("id")?;
    request_id_to_key(id)
}

fn request_id_key(message: &Value) -> Option<String> {
    let id = message.get("id")?;
    request_id_to_key(id)
}

fn request_id_to_key(id: &Value) -> Option<String> {
    match id {
        Value::String(s) => Some(format!("s:{s}")),
        Value::Number(n) => Some(format!("n:{n}")),
        _ => None,
    }
}

#[cfg(test)]
#[path = "./tests/t_server.rs"]
mod tests;
