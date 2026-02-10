//! LSP transport framing utilities (Content-Length based JSON-RPC messages).

use std::io::{self, BufRead, Write};

use serde_json::Value;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TransportError {
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("missing Content-Length header")]
    MissingContentLength,
    #[error("invalid Content-Length header")]
    InvalidContentLength,
    #[error("invalid JSON payload: {0}")]
    InvalidJson(#[from] serde_json::Error),
}

pub type TransportResult<T> = Result<T, TransportError>;

pub fn read_message<R: BufRead>(reader: &mut R) -> TransportResult<Option<Value>> {
    let mut content_length: Option<usize> = None;

    loop {
        let mut line = String::new();
        let bytes = reader.read_line(&mut line)?;
        if bytes == 0 {
            if content_length.is_none() {
                return Ok(None);
            }
            return Err(TransportError::MissingContentLength);
        }

        if line == "\r\n" || line == "\n" {
            break;
        }

        let trimmed = line.trim_end_matches(['\r', '\n']);
        if let Some(value) = trimmed.strip_prefix("Content-Length:") {
            let parsed = value
                .trim()
                .parse::<usize>()
                .map_err(|_| TransportError::InvalidContentLength)?;
            content_length = Some(parsed);
        }
    }

    let Some(length) = content_length else {
        return Err(TransportError::MissingContentLength);
    };

    let mut body = vec![0_u8; length];
    reader.read_exact(&mut body)?;
    let payload = serde_json::from_slice::<Value>(&body)?;
    Ok(Some(payload))
}

pub fn write_message<W: Write>(writer: &mut W, payload: &Value) -> TransportResult<()> {
    let bytes = serde_json::to_vec(payload)?;
    write!(writer, "Content-Length: {}\r\n\r\n", bytes.len())?;
    writer.write_all(&bytes)?;
    writer.flush()?;
    Ok(())
}
