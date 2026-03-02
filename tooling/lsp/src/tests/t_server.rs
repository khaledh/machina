use std::io::Cursor;

use super::run_stdio;
use crate::transport::read_message;

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

#[test]
fn cancelled_request_returns_request_cancelled_error() {
    let cancel = r#"{"jsonrpc":"2.0","method":"$/cancelRequest","params":{"id":7}}"#;
    let hover = r#"{"jsonrpc":"2.0","id":7,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/cancelled.mc"},"position":{"line":0,"character":0},"mcDocVersion":1}}"#;
    let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;
    let input = format!("{}{}{}", frame(cancel), frame(hover), frame(exit));
    let mut out = Vec::new();

    run_stdio(Cursor::new(input.into_bytes()), &mut out).expect("server should run");

    let mut reader = Cursor::new(out);
    let response = read_message(&mut reader)
        .expect("response parse should succeed")
        .expect("expected cancellation response");
    assert_eq!(response["id"], 7);
    assert_eq!(response["error"]["code"], -32800);
}

#[test]
fn cancel_does_not_block_different_request_id() {
    let cancel = r#"{"jsonrpc":"2.0","method":"$/cancelRequest","params":{"id":7}}"#;
    let initialize = r#"{"jsonrpc":"2.0","id":8,"method":"initialize"}"#;
    let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;
    let input = format!("{}{}{}", frame(cancel), frame(initialize), frame(exit));
    let mut out = Vec::new();

    run_stdio(Cursor::new(input.into_bytes()), &mut out).expect("server should run");

    let mut reader = Cursor::new(out);
    let response = read_message(&mut reader)
        .expect("response parse should succeed")
        .expect("expected initialize response");
    assert_eq!(response["id"], 8);
    assert!(response.get("result").is_some());
}
