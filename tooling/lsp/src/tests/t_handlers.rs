use serde_json::{Value, json};
use std::fs;

use crate::session::AnalysisSession;

use super::{HandlerAction, handle_message};

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
fn hover_over_linear_machine_handle_type_returns_machine_hover() {
    let mut session = AnalysisSession::new();
    let source = r#"@linear
type Payment = {
    id: u64,

    states {
        Draft,
        Approved,
    }

    actions {
        approve: Draft -> Approved,
    }

    roles {
        Reviewer { approve }
    }
}

Payment :: {
    fn approve(self) -> Approved { Approved {} }
}

machine PaymentService hosts Payment(key: id) {
    fn new() -> Self { Self {} }
}

fn helper(service: Machine<PaymentService>) -> Machine<PaymentService> {
    service
}
"#;
    let (line, character) =
        lsp_position_for_substring(source, "Machine<PaymentService>", "Machine<".len());
    let _ = handle_message(
        &mut session,
        json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": "file:///tmp/lsp-linear-machine-hover.mc",
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
            "id": 141,
            "method": "textDocument/hover",
            "params": {
                "textDocument": { "uri": "file:///tmp/lsp-linear-machine-hover.mc" },
                "position": { "line": line, "character": character },
                "mcDocVersion": 1
            }
        }),
    );
    let response = response.expect("expected hover response");
    let value = response["result"]["contents"]["value"]
        .as_str()
        .expect("hover markdown value should be a string");
    assert!(
        value.contains("PaymentService"),
        "expected machine hover, got: {value}"
    );
}

#[test]
fn definition_request_resolves_linear_machine_handle_type() {
    let mut session = AnalysisSession::new();
    let source = r#"@linear
type Payment = {
    id: u64,

    states {
        Draft,
        Approved,
    }

    actions {
        approve: Draft -> Approved,
    }

    roles {
        Reviewer { approve }
    }
}

Payment :: {
    fn approve(self) -> Approved { Approved {} }
}

machine PaymentService hosts Payment(key: id) {
    fn new() -> Self { Self {} }
}

fn helper(service: Machine<PaymentService>) -> Machine<PaymentService> {
    service
}
"#;
    let (line, character) =
        lsp_position_for_substring(source, "Machine<PaymentService>", "Machine<".len());
    let _ = handle_message(
        &mut session,
        json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": "file:///tmp/lsp-linear-machine-definition.mc",
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
            "id": 142,
            "method": "textDocument/definition",
            "params": {
                "textDocument": { "uri": "file:///tmp/lsp-linear-machine-definition.mc" },
                "position": { "line": line, "character": character },
                "mcDocVersion": 1
            }
        }),
    );

    let response = response.expect("expected definition response");
    let locations = response["result"]
        .as_array()
        .expect("expected location array");
    assert_eq!(
        locations.len(),
        1,
        "expected one machine definition location"
    );
    assert_eq!(
        locations[0]["uri"],
        "file:///tmp/lsp-linear-machine-definition.mc"
    );
}

#[test]
fn completion_request_includes_linear_machine_defs_for_handle_annotations() {
    let mut session = AnalysisSession::new();
    let source = r#"@linear
type Payment = {
    id: u64,

    states {
        Draft,
        Approved,
    }

    actions {
        approve: Draft -> Approved,
    }

    roles {
        Reviewer { approve }
    }
}

Payment :: {
    fn approve(self) -> Approved { Approved {} }
}

machine PaymentService hosts Payment(key: id) {
    fn new() -> Self { Self {} }
}

fn helper(service: Machine<Pay>) -> u64 {
    0
}
"#;
    let (line, character) = lsp_position_for_substring(source, "Pay", 1);
    let _ = handle_message(
        &mut session,
        json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": "file:///tmp/lsp-linear-machine-completion.mc",
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
            "id": 143,
            "method": "textDocument/completion",
            "params": {
                "textDocument": { "uri": "file:///tmp/lsp-linear-machine-completion.mc" },
                "position": { "line": line, "character": character },
                "mcDocVersion": 1
            }
        }),
    );

    let response = response.expect("expected completion response");
    let items = response["result"]["items"]
        .as_array()
        .expect("expected completion items");
    assert!(
        items.iter().any(|item| item["label"] == "PaymentService"),
        "expected machine completion for `PaymentService`"
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
fn hover_request_uses_definition_range_for_local_symbol() {
    let mut session = AnalysisSession::new();
    let source = "fn foo() {}
fn main() {
    foo();
}
";
    let _ = handle_message(
        &mut session,
        json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": "file:///tmp/lsp-hover-local-range.mc",
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
            "id": 89,
            "method": "textDocument/hover",
            "params": {
                "textDocument": { "uri": "file:///tmp/lsp-hover-local-range.mc" },
                "position": { "line": 2, "character": 5 },
                "mcDocVersion": 1
            }
        }),
    );

    let response = response.expect("expected hover response");
    assert_eq!(response["id"], 89);
    assert_eq!(response["result"]["range"]["start"]["line"], 0);
    assert_eq!(response["result"]["range"]["end"]["line"], 0);
}

#[test]
fn hover_request_uses_current_document_range_for_imported_symbol() {
    let run_id = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("system time should be valid")
        .as_nanos();
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_lsp_hover_import_{}_{}",
        std::process::id(),
        run_id
    ));
    let app_dir = temp_dir.join("app");
    fs::create_dir_all(&app_dir).expect("failed to create temp module ast");

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
    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

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
            "id": 90,
            "method": "textDocument/hover",
            "params": {
                "textDocument": { "uri": entry_uri },
                "position": { "line": 5, "character": 5 },
                "mcDocVersion": 1
            }
        }),
    );

    let response = response.expect("expected hover response");
    assert_eq!(response["id"], 90);
    assert_eq!(response["result"]["range"]["start"]["line"], 5);
    assert_eq!(response["result"]["range"]["end"]["line"], 5);

    let _ = fs::remove_dir_all(&temp_dir);
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
    fs::create_dir_all(&app_dir).expect("failed to create temp module ast");

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
    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

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

    let _ = fs::remove_dir_all(&temp_dir);
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
    fs::create_dir_all(&app_dir).expect("failed to create temp module ast");

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
    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");
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

    let _ = fs::remove_dir_all(&temp_dir);
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
    let edit_text = union_fix["edit"]["changes"]["file:///tmp/lsp-code-action.mc"][0]["newText"]
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
    let edit_text =
        wildcard_fix["edit"]["changes"]["file:///tmp/lsp-code-action-match.mc"][0]["newText"]
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
    let edit_text = wildcard_fix["edit"]["changes"]["file:///tmp/lsp-code-action-program-match.mc"]
        [0]["newText"]
        .as_str()
        .expect("expected generated wildcard edit text");
    assert!(edit_text.contains("_ => {"));
}

fn lsp_position_for_substring(source: &str, needle: &str, char_offset: usize) -> (usize, usize) {
    let start = source
        .find(needle)
        .expect("needle should exist in source for LSP test helper");
    let offset = start + char_offset;
    let mut line0 = 0usize;
    let mut col0 = 0usize;
    for ch in source[..offset].chars() {
        if ch == '\n' {
            line0 += 1;
            col0 = 0;
        } else {
            col0 += 1;
        }
    }
    (line0, col0)
}
