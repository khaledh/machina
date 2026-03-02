use super::{
    is_generated_handler_name, is_generated_state_name, parse_generated_handler_site_label,
    parse_generated_state_name,
};

#[test]
fn parses_generated_state_names() {
    assert_eq!(
        parse_generated_state_name("__ts_Connection_Connected"),
        Some(("Connection".to_string(), "Connected".to_string()))
    );
    assert!(is_generated_state_name("__ts_Connection_Connected"));
    assert!(!is_generated_state_name("Connection"));
}

#[test]
fn parses_generated_handler_names() {
    assert!(is_generated_handler_name("__ts_on_0"));
    assert_eq!(
        parse_generated_handler_site_label("__ts_on_1__site_auth"),
        Some("auth")
    );
    assert_eq!(parse_generated_handler_site_label("__ts_on_1"), None);
}
