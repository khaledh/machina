use super::*;

fn assert_span_eq(span: Span, expected_left: (usize, usize), expected_right: (usize, usize)) {
    assert_eq!(span.start.line, expected_left.0);
    assert_eq!(span.start.column, expected_left.1);
    assert_eq!(span.end.line, expected_right.0);
    assert_eq!(span.end.column, expected_right.1);
}

#[test]
fn test_lex_identifier() {
    let mut lexer = Lexer::new("foo");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::Ident("foo".to_string()));
    assert_span_eq(token.span, (1, 1), (1, 4));
}

#[test]
fn test_lex_unexpected_character() {
    let mut lexer = Lexer::new("@");
    let result = lexer.next_token();

    assert!(matches!(result, Err(LexError::UnexpectedCharacter('@', _))));
}
