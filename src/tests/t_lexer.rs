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

#[test]
fn test_lex_line_comment_skipped() {
    let mut lexer = Lexer::new("foo // comment\nbar");

    let t1 = lexer.next_token().unwrap();
    let t2 = lexer.next_token().unwrap();
    let t3 = lexer.next_token().unwrap();

    assert_eq!(t1.kind, TokenKind::Ident("foo".to_string()));
    assert_eq!(t2.kind, TokenKind::Ident("bar".to_string()));
    assert_eq!(t3.kind, TokenKind::Eof);
}

#[test]
fn test_lex_comment_at_start() {
    let mut lexer = Lexer::new("// comment\nfoo");

    let t1 = lexer.next_token().unwrap();
    let t2 = lexer.next_token().unwrap();

    assert_eq!(t1.kind, TokenKind::Ident("foo".to_string()));
    assert_eq!(t2.kind, TokenKind::Eof);
}

#[test]
fn test_lex_comment_at_end() {
    let mut lexer = Lexer::new("foo // comment");

    let t1 = lexer.next_token().unwrap();
    let t2 = lexer.next_token().unwrap();

    assert_eq!(t1.kind, TokenKind::Ident("foo".to_string()));
    assert_eq!(t2.kind, TokenKind::Eof);
}
