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

#[test]
fn test_lex_char_literal_ascii() {
    let mut lexer = Lexer::new("'a'");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::CharLit(b'a'));
    assert_span_eq(token.span, (1, 1), (1, 4));
}

#[test]
fn test_lex_char_literal_escape() {
    let mut lexer = Lexer::new("'\\n'");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::CharLit(b'\n'));
}

#[test]
fn test_lex_char_literal_hex_escape() {
    let mut lexer = Lexer::new("'\\x41'");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::CharLit(b'A'));
}

#[test]
fn test_lex_char_literal_empty() {
    let mut lexer = Lexer::new("''");
    let result = lexer.next_token();

    assert!(matches!(result, Err(LexError::InvalidEscapeSequence(_, _))));
}

#[test]
fn test_lex_char_literal_multi_char() {
    let mut lexer = Lexer::new("'ab'");
    let result = lexer.next_token();

    assert!(matches!(result, Err(LexError::UnexpectedCharacter('b', _))));
}

#[test]
fn test_lex_char_literal_non_ascii() {
    let mut lexer = Lexer::new("'é'");
    let result = lexer.next_token();

    assert!(matches!(result, Err(LexError::InvalidEscapeSequence(_, _))));
}
