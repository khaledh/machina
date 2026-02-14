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
fn test_lex_keywords_and_bools() {
    let mut lexer = Lexer::new(
        "fn let var if else match while for in inout out sink move self requires protocol role flow on emit reply true false",
    );

    let expected = [
        TokenKind::KwFn,
        TokenKind::KwLet,
        TokenKind::KwVar,
        TokenKind::KwIf,
        TokenKind::KwElse,
        TokenKind::KwMatch,
        TokenKind::KwWhile,
        TokenKind::KwFor,
        TokenKind::KwIn,
        TokenKind::KwInOut,
        TokenKind::KwOut,
        TokenKind::KwSink,
        TokenKind::KwMove,
        TokenKind::KwSelf,
        TokenKind::KwRequires,
        TokenKind::KwProtocol,
        TokenKind::KwRole,
        TokenKind::KwFlow,
        TokenKind::KwOn,
        TokenKind::KwEmit,
        TokenKind::KwReply,
        TokenKind::BoolLit(true),
        TokenKind::BoolLit(false),
        TokenKind::Eof,
    ];

    for expected_kind in expected {
        let token = lexer.next_token().unwrap();
        assert_eq!(token.kind, expected_kind);
    }
}

#[test]
fn test_lex_percent_operator() {
    let mut lexer = Lexer::new("%");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::Percent);
    assert_span_eq(token.span, (1, 1), (1, 2));
}

#[test]
fn test_lex_int_literal_bases() {
    let mut lexer = Lexer::new("0b1010 0o12 0x1f 42 1_000 0b1010_0110");

    let t1 = lexer.next_token().unwrap();
    let t2 = lexer.next_token().unwrap();
    let t3 = lexer.next_token().unwrap();
    let t4 = lexer.next_token().unwrap();
    let t5 = lexer.next_token().unwrap();
    let t6 = lexer.next_token().unwrap();
    let t7 = lexer.next_token().unwrap();

    assert_eq!(t1.kind, TokenKind::IntLit(10));
    assert_eq!(t2.kind, TokenKind::IntLit(10));
    assert_eq!(t3.kind, TokenKind::IntLit(31));
    assert_eq!(t4.kind, TokenKind::IntLit(42));
    assert_eq!(t5.kind, TokenKind::IntLit(1000));
    assert_eq!(t6.kind, TokenKind::IntLit(166));
    assert_eq!(t7.kind, TokenKind::Eof);
}

#[test]
fn test_lex_invalid_binary_literal() {
    let mut lexer = Lexer::new("0b102");
    let result = lexer.next_token();

    assert!(matches!(result, Err(LexError::InvalidInteger(_, _))));
}

#[test]
fn test_lex_bitwise_operators() {
    let mut lexer = Lexer::new("& | ^ ~ << >>");

    let t1 = lexer.next_token().unwrap();
    let t2 = lexer.next_token().unwrap();
    let t3 = lexer.next_token().unwrap();
    let t4 = lexer.next_token().unwrap();
    let t5 = lexer.next_token().unwrap();
    let t6 = lexer.next_token().unwrap();
    let t7 = lexer.next_token().unwrap();

    assert_eq!(t1.kind, TokenKind::Ampersand);
    assert_eq!(t2.kind, TokenKind::Pipe);
    assert_eq!(t3.kind, TokenKind::Caret);
    assert_eq!(t4.kind, TokenKind::Tilde);
    assert_eq!(t5.kind, TokenKind::ShiftLeft);
    assert_eq!(t6.kind, TokenKind::ShiftRight);
    assert_eq!(t7.kind, TokenKind::Eof);
}

#[test]
fn test_lex_unexpected_character() {
    let mut lexer = Lexer::new("#");
    let result = lexer.next_token();

    assert!(matches!(result, Err(LexError::UnexpectedCharacter('#', _))));
}

#[test]
fn test_lex_attribute_prefix() {
    let mut lexer = Lexer::new("@intrinsic");

    let t1 = lexer.next_token().unwrap();
    let t2 = lexer.next_token().unwrap();
    let t3 = lexer.next_token().unwrap();

    assert_eq!(t1.kind, TokenKind::At);
    assert_eq!(t2.kind, TokenKind::Ident("intrinsic".to_string()));
    assert_eq!(t3.kind, TokenKind::Eof);
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
fn test_lex_char_literal() {
    let mut lexer = Lexer::new("'a'");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::CharLit('a'));
    assert_span_eq(token.span, (1, 1), (1, 4));
}

#[test]
fn test_lex_char_literal_escape() {
    let mut lexer = Lexer::new("'\\n'");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::CharLit('\n'));
}

#[test]
fn test_lex_char_literal_hex_escape() {
    let mut lexer = Lexer::new("'\\x41'");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::CharLit('A'));
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
fn test_lex_char_literal_utf8() {
    let mut lexer = Lexer::new("'é'");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::CharLit('é'));
    assert_span_eq(token.span, (1, 1), (1, 4));
}

#[test]
fn test_lex_string_ascii() {
    let mut lexer = Lexer::new("\"hello\"");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::StringLit("hello".to_string()));
    assert_span_eq(token.span, (1, 1), (1, 8));
}

#[test]
fn test_lex_string_escape_newline() {
    let mut lexer = Lexer::new("\"a\\n b\"");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::StringLit("a\n b".to_string()));
}

#[test]
fn test_lex_string_escape_hex() {
    let mut lexer = Lexer::new("\"A\\x42C\"");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::StringLit("ABC".to_string()));
}

#[test]
fn test_lex_string_utf8() {
    let mut lexer = Lexer::new("\"caf\u{00e9}\"");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::StringLit("caf\u{00e9}".to_string()));
}

#[test]
fn test_lex_string_unterminated() {
    let mut lexer = Lexer::new("\"abc");
    let result = lexer.next_token();

    assert!(matches!(result, Err(LexError::UnterminatedString(_))));
}

#[test]
fn test_lex_string_bad_escape() {
    let mut lexer = Lexer::new("\"a\\q\"");
    let result = lexer.next_token();

    assert!(matches!(result, Err(LexError::InvalidEscapeSequence(_, _))));
}
