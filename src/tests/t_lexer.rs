use super::*;

#[test]
fn test_lex_identifier() {
    let mut lexer = Lexer::new("foo");
    let token = lexer.next_token().unwrap();

    assert_eq!(token.kind, TokenKind::Ident("foo".to_string()));
}

#[test]
fn test_lex_unexpected_character() {
    let mut lexer = Lexer::new("@");
    let result = lexer.next_token();

    assert!(matches!(result, Err(LexError::UnexpectedCharacter('@'))));
}
