use thiserror::Error;

use crate::core::diag::{Span, SpannedError};
use crate::core::lexer::{Token, TokenKind};

#[derive(Debug, Error)]
#[allow(clippy::enum_variant_names)]
pub enum ParseErrorKind {
    #[error("Expected declaration, found: {0}")]
    ExpectedDecl(Token),

    #[error("Expected {0}, found: {1}")]
    ExpectedToken(TokenKind, Token),

    #[error("Expected identifier, found: {0}")]
    ExpectedIdent(Token),

    #[error("Expected self, found: {0}")]
    ExpectedSelf(Token),

    #[error("Expected type, found: {0}")]
    ExpectedType(Token),

    #[error("Expected primary expression, found: {0}")]
    ExpectedPrimary(Token),

    #[error("Expected integer literal, found: {0}")]
    ExpectedIntLit(Token),

    #[error("Expected string literal, found: {0}")]
    ExpectedStringLit(Token),

    #[error("Expected pattern, found: {0}")]
    ExpectedPattern(Token),

    #[error("Single field tuple missing trailing comma: {0}")]
    SingleFieldTupleMissingComma(Token),

    #[error("Single element set missing trailing comma: {0}")]
    SingleElementSetMissingComma(Token),

    #[error("Expected struct field, found: {0}")]
    ExpectedStructField(Token),

    #[error("Expected match arm, found: {0}")]
    ExpectedMatchArm(Token),

    #[error("Expected match pattern, found: {0}")]
    ExpectedMatchPattern(Token),

    #[error("Expected array index or slice range, found: {0}")]
    ExpectedArrayIndexOrRange(Token),

    #[error("Expected refinement (bounds/nonzero), found: {0}")]
    ExpectedRefinement(Token),

    #[error("Unknown attribute `{0}`")]
    UnknownAttribute(String),

    #[error("Attribute not allowed here")]
    AttributeNotAllowed,

    #[error("Feature `{feature}` is not enabled; pass `--experimental {feature}`")]
    FeatureDisabled { feature: &'static str },

    #[error("Unmatched format brace")]
    UnmatchedFormatBrace,

    #[error("Invalid format expression")]
    InvalidFormatExpr,

    #[error("Empty format expression")]
    EmptyFormatExpr,

    #[error("Unterminated format expression")]
    UnterminatedFormatExpr,
}

pub type ParseError = SpannedError<ParseErrorKind>;

impl ParseErrorKind {
    pub fn at(self, span: Span) -> ParseError {
        ParseError::new(self, span)
    }
}
