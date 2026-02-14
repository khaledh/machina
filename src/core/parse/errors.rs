use thiserror::Error;

use crate::core::diag::Span;
use crate::core::lexer::{Token, TokenKind};

#[derive(Debug, Error)]
#[allow(clippy::enum_variant_names)]
pub enum ParseError {
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
    UnknownAttribute(String, Span),

    #[error("Attribute not allowed here")]
    AttributeNotAllowed(Span),

    #[error("Feature `{feature}` is not enabled; pass `--experimental {feature}`")]
    FeatureDisabled { feature: &'static str, span: Span },

    #[error("Unmatched format brace at {0}")]
    UnmatchedFormatBrace(Span),

    #[error("Invalid format expression at {0}")]
    InvalidFormatExpr(Span),

    #[error("Empty format expression: {0}")]
    EmptyFormatExpr(Span),

    #[error("Unterminated format expression: {0}")]
    UnterminatedFormatExpr(Span),
}

impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::ExpectedDecl(token) => token.span,
            ParseError::ExpectedToken(_, token) => token.span,
            ParseError::ExpectedIdent(token) => token.span,
            ParseError::ExpectedSelf(token) => token.span,
            ParseError::ExpectedType(token) => token.span,
            ParseError::ExpectedPrimary(token) => token.span,
            ParseError::ExpectedIntLit(token) => token.span,
            ParseError::ExpectedStringLit(token) => token.span,
            ParseError::ExpectedPattern(token) => token.span,
            ParseError::SingleFieldTupleMissingComma(token) => token.span,
            ParseError::SingleElementSetMissingComma(token) => token.span,
            ParseError::ExpectedStructField(token) => token.span,
            ParseError::ExpectedMatchArm(token) => token.span,
            ParseError::ExpectedMatchPattern(token) => token.span,
            ParseError::ExpectedArrayIndexOrRange(token) => token.span,
            ParseError::ExpectedRefinement(token) => token.span,
            ParseError::UnknownAttribute(_, span) => *span,
            ParseError::AttributeNotAllowed(span) => *span,
            ParseError::FeatureDisabled { span, .. } => *span,
            ParseError::UnmatchedFormatBrace(span) => *span,
            ParseError::InvalidFormatExpr(span) => *span,
            ParseError::EmptyFormatExpr(span) => *span,
            ParseError::UnterminatedFormatExpr(span) => *span,
        }
    }
}
