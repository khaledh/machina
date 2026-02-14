//! Unified diagnostics model for analysis queries and IDE adapters.
//!
//! This layer normalizes parse/resolve/typecheck/semcheck diagnostics into one
//! phase-tagged representation with stable codes and structured metadata.

use std::collections::BTreeMap;

use crate::core::diag::Span;
use crate::core::lexer::LexError;
use crate::core::parse::ParseError;
use crate::core::resolve::ResolveError;
use crate::core::semck::SemCheckError;
use crate::core::typecheck::{TypeCheckError, TypeCheckErrorKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticPhase {
    Parse,
    Resolve,
    Typecheck,
    Semcheck,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticSeverity {
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticValue {
    String(String),
    Number(i64),
    Bool(bool),
    StringList(Vec<String>),
}

pub type DiagnosticMetadata = BTreeMap<String, DiagnosticValue>;
pub const ANALYSIS_FILE_PATH_KEY: &str = "analysis_file_path";
pub const ANALYSIS_FILE_ID_KEY: &str = "analysis_file_id";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub phase: DiagnosticPhase,
    pub code: String,
    pub severity: DiagnosticSeverity,
    pub span: Span,
    pub message: String,
    pub metadata: DiagnosticMetadata,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WireDiagnostic {
    pub phase: DiagnosticPhase,
    pub code: String,
    pub severity: DiagnosticSeverity,
    pub span: Span,
    pub message: String,
    pub metadata: Vec<(String, DiagnosticValue)>,
}

impl Diagnostic {
    pub fn from_lex_error(error: &LexError) -> Self {
        let mut metadata = DiagnosticMetadata::new();
        let code = match error {
            LexError::UnexpectedCharacter(ch, _) => {
                metadata.insert(
                    "character".to_string(),
                    DiagnosticValue::String(ch.to_string()),
                );
                "MC-LEX-UNEXPECTED-CHARACTER"
            }
            LexError::InvalidInteger(_, _) => "MC-LEX-INVALID-INTEGER",
            LexError::InvalidEscapeSequence(seq, _) => {
                metadata.insert("escape".to_string(), DiagnosticValue::String(seq.clone()));
                "MC-LEX-INVALID-ESCAPE-SEQUENCE"
            }
            LexError::UnterminatedString(_) => "MC-LEX-UNTERMINATED-STRING",
        };
        Self {
            phase: DiagnosticPhase::Parse,
            code: code.to_string(),
            severity: DiagnosticSeverity::Error,
            span: error.span(),
            message: error.to_string(),
            metadata,
        }
    }

    pub fn to_wire(&self) -> WireDiagnostic {
        WireDiagnostic {
            phase: self.phase,
            code: self.code.clone(),
            severity: self.severity,
            span: self.span,
            message: self.message.clone(),
            metadata: self
                .metadata
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        }
    }

    pub fn from_parse_error(error: &ParseError) -> Self {
        let mut metadata = DiagnosticMetadata::new();
        let code = match error {
            ParseError::ExpectedDecl(_) => "MC-PARSE-EXPECTED-DECL",
            ParseError::ExpectedToken(expected, found) => {
                metadata.insert(
                    "expected".to_string(),
                    DiagnosticValue::String(expected.to_string()),
                );
                metadata.insert(
                    "found".to_string(),
                    DiagnosticValue::String(found.kind.to_string()),
                );
                "MC-PARSE-EXPECTED-TOKEN"
            }
            ParseError::ExpectedIdent(_) => "MC-PARSE-EXPECTED-IDENT",
            ParseError::ExpectedSelf(_) => "MC-PARSE-EXPECTED-SELF",
            ParseError::ExpectedType(_) => "MC-PARSE-EXPECTED-TYPE",
            ParseError::ExpectedPrimary(_) => "MC-PARSE-EXPECTED-PRIMARY",
            ParseError::ExpectedIntLit(_) => "MC-PARSE-EXPECTED-INT-LIT",
            ParseError::ExpectedStringLit(_) => "MC-PARSE-EXPECTED-STRING-LIT",
            ParseError::ExpectedPattern(_) => "MC-PARSE-EXPECTED-PATTERN",
            ParseError::SingleFieldTupleMissingComma(_) => "MC-PARSE-TUPLE-MISSING-COMMA",
            ParseError::SingleElementSetMissingComma(_) => "MC-PARSE-SET-MISSING-COMMA",
            ParseError::ExpectedStructField(_) => "MC-PARSE-EXPECTED-STRUCT-FIELD",
            ParseError::ExpectedMatchArm(_) => "MC-PARSE-EXPECTED-MATCH-ARM",
            ParseError::ExpectedMatchPattern(_) => "MC-PARSE-EXPECTED-MATCH-PATTERN",
            ParseError::ExpectedArrayIndexOrRange(_) => "MC-PARSE-EXPECTED-INDEX-OR-RANGE",
            ParseError::ExpectedRefinement(_) => "MC-PARSE-EXPECTED-REFINEMENT",
            ParseError::UnknownAttribute(name, _) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-PARSE-UNKNOWN-ATTRIBUTE"
            }
            ParseError::AttributeNotAllowed(_) => "MC-PARSE-ATTRIBUTE-NOT-ALLOWED",
            ParseError::FeatureDisabled { feature, .. } => {
                metadata.insert(
                    "feature".to_string(),
                    DiagnosticValue::String((*feature).to_string()),
                );
                "MC-PARSE-FEATURE-DISABLED"
            }
            ParseError::UnmatchedFormatBrace(_) => "MC-PARSE-UNMATCHED-FORMAT-BRACE",
            ParseError::InvalidFormatExpr(_) => "MC-PARSE-INVALID-FORMAT-EXPR",
            ParseError::EmptyFormatExpr(_) => "MC-PARSE-EMPTY-FORMAT-EXPR",
            ParseError::UnterminatedFormatExpr(_) => "MC-PARSE-UNTERMINATED-FORMAT-EXPR",
        };
        Self {
            phase: DiagnosticPhase::Parse,
            code: code.to_string(),
            severity: DiagnosticSeverity::Error,
            span: error.span(),
            message: error.to_string(),
            metadata,
        }
    }

    pub fn from_resolve_error(error: &ResolveError) -> Self {
        let mut metadata = DiagnosticMetadata::new();
        let code = match error {
            ResolveError::SymbolAlreadyDefined(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-SYMBOL-ALREADY-DEFINED"
            }
            ResolveError::VarUndefined(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-VAR-UNDEFINED"
            }
            ResolveError::VarImmutable(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-VAR-IMMUTABLE"
            }
            ResolveError::FuncUndefined(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-FUNC-UNDEFINED"
            }
            ResolveError::InvalidAssignmentTarget(_, _) => "MC-RESOLVE-INVALID-ASSIGNMENT-TARGET",
            ResolveError::InvalidCallee(_, _) => "MC-RESOLVE-INVALID-CALLEE",
            ResolveError::ExpectedType(name, kind, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                metadata.insert(
                    "found_kind".to_string(),
                    DiagnosticValue::String(kind.to_string()),
                );
                "MC-RESOLVE-EXPECTED-TYPE"
            }
            ResolveError::ExpectedTrait(name, kind, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                metadata.insert(
                    "found_kind".to_string(),
                    DiagnosticValue::String(kind.to_string()),
                );
                "MC-RESOLVE-EXPECTED-TRAIT"
            }
            ResolveError::TypeUndefined(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-TYPE-UNDEFINED"
            }
            ResolveError::TraitUndefined(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-TRAIT-UNDEFINED"
            }
            ResolveError::StructUndefined(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-STRUCT-UNDEFINED"
            }
            ResolveError::EnumUndefined(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-ENUM-UNDEFINED"
            }
            ResolveError::EnumVariantUndefined(enum_name, variant_name, _) => {
                metadata.insert(
                    "enum".to_string(),
                    DiagnosticValue::String(enum_name.clone()),
                );
                metadata.insert(
                    "variant".to_string(),
                    DiagnosticValue::String(variant_name.clone()),
                );
                "MC-RESOLVE-ENUM-VARIANT-UNDEFINED"
            }
            ResolveError::MethodDeclOnNonIntrinsicType(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-METHOD-DECL-NON-INTRINSIC-TYPE"
            }
            ResolveError::MethodDeclMissingIntrinsic(name, _) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-METHOD-DECL-MISSING-INTRINSIC"
            }
            ResolveError::UnknownAttribute(name, _) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-RESOLVE-UNKNOWN-ATTRIBUTE"
            }
            ResolveError::AttrDuplicate(name, _) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-RESOLVE-ATTRIBUTE-DUPLICATE"
            }
            ResolveError::AttrWrongArgCount(name, expected, found, _) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert(
                    "expected".to_string(),
                    DiagnosticValue::Number(*expected as i64),
                );
                metadata.insert("found".to_string(), DiagnosticValue::Number(*found as i64));
                "MC-RESOLVE-ATTRIBUTE-WRONG-ARG-COUNT"
            }
            ResolveError::AttrWrongArgType(name, _) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-RESOLVE-ATTRIBUTE-WRONG-ARG-TYPE"
            }
            ResolveError::AttrNotAllowed(name, where_, _) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert(
                    "location_kind".to_string(),
                    DiagnosticValue::String((*where_).to_string()),
                );
                "MC-RESOLVE-ATTRIBUTE-NOT-ALLOWED"
            }
            ResolveError::DuplicateRequireAlias(alias, _) => {
                metadata.insert("alias".to_string(), DiagnosticValue::String(alias.clone()));
                "MC-RESOLVE-DUPLICATE-REQUIRE-ALIAS"
            }
            ResolveError::ModuleQualifiedAccessUnsupported(alias, member, _) => {
                metadata.insert("alias".to_string(), DiagnosticValue::String(alias.clone()));
                metadata.insert(
                    "member".to_string(),
                    DiagnosticValue::String(member.clone()),
                );
                "MC-RESOLVE-MODULE-QUALIFIED-ACCESS-UNSUPPORTED"
            }
            ResolveError::ModuleMemberUndefined(module, member, _) => {
                metadata.insert(
                    "module".to_string(),
                    DiagnosticValue::String(module.clone()),
                );
                metadata.insert(
                    "member".to_string(),
                    DiagnosticValue::String(member.clone()),
                );
                "MC-RESOLVE-MODULE-MEMBER-UNDEFINED"
            }
            ResolveError::ProtocolRoleUndefined(path, _) => {
                metadata.insert("path".to_string(), DiagnosticValue::String(path.clone()));
                "MC-RESOLVE-PROTOCOL-ROLE-UNDEFINED"
            }
            ResolveError::ExpectedProtocolRole(path, found, _) => {
                metadata.insert("path".to_string(), DiagnosticValue::String(path.clone()));
                metadata.insert(
                    "found_kind".to_string(),
                    DiagnosticValue::String(found.to_string()),
                );
                "MC-RESOLVE-EXPECTED-PROTOCOL-ROLE"
            }
            ResolveError::ProtocolFlowRoleUndefined(protocol, role, _) => {
                metadata.insert(
                    "protocol".to_string(),
                    DiagnosticValue::String(protocol.clone()),
                );
                metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
                "MC-RESOLVE-PROTOCOL-FLOW-ROLE-UNDEFINED"
            }
            ResolveError::TypestateRoleImplMalformedPath(typestate, path, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(typestate.clone()),
                );
                metadata.insert("path".to_string(), DiagnosticValue::String(path.clone()));
                "MC-TYPESTATE-ROLE-IMPL-MALFORMED-PATH"
            }
            ResolveError::TypestateRoleImplRoleUndefined(typestate, path, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(typestate.clone()),
                );
                metadata.insert("path".to_string(), DiagnosticValue::String(path.clone()));
                "MC-TYPESTATE-ROLE-IMPL-UNDEFINED"
            }
            ResolveError::TypestateRoleImplExpectedRole(typestate, path, found, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(typestate.clone()),
                );
                metadata.insert("path".to_string(), DiagnosticValue::String(path.clone()));
                metadata.insert(
                    "found_kind".to_string(),
                    DiagnosticValue::String(found.to_string()),
                );
                "MC-TYPESTATE-ROLE-IMPL-EXPECTED-ROLE"
            }
            ResolveError::TypestateMissingState(name, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-MISSING-STATE"
            }
            ResolveError::TypestateDuplicateState(name, state, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                "MC-TYPESTATE-DUPLICATE-STATE"
            }
            ResolveError::TypestateDuplicateFieldsBlock(name, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-DUPLICATE-FIELDS-BLOCK"
            }
            ResolveError::TypestateDuplicateStateFieldsBlock(name, state, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                "MC-TYPESTATE-DUPLICATE-STATE-FIELDS-BLOCK"
            }
            ResolveError::TypestateStateFieldShadowsCarriedField(name, state, field, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert("field".to_string(), DiagnosticValue::String(field.clone()));
                "MC-TYPESTATE-STATE-FIELD-SHADOWS-CARRIED-FIELD"
            }
            ResolveError::TypestateMissingNew(name, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-MISSING-NEW"
            }
            ResolveError::TypestateDuplicateNew(name, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-DUPLICATE-NEW"
            }
            ResolveError::TypestateInvalidNewReturn(name, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-INVALID-NEW-RETURN"
            }
            ResolveError::TypestateExplicitSelfNotAllowed(name, state, method, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert(
                    "method".to_string(),
                    DiagnosticValue::String(method.clone()),
                );
                "MC-TYPESTATE-EXPLICIT-SELF-NOT-ALLOWED"
            }
            ResolveError::TypestateInvalidTransitionReturn(name, state, method, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert(
                    "method".to_string(),
                    DiagnosticValue::String(method.clone()),
                );
                "MC-TYPESTATE-TRANSITION-RETURN"
            }
            ResolveError::TypestateDuplicateTransition(name, state, method, _) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert(
                    "method".to_string(),
                    DiagnosticValue::String(method.clone()),
                );
                "MC-TYPESTATE-DUPLICATE-TRANSITION"
            }
            ResolveError::TypestateStateLiteralOutsideTypestate(state, _) => {
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                "MC-TYPESTATE-STATE-LITERAL-OUTSIDE-TYPESTATE"
            }
        };
        Self {
            phase: DiagnosticPhase::Resolve,
            code: code.to_string(),
            severity: DiagnosticSeverity::Error,
            span: error.span(),
            message: error.to_string(),
            metadata,
        }
    }

    pub fn from_semcheck_error(error: &SemCheckError) -> Self {
        let mut metadata = DiagnosticMetadata::new();
        populate_semcheck_metadata(error, &mut metadata);
        Self {
            phase: DiagnosticPhase::Semcheck,
            code: semcheck_code(error).to_string(),
            severity: DiagnosticSeverity::Error,
            span: error.span(),
            message: error.to_string(),
            metadata,
        }
    }

    pub fn from_typecheck_error(error: &TypeCheckError) -> Self {
        let kind = error.kind();
        let mut metadata = DiagnosticMetadata::new();
        populate_typecheck_metadata(kind, &mut metadata);
        Self {
            phase: DiagnosticPhase::Typecheck,
            code: typecheck_code(kind).to_string(),
            severity: DiagnosticSeverity::Error,
            span: error.span(),
            message: error.to_string(),
            metadata,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PartialDiagnostics {
    pub parse: Vec<Diagnostic>,
    pub resolve: Vec<Diagnostic>,
    pub typecheck: Vec<Diagnostic>,
}

impl PartialDiagnostics {
    pub fn all(&self) -> Vec<&Diagnostic> {
        self.parse
            .iter()
            .chain(self.resolve.iter())
            .chain(self.typecheck.iter())
            .collect()
    }
}

pub fn collect_partial_diagnostics(
    parse_errors: Option<&[ParseError]>,
    resolve_errors: Option<&[ResolveError]>,
    typecheck_errors: Option<&[TypeCheckError]>,
) -> PartialDiagnostics {
    PartialDiagnostics {
        parse: parse_errors
            .unwrap_or(&[])
            .iter()
            .map(Diagnostic::from_parse_error)
            .collect(),
        resolve: resolve_errors
            .unwrap_or(&[])
            .iter()
            .map(Diagnostic::from_resolve_error)
            .collect(),
        typecheck: typecheck_errors
            .unwrap_or(&[])
            .iter()
            .map(Diagnostic::from_typecheck_error)
            .collect(),
    }
}

fn populate_typecheck_metadata(kind: &TypeCheckErrorKind, metadata: &mut DiagnosticMetadata) {
    match kind {
        TypeCheckErrorKind::ArgCountMismatch(name, expected, found, _) => {
            metadata.insert(
                "callable".to_string(),
                DiagnosticValue::String(name.clone()),
            );
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::Number(*expected as i64),
            );
            metadata.insert("found".to_string(), DiagnosticValue::Number(*found as i64));
        }
        TypeCheckErrorKind::ArgTypeMismatch(index, expected, found, _) => {
            metadata.insert("index".to_string(), DiagnosticValue::Number(*index as i64));
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::String(expected.to_string()),
            );
            metadata.insert(
                "found".to_string(),
                DiagnosticValue::String(found.to_string()),
            );
        }
        TypeCheckErrorKind::DeclTypeMismatch(expected, found, _) => {
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::String(expected.to_string()),
            );
            metadata.insert(
                "found".to_string(),
                DiagnosticValue::String(found.to_string()),
            );
        }
        TypeCheckErrorKind::TryErrorNotInReturn(missing, ret, _) => {
            metadata.insert(
                "missing".to_string(),
                DiagnosticValue::StringList(missing.clone()),
            );
            metadata.insert(
                "return_variants".to_string(),
                DiagnosticValue::StringList(ret.clone()),
            );
            metadata.insert("quick_fixable".to_string(), DiagnosticValue::Bool(true));
        }
        TypeCheckErrorKind::ReturnNotInErrorUnion(expected, found, _) => {
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::StringList(expected.clone()),
            );
            metadata.insert(
                "found".to_string(),
                DiagnosticValue::String(found.to_string()),
            );
        }
        TypeCheckErrorKind::TraitBoundNotSatisfied(trait_name, ty, _) => {
            metadata.insert(
                "trait".to_string(),
                DiagnosticValue::String(trait_name.clone()),
            );
            metadata.insert("type".to_string(), DiagnosticValue::String(ty.to_string()));
        }
        TypeCheckErrorKind::ProtocolFlowHandlerMissing(typestate, role, payload, _) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert(
                "payload".to_string(),
                DiagnosticValue::String(payload.to_string()),
            );
        }
        TypeCheckErrorKind::ProtocolOutgoingPayloadNotAllowed(typestate, role, payload, _) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert(
                "payload".to_string(),
                DiagnosticValue::String(payload.to_string()),
            );
        }
        _ => {}
    }
}

fn semcheck_code(error: &SemCheckError) -> &'static str {
    match error {
        SemCheckError::ValueOutOfRange(..) => "MC-SEMCK-ValueOutOfRange",
        SemCheckError::ValueNotNonZero(..) => "MC-SEMCK-ValueNotNonZero",
        SemCheckError::InvalidRangeBounds(..) => "MC-SEMCK-InvalidRangeBounds",
        SemCheckError::DivisionByZero(..) => "MC-SEMCK-DivisionByZero",
        SemCheckError::InvalidCallee(..) => "MC-SEMCK-InvalidCallee",
        SemCheckError::UnknownStructType(..) => "MC-SEMCK-UnknownStructType",
        SemCheckError::DuplicateStructField(..) => "MC-SEMCK-DuplicateStructField",
        SemCheckError::UnknownStructField(..) => "MC-SEMCK-UnknownStructField",
        SemCheckError::StructFieldsMissing(..) => "MC-SEMCK-StructFieldsMissing",
        SemCheckError::UnknownEnumType(..) => "MC-SEMCK-UnknownEnumType",
        SemCheckError::UnknownEnumVariant(..) => "MC-SEMCK-UnknownEnumVariant",
        SemCheckError::EnumVariantPayloadArityMismatch(..) => {
            "MC-SEMCK-EnumVariantPayloadArityMismatch"
        }
        SemCheckError::MatchTargetNotEnum(..) => "MC-SEMCK-MatchTargetNotEnum",
        SemCheckError::MatchPatternEnumMismatch(..) => "MC-SEMCK-MatchPatternEnumMismatch",
        SemCheckError::NonExhaustiveMatch(..) => "MC-SEMCK-NonExhaustiveMatch",
        SemCheckError::NonExhaustiveUnionMatch(..) => "MC-SEMCK-NonExhaustiveUnionMatch",
        SemCheckError::DuplicateMatchVariant(..) => "MC-SEMCK-DuplicateMatchVariant",
        SemCheckError::InvalidMatchPattern(..) => "MC-SEMCK-InvalidMatchPattern",
        SemCheckError::WildcardArmNotLast(..) => "MC-SEMCK-WildcardArmNotLast",
        SemCheckError::TupleMatchRequiresSingleArm(..) => "MC-SEMCK-TupleMatchRequiresSingleArm",
        SemCheckError::TuplePatternArityMismatch(..) => "MC-SEMCK-TuplePatternArityMismatch",
        SemCheckError::InOutParamNotAggregate(..) => "MC-SEMCK-InOutParamNotAggregate",
        SemCheckError::InOutArgNotLvalue(..) => "MC-SEMCK-InOutArgNotLvalue",
        SemCheckError::InOutArgNotMutable(..) => "MC-SEMCK-InOutArgNotMutable",
        SemCheckError::InOutArgMissingMode(..) => "MC-SEMCK-InOutArgMissingMode",
        SemCheckError::InOutArgUnexpected(..) => "MC-SEMCK-InOutArgUnexpected",
        SemCheckError::OutParamNotAggregate(..) => "MC-SEMCK-OutParamNotAggregate",
        SemCheckError::OutArgNotLvalue(..) => "MC-SEMCK-OutArgNotLvalue",
        SemCheckError::OutArgNotMutable(..) => "MC-SEMCK-OutArgNotMutable",
        SemCheckError::OutArgMissingMode(..) => "MC-SEMCK-OutArgMissingMode",
        SemCheckError::OutArgUnexpected(..) => "MC-SEMCK-OutArgUnexpected",
        SemCheckError::OutSelfNotAllowed(..) => "MC-SEMCK-OutSelfNotAllowed",
        SemCheckError::OutParamNotInitialized(..) => "MC-SEMCK-OutParamNotInitialized",
        SemCheckError::SinkArgMissingMove(..) => "MC-SEMCK-SinkArgMissingMove",
        SemCheckError::MoveArgUnexpected(..) => "MC-SEMCK-MoveArgUnexpected",
        SemCheckError::PartialInitNotAllowed(..) => "MC-SEMCK-PartialInitNotAllowed",
        SemCheckError::OverlappingLvalueArgs(..) => "MC-SEMCK-OverlappingLvalueArgs",
        SemCheckError::SinkParamNotOwned(..) => "MC-SEMCK-SinkParamNotOwned",
        SemCheckError::UseBeforeInit(..) => "MC-SEMCK-UseBeforeInit",
        SemCheckError::UseAfterMove(..) => "MC-SEMCK-UseAfterMove",
        SemCheckError::InvalidMoveTarget(..) => "MC-SEMCK-InvalidMoveTarget",
        SemCheckError::MoveFromParam(..) => "MC-SEMCK-MoveFromParam",
        SemCheckError::OwnedMoveRequired(..) => "MC-SEMCK-OwnedMoveRequired",
        SemCheckError::SliceEscapeReturn(..) => "MC-SEMCK-SliceEscapeReturn",
        SemCheckError::SliceEscapeStore(..) => "MC-SEMCK-SliceEscapeStore",
        SemCheckError::SliceBorrowConflict(..) => "MC-SEMCK-SliceBorrowConflict",
        SemCheckError::SliceTargetNotLvalue(..) => "MC-SEMCK-SliceTargetNotLvalue",
        SemCheckError::ClosureCaptureMove(..) => "MC-SEMCK-ClosureCaptureMove",
        SemCheckError::ClosureCaptureUnused(..) => "MC-SEMCK-ClosureCaptureUnused",
        SemCheckError::ClosureBorrowConflict(..) => "MC-SEMCK-ClosureBorrowConflict",
        SemCheckError::ClosureEscapeReturn(..) => "MC-SEMCK-ClosureEscapeReturn",
        SemCheckError::ClosureEscapeStore(..) => "MC-SEMCK-ClosureEscapeStore",
        SemCheckError::ClosureEscapeArg(..) => "MC-SEMCK-ClosureEscapeArg",
    }
}

fn populate_semcheck_metadata(error: &SemCheckError, metadata: &mut DiagnosticMetadata) {
    match error {
        SemCheckError::UnknownStructType(name, _)
        | SemCheckError::DuplicateStructField(name, _)
        | SemCheckError::UnknownStructField(name, _)
        | SemCheckError::StructFieldsMissing(name, _)
        | SemCheckError::UnknownEnumType(name, _)
        | SemCheckError::DuplicateMatchVariant(name, _)
        | SemCheckError::OutParamNotInitialized(name, _)
        | SemCheckError::UseBeforeInit(name, _)
        | SemCheckError::UseAfterMove(name, _)
        | SemCheckError::ClosureCaptureMove(name, _)
        | SemCheckError::ClosureCaptureUnused(name, _)
        | SemCheckError::ClosureBorrowConflict(name, _) => {
            metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
        }
        SemCheckError::UnknownEnumVariant(enum_name, variant_name, _)
        | SemCheckError::MatchPatternEnumMismatch(enum_name, variant_name, _) => {
            metadata.insert(
                "enum".to_string(),
                DiagnosticValue::String(enum_name.clone()),
            );
            metadata.insert(
                "variant".to_string(),
                DiagnosticValue::String(variant_name.clone()),
            );
        }
        SemCheckError::EnumVariantPayloadArityMismatch(variant, expected, found, _) => {
            metadata.insert(
                "variant".to_string(),
                DiagnosticValue::String(variant.clone()),
            );
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::Number(*expected as i64),
            );
            metadata.insert("found".to_string(), DiagnosticValue::Number(*found as i64));
        }
        SemCheckError::NonExhaustiveUnionMatch(missing, _) => {
            metadata.insert(
                "missing".to_string(),
                DiagnosticValue::StringList(missing.clone()),
            );
        }
        SemCheckError::MatchTargetNotEnum(ty, _)
        | SemCheckError::InvalidMatchPattern(ty, _)
        | SemCheckError::InOutParamNotAggregate(ty, _)
        | SemCheckError::OutParamNotAggregate(ty, _)
        | SemCheckError::SinkParamNotOwned(ty, _) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(ty.to_string()));
        }
        _ => {}
    }
}

macro_rules! with_typecheck_variants {
    ($m:ident, $kind:expr) => {
        $m!(
            $kind;
            ArithTypeMismatch,
            ArithOperandNotInt,
            CmpOperandNotInt,
            TypeNotEquatable,
            NegationOperandNotInt,
            LogicalOperandNotBoolean,
            TryOperandNotErrorUnion,
            TryOutsideFunction,
            TryReturnTypeNotErrorUnion,
            TryErrorNotInReturn,
            JoinArmTypeMismatch,
            JoinArmNotInErrorUnion,
            CondNotBoolean,
            BreakOutsideLoop,
            ContinueOutsideLoop,
            ReturnOutsideFunction,
            ReturnValueMissing,
            ReturnValueUnexpected,
            ReturnTypeMismatch,
            ReturnNotInErrorUnion,
            ThenElseTypeMismatch,
            AssignTypeMismatch,
            ArgCountMismatch,
            ArgTypeMismatch,
            TypeArgCountMismatch,
            InvalidCallee,
            EmptyArrayLiteral,
            TooManyIndices,
            ArrayElementTypeMismatch,
            TypeNotHashable,
            IndexTypeNotInt,
            InvalidIndexTargetType,
            MapKeyTypeMismatch,
            MapIndexValueNotCopySafe,
            MapIndexAssignUnsupported,
            UnknownType,
            UnionNotAllowedHere,
            PatternTypeMismatch,
            DeclTypeMismatch,
            DeclTypeMismatchMulti,
            ArrayPatternLengthMismatch,
            EmptyTupleLiteral,
            TupleFieldOutOfBounds,
            InvalidTupleFieldTarget,
            TuplePatternLengthMismatch,
            UnknownStructType,
            DuplicateStructField,
            StructFieldTypeMismatch,
            UnknownStructField,
            StructFieldsMissing,
            InvalidStructFieldTarget,
            OpaqueTypeConstruction,
            OpaqueFieldAccess,
            PropertyNotWritable,
            PropertyNotReadable,
            PropertyGetterHasParams,
            PropertySetterParamCount,
            PropertySetterReturnType,
            PropertyAccessorTypeMismatch,
            PropertyAccessorDuplicate,
            PropertyCalledAsMethod,
            PropertyConflictsWithField,
            TraitImplDuplicate,
            TraitMethodDuplicate,
            TraitMethodNotInTrait,
            TraitMethodImplDuplicate,
            TraitMethodMissingImpl,
            TraitMethodSignatureMismatch,
            TraitPropertyDuplicate,
            TraitPropertyNotInTrait,
            TraitPropertyMissingImpl,
            TraitPropertyTypeMismatch,
            TraitPropertyMissingGetter,
            TraitPropertyMissingSetter,
            UnknownEnumType,
            UnknownEnumVariant,
            InvalidStructUpdateTarget,
            EnumVariantPayloadArityMismatch,
            EnumVariantPayloadTypeMismatch,
            MatchTargetNotEnum,
            MatchArmTypeMismatch,
            MatchPatternEnumMismatch,
            MatchTypedBindingTypeMismatch,
            NonExhaustiveMatch,
            DuplicateMatchVariant,
            StringIndexAssign,
            InvalidRangeBounds,
            RefinementBaseNotInt,
            BoundsOutOfRange,
            ValueOutOfRange,
            ValueNotNonZero,
            RedundantNonZero,
            ForIterNotIterable,
            DivisionByZero,
            OverloadNoMatch,
            OverloadAmbiguous,
            CallableNotAccessible,
            PropertyNotAccessible,
            TraitBoundNotSatisfied,
            SliceTargetNotArrayOrString,
            SliceTargetZeroDimArray,
            StringFmtExprUnsupportedType,
            LenTargetNotLvalue,
            OpaquePatternDestructure,
            ProtocolFlowHandlerMissing,
            ProtocolOutgoingPayloadNotAllowed,
            ReplyOutsideHandler,
            ReplyCapExpected,
            ReplyPayloadNotAllowed,
            ReplyCapMustBeConsumed,
            ReplyCapConsumedMultipleTimes,
            ReplyCapParamRequired
        )
    };
}

macro_rules! typecheck_code_match {
    ($kind:expr; $($variant:ident),+ $(,)?) => {
        match $kind {
            $(TypeCheckErrorKind::$variant(..) => concat!("MC-TYPECHECK-", stringify!($variant)),)+
        }
    };
}

fn typecheck_code(kind: &TypeCheckErrorKind) -> &'static str {
    with_typecheck_variants!(typecheck_code_match, kind)
}

#[cfg(test)]
#[path = "../../tests/analysis/t_diagnostics.rs"]
mod tests;
