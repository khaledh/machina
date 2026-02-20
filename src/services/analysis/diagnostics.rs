//! Unified diagnostics model for analysis queries and IDE adapters.
//!
//! This layer normalizes parse/resolve/typecheck/semcheck diagnostics into one
//! phase-tagged representation with stable codes and structured metadata.

use std::collections::BTreeMap;

use crate::core::diag::Span;
use crate::core::lexer::{LexError, LexErrorKind};
use crate::core::parse::{ParseError, ParseErrorKind};
use crate::core::resolve::{ResolveError, ResolveErrorKind};
use crate::core::semck::{SemCheckError, SemCheckErrorKind};
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
        let code = match error.kind() {
            LexErrorKind::UnexpectedCharacter(ch) => {
                metadata.insert(
                    "character".to_string(),
                    DiagnosticValue::String(ch.to_string()),
                );
                "MC-LEX-UNEXPECTED-CHARACTER"
            }
            LexErrorKind::InvalidInteger(_) => "MC-LEX-INVALID-INTEGER",
            LexErrorKind::InvalidEscapeSequence(seq) => {
                metadata.insert("escape".to_string(), DiagnosticValue::String(seq.clone()));
                "MC-LEX-INVALID-ESCAPE-SEQUENCE"
            }
            LexErrorKind::UnterminatedString => "MC-LEX-UNTERMINATED-STRING",
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
        let code = match error.kind() {
            ParseErrorKind::ExpectedDecl(_) => "MC-PARSE-EXPECTED-DECL",
            ParseErrorKind::ExpectedToken(expected, found) => {
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
            ParseErrorKind::ExpectedIdent(_) => "MC-PARSE-EXPECTED-IDENT",
            ParseErrorKind::ExpectedSelf(_) => "MC-PARSE-EXPECTED-SELF",
            ParseErrorKind::ExpectedType(_) => "MC-PARSE-EXPECTED-TYPE",
            ParseErrorKind::ExpectedPrimary(_) => "MC-PARSE-EXPECTED-PRIMARY",
            ParseErrorKind::ExpectedIntLit(_) => "MC-PARSE-EXPECTED-INT-LIT",
            ParseErrorKind::ExpectedStringLit(_) => "MC-PARSE-EXPECTED-STRING-LIT",
            ParseErrorKind::ExpectedPattern(_) => "MC-PARSE-EXPECTED-PATTERN",
            ParseErrorKind::SingleFieldTupleMissingComma(_) => "MC-PARSE-TUPLE-MISSING-COMMA",
            ParseErrorKind::SingleElementSetMissingComma(_) => "MC-PARSE-SET-MISSING-COMMA",
            ParseErrorKind::ExpectedStructField(_) => "MC-PARSE-EXPECTED-STRUCT-FIELD",
            ParseErrorKind::ExpectedMatchArm(_) => "MC-PARSE-EXPECTED-MATCH-ARM",
            ParseErrorKind::ExpectedMatchPattern(_) => "MC-PARSE-EXPECTED-MATCH-PATTERN",
            ParseErrorKind::ExpectedArrayIndexOrRange(_) => "MC-PARSE-EXPECTED-INDEX-OR-RANGE",
            ParseErrorKind::ExpectedRefinement(_) => "MC-PARSE-EXPECTED-REFINEMENT",
            ParseErrorKind::UnknownAttribute(name) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-PARSE-UNKNOWN-ATTRIBUTE"
            }
            ParseErrorKind::AttributeNotAllowed => "MC-PARSE-ATTRIBUTE-NOT-ALLOWED",
            ParseErrorKind::FeatureDisabled { feature } => {
                metadata.insert(
                    "feature".to_string(),
                    DiagnosticValue::String((*feature).to_string()),
                );
                "MC-PARSE-FEATURE-DISABLED"
            }
            ParseErrorKind::UnmatchedFormatBrace => "MC-PARSE-UNMATCHED-FORMAT-BRACE",
            ParseErrorKind::InvalidFormatExpr => "MC-PARSE-INVALID-FORMAT-EXPR",
            ParseErrorKind::EmptyFormatExpr => "MC-PARSE-EMPTY-FORMAT-EXPR",
            ParseErrorKind::UnterminatedFormatExpr => "MC-PARSE-UNTERMINATED-FORMAT-EXPR",
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
        let code = match error.kind() {
            ResolveErrorKind::SymbolAlreadyDefined(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-SYMBOL-ALREADY-DEFINED"
            }
            ResolveErrorKind::VarUndefined(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-VAR-UNDEFINED"
            }
            ResolveErrorKind::VarImmutable(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-VAR-IMMUTABLE"
            }
            ResolveErrorKind::FuncUndefined(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-FUNC-UNDEFINED"
            }
            ResolveErrorKind::InvalidAssignmentTarget(_) => "MC-RESOLVE-INVALID-ASSIGNMENT-TARGET",
            ResolveErrorKind::InvalidCallee(_) => "MC-RESOLVE-INVALID-CALLEE",
            ResolveErrorKind::ExpectedType(name, kind) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                metadata.insert(
                    "found_kind".to_string(),
                    DiagnosticValue::String(kind.to_string()),
                );
                "MC-RESOLVE-EXPECTED-TYPE"
            }
            ResolveErrorKind::ExpectedTrait(name, kind) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                metadata.insert(
                    "found_kind".to_string(),
                    DiagnosticValue::String(kind.to_string()),
                );
                "MC-RESOLVE-EXPECTED-TRAIT"
            }
            ResolveErrorKind::TypeUndefined(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-TYPE-UNDEFINED"
            }
            ResolveErrorKind::TraitUndefined(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-TRAIT-UNDEFINED"
            }
            ResolveErrorKind::StructUndefined(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-STRUCT-UNDEFINED"
            }
            ResolveErrorKind::EnumUndefined(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-ENUM-UNDEFINED"
            }
            ResolveErrorKind::EnumVariantUndefined(enum_name, variant_name) => {
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
            ResolveErrorKind::MethodDeclOnNonIntrinsicType(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-METHOD-DECL-NON-INTRINSIC-TYPE"
            }
            ResolveErrorKind::MethodDeclMissingIntrinsic(name) => {
                metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
                "MC-RESOLVE-METHOD-DECL-MISSING-INTRINSIC"
            }
            ResolveErrorKind::UnknownAttribute(name) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-RESOLVE-UNKNOWN-ATTRIBUTE"
            }
            ResolveErrorKind::AttrDuplicate(name) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-RESOLVE-ATTRIBUTE-DUPLICATE"
            }
            ResolveErrorKind::AttrWrongArgCount(name, expected, found) => {
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
            ResolveErrorKind::AttrWrongArgType(name) => {
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-RESOLVE-ATTRIBUTE-WRONG-ARG-TYPE"
            }
            ResolveErrorKind::AttrNotAllowed(name, where_) => {
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
            ResolveErrorKind::AttrMachinesRequiresMain => "MC-RESOLVE-ATTRIBUTE-MACHINES-MAIN-ONLY",
            ResolveErrorKind::DuplicateRequireAlias(alias) => {
                metadata.insert("alias".to_string(), DiagnosticValue::String(alias.clone()));
                "MC-RESOLVE-DUPLICATE-REQUIRE-ALIAS"
            }
            ResolveErrorKind::ModuleQualifiedAccessUnsupported(alias, member) => {
                metadata.insert("alias".to_string(), DiagnosticValue::String(alias.clone()));
                metadata.insert(
                    "member".to_string(),
                    DiagnosticValue::String(member.clone()),
                );
                "MC-RESOLVE-MODULE-QUALIFIED-ACCESS-UNSUPPORTED"
            }
            ResolveErrorKind::ModuleMemberUndefined(module, member) => {
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
            ResolveErrorKind::ProtocolRoleUndefined(path) => {
                metadata.insert("path".to_string(), DiagnosticValue::String(path.clone()));
                "MC-RESOLVE-PROTOCOL-ROLE-UNDEFINED"
            }
            ResolveErrorKind::ExpectedProtocolRole(path, found) => {
                metadata.insert("path".to_string(), DiagnosticValue::String(path.clone()));
                metadata.insert(
                    "found_kind".to_string(),
                    DiagnosticValue::String(found.to_string()),
                );
                "MC-RESOLVE-EXPECTED-PROTOCOL-ROLE"
            }
            ResolveErrorKind::ProtocolRequestContractRoleUndefined(protocol, role) => {
                metadata.insert(
                    "protocol".to_string(),
                    DiagnosticValue::String(protocol.clone()),
                );
                metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
                "MC-RESOLVE-PROTOCOL-REQUEST-CONTRACT-ROLE-UNDEFINED"
            }
            ResolveErrorKind::ProtocolTransitionSourceRoleUndefined(
                protocol,
                role,
                state,
                source_role,
            ) => {
                metadata.insert(
                    "protocol".to_string(),
                    DiagnosticValue::String(protocol.clone()),
                );
                metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert(
                    "source_role".to_string(),
                    DiagnosticValue::String(source_role.clone()),
                );
                "MC-RESOLVE-PROTOCOL-TRANSITION-SOURCE-ROLE-UNDEFINED"
            }
            ResolveErrorKind::ProtocolTransitionEffectRoleUndefined(
                protocol,
                role,
                state,
                effect_role,
            ) => {
                metadata.insert(
                    "protocol".to_string(),
                    DiagnosticValue::String(protocol.clone()),
                );
                metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert(
                    "effect_role".to_string(),
                    DiagnosticValue::String(effect_role.clone()),
                );
                "MC-RESOLVE-PROTOCOL-TRANSITION-EFFECT-ROLE-UNDEFINED"
            }
            ResolveErrorKind::ProtocolTransitionNextStateUndefined(
                protocol,
                role,
                state,
                next_state,
            ) => {
                metadata.insert(
                    "protocol".to_string(),
                    DiagnosticValue::String(protocol.clone()),
                );
                metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert(
                    "next_state".to_string(),
                    DiagnosticValue::String(next_state.clone()),
                );
                "MC-RESOLVE-PROTOCOL-TRANSITION-NEXT-STATE-UNDEFINED"
            }
            ResolveErrorKind::ProtocolTransitionTriggerConflict(
                protocol,
                role,
                state,
                trigger_ty,
                source_role,
            ) => {
                metadata.insert(
                    "protocol".to_string(),
                    DiagnosticValue::String(protocol.clone()),
                );
                metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert(
                    "trigger_ty".to_string(),
                    DiagnosticValue::String(trigger_ty.clone()),
                );
                metadata.insert(
                    "source_role".to_string(),
                    DiagnosticValue::String(source_role.clone()),
                );
                "MC-RESOLVE-PROTOCOL-TRANSITION-TRIGGER-CONFLICT"
            }
            ResolveErrorKind::TypestateRoleImplMalformedPath(typestate, path) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(typestate.clone()),
                );
                metadata.insert("path".to_string(), DiagnosticValue::String(path.clone()));
                "MC-TYPESTATE-ROLE-IMPL-MALFORMED-PATH"
            }
            ResolveErrorKind::TypestateRoleImplRoleUndefined(typestate, path) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(typestate.clone()),
                );
                metadata.insert("path".to_string(), DiagnosticValue::String(path.clone()));
                "MC-TYPESTATE-ROLE-IMPL-UNDEFINED"
            }
            ResolveErrorKind::TypestateRoleImplExpectedRole(typestate, path, found) => {
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
            ResolveErrorKind::TypestateRoleBindingInvalidType(typestate, field) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(typestate.clone()),
                );
                metadata.insert("field".to_string(), DiagnosticValue::String(field.clone()));
                "MC-TYPESTATE-ROLE-BINDING-INVALID-TYPE"
            }
            ResolveErrorKind::TypestateRoleBindingRoleUndefined(typestate, field, role) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(typestate.clone()),
                );
                metadata.insert("field".to_string(), DiagnosticValue::String(field.clone()));
                metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
                "MC-TYPESTATE-ROLE-BINDING-ROLE-UNDEFINED"
            }
            ResolveErrorKind::TypestateRoleBindingDuplicateRole(typestate, role) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(typestate.clone()),
                );
                metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
                "MC-TYPESTATE-ROLE-BINDING-DUPLICATE-ROLE"
            }
            ResolveErrorKind::TypestateRoleBindingMissing(typestate, role) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(typestate.clone()),
                );
                metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
                "MC-TYPESTATE-ROLE-BINDING-MISSING"
            }
            ResolveErrorKind::TypestateMissingState(name) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-MISSING-STATE"
            }
            ResolveErrorKind::TypestateDuplicateState(name, state) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                "MC-TYPESTATE-DUPLICATE-STATE"
            }
            ResolveErrorKind::TypestateDuplicateFieldsBlock(name) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-DUPLICATE-FIELDS-BLOCK"
            }
            ResolveErrorKind::TypestateDuplicateStateFieldsBlock(name, state) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                "MC-TYPESTATE-DUPLICATE-STATE-FIELDS-BLOCK"
            }
            ResolveErrorKind::TypestateStateFieldShadowsCarriedField(name, state, field) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert("field".to_string(), DiagnosticValue::String(field.clone()));
                "MC-TYPESTATE-STATE-FIELD-SHADOWS-CARRIED-FIELD"
            }
            ResolveErrorKind::TypestateMissingNew(name) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-MISSING-NEW"
            }
            ResolveErrorKind::TypestateDuplicateNew(name) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-DUPLICATE-NEW"
            }
            ResolveErrorKind::TypestateInvalidNewReturn(name) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-INVALID-NEW-RETURN"
            }
            ResolveErrorKind::TypestateExplicitSelfNotAllowed(name, state, method) => {
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
            ResolveErrorKind::TypestateInvalidTransitionReturn(name, state, method) => {
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
            ResolveErrorKind::TypestateInvalidOnHandlerReturn(name) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                "MC-TYPESTATE-ON-HANDLER-RETURN"
            }
            ResolveErrorKind::TypestateInvalidStateOnHandlerReturn(name, state) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                "MC-TYPESTATE-STATE-ON-HANDLER-RETURN"
            }
            ResolveErrorKind::TypestateDuplicateTransition(name, state, method) => {
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
            ResolveErrorKind::TypestateUnknownStateAttribute(name, state, attr) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                metadata.insert(
                    "attribute".to_string(),
                    DiagnosticValue::String(attr.clone()),
                );
                "MC-TYPESTATE-UNKNOWN-STATE-ATTRIBUTE"
            }
            ResolveErrorKind::TypestateFinalStateHasTransition(name, state) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                "MC-TYPESTATE-FINAL-STATE-HAS-TRANSITION"
            }
            ResolveErrorKind::TypestateFinalStateHasHandler(name, state) => {
                metadata.insert(
                    "typestate".to_string(),
                    DiagnosticValue::String(name.clone()),
                );
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                "MC-TYPESTATE-FINAL-STATE-HAS-HANDLER"
            }
            ResolveErrorKind::TypestateStateLiteralOutsideTypestate(state) => {
                metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
                "MC-TYPESTATE-STATE-LITERAL-OUTSIDE-TYPESTATE"
            }
            ResolveErrorKind::TypestateSpawnRequiresMachinesOptIn => {
                "MC-TYPESTATE-SPAWN-REQUIRES-MACHINES-OPT-IN"
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
        TypeCheckErrorKind::ArgCountMismatch(name, expected, found, ..) => {
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
        TypeCheckErrorKind::ArgTypeMismatch(index, expected, found, ..) => {
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
        TypeCheckErrorKind::DeclTypeMismatch(expected, found, ..) => {
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::String(expected.to_string()),
            );
            metadata.insert(
                "found".to_string(),
                DiagnosticValue::String(found.to_string()),
            );
        }
        TypeCheckErrorKind::TryErrorNotInReturn(missing, ret, ..) => {
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
        TypeCheckErrorKind::ReturnNotInErrorUnion(expected, found, ..) => {
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::StringList(expected.clone()),
            );
            metadata.insert(
                "found".to_string(),
                DiagnosticValue::String(found.to_string()),
            );
        }
        TypeCheckErrorKind::TraitBoundNotSatisfied(trait_name, ty, ..) => {
            metadata.insert(
                "trait".to_string(),
                DiagnosticValue::String(trait_name.clone()),
            );
            metadata.insert("type".to_string(), DiagnosticValue::String(ty.to_string()));
        }
        TypeCheckErrorKind::ProtocolStateHandlerMissing(typestate, role, state, payload, ..) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            metadata.insert(
                "payload".to_string(),
                DiagnosticValue::String(payload.to_string()),
            );
        }
        TypeCheckErrorKind::ProtocolStateOutgoingPayloadNotAllowed(
            typestate,
            role,
            state,
            payload,
            ..,
        ) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            metadata.insert(
                "payload".to_string(),
                DiagnosticValue::String(payload.to_string()),
            );
        }
        TypeCheckErrorKind::ProtocolStateEmitDestinationRoleMismatch(
            typestate,
            role,
            state,
            payload,
            expected_role,
            field,
            bound_role,
            ..,
        ) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            metadata.insert(
                "payload".to_string(),
                DiagnosticValue::String(payload.to_string()),
            );
            metadata.insert(
                "expectedRole".to_string(),
                DiagnosticValue::String(expected_role.clone()),
            );
            metadata.insert("field".to_string(), DiagnosticValue::String(field.clone()));
            metadata.insert(
                "boundRole".to_string(),
                DiagnosticValue::String(bound_role.clone()),
            );
        }
        TypeCheckErrorKind::ProtocolStateEmitDestinationRoleUnbound(
            typestate,
            role,
            state,
            payload,
            expected_role,
            ..,
        ) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            metadata.insert(
                "payload".to_string(),
                DiagnosticValue::String(payload.to_string()),
            );
            metadata.insert(
                "expectedRole".to_string(),
                DiagnosticValue::String(expected_role.clone()),
            );
        }
        TypeCheckErrorKind::ProtocolRequestContractMissing(
            typestate,
            role,
            request,
            to_role,
            ..,
        ) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert(
                "request".to_string(),
                DiagnosticValue::String(request.to_string()),
            );
            metadata.insert(
                "toRole".to_string(),
                DiagnosticValue::String(to_role.clone()),
            );
        }
        TypeCheckErrorKind::ProtocolRequestContractAmbiguous(
            typestate,
            role,
            request,
            to_role,
            ..,
        ) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert(
                "request".to_string(),
                DiagnosticValue::String(request.to_string()),
            );
            metadata.insert(
                "toRole".to_string(),
                DiagnosticValue::String(to_role.clone()),
            );
        }
        TypeCheckErrorKind::ProtocolRequestResponseNotInContract(
            typestate,
            role,
            request,
            to_role,
            response,
            ..,
        ) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert(
                "request".to_string(),
                DiagnosticValue::String(request.to_string()),
            );
            metadata.insert(
                "toRole".to_string(),
                DiagnosticValue::String(to_role.clone()),
            );
            metadata.insert(
                "response".to_string(),
                DiagnosticValue::String(response.to_string()),
            );
        }
        _ => {}
    }
}

fn semcheck_code(error: &SemCheckError) -> &'static str {
    match error.kind() {
        SemCheckErrorKind::ValueOutOfRange(..) => "MC-SEMCK-ValueOutOfRange",
        SemCheckErrorKind::ValueNotNonZero(..) => "MC-SEMCK-ValueNotNonZero",
        SemCheckErrorKind::InvalidRangeBounds(..) => "MC-SEMCK-InvalidRangeBounds",
        SemCheckErrorKind::DivisionByZero => "MC-SEMCK-DivisionByZero",
        SemCheckErrorKind::InvalidCallee(..) => "MC-SEMCK-InvalidCallee",
        SemCheckErrorKind::UnknownStructType(..) => "MC-SEMCK-UnknownStructType",
        SemCheckErrorKind::DuplicateStructField(..) => "MC-SEMCK-DuplicateStructField",
        SemCheckErrorKind::UnknownStructField(..) => "MC-SEMCK-UnknownStructField",
        SemCheckErrorKind::StructFieldsMissing(..) => "MC-SEMCK-StructFieldsMissing",
        SemCheckErrorKind::UnknownEnumType(..) => "MC-SEMCK-UnknownEnumType",
        SemCheckErrorKind::UnknownEnumVariant(..) => "MC-SEMCK-UnknownEnumVariant",
        SemCheckErrorKind::EnumVariantPayloadArityMismatch(..) => {
            "MC-SEMCK-EnumVariantPayloadArityMismatch"
        }
        SemCheckErrorKind::MatchTargetNotEnum(..) => "MC-SEMCK-MatchTargetNotEnum",
        SemCheckErrorKind::MatchPatternEnumMismatch(..) => "MC-SEMCK-MatchPatternEnumMismatch",
        SemCheckErrorKind::NonExhaustiveMatch => "MC-SEMCK-NonExhaustiveMatch",
        SemCheckErrorKind::NonExhaustiveUnionMatch(..) => "MC-SEMCK-NonExhaustiveUnionMatch",
        SemCheckErrorKind::DuplicateMatchVariant(..) => "MC-SEMCK-DuplicateMatchVariant",
        SemCheckErrorKind::InvalidMatchPattern(..) => "MC-SEMCK-InvalidMatchPattern",
        SemCheckErrorKind::WildcardArmNotLast => "MC-SEMCK-WildcardArmNotLast",
        SemCheckErrorKind::TupleMatchRequiresSingleArm => "MC-SEMCK-TupleMatchRequiresSingleArm",
        SemCheckErrorKind::TuplePatternArityMismatch(..) => "MC-SEMCK-TuplePatternArityMismatch",
        SemCheckErrorKind::InOutParamNotAggregate(..) => "MC-SEMCK-InOutParamNotAggregate",
        SemCheckErrorKind::InOutArgNotLvalue => "MC-SEMCK-InOutArgNotLvalue",
        SemCheckErrorKind::InOutArgNotMutable => "MC-SEMCK-InOutArgNotMutable",
        SemCheckErrorKind::InOutArgMissingMode => "MC-SEMCK-InOutArgMissingMode",
        SemCheckErrorKind::InOutArgUnexpected => "MC-SEMCK-InOutArgUnexpected",
        SemCheckErrorKind::OutParamNotAggregate(..) => "MC-SEMCK-OutParamNotAggregate",
        SemCheckErrorKind::OutArgNotLvalue => "MC-SEMCK-OutArgNotLvalue",
        SemCheckErrorKind::OutArgNotMutable => "MC-SEMCK-OutArgNotMutable",
        SemCheckErrorKind::OutArgMissingMode => "MC-SEMCK-OutArgMissingMode",
        SemCheckErrorKind::OutArgUnexpected => "MC-SEMCK-OutArgUnexpected",
        SemCheckErrorKind::OutSelfNotAllowed => "MC-SEMCK-OutSelfNotAllowed",
        SemCheckErrorKind::OutParamNotInitialized(..) => "MC-SEMCK-OutParamNotInitialized",
        SemCheckErrorKind::SinkArgMissingMove => "MC-SEMCK-SinkArgMissingMove",
        SemCheckErrorKind::MoveArgUnexpected => "MC-SEMCK-MoveArgUnexpected",
        SemCheckErrorKind::PartialInitNotAllowed(..) => "MC-SEMCK-PartialInitNotAllowed",
        SemCheckErrorKind::OverlappingLvalueArgs => "MC-SEMCK-OverlappingLvalueArgs",
        SemCheckErrorKind::SinkParamNotOwned(..) => "MC-SEMCK-SinkParamNotOwned",
        SemCheckErrorKind::UseBeforeInit(..) => "MC-SEMCK-UseBeforeInit",
        SemCheckErrorKind::UseAfterMove(..) => "MC-SEMCK-UseAfterMove",
        SemCheckErrorKind::InvalidMoveTarget => "MC-SEMCK-InvalidMoveTarget",
        SemCheckErrorKind::MoveFromParam => "MC-SEMCK-MoveFromParam",
        SemCheckErrorKind::OwnedMoveRequired => "MC-SEMCK-OwnedMoveRequired",
        SemCheckErrorKind::SliceEscapeReturn => "MC-SEMCK-SliceEscapeReturn",
        SemCheckErrorKind::SliceEscapeStore => "MC-SEMCK-SliceEscapeStore",
        SemCheckErrorKind::SliceBorrowConflict => "MC-SEMCK-SliceBorrowConflict",
        SemCheckErrorKind::SliceTargetNotLvalue => "MC-SEMCK-SliceTargetNotLvalue",
        SemCheckErrorKind::ClosureCaptureMove(..) => "MC-SEMCK-ClosureCaptureMove",
        SemCheckErrorKind::ClosureCaptureUnused(..) => "MC-SEMCK-ClosureCaptureUnused",
        SemCheckErrorKind::ClosureBorrowConflict(..) => "MC-SEMCK-ClosureBorrowConflict",
        SemCheckErrorKind::ClosureEscapeReturn => "MC-SEMCK-ClosureEscapeReturn",
        SemCheckErrorKind::ClosureEscapeStore => "MC-SEMCK-ClosureEscapeStore",
        SemCheckErrorKind::ClosureEscapeArg => "MC-SEMCK-ClosureEscapeArg",
        SemCheckErrorKind::ProtocolProgressionMissingTriggerTransition(..) => {
            "MC-SEMCK-ProtocolProgressionMissingTriggerTransition"
        }
        SemCheckErrorKind::ProtocolProgressionImpossibleEmit(..) => {
            "MC-SEMCK-ProtocolProgressionImpossibleEmit"
        }
        SemCheckErrorKind::ProtocolProgressionImpossibleReturnState(..) => {
            "MC-SEMCK-ProtocolProgressionImpossibleReturnState"
        }
    }
}

fn populate_semcheck_metadata(error: &SemCheckError, metadata: &mut DiagnosticMetadata) {
    match error.kind() {
        SemCheckErrorKind::UnknownStructType(name)
        | SemCheckErrorKind::DuplicateStructField(name)
        | SemCheckErrorKind::UnknownStructField(name)
        | SemCheckErrorKind::StructFieldsMissing(name)
        | SemCheckErrorKind::UnknownEnumType(name)
        | SemCheckErrorKind::DuplicateMatchVariant(name)
        | SemCheckErrorKind::OutParamNotInitialized(name)
        | SemCheckErrorKind::UseBeforeInit(name)
        | SemCheckErrorKind::UseAfterMove(name)
        | SemCheckErrorKind::ClosureCaptureMove(name)
        | SemCheckErrorKind::ClosureCaptureUnused(name)
        | SemCheckErrorKind::ClosureBorrowConflict(name) => {
            metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
        }
        SemCheckErrorKind::UnknownEnumVariant(enum_name, variant_name)
        | SemCheckErrorKind::MatchPatternEnumMismatch(enum_name, variant_name) => {
            metadata.insert(
                "enum".to_string(),
                DiagnosticValue::String(enum_name.clone()),
            );
            metadata.insert(
                "variant".to_string(),
                DiagnosticValue::String(variant_name.clone()),
            );
        }
        SemCheckErrorKind::EnumVariantPayloadArityMismatch(variant, expected, found) => {
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
        SemCheckErrorKind::NonExhaustiveUnionMatch(missing) => {
            metadata.insert(
                "missing".to_string(),
                DiagnosticValue::StringList(missing.clone()),
            );
        }
        SemCheckErrorKind::ProtocolProgressionMissingTriggerTransition(
            typestate,
            protocol,
            role,
            state,
            selector,
        ) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert(
                "protocol".to_string(),
                DiagnosticValue::String(protocol.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            metadata.insert(
                "selector".to_string(),
                DiagnosticValue::String(selector.to_string()),
            );
        }
        SemCheckErrorKind::ProtocolProgressionImpossibleEmit(
            typestate,
            protocol,
            role,
            state,
            selector,
            payload,
            to_role,
        ) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert(
                "protocol".to_string(),
                DiagnosticValue::String(protocol.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            metadata.insert(
                "selector".to_string(),
                DiagnosticValue::String(selector.to_string()),
            );
            metadata.insert(
                "payload".to_string(),
                DiagnosticValue::String(payload.to_string()),
            );
            metadata.insert(
                "to_role".to_string(),
                DiagnosticValue::String(to_role.clone()),
            );
        }
        SemCheckErrorKind::ProtocolProgressionImpossibleReturnState(
            typestate,
            protocol,
            role,
            state,
            selector,
            to_state,
        ) => {
            metadata.insert(
                "typestate".to_string(),
                DiagnosticValue::String(typestate.clone()),
            );
            metadata.insert(
                "protocol".to_string(),
                DiagnosticValue::String(protocol.clone()),
            );
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            metadata.insert(
                "selector".to_string(),
                DiagnosticValue::String(selector.to_string()),
            );
            metadata.insert(
                "to_state".to_string(),
                DiagnosticValue::String(to_state.clone()),
            );
        }
        SemCheckErrorKind::MatchTargetNotEnum(ty)
        | SemCheckErrorKind::InvalidMatchPattern(ty)
        | SemCheckErrorKind::InOutParamNotAggregate(ty)
        | SemCheckErrorKind::OutParamNotAggregate(ty)
        | SemCheckErrorKind::SinkParamNotOwned(ty) => {
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
            TryHandlerNotCallable,
            TryHandlerArity,
            TryHandlerArgTypeMismatch,
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
            ProtocolStateHandlerMissing,
            ProtocolStateOutgoingPayloadNotAllowed,
            ProtocolStateEmitDestinationRoleMismatch,
            ProtocolStateEmitDestinationRoleUnbound,
            ProtocolRequestContractMissing,
            ProtocolRequestContractAmbiguous,
            ProtocolRequestResponseNotInContract,
            TypestateOverlappingOnHandlers,
            TypestateAmbiguousResponseProvenance,
            TypestateRequestMissingResponseHandler,
            TypestateHandlerUnsupportedResponseVariant,
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
            $(TypeCheckErrorKind::$variant { .. } => concat!("MC-TYPECHECK-", stringify!($variant)),)+
        }
    };
}

fn typecheck_code(kind: &TypeCheckErrorKind) -> &'static str {
    with_typecheck_variants!(typecheck_code_match, kind)
}

#[cfg(test)]
#[path = "../../tests/analysis/t_diagnostics.rs"]
mod tests;
