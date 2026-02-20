//! Semcheck diagnostic mapping.

use crate::core::semck::{SemCheckError, SemCheckErrorKind};

use super::{Diagnostic, DiagnosticMetadata, DiagnosticPhase, DiagnosticSeverity, DiagnosticValue};

pub(super) fn from_semcheck_error(error: &SemCheckError) -> Diagnostic {
    let mut metadata = DiagnosticMetadata::new();
    populate_semcheck_metadata(error, &mut metadata);
    Diagnostic {
        phase: DiagnosticPhase::Semcheck,
        code: semcheck_code(error).to_string(),
        severity: DiagnosticSeverity::Error,
        span: error.span(),
        message: error.to_string(),
        metadata,
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
