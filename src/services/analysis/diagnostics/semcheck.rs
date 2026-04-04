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
        SemCheckErrorKind::CatchAllArmNotLast => "MC-SEMCK-CatchAllArmNotLast",
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
        SemCheckErrorKind::BorrowEscapeReturn => "MC-SEMCK-BorrowEscapeReturn",
        SemCheckErrorKind::BorrowEscapeStatic => "MC-SEMCK-BorrowEscapeStatic",
        SemCheckErrorKind::BorrowEscapeStore => "MC-SEMCK-BorrowEscapeStore",
        SemCheckErrorKind::SliceBorrowConflict => "MC-SEMCK-SliceBorrowConflict",
        SemCheckErrorKind::SliceTargetNotLvalue => "MC-SEMCK-SliceTargetNotLvalue",
        SemCheckErrorKind::ClosureCaptureMove(..) => "MC-SEMCK-ClosureCaptureMove",
        SemCheckErrorKind::ClosureCaptureUnused(..) => "MC-SEMCK-ClosureCaptureUnused",
        SemCheckErrorKind::ClosureBorrowConflict(..) => "MC-SEMCK-ClosureBorrowConflict",
        SemCheckErrorKind::ClosureBorrowCapture(..) => "MC-SEMCK-ClosureBorrowCapture",
        SemCheckErrorKind::ClosureEscapeReturn => "MC-SEMCK-ClosureEscapeReturn",
        SemCheckErrorKind::ClosureEscapeStore => "MC-SEMCK-ClosureEscapeStore",
        SemCheckErrorKind::ClosureEscapeArg => "MC-SEMCK-ClosureEscapeArg",
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
        | SemCheckErrorKind::ClosureBorrowConflict(name)
        | SemCheckErrorKind::ClosureBorrowCapture(name) => {
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
        _ => {}
    }
}
