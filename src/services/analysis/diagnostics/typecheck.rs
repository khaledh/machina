//! Typecheck diagnostic mapping.

use crate::core::typecheck::{TypeCheckError, TypeCheckErrorKind};

use super::{Diagnostic, DiagnosticMetadata, DiagnosticPhase, DiagnosticSeverity, DiagnosticValue};

pub(super) fn from_typecheck_error(error: &TypeCheckError) -> Diagnostic {
    let kind = error.kind();
    let mut metadata = DiagnosticMetadata::new();
    populate_typecheck_metadata(kind, &mut metadata);
    Diagnostic {
        phase: DiagnosticPhase::Typecheck,
        code: typecheck_code(kind).to_string(),
        severity: DiagnosticSeverity::Error,
        span: error.span(),
        message: error.to_string(),
        metadata,
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
            DeferExprFallible,
            DeferBareTry,
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
