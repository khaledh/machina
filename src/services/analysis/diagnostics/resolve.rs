//! Resolve-phase diagnostic mapping.

use crate::core::resolve::{ResolveError, ResolveErrorKind};

use super::{Diagnostic, DiagnosticMetadata, DiagnosticPhase, DiagnosticSeverity, DiagnosticValue};

pub(super) fn from_resolve_error(error: &ResolveError) -> Diagnostic {
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
        ResolveErrorKind::LinearNoStates(name) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            "MC-TYPE-NO-STATES"
        }
        ResolveErrorKind::LinearNoNonFinalStates(name) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            "MC-TYPE-NO-NON-FINAL-STATES"
        }
        ResolveErrorKind::LinearDuplicateState(name, state) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            "MC-TYPE-DUPLICATE-STATE"
        }
        ResolveErrorKind::LinearUnknownStateInAction(name, action, state) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            "MC-TYPE-UNKNOWN-STATE-IN-ACTION"
        }
        ResolveErrorKind::LinearUnknownStateInTrigger(name, trigger, state) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "trigger".to_string(),
                DiagnosticValue::String(trigger.clone()),
            );
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            "MC-TYPE-UNKNOWN-STATE-IN-TRIGGER"
        }
        ResolveErrorKind::LinearDuplicateAction(name, action, state) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            "MC-TYPE-DUPLICATE-ACTION"
        }
        ResolveErrorKind::LinearDuplicateTrigger(name, trigger, state) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "trigger".to_string(),
                DiagnosticValue::String(trigger.clone()),
            );
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            "MC-TYPE-DUPLICATE-TRIGGER"
        }
        ResolveErrorKind::LinearFinalStateAsSource(name, transition, state) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "transition".to_string(),
                DiagnosticValue::String(transition.clone()),
            );
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            "MC-TYPE-FINAL-STATE-AS-SOURCE"
        }
        ResolveErrorKind::LinearUnknownActionInRole(name, role, action) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert("role".to_string(), DiagnosticValue::String(role.clone()));
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            "MC-TYPE-UNKNOWN-ACTION-IN-ROLE"
        }
        ResolveErrorKind::LinearUnknownStateAttribute(name, state, attr) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            metadata.insert(
                "attribute".to_string(),
                DiagnosticValue::String(attr.clone()),
            );
            "MC-TYPE-UNKNOWN-STATE-ATTRIBUTE"
        }
        ResolveErrorKind::LinearMethodMissingAction(name, action, state) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            metadata.insert("state".to_string(), DiagnosticValue::String(state.clone()));
            "MC-METHOD-MISSING-ACTION"
        }
        ResolveErrorKind::LinearMethodAmbiguousReceiver(name, action) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            "MC-METHOD-AMBIGUOUS-RECEIVER"
        }
        ResolveErrorKind::LinearMethodSourceStateMismatch(name, action, expected, found) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::String(expected.clone()),
            );
            metadata.insert("found".to_string(), DiagnosticValue::String(found.clone()));
            "MC-METHOD-SOURCE-STATE-MISMATCH"
        }
        ResolveErrorKind::LinearMethodTargetStateMismatch(name, action, expected, found) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            metadata.insert(
                "expected".to_string(),
                DiagnosticValue::String(expected.clone()),
            );
            metadata.insert("found".to_string(), DiagnosticValue::String(found.clone()));
            "MC-METHOD-TARGET-STATE-MISMATCH"
        }
        ResolveErrorKind::LinearMethodParamMismatch(name, action) => {
            metadata.insert("type".to_string(), DiagnosticValue::String(name.clone()));
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            "MC-METHOD-PARAM-MISMATCH"
        }
        ResolveErrorKind::LinearUseAfterConsume(name) => {
            metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
            "MC-TYPE-USE-AFTER-CONSUME"
        }
        ResolveErrorKind::MachineHostedTypeUndefined(machine, ty) => {
            metadata.insert(
                "machine".to_string(),
                DiagnosticValue::String(machine.clone()),
            );
            metadata.insert("type".to_string(), DiagnosticValue::String(ty.clone()));
            "MC-MACHINE-HOSTED-TYPE-UNDEFINED"
        }
        ResolveErrorKind::MachineHostedTypeNotLinear(machine, ty) => {
            metadata.insert(
                "machine".to_string(),
                DiagnosticValue::String(machine.clone()),
            );
            metadata.insert("type".to_string(), DiagnosticValue::String(ty.clone()));
            "MC-MACHINE-HOSTED-TYPE-NOT-LINEAR"
        }
        ResolveErrorKind::MachineInvalidKeyField(machine, ty, field) => {
            metadata.insert(
                "machine".to_string(),
                DiagnosticValue::String(machine.clone()),
            );
            metadata.insert("type".to_string(), DiagnosticValue::String(ty.clone()));
            metadata.insert("field".to_string(), DiagnosticValue::String(field.clone()));
            "MC-MACHINE-INVALID-KEY-FIELD"
        }
        ResolveErrorKind::MachineMissingTriggerHandler(machine, trigger) => {
            metadata.insert(
                "machine".to_string(),
                DiagnosticValue::String(machine.clone()),
            );
            metadata.insert(
                "trigger".to_string(),
                DiagnosticValue::String(trigger.clone()),
            );
            "MC-MACHINE-MISSING-TRIGGER-HANDLER"
        }
        ResolveErrorKind::MachineExtraHandler(machine, kind, name) => {
            metadata.insert(
                "machine".to_string(),
                DiagnosticValue::String(machine.clone()),
            );
            metadata.insert(
                "kind".to_string(),
                DiagnosticValue::String((*kind).to_string()),
            );
            metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
            "MC-MACHINE-EXTRA-HANDLER"
        }
        ResolveErrorKind::MachineHandlerTypeMismatch(machine, kind, name) => {
            metadata.insert(
                "machine".to_string(),
                DiagnosticValue::String(machine.clone()),
            );
            metadata.insert(
                "kind".to_string(),
                DiagnosticValue::String((*kind).to_string()),
            );
            metadata.insert("name".to_string(), DiagnosticValue::String(name.clone()));
            "MC-MACHINE-HANDLER-TYPE-MISMATCH"
        }
        ResolveErrorKind::MachineOverrideErrorSubset(machine, action) => {
            metadata.insert(
                "machine".to_string(),
                DiagnosticValue::String(machine.clone()),
            );
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            "MC-MACHINE-OVERRIDE-ERROR-SUBSET"
        }
        ResolveErrorKind::MachineHostedActionEmitUnsupported(machine, action) => {
            metadata.insert(
                "machine".to_string(),
                DiagnosticValue::String(machine.clone()),
            );
            metadata.insert(
                "action".to_string(),
                DiagnosticValue::String(action.clone()),
            );
            "MC-MACHINE-HOSTED-ACTION-EMIT-UNSUPPORTED"
        }
    };
    Diagnostic {
        phase: DiagnosticPhase::Resolve,
        code: code.to_string(),
        severity: DiagnosticSeverity::Error,
        span: error.span(),
        message: error.to_string(),
        metadata,
    }
}
