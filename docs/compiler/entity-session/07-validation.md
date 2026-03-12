# Validation and Diagnostics

## Linear Type Validation

1. At least one state in the `states` block.
2. At least one non-`@final` state.
3. Unique state names.
4. Each action and trigger must reference declared states as source and target.
5. No action or trigger may have a `@final` state as its source.
6. Same-named actions/triggers with different source states are allowed.
7. Same-named actions/triggers with the same source state are disallowed.
8. Trigger names must be externally declared types (event types).
9. Each role must reference declared actions in its permission set.
10. Roles do not constrain triggers.

## Method Block Validation

1. Every declared action must have a matching method in the type's method block.
2. The method's receiver type must match the action's source state.
3. For actions with a unique name, bare `self` is allowed (source state
   inferred). For same-named actions with different source states, the receiver
   must be explicitly annotated (`self: StateName`).
4. The method's return type must match the action's target state (and declared
   error types for fallible actions).
5. The method's parameter list must match the action's declared parameters.
6. Methods whose names do not match any declared action are treated as regular
   methods (no transition validation).

## Machine Validation

1. The `hosts` clause must reference a `@linear type` with roles.
2. The `key` field in the `hosts` clause must reference a field of the hosted
   type that supports equality and hashing.
3. The machine must provide a handler for every trigger in the type.
4. Action overrides are optional. If provided, they must reference declared
   actions.
5. Each handler's/override's first parameter must accept the source state type.
6. Each handler's/override's return type must match the target state.
7. An action override's error types must be a superset of the base action's
   error types (may add new error types, must not drop existing ones).
8. Additional handler parameters must match the declared parameters.
9. The machine must not declare handlers for undeclared actions/triggers.
10. `fn new() -> Self` is required.
11. `emit` values must be valid typed expressions.

## Session Validation (compile-time)

1. Session method calls are checked against the role's permission set.
2. Session method calls are checked against the current state type.
3. The session type advances to the action's target state after each call.
4. Calling an action not allowed for the role or not valid for the current state
   is a compile-time error.
5. A session at a `@final` state cannot call further actions or wait.
6. A session value must be consumed — dropping without use is a compile-time
   error (linear enforcement).

---

## Diagnostics

Linear type diagnostics:
- `MC-TYPE-NO-STATES`
- `MC-TYPE-NO-NON-FINAL-STATES`
- `MC-TYPE-UNKNOWN-STATE-IN-ACTION`
- `MC-TYPE-UNKNOWN-STATE-IN-TRIGGER`
- `MC-TYPE-DUPLICATE-ACTION`
- `MC-TYPE-DUPLICATE-TRIGGER`
- `MC-TYPE-FINAL-STATE-AS-SOURCE`
- `MC-TYPE-UNKNOWN-ACTION-IN-ROLE`

Method block diagnostics:
- `MC-METHOD-MISSING-ACTION` — declared action has no matching method
- `MC-METHOD-AMBIGUOUS-RECEIVER` — same-named action requires explicit receiver
  annotation (`self: StateName`)
- `MC-METHOD-SOURCE-STATE-MISMATCH` — method receiver type doesn't match source
  state
- `MC-METHOD-TARGET-STATE-MISMATCH` — return type doesn't match target state
- `MC-METHOD-PARAM-MISMATCH` — parameters don't match action declaration

Machine diagnostics:
- `MC-MACHINE-INVALID-KEY-FIELD` — key field not found or type not hashable
- `MC-MACHINE-MISSING-TRIGGER-HANDLER`
- `MC-MACHINE-HANDLER-TYPE-MISMATCH`
- `MC-MACHINE-OVERRIDE-ERROR-SUBSET` — override drops error types from base
- `MC-MACHINE-EXTRA-HANDLER` — handler for undeclared action/trigger

Session diagnostics:
- `MC-SESSION-ACTION-NOT-ALLOWED` — includes the session's role in the message
- `MC-SESSION-ACTION-INVALID-STATE`
- `MC-SESSION-FINAL-STATE-ACTION`
- `MC-SESSION-UNCONSUMED` — linear value not used
- `MC-SESSION-USE-AFTER-CONSUME` — linear value used after move
- `MC-SESSION-INSTANCE-NOT-FOUND` (runtime)
- `MC-SESSION-INVALID-INSTANCE-STATE` (runtime)
