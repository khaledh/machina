# Internals

## Instance Storage Model

### Compiler-Managed Instance Table

For in-memory hosting, the machine's state word holds both:
1. the machine's own fields,
2. an instance table mapping `identity_key -> instance_state`.

```
Machine State (heap object):
+--------------------------+
| machine fields           |
|   ci_service: Machine    |
+--------------------------+
| instance table           |
|   id=1 -> Draft { }      |
|   id=2 -> Review { }     |
|   id=3 -> Merged { }     |
+--------------------------+
```

The instance table is opaque to the runtime. The runtime sees one state word per
machine slot, as today.

### Instance Lookup on Action/Trigger Dispatch

When a session action or delivered trigger arrives:

1. Unpack machine state word.
2. Look up instance by identity key.
   - Missing: `SessionError::InstanceNotFound`.
3. Check instance state tag vs action/trigger source state.
   - Mismatch: `SessionError::InvalidState`.
4. Call handler with instance state and params.
5. Handler returns next state; runtime collects emitted values.
6. Write next state back to instance table.
7. If next state differs from old state, generate transition notification.
8. If next state is `@final`, optionally auto-remove instance.
9. Pack updated machine state as new state word.
10. Stage session reply (if action) with success + next state token.
11. Persist if applicable.
12. (V2) Deliver emitted values and transition notifications to co-hosted
    channels.

Steps 2-3 are runtime-level checks (instance state is dynamic). The
compile-time guarantee is that all handlers exist and have correct type
signatures.

---

## Session-Machine Interaction

Session operations lower to the existing runtime dispatch model:

**Create:**
1. Client sends a `SessionCreate` envelope to the machine.
2. Machine allocates instance with fresh identity key in initial state,
   registers session.
3. Machine stages reply with session token + identity key + initial state tag.
4. Client constructs typed session value.

**Resume:**
1. Client sends a `SessionResume` envelope with `(role, identity_key)`.
2. Machine loads instance state by identity key (from memory or storage).
3. Machine stages reply with session token + current state tag.
4. Client constructs the appropriate session type variant.

**Action step:**
1. Client sends a `SessionAction` envelope with `(session_id, action, params)`.
2. Machine dispatches to the action handler.
3. Handler returns next state; runtime collects emitted values.
4. Machine persists state by identity key, stages reply with success + new
   state tag.
5. Client advances session type.

**Wait:**
1. Client sends a `SessionWait` envelope with `(session_id)`.
2. Machine registers the client as waiting for the next state change on this
   instance.
3. When a trigger transitions the instance, the machine notifies the waiting
   client with the new state tag.
4. Client matches on the new state.

**Close:**
1. Session value dropped or instance reaches `@final`.
2. Machine receives `SessionClose` (explicit or implicit on drop).
3. Machine cleans up session record. If `@final`, may remove instance.

At the server side, action and trigger handling is synchronous
run-to-completion within one dispatch step, exactly like current `on` handlers.

---

## Workflow Layer vs Transport Layer

This distinction is central to the design.

### Workflow/session layer

Defines: states, actions, triggers, roles, allowed transitions, typed client
progression, persistence boundaries, identity keys.

### Transport/runtime layer

Defines: routing, buffering, correlation, serialization, delivery mechanism,
waiting behavior, cancellation/timeouts.

The same workflow/session model should be able to ride over multiple transport
strategies:
- in-memory hosted machine transport,
- network transport,
- file/log transport.

V1 targets in-memory transport only. But the model avoids baking in the
assumption that a session is only a local mailbox conversation.

---

## Lowering Strategy

### Linear Type Lowering

A `@linear type` declaration lowers to:
1. One struct per state: `PullRequest$Draft`, `PullRequest$Review`, etc.
   Each includes the type's flat fields plus any state-specific payload fields.
2. A union type of all states: `PullRequest$State = Draft | Review | ...`.
3. An action metadata table: `(action_name, source_state) -> target_state`.
4. A trigger metadata table: `(trigger_name, source_state) -> target_state`.
5. Role permission tables: `role -> Set<action_name>`.
6. Identity key field metadata (which field is the key, from the machine's
   `hosts` clause).
7. Flat fields struct (shared across all states).

The type's method block compiles to concrete function bodies for each action.
These serve as the base implementations — callable in direct mode and as
defaults in hosted mode.

Metadata tables (3-7) are compile-time artifacts for validation and session type
derivation.

### Machine + Hosts Lowering

A `machine ... hosts Type(key: field)` declaration lowers to:
1. A typestate-like definition for the machine, with:
   - machine fields as carried fields,
   - `on` handlers lowered as today,
   - action override and `trigger` handlers lowered to dispatch thunks.
2. An instance table type (keyed by the designated identity key field) embedded
   in the machine state layout.
3. Action/trigger dispatch thunks that:
   - unpack instance state from the table by identity key,
   - call the override thunk (if the machine overrides the action) or the base
     method from the type's method block (if not overridden),
   - collect emitted values,
   - write back the result,
   - stage the session reply (if action).

### Session Type Derivation

The compiler derives session types from the linear type + role. Each session
type is a wrapper around a session handle with the current state. Method
availability is the role's permission set intersected with actions whose source
state matches.

### Action/Trigger Dispatch Table

The dispatch table extends the existing descriptor format:

```
Dispatch (per hosted type):
+--------------+----------------+----------+--------------+
| Kind         | Source State   | Thunk ID | Target State |
+--------------+----------------+----------+--------------+
| 1 (submit)   | 1 (Draft)      | thunk_50 | 2 (PendingCI)|
| 2 (revise)   | 1 (Draft)      | thunk_51 | 1 (Draft)    |
| 3 (comment)  | 1 (Draft)      | thunk_52 | 1 (Draft)    |
| 3 (comment)  | 3 (Review)     | thunk_53 | 3 (Review)   |
| 4 (approve)  | 3 (Review)     | thunk_54 | 4 (Approved) |
| 5 (reject)   | 3 (Review)     | thunk_55 | 1 (Draft)    |
| 6 (merge)    | 4 (Approved)   | thunk_56 | 5 (Merged)   |
| 7 (CIPassed) | 2 (PendingCI)  | thunk_57 | 3 (Review)   |
| 8 (CIFailed) | 2 (PendingCI)  | thunk_58 | 1 (Draft)    |
+--------------+----------------+----------+--------------+
```

Structurally identical to the existing `(state_tag, message_kind) -> thunk_id`
dispatch table. Actions and triggers share the same table format.

---

## Runtime Impact

### Runtime Additions

1. **Session table**: a per-machine table mapping `session_id -> (role,
   identity_key, client_id)`. Analogous to the existing pending table.

2. **Session envelope fields**: the envelope gains `session_id` and
   `identity_key` fields (or reuses existing payload slots). Session
   create/resume/action/wait/close are distinguished by envelope kind tags.

3. **Emit collection buffer**: a per-dispatch buffer for values emitted via
   `emit`. In V1, available for machine-internal use (logging, auditing).
   In V2, flushed to co-hosted channels.

Everything else — mailbox delivery, ready queue, dispatch loop, transaction
model, preflight/commit — stays the same.

V2 additions:
- **Channel infrastructure**: channel state, reader/writer registration,
  message buffering, `listen()` bridge to mailbox.
