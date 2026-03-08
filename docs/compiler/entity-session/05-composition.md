# Cross-Machine Composition

Action overrides and trigger handlers may need to coordinate with other machines
— checking state in another service, creating instances in another service, etc.
Sessions are the composition mechanism: the same blocking session calls used by
external clients work inside handlers.

```mc
action add_reviewer(draft, reviewer: UserId) {
    // Block on a call to another machine's hosted type
    let session = self.user_service.resume(User as Admin, reviewer)?;
    match session {
        active: Active => {
            draft.reviewers.push(reviewer);
            draft
        }
        _ => error(ServiceError::ReviewerInactive)
    }
}
```

The handler blocks on `resume`, the runtime yields this machine's dispatch,
processes the other machine's handler, returns the result, and resumes the
original handler. Same blocking semantics as client-side session calls.

## Transaction Boundaries

Cross-machine session calls involve separate transactions. If the action handler
blocks on a call to machine B, machine B commits its transaction independently.
If the outer handler on machine A subsequently fails, machine B's commit is not
rolled back. This is the standard behavior for any distributed system — no
distributed transactions.

For most use cases (human-scale workflows), this is acceptable. Compensation
patterns (undo actions, sagas) can handle failure cases where needed.

## Relationship to Request/Reply

The retirement of `protocol` does not affect request/reply. Request/reply is a
runtime mechanism, not a protocol construct — it predates protocols and
continues to work unchanged. The existing request/reply mechanism with
correlated responses remains available for machine-level `on` handlers. The two
styles are complementary:

- **Blocking session calls** — sequential, inline, used in action overrides
  for cross-machine coordination. The cross-machine interaction is invisible
  to the type's state graph.
- **Request/reply with `on` handlers** — asynchronous, message-driven, used at
  the machine level for external system integration. Replies arrive as separate
  messages in the machine's mailbox, routed to instances via
  `self.deliver()`.

The blocking style is syntactic sugar over the same underlying request/reply
mechanism. The runtime handles the interleaving.

## Relationship to Existing Constructs

### Typestate

The `@linear type` replaces `typestate`. A linear type without roles or
triggers is a direct-mode linear type — functionally identical to the former
typestate construct. The compiler can share state-struct generation, linear
checking, and state-dispatch logic between direct and hosted modes.

Usage tiers:

- **Direct linear type** (no roles): replaces typestate. Caller owns the value.
  Method block provides action implementations; calls are direct function calls.
- **Hosted linear type** (roles + machine): managed workflow with sessions.
  Method block provides base implementations; machine overrides as needed.
- **Full linear type** (roles + triggers + machine): complete model with
  system-driven transitions.

### Protocols

Linear type workflows with role-constrained actions replace protocol message
contracts. A protocol's send/recv sequencing maps to action/trigger sequencing.
Role permissions replace protocol role conformance. Sequencing guarantees come
from the state machine rather than channel-oriented syntax.

What protocols provided and how linear types cover it:

- **Message shape contracts** → action signatures.
- **Role-based send/recv permissions** → role blocks.
- **Sequencing guarantees** → state machine transitions.
- **Conformance checking** → compiler validates session usage against workflow.

### `on` Handlers

`on` handlers remain as machine-level message handlers. They process messages
from the machine's general mailbox.

`action` and `trigger` handlers are instance-level handlers that operate on
specific instances.
