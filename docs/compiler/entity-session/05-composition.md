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

Request/reply is a runtime mechanism. The existing request/reply mechanism with
correlated responses remains available for machine-level `on` handlers. The two
styles are complementary:

- **Blocking session calls** — sequential, inline, used in action overrides
  for cross-machine coordination. The cross-machine interaction is invisible
  to the type's state graph.
- **Request/reply with `on` handlers** — asynchronous, message-driven, used at
  the machine level for external system integration. Replies arrive as separate
  messages in the machine's mailbox, routed to instances as triggers via
  `self.deliver(key, event)`.

The blocking style is syntactic sugar over the same underlying request/reply
mechanism. The runtime handles the interleaving.

## Relationship to Other Constructs

### Usage Tiers

- **Direct linear type** (no roles): Caller owns the value. Method block
  provides action implementations; calls are direct function calls.
- **Hosted linear type** (roles + machine): managed workflow with sessions.
  Method block provides base implementations; machine overrides as needed.
- **Full linear type** (roles + triggers + machine): complete model with
  system-driven transitions.

The compiler shares state-struct generation, linear checking, and
state-dispatch logic between direct and hosted modes.

### `on` Handlers

`on` handlers remain as machine-level message handlers. They process messages
from the machine's general mailbox.

`action` and `trigger` handlers are instance-level handlers that operate on
specific instances.
