# Interaction Handle as a Derived Value

> This note is the first binding design step for `#182`. It makes a concrete
> proposal for what the interaction handle **is** as a value in the language,
> while keeping the interaction itself **derived** from existing declarations
> rather than introduced as a new top-level construct.

## Binding Proposal

An interaction handle is:

- a **compiler-derived linear value**
- created by one interaction-establishing `send(...)`
- owned by the waiting entity state
- consumed exactly once when the interaction resolves

The important distinction is:
- the interaction handle is a real value in the language model
- but the user does **not** declare a separate `interaction` type by hand

## Where the Interaction Comes From

The interaction should be derived from information the user already provides:

1. **The action override performs a directed `send(...)`**
2. **The action transitions into a waiting state**
3. **The waiting state exits only through triggers**

Those existing declarations already contain the essential interaction shape:

- the outgoing request is the `send(...)`
- the waiting phase is the target state
- the allowed replies are the triggers whose source is that waiting state

So the compiler should recognize the pattern instead of requiring the user to
write a second, redundant interaction declaration.

## Auth-Check Example

In the auth-check flow:

```mc
action submit(draft) -> PendingAuth {
    send(self.auth_service, AuthCheck {
        order_id: draft.id,
        user_id: draft.user_id,
    });
    draft.submit()
}
```

and:

```mc
triggers {
    AuthApproved: PendingAuth -> Confirmed,
    AuthDenied: PendingAuth -> Rejected,
}
```

The compiler can derive:

- request type: `AuthCheck`
- waiting state: `PendingAuth`
- reply set: `AuthApproved | AuthDenied`

That is already enough to recognize a pending interaction pattern.

## Why the Handle Is Still a Real Value

Even though the interaction is compiler-derived, the pending interaction should
still be modeled as a **real linear value** in the language semantics.

Otherwise we lose the strongest benefits:
- no explicit owner
- no precise meaning for "already resolved"
- no principled place for correlation to live

So the right model is:
- **derived by the compiler**
- **owned like a linear value**

## Where It Lives

The derived handle lives in the **waiting entity state**, not in machine config
and not as a pure compiler ghost.

Conceptually, entering `PendingAuth` means:
- the order is waiting for auth
- the order owns one pending auth interaction

The user may not need to spell a field like `auth: ...` directly, but the
language model should still treat that state as carrying the pending
interaction.

That gives the state a stronger meaning:
- not just "we once sent something"
- but "this instance currently owns a pending interaction with a known reply
  contract"

## First-Version Recognition Rule

To keep the model crisp, the first version should be intentionally narrow.

An interaction is formed only when all of the following are true:

1. the action override performs **one distinguished directed `send(...)`**
2. the action transitions into a **trigger-only waiting state**
3. the waiting state's outgoing triggers define the reply set

If those conditions are not met, `send(...)` remains plain fire-and-forget
routing with no derived interaction semantics.

This narrow rule is a feature, not a limitation. It keeps the compiler's
pattern recognition explainable.

## Why Not a User-Declared `AuthInteraction` Type

The user has already declared the workflow:
- `submit` sends a request and enters `PendingAuth`
- `PendingAuth` exits via `AuthApproved` or `AuthDenied`

Requiring an additional user-declared interaction type would repeat
information that is already present in the state/trigger structure.

That would make the design feel heavier than it needs to be.

The better fit for Machina is:
- user declares the workflow once
- compiler derives the interaction value and enforces the pattern

## Session Consequence

This still fits the current session story well.

From the client's perspective:

```mc
let pending = buyer.submit()?;
let next = pending.wait()?;
```

That remains the right high-level flow.

The improvement is not that the client now manipulates an interaction token
manually. The improvement is that the machine/runtime has a principled pending
value associated with the waiting entity while the client is blocked in
`wait()`.

## What Resolution Consumes

When a matching reply arrives:

1. the system finds the waiting instance
2. it resolves the derived pending interaction associated with that instance's
   waiting state
3. that resolution **consumes** the pending interaction
4. the instance transitions to its next state

So the derived handle's lifecycle is:

- created by the outgoing `send(...)`
- attached to the waiting state
- consumed by resolution

That gives "resolved" a precise meaning even though the interaction was not
hand-declared by the user.

## What This Clarifies

This binding proposal gives concrete answers to the questions raised in
[14-correlation-identity.md](14-correlation-identity.md):

- **Is the handle linear?**
  - Yes.

- **Where can it be stored?**
  - In the waiting entity state, as a compiler-derived linear value.

- **Is it attached to an entity/session or free-standing?**
  - Primarily attached to the waiting entity state.

- **What does it mean to consume it?**
  - It is consumed when a matching reply resolves the interaction.

## Open Questions That Still Remain

Even with this binding proposal, a few important questions remain:

1. **How visible is the derived handle to user code?**
   - Is it entirely implicit?
   - Can user code inspect it in any way?

2. **How does the compiler surface the derived interaction pattern?**
   - purely by convention
   - or with an optional annotation for clarity/debuggability

3. **How does reply matching locate the owning instance?**
   - does the derived handle carry enough runtime identity directly?
   - or does the machine still use an explicit key lookup step?

4. **What happens when a state mixes actions and triggers?**
   - in the first version, those states should simply not qualify as derived
     interaction states

These are now much better-shaped questions than before.

## Recommendation

Use this as the binding working model for `#182`:

- interaction handles are real linear values in the language semantics
- they are **compiler-derived**, not user-declared
- they attach to trigger-only waiting states
- they are consumed by resolution
- they are backed by opaque runtime correlation ids internally

That is concrete enough to begin shaping syntax, diagnostics, and
implementation around.
