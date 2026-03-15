# Pending Interaction Resolution

> This note is the next narrow design step for `#182`. It builds on
> [12-interaction-handle.md](12-interaction-handle.md) and focuses only on one
> question: **how does an inbound reply resolve a pending interaction?**

## Goal

The interaction-handle note established that a directed send may create a
pending interaction value with:
- destination identity
- correlation identity
- expected reply set
- pending state

That is useful, but still incomplete. The auth-check pressure test exposed the
real gap:

- `OrderService` sends `AuthCheck`
- later `AuthApproved` arrives
- what exactly makes that reply count as the resolution of the original auth
  interaction?

This note defines the minimal resolution model needed to make that idea
concrete.

## Resolution Model

A pending interaction may be resolved only by an inbound reply that satisfies
all of the following:

1. **The interaction is still active**
   - it has not already been resolved
   - it has not otherwise been finalized by some future mechanism

2. **The reply matches the interaction's expected reply set**
   - if the interaction expects `AuthApproved | AuthDenied`, then some other
     reply type does not resolve it

3. **The reply matches the interaction's correlation identity**
   - the inbound reply must refer to the same in-flight interaction, not just
     some value of a compatible type

If all three conditions hold, the reply resolves the interaction.

## Resolution Outcome

At this stage, the important semantic change is:
- a reply does not just arrive as a freestanding event
- it is first interpreted as an attempt to resolve a specific pending
  interaction

Conceptually, reply handling becomes:

1. receive an inbound reply event
2. attempt to match it to an active pending interaction
3. if it matches, mark that interaction resolved
4. only then drive the waiting machine/entity logic forward

That makes resolution a first-class semantic step instead of an ad hoc
`order_id` lookup in user code.

## Auth-Check Example

Conceptually, the auth-check flow becomes:

1. `OrderService` sends `AuthCheck`
2. that send creates a pending auth interaction for the order
3. `AuthService` later sends `AuthApproved`
4. `OrderService` receives `AuthApproved`
5. before delivering it to the `Order`, the runtime/compiler model asks:
   - is there an active auth interaction waiting for a reply?
   - is `AuthApproved` one of its allowed replies?
   - does this reply correlate to that interaction?
6. if yes, the interaction resolves
7. the resolved result is then delivered to the waiting order

This is the missing concrete step in the earlier auth-check rewrite.

## Matching Rules

This note does not yet define exact syntax or runtime representation, but it
does imply a few important matching rules:

### Reply type alone is not enough

`AuthApproved` by itself cannot be considered sufficient. If two auth checks
are in flight, the model needs more than "same reply type" to identify the
correct pending interaction.

### Correlation must be interaction-scoped

The correlation identity should be treated as part of the interaction itself,
not just a field convention that user code happens to repeat through multiple
structs.

### Resolution is one-way

Once a reply resolves an interaction, that interaction is no longer active.
Further replies cannot continue resolving the same interaction.

This does not yet define the policy for duplicate replies, but it does define
the key invariant:
- a resolved interaction is no longer eligible for matching

## What This Buys Us

Even before deciding timeouts or reply completeness, the resolution model gives
us three concrete improvements over V1:

1. **Reply handling has a real semantic step**
   - match-and-resolve happens before entity delivery

2. **Correlation stops being pure user convention**
   - the system now has a place to attach matching semantics

3. **Duplicate handling gets a clear boundary**
   - duplicates are no longer "just another reply"
   - they are replies that fail to match an active pending interaction

That last point is especially useful: it gives us a cleaner starting point for
designing duplicate-reply behavior later.

## Deliberately Out of Scope for This Note

This note still does **not** decide:
- how correlation identity is represented
- whether unmatched replies are ignored, logged, or surfaced as errors
- how duplicate replies are reported
- timeout semantics
- reply completeness checking
- exact syntax for creating, storing, or resolving interaction handles

Those are all downstream decisions.

## Next Step

The next useful design move is to choose the representation of correlation
identity itself:
- opaque runtime identity
- typed token surfaced to user code
- compiler-synthesized link

That choice should come before policy decisions like unmatched or duplicate
reply handling, because it determines what the language and runtime can even
observe. See [14-correlation-identity.md](14-correlation-identity.md).
