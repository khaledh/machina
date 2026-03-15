# Correlation Identity Options

> This note is the next narrow design step for `#182`. It asks one focused
> question: **what represents the identity of a pending interaction?**

## Goal

[12-interaction-handle.md](12-interaction-handle.md) established that an
interaction handle needs correlation identity.
[13-interaction-resolution.md](13-interaction-resolution.md) established that a
reply resolves a pending interaction only if it matches that correlation
identity.

That leaves the central open question:

**What is the correlation identity, and where does it live?**

This note compares the three candidate directions already implied by the
current design work.

## Option A: Opaque Runtime Identity

The runtime mints an internal interaction id and uses it to match replies to
pending interactions.

User code does not manipulate that identity directly. It only observes the
effects:
- a request creates a pending interaction
- a reply either resolves it or fails to match

### Why this is plausible

Machina already has runtime prior art here. The existing request/reply
transport still mints opaque correlation values internally:
- `pending_id`
- `reply_cap_id`
- `request_site_key`

These are used in the runtime pending tables and envelope routing to match
responses back to in-flight requests.

So from an implementation perspective, opaque runtime identity is very real.

### Strengths

- Keeps the low-level correlation mechanism out of user code
- Makes runtime matching straightforward
- Fits naturally with mailbox transport and staged effects

### Weaknesses

- Gives user code very little explicit control over correlation
- Risks making the interaction model feel "magic"
- Makes it harder for the type system to express ownership of a pending
  interaction if the identity never appears at the surface

## Option B: Typed Correlation Token

The language surfaces a typed token or handle representing the pending
interaction.

That token is what user code carries, stores, or resolves. Correlation is no
longer just a hidden runtime detail; it becomes part of the user-visible
interaction model.

### Strengths

- Best match for the "interactions as linear types" direction
- Gives pending interactions an explicit owner in the type system
- Makes it easier to express "this reply resolves that interaction"
- Gives later features like timeout or at-most-once semantics a natural place
  to attach

### Weaknesses

- Adds visible surface complexity
- Raises design questions about where the token can be stored and how it moves
- Requires care so the language stays understandable rather than ceremonious

## Option C: Compiler-Synthesized Link

The compiler infers or synthesizes the connection between outbound request and
inbound reply without exposing a token directly.

User code still writes in a high-level interaction style, but the compiler
creates the correlation structure behind the scenes.

### Strengths

- Could produce a very lightweight surface syntax
- Keeps boilerplate low if the compiler can infer the common case

### Weaknesses

- Hardest model to explain clearly
- Risks hidden control flow and hidden state
- Less composable when interactions need to cross helper boundaries, be stored,
  or be reasoned about explicitly
- Makes failures and diagnostics harder to explain

## Comparison

| Option | Where correlation lives | Main upside | Main risk |
|---|---|---|---|
| Opaque runtime identity | Runtime only | Simple implementation model | Too hidden for the type system |
| Typed correlation token | Surface + type system | Best semantic fit for linear interactions | More user-facing complexity |
| Compiler-synthesized link | Mostly compiler-internal | Potentially concise syntax | Too magical and hard to reason about |

## Recommendation

The strongest direction appears to be a **layered design**:

1. **Surface model:** typed, compiler-derived interaction handle / correlation
   token
2. **Runtime representation:** opaque runtime-managed correlation identity

In other words:
- the language should reason about a typed pending interaction
- that interaction may be compiler-derived rather than manually declared
- the runtime can still back that interaction with minted opaque ids internally

This gets the best of both worlds:
- the interaction remains a real value in the language
- the runtime can use an efficient opaque matching mechanism underneath

## Why This Looks Better Than the Alternatives

### Better than pure opaque runtime identity

If correlation is *only* hidden in the runtime, then the language never really
owns the interaction. That weakens the whole point of introducing an
interaction handle in the first place.

### Better than a purely compiler-synthesized link

If correlation is *only* compiler magic, then it becomes hard to explain:
- what is pending
- who owns it
- what it means for a reply to resolve it

That would make the design harder to trust and harder to diagnose.

### Good fit for Machina's direction

Machina has been moving toward making important lifecycle state explicit:
- linear values are explicit
- sessions are explicit
- hosted routing is explicit through `send(target, value)`

A typed pending interaction fits that style better than a hidden-only model.

## Consequence for the Next Step

If this direction is right, the next design step is to make the typed handle
concrete as a **compiler-derived value** rather than a separately declared
surface construct.

That binding proposal is in
[15-interaction-handle-value.md](15-interaction-handle-value.md).
