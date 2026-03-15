# Entity and Session Design

This design has been split into focused documents under
[`entity-session/`](entity-session/). Read in order:

**Core (V1):**

1. [Overview](00-overview.md) — Problem, terms, design principles, V1 scope
2. [Entity](01-entity.md) — Transition model, entity declaration, direct mode, key, fields, states, actions, triggers, roles
3. [Machine](02-machine.md) — Machine declaration, action/trigger handlers, `emit`, `on` handlers, `self.deliver()`
4. [Session](03-session.md) — Session types, create/resume, lifecycle, blocking, persistence, concurrency/staleness

**Extensions (V2):**

5. [Channel](04-channel.md) — Channel declaration, event flow, pull/push consumption, subscriptions
6. [Inter-Machine Communication](10-inter-machine-communication.md) — V1 boundary, interaction semantics gap, V2 direction

**Cross-cutting:**

7. [Composition](05-composition.md) — Cross-machine composition, transaction boundaries, relationship to existing constructs
8. [Internals](06-internals.md) — Instance storage, session-machine interaction, lowering, runtime impact
9. [Validation](07-validation.md) — Validation rules, diagnostics

**Reference:**

10. [Examples](08-examples.md) — Core example (V1) and channel extension example (V2)
11. [Open Questions](09-open-questions.md) — Unsettled design decisions

This document set supersedes `machine-session-design.md` and
`machine-session-server-design.md`.
