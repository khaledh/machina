# Typestate Machines: Surface Ergonomics & Developer Experience — Design Review

Review of the proposal in
[typestate-machines-surface-dx-design.md](typestate-machines-surface-dx-design.md).

## Status

Review completed February 2026.

Three independent expert panels evaluated the design from different
perspectives, each running on a different model architecture.

| Panel | Model | Perspective | Rating |
|-------|-------|-------------|--------|
| A | Claude Opus 4.6 | Language Design, Ergonomics vs Safety Tradeoffs, Prior Art | 7.3/10 |
| B | GPT-5.3-Codex | Compiler Implementation Feasibility and Lowering Strategy | 6/10 |
| C | Gemini 3 Pro | Devil's Advocate, Real-World Usability, Hidden Foot-Guns | 6/10 |

Cross-model average: **6.4/10** — strong handler sugar, but the proposal's
architectural abstractions (implicit runtime, implicit correlation) overshoot
the "magic budget" for a systems language.

## Relationship to Prior Reviews

This is the third and final design review in the typestate machines series.
The first two reviews established:

| Previous Finding | Status in This Design |
|------------------|-----------------------|
| Correlation by convention (Review 1) | **Addressed** — `for` keyword provides typed provenance binding |
| Payload capacity (Review 2, blocking) | **Not addressed** — surface sugar assumes typed payloads work, but boxing strategy is still unspecified |
| State data storage/rollback (Review 2, blocking) | **Not addressed** — implicit same-state transitions add new questions about field preservation |
| Phantom Reply deadlock (Review 2) | **Partially addressed** — implicit correlation hides `Pending<T>` but the underlying delivery failure problem remains |
| Runtime txn model gap for request/reply (Review 2) | **Not addressed** — implicit `reply()` sugar depends on the staged-reply fix |

The surface DX layer cannot ship independently of the blocking issues from
Review 2. The sugar assumes a working typed-payload and transactional-reply
foundation.

---

## Consensus Strengths

### 1. Handler Sugar Is Excellent (3/3)

All panels agreed that the handler-side sugar transforms usability:
- `on Ping(p)` instead of `on Ping(p: Ping)` — type inference from event
  selector is clean and unambiguous.
- `send(to, Msg{})` instead of `emit Send(to: to, Msg{})` — removes ceremony
  without losing meaning.
- `reply(Resp{})` instead of `reply(cap, Resp{})` — implicit capability in the
  common single-reply case.

The current examples have a ~15:1 boilerplate-to-logic ratio. This sugar
reduces it to roughly 1:1 for typical handlers.

Panel B confirmed these sugars (1/2/3) are straightforward to implement in the
existing pre-resolve typestate desugaring pass (`src/core/typestate/mod.rs`).

### 2. `Machine<T>` Typed Handle Is Well-Designed (3/3)

`Client::spawn()` returning `Machine<Client>` with `.send()` and `.request()`
methods is standard practice across actor frameworks. All panels approved this
without significant reservations. Panel A compared it favorably to Akka's
`ActorRef[Cmd]` and Rust Actix's `Addr<MyActor>`.

### 3. Direct Mode Coexistence Is Correct (3/3)

Keeping value-level typestate unchanged (direct transitions without runtime
scheduling) preserves Machina's existing local-reasoning story. The managed
mode is opt-in via `spawn()`.

### 4. Escape Hatches Exist (3/3)

Retaining `Pending<T>`, `ReplyCap<T>`, explicit `emit`, and the low-level
runtime API as advanced tools means power users aren't locked out. The panels
noted this needs one clarification: can implicit and explicit correlation be
mixed in the same typestate?

---

## Consensus Concerns

### 1. CRITICAL: Implicit Runtime Injection (3/3)

Unanimously identified as the proposal's most problematic element. The design
calls for the compiler to inject runtime init, a background dispatch loop, and
graceful shutdown around `main` — all invisible to the developer.

**Why all three panels objected:**

- **Panel A:** "An invisible background dispatch loop contradicts the philosophy
  of a language with explicit ownership and visible mutation." Proposes
  `#[machines]` annotation (similar to Rust's `#[tokio::main]`).

- **Panel B:** "Binary should own runtime; libraries using spawn should borrow
  ambient runtime, not create private runtimes. Background thread is not a good
  V1 choice — use cooperative polling loop first."

- **Panel C:** "A systems language must be embeddable. If Machina hijacks
  `main`, I can't call Machina from C. This pushes the language from 'systems'
  to 'application framework' territory."

**The fundamental tension:** Machina's tagline is "explicit by default." An
invisible dispatch loop is the opposite.

**Recommended resolution (consensus):**

Replace fully invisible injection with a one-line opt-in mechanism. Two
concrete proposals emerged:

```mc
// Panel A: annotation style
#[machines]
fn main() {
    let c = Client::spawn();
    c.send(Ping { id: 1 });
}

// Panel C: explicit call style
fn main() {
    machines::run(|| {
        let c = Client::spawn();
        c.send(Ping { id: 1 });
    });
}
```

Either preserves the ergonomic win (no manual runtime plumbing) while keeping
the execution model visible.

### 2. HIGH: Implicit Same-State Transitions Are Ambiguous (3/3)

Omitting `-> Connected` and `Connected {}` when staying in the same state:

```mc
state Connected {
    on Ping(p) {
        println(f"ping {p.id}");
    }
}
```

All panels raised the same concern from different angles:

- **Panel A:** "In a language built on explicit state transitions, 'nothing
  means stay' is a philosophical contradiction." Proposes `stay` keyword.

- **Panel B:** "Allow omitted arrow only for pure same-state success; require
  explicit `-> S | Error...` when error returns are used."

- **Panel C:** "If `self` is consumed (sink semantics), what does implicit
  same-state mean? The compiler must synthesize `S { f1: self.f1, ... }` — but
  does that capture mutations or the original values?"

**The state-with-fields problem:**

```mc
state Connected {
    fields { count: u64, }
    on Ping(p) {
        self.count += 1;    // Mutation on sink self
        // Implicit same-state — does the new count carry?
    }
}
```

Panel B confirmed reconstruction is feasible in typestate lowering (synthesize
tail `S { f1: self.f1, ... }` where `self` mutations are observed), but the
mental model is unclear to users.

**Recommended resolution (consensus):**

Introduce an explicit `stay` keyword:

```mc
state Connected {
    on Ping(p) -> stay {             // explicit intent, fields preserved
        println(f"ping {p.id}");
    }
}
```

For states with no fields, implicit same-state (no `-> ...`) could be allowed
as a convenience. For states with fields, require `stay` or explicit
reconstruction.

### 3. HIGH: `for` Keyword Ambiguity With Concurrent Requests (3/3)

The `for` keyword for request provenance is novel and reads well in the
single-request case:

```mc
on AuthApproved(ok) for AuthCheck(req) {
    println(f"approved conn={req.conn_id}");
}
```

But all panels identified the concurrent-request ambiguity:

```mc
on Tick(t) {
    request(self.auth, AuthCheck { conn_id: 1, token: "a" });
    request(self.auth, AuthCheck { conn_id: 2, token: "b" });
}

on AuthApproved(ok) for AuthCheck(req) {
    // Which AuthCheck does req refer to?
}
```

**Panel A:** Recommends forbid-by-default — compiler error on multiple
same-type inflight requests without labels. Labels opt in to disambiguation.

**Panel B:** Notes the runtime needs `(response_kind, request_kind, pending_id)`
routing semantics. Multiple concurrent same-type requests are fine if each
pending ID maps to its own inflight record.

**Panel C:** Points out that hiding the correlation ID makes serialization
across network boundaries (future distributed mode) hard to debug. "You are
making correlation data into metadata."

**Recommended resolution:**

Forbid multiple concurrent same-type requests by default. When needed, require
labels at both request site and handler:

```mc
request:auth1(self.auth, AuthCheck { conn_id: 1, token: "a" });
request:auth2(self.auth, AuthCheck { conn_id: 2, token: "b" });

on AuthApproved(ok) for AuthCheck:auth1(req) { ... }
on AuthApproved(ok) for AuthCheck:auth2(req) { ... }
```

### 4. HIGH: Implicit Correlation Linearity Transfer (2/3)

The existing `Pending<T>` / `ReplyCap<T>` system has proven linearity:
double-consume is a compile error, must-consume-on-all-paths is enforced.
The implicit correlation model must preserve these guarantees.

**Panel A:** "The compiler must verify that for every `request()` call, a
complete handler set exists covering all response types, and each response
handler is reachable exactly once per request."

**Panel B:** "Desugaring target can still map to hidden `Pending<T>` /
`ReplyCap<T>` flow, but runtime-backed correlation is needed because `Pending`
is not hashable/equatable at source level."

The design doc does not specify how linearity is enforced when the user
never sees `Pending<T>`.

---

## Panel-Specific Findings

### Panel A: Language Design (Claude Opus 4.6)

Unique contributions:

1. **Prior-art comparison matrix.** Detailed comparison against Erlang
   gen_statem, Akka Typed, XState, Swift actors, and Rust Actix. Key finding:
   Machina's compile-time state×event exhaustiveness checking is unique —
   "don't dilute it with too much implicit behavior."

2. **UML internal vs self-transition distinction.** An "internal transition"
   (no exit/reenter) is semantically different from a "self-transition" (exit,
   reenter, run exit/entry actions). Machina should decide which semantics
   implicit `stay` has.

3. **`for` reads like lifetime elision.** When `for` is omitted and there's
   exactly one possible request source, the compiler should infer it silently.
   When ambiguous, require explicit annotation. This mirrors Rust's lifetime
   elision rules.

4. **Library vs binary distinction.** Libraries must never inject runtime.
   `Machine<T>` handles are valid values in library code, but the dispatch
   loop only exists in the binary that links them.

### Panel B: Compiler Implementation (GPT-5.3-Codex)

Unique contributions:

1. **Precise pipeline placement for each sugar.** Sugar 1/2/3 belong in the
   existing pre-resolve typestate desugaring pass. Sugar 4 (implicit
   correlation) needs parser + typecheck + elaborate + runtime changes. Sugar 6
   (implicit runtime) is driver-level entrypoint synthesis, not parser sugar.

2. **Error-union interaction with implicit same-state.** Omitting the return
   type can safely mean exact `-> S`. Inferring `-> S | Error...` needs new
   return-type inference machinery and should be deferred. Practical rule:
   require explicit `-> S | E` when `?`/error returns are used.

3. **Implementation phasing.** Ship Sugar 1/2/3 + `Machine<T>` with explicit
   runtime ownership first. Implicit correlation and implicit runtime are a
   separate architecture pass. Each phase is independently shippable.

4. **Desugaring hygiene.** Add one dedicated "typestate surface normalization"
   subpass in `src/core/typestate/mod.rs`. After this pass, no surface sugar
   forms should remain for resolve/typecheck. Add invariant tests to enforce
   this boundary.

### Panel C: Devil's Advocate (Gemini 3 Pro)

Unique contributions:

1. **The "magic budget" framework.** Systems programmers choose systems
   languages to see what's happening. The proposal hides six things; the
   actions (send/reply sugar) are within budget, but the execution model
   (implicit runtime, implicit correlation) overspends it.

2. **FFI and embedding concern.** If Machina hijacks `main`, you can't export
   a function that spins up a Machina runtime inside a C++ app. The implicit
   runtime blocks the embedding use case.

3. **Memory leak vector in implicit correlation.** The `for` keyword implies
   the runtime holds a `pending_id → context` map. If the response never comes
   (network failure, crashed machine), this map grows without bound. Cleanup
   policy must be specified.

4. **Time-travel debugging concern.** If handlers can mutate `self` and
   implicit same-state returns the mutated value, you lose the functional-core
   property of state machines. This makes state snapshots and replay harder.

---

## Concrete Failure Scenarios

### Scenario 1: The Zombie Machine

```mc
fn main() {
    let client = Client::spawn();
    // main exits immediately
}
```

**Expected:** Program exits, client is cleaned up.
**Actual (with implicit runtime):** Depends on undocumented shutdown policy. The
runtime might hang forever (client keeps it alive), exit immediately (killing
client before it processes anything), or drain with timeout (but what timeout?).

**Root cause:** The ownership relationship between `main`'s scope, the
`Machine<Client>` handle, and the background dispatch loop is unspecified.

### Scenario 2: Magic Mutation in Implicit Same-State

```mc
state Active {
    fields { counter: u64, }
    on Tick(t) {
        self.counter += 1;
        // implicit same-state — developer expects counter to increment
    }
}
```

**Expected:** Counter increments on each Tick.
**Actual:** Depends on whether `self` is sink-consumed and the compiler
synthesizes `Active { counter: self.counter }` (capturing the mutation) or
copies the original state before the handler runs (losing the mutation).

**Root cause:** The ownership model for implicit same-state is unspecified
when combined with Machina's `sink self` handler signature.

### Scenario 3: Wrong-Context Binding With Concurrent Requests

```mc
state Running {
    on Trigger(t) {
        request(self.server, Req { id: 1 });
        request(self.server, Req { id: 2 });
    }

    on Resp(r) for Req(original) {
        println(f"response for id={original.id}");
    }
}
```

**Expected:** First response prints "response for id=1", second prints "id=2".
**Actual:** Depends on whether the runtime correlates by FIFO order,
`pending_id`, or some other mechanism. If responses arrive out of order, the
binding may be wrong. If the runtime uses FIFO matching, reordering silently
produces incorrect output.

**Root cause:** The `for` syntax suggests type-level matching (`for Req`), but
correctness requires instance-level matching (specific pending ID). The
abstraction hides the distinction.

---

## Synthesized Recommendations

Ordered by priority. Confidence reflects cross-model agreement.

| # | Recommendation | Confidence | Source |
|---|---------------|------------|--------|
| 1 | **Replace implicit runtime with explicit opt-in** — use `#[machines]` annotation or `machines::run()` call instead of invisible injection | Very High | All 3 |
| 2 | **Introduce `stay` keyword for same-state transitions** — do not allow fully implicit stay for states with fields; allow for fieldless states as convenience | Very High | All 3 |
| 3 | **Forbid concurrent same-type requests by default** — require labels for disambiguation when needed | Very High | All 3 |
| 4 | **Specify linearity preservation rules** — for every implicit `request()`, compiler must verify exhaustive response handlers exist | High | A, B |
| 5 | **Ship Sugar 1/2/3 + `Machine<T>` first** — handler payload shorthand, command sugar, typed handles are independently valuable and low-risk | High | B |
| 6 | **Defer implicit correlation to second phase** — `for` keyword and hidden `Pending<T>` require new runtime/descriptor contracts | High | B, C |
| 7 | **Require explicit `-> S \| Error` when error returns are used** — do not infer error-union returns from implicit same-state | High | B |
| 8 | **Specify pending-entry cleanup policy** — timeout or dead-letter for inflight correlation entries when response never arrives | High | C |
| 9 | **Keep `emit` in documentation as the canonical lower-level form** — sugar desugars to `emit`/`Pending`/`ReplyCap`; users who need to understand the machinery have a reference | Medium | A |
| 10 | **Libraries must never inject runtime** — only binary targets with opt-in annotation; library crates export descriptors only | Medium | A, C |
| 11 | **Single-thread cooperative dispatch in V1** — no background thread; use cooperative polling semantics inside the opt-in wrapper | Medium | B, C |
| 12 | **Decide internal vs self-transition semantics for `stay`** — UML distinguishes these; Machina should specify whether exit/entry actions run on same-state | Low | A |

---

## Open Questions Assessment

The design doc listed five open questions. Panel assessment:

1. **Implicit runtime: always-on or activated on first `spawn`?**
   Superseded by recommendation #1. With explicit opt-in (`#[machines]` or
   `machines::run()`), this question disappears. If opt-in is annotation-based,
   Panel A recommends static analysis: if `spawn()` is reachable from `main()`,
   the annotation is required. (Panels A, C)

2. **Default shutdown drain timeout?**
   Panel A proposes 5 seconds, configurable via annotation parameter
   (`#[machines(drain_timeout: 10s)]`). Panel C notes this must be documented
   prominently — "what happens when main exits?" is the first question every
   user will ask.

3. **Single-thread dispatch in V1?**
   Unanimous: yes. The transactional model in `machine_runtime.h` is
   single-threaded. Multi-thread can come later when semantics are proven.
   (All 3)

4. **Should `emit` stay in docs or be treated as internal?**
   Keep as reference documentation for advanced users and as the desugaring
   target specification. Don't promote in tutorials or examples. (Panel A)

5. **Implicit correlation with multiple concurrent same-type requests?**
   Forbid by default. Require labels for disambiguation. See recommendation #3.
   (All 3)

---

## Summary Verdict

The proposal splits cleanly into two tiers:

**Tier 1 (ship now):** Handler sugar (`on Ping(p)`, `send()`, `reply()`),
payload type shorthand, `Machine<T>` typed handles, `stay` keyword. These are
low-risk, high-impact improvements that work within the existing compiler
pipeline as pre-resolve desugaring. Panel B estimates medium implementation
effort.

**Tier 2 (ship after architecture pass):** Implicit correlation with `for`
keyword, implicit runtime injection. These require new runtime/descriptor
contracts, correlation metadata schemas, and careful specification of linearity
transfer. They should follow the blocking fixes from Review 2 (payload boxing,
state data rollback, staged request/reply in transactions).

The handler sugar alone transforms the 15:1 boilerplate ratio into something
genuinely pleasant. That's a win worth shipping independently. The
architectural abstractions need one more design pass to stay within the "magic
budget" appropriate for a systems language that promises explicitness.

**Overall assessment:** "needs phased implementation" — ship the syntax sugar
now, ship the architectural sugar after the runtime bridge gaps are resolved.
