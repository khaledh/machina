# Typestate Machines: Design Review

Review of the proposal in
[typestate-machines-async-events-design.md](typestate-machines-async-events-design.md).

## Status

Review completed February 2026.

Three independent expert panels evaluated the design from different
perspectives, each running on a different model architecture to maximize
diversity of analysis.

| Panel | Model | Perspective | Rating |
|-------|-------|-------------|--------|
| A | Claude Opus 4.6 | PL Theory, Type Systems, Session Types | 5.5/10 |
| B | GPT-5.3-Codex | Runtime Systems, Compiler Implementation | 6.5/10 |
| C | Gemini 3 Pro | Devil's Advocate, DX, Creative Alternatives | 5/10 |

Cross-model average: **5.7/10** — strong direction, significant gaps before
implementation.

---

## Consensus Strengths

These strengths were independently identified by all three panels.

### 1. Clean Semantic Core

The design principle — *"async is modeled as event delivery and effect
completion, not suspension"* — is the proposal's strongest idea. Synchronous
run-to-completion handlers with explicit waiting states (`AwaitAuth`) instead
of paused call stacks is a proven, debuggable concurrency model that maps
cleanly onto the existing typestate semantics.

### 2. First-Class Typestate Is a Genuine Differentiator

The existing `typestate` feature is already cleaner than Rust's phantom-type
encoding. Extending it to managed/actor mode is a natural and valuable
evolution. Very few languages offer this level of integration between state
machines and the type system.

### 3. Separation of Effects from State Transitions

By making `emit` produce effects that complete asynchronously (re-entering as
events), the design avoids the "async coloring" problem. The state machine is
always synchronous internally, with asynchrony at the boundary.

### 4. Protocols as Named Contracts

`protocol`/`role`/`flow` declarations document intent, enable compiler
guidance, and set up the path toward session-type-like verification. Even as a
message catalog (without sequencing), they add diagnostic value and serve as a
foundation for future strengthening.

### 5. Preserving Direct Mode

Keeping the existing `c.connect()` direct typestate as first-class and making
managed mode opt-in avoids forcing runtime overhead onto users who just want
compile-time lifecycle enforcement on local values.

---

## Consensus Concerns

These concerns were independently flagged by all three panels. Treat them as
high-confidence findings.

### 1. Correlation-by-Convention Is the Critical Flaw

**Severity: Critical.** Unanimously identified as the single most dangerous gap.

The `corr: u64` field is a regular integer with no type-level significance.
The compiler cannot catch:

- Forgetting to increment `next_corr` (same ID reused for different requests).
- Correlation ID collision across machines.
- Silently dropping mismatched correlations (the `else { AwaitAuth }` branch
  swallows the event with no diagnostic).
- Responding with wrong correlation (`reply(AuthApproved { corr: 999 })`
  compiles fine).
- Stale responses accepted after retry (dangling correlation bug — see
  Failure Scenarios below).

In session types, this entire problem class is eliminated by channel linearity:
each interaction happens on a unique, typed channel endpoint. The "correlation"
is the channel identity itself, enforced by the type system.

**Recommendation:** Make `reply(...)` consume a compiler-tracked reply
capability or typed request context. Correlation should be intrinsic (the
capability *is* the correlation), not a convention the user manually implements.

### 2. Failure Semantics Are Dangerously Underspecified

**Severity: Critical.** The proposal does not answer:

- What happens if a machine crashes mid-handler? Do emitted effects still
  commit?
- What happens if `emit Send(...)` targets a stopped/crashed machine?
- What happens when a mailbox fills up?
- Is handler execution transactional (rollback on error) or fire-and-forget?
- What happens to pending correlations when entering an error state?
- If `reply()` is not called on all control-flow paths, is that a type error?

Additionally, every state must potentially handle every possible asynchronous
error from effects emitted in previous states, creating an O(N * M) complexity
explosion where N is states and M is possible effect errors.

**Recommendation:** Define transactional handler semantics: effects commit only
on handler success; crash = rollback + fault state. Add hierarchical/default
event handlers at the typestate level to avoid error-handling explosion.

### 3. Dual Mode Is a Soundness Hazard

**Severity: High.** If direct calls are allowed on managed machines, users can
silently bypass mailbox ordering, protocol constraints, and crash semantics.
States change via two semantic paths, and typestate *seems* exhaustive but
runtime events arrive in states not modeled in the direct path.

Additional concern: in Direct Mode, `emit` returns effects as data. If the
caller ignores the return value, effects are silently dropped, leading to
deadlocks or lost messages.

**Recommendation:** Make managed registration a consuming (move-semantic)
operation. A managed machine must never be accessed directly. Managed machines
are mailbox-only from user code.

### 4. Protocol Conformance Is Weaker Than It Appears

**Severity: High.** The `protocol` block uses session-type vocabulary without
session-type guarantees. It is a message catalog (what types are exchanged
between which roles), not a behavioral specification (in what order, with what
obligations).

Three levels of conformance were identified:

| Level | What It Checks | Status |
|-------|---------------|--------|
| Shape | Handler exists for each message type | Achievable in V1 |
| Coverage | Per-state handler completeness for all reachable states | Tractable, not addressed |
| Refinement | Machine's FSM refines the projected role-local FSM | Requires projection algorithm |

The proposal appears to target Shape but claims the vocabulary of Refinement.

**Recommendation:** State explicitly which level V1 targets. Document that
Coverage and Refinement are future work. Consider adding minimal sequential
flow syntax to enable projection.

---

## Panel-Specific Findings

### Panel A: Type Theory (Claude Opus 4.6)

**Key findings not raised by other panels:**

1. **May-send vs. must-send discipline.** `reply()` checks what you *may* send
   (payload type is in the allowed response set) but not what you *must* send
   (failing to reply is invisible to the compiler). This is the difference
   between interface satisfaction and session fidelity.

2. **Local soundness vs. interaction soundness.** The compiler can verify that
   each machine individually conforms to its role's message types. It cannot
   verify that the composed system correctly implements the protocol, because:
   - Machine IDs are shared freely (no channel linearity).
   - No global composition check across machines.
   - FIFO per-mailbox but no global ordering — interleavings from multiple
     senders are unchecked.

3. **Untyped `machine_id` breaks role conformance.** Nothing prevents
   `emit Send(to: some_random_id, AuthorizeReq { ... })` where `some_random_id`
   is a logging service. Role conformance checks on the sender are meaningless
   if the target is unchecked.

4. **Sequential flow syntax.** Even minimal sequencing in `flow` declarations
   would enable projection — deriving expected FSMs per role:
   ```
   protocol Auth {
       role Client;
       role Server;
       flow {
           Client -> Server: AuthorizeReq;
           Server -> Client: AuthApproved | AuthDenied;
       }
   }
   ```

5. **Comparison to Sing#.** Microsoft Research's Singularity OS had channel
   contracts with state machines enforced at compile time, proving that
   first-class protocol state machines in a systems language are tractable.

6. **Single highest-impact change:** Typed machine handles
   (`Handle<Protocol, Role>`) + sequential flow syntax. Together these enable
   verifying both sides of an interaction.

### Panel B: Runtime and Compiler (GPT-5.3-Codex)

**Key findings not raised by other panels:**

1. **Concrete lowering strategy.** Lock this early:
   - Each managed typestate lowers to `StateEnum`, `EventEnum`,
     `dispatch(state, event, ctx) -> TransitionResult`, and `Outbox`.
   - Effects go through the Outbox; commit only on handler success.
   - The runtime stays behind a library boundary — compiler emits calls to
     `machina_rt` traits/functions, no backend special cases.

2. **Pipeline placement.**
   - Lexer/Parser: add `protocol`, `role`, `flow`, `on`, `emit`, `reply`
     grammar and AST nodes.
   - Resolve: bind `Auth::Client`, event symbols, payload types, flow
     references.
   - Typecheck: validate handler signatures, payload typing, state transition
     validity.
   - Semck: conformance and cross-entity rules (flow legality, missing
     handlers, illegal transitions, role mismatches).
   - Elaborate: lower to dispatch + transition + outbox shape.
   - Backend: mostly unchanged if lowering is explicit and runtime is
     library-based.

3. **Phase reordering.** The original 4-phase plan has hidden dependencies.
   Recommended reorder:
   - Phase 0 (new): Semantic freeze — decide correlation model, ABI, error
     policy, lifecycle states.
   - Phase 1: Parse/resolve/typecheck + local conformance rules.
   - Phase 2: Managed runtime core with bounded mailboxes, direct handle send,
     deterministic dispatch.
   - Phase 3: Replies/correlation + effect completion events + dead-letter/fault
     policy.
   - Phase 4: Subscriptions, policy hardening, IDE/tooling.

4. **Runtime contracts.**
   - Bounded mailboxes by default; send returns typed `SendError`.
   - Machine lifecycle states: `Running | Faulted | Stopped`.
   - Dead-letter hooks for observability.
   - Observability from day one: queue depth, drops, handler duration, fault
     counts.

5. **Memory layout.** Tagged union for state payloads in MVP; optional heap
   boxing for oversized variants can be deferred. Generated dispatch is a
   switch on `(state_tag, event_tag)`.

6. **Scope reality check.** Viable MVP: local process only, single-thread
   scheduler, bounded mailbox, typed direct handles, deterministic dispatch,
   explicit send errors. Defer: centralized subscription bus, rich envelope
   routing, advanced protocol conformance, restart trees, policy engines.

### Panel C: Devil's Advocate (Gemini 3 Pro)

**Key findings not raised by other panels:**

1. **The Killer Question.** *"How does a machine recover from an asynchronous
   failure of an effect emitted in a previous state?"* If `emit Write(fd, data)`
   is issued in `Connected` and the machine transitions to `AwaitAuth`, but the
   write fails, `AwaitAuth` likely has no handler for `WriteError`. The error is
   either dropped (silent data loss) or panics the machine (brittleness).

2. **Mental model friction quantified.** Converting a 4-line async sequence
   (Open -> Read -> Close -> Parse) into 4 separate states and 3 event handlers.
   The developer is forced to manually compile their linear logic into a state
   machine — the proposal asks developers to be the compiler.

3. **The "Zombie State" foot-gun.** If a machine transitions to `AwaitAuth` but
   no handler exists for peer disconnection (`PeerClosed`), the machine hangs
   forever. The compiler has no opinion on completeness of event coverage for
   non-protocol events.

4. **Flat state graph assumption.** Real protocols have orthogonal concerns
   (authenticating while buffering while keepalive-timer-running). Without
   hierarchical states or parallel regions, developers create Cartesian product
   states: `Connected_Buffering`, `Connected_Idle`, `AwaitAuth_Buffering`.

5. **Hierarchical event handlers.** Allow `on Event` at the typestate level (not
   just per-state) as a catch-all for system errors:
   ```
   typestate Connection {
       on WriteError(e) -> Closing { ... }   // applies to ALL states
       state Connected { ... }
       state AwaitAuth { ... }
   }
   ```

6. **"Protocol Channels" alternative.** Instead of `MachineId` + manual `corr`,
   make typed channel endpoints the first-class primitive:
   ```
   state Connected {
       fn authenticate(self, creds) -> AwaitAuth {
           let (next_chan, req) = self.chan.send(AuthReq { ... });
           emit Output(req);
           AwaitAuth { chan: next_chan }
       }
   }

   state AwaitAuth {
       fields { chan: Channel<Recv<AuthResp>> }
       on Receive(self.chan, resp: AuthResp) -> Connected {
           // No correlation check needed.
           // If we got it on this channel, it IS the response.
       }
   }
   ```
   The channel *is* the correlation — routing is by channel ID, not payload
   field matching.

---

## Concrete Failure Scenario: The Dangling Correlation Bug

A realistic scenario where the design leads to silent, dangerous behavior.

**Setup:** A developer implements retry logic for auth requests.

```
state AwaitAuth {
    fields { pending_corr: u64, retries: u64 }

    on Timeout(_) -> AwaitAuth {
        let new_corr = self.pending_corr + 1;
        emit Send(self.auth, AuthReq { corr: new_corr });
        AwaitAuth { pending_corr: new_corr, retries: self.retries + 1 }
    }

    on AuthApproved(ok: AuthApproved) -> Connected {
        // Developer forgets correlation check, or gets it wrong
        emit Write(self.fd, "Welcome");
        Connected { ... }
    }
}
```

**What happens at runtime:**

1. T=0ms: Machine sends `AuthReq(corr=100)`. Enters `AwaitAuth`.
2. T=500ms: Timeout fires. Machine sends `AuthReq(corr=101)`. Updates
   `pending_corr = 101`.
3. T=505ms: Delayed response `AuthApproved(corr=100)` arrives from the first
   attempt.
4. T=506ms: Handler runs. No correlation check (or wrong check). Machine
   accepts the stale response and transitions to `Connected`.
5. T=600ms: The actual response `AuthApproved(corr=101)` arrives. Machine is
   now in `Connected`, which has no handler for `AuthApproved`. Event is
   silently dropped.
6. **Result:** The system established a session using a stale response. If the
   payload contained a session key, the session may use a key the server has
   already discarded.

**No compiler error is produced.** Payload types match. Handlers exist. The bug
only manifests under load.

**Desired compiler behavior:** Either enforce correlation structurally (typed
tokens) or at minimum emit a warning: *"Event field `ok.corr` is unused in
handler; this pattern suggests a missing correlation check against
`self.pending_corr`."*

---

## Synthesized Recommendations

Ordered by priority. Confidence reflects cross-model agreement.

| # | Recommendation | Confidence | Source |
|---|---------------|------------|--------|
| 1 | Make correlation non-conventional — typed tokens, channel endpoints, or `ReplyCap<T>` consumed by `reply()` | Very High | All 3 |
| 2 | Specify transactional handler semantics — effects commit only on success; crash = rollback + fault state | Very High | All 3 |
| 3 | Managed mode = move-semantic, mailbox-only — no direct calls on managed machines | Very High | All 3 |
| 4 | Add Phase 0: semantic freeze — decide correlation model, ABI, error policy before Phase 1 code | High | B, A |
| 5 | Add hierarchical/default event handlers at the typestate level | High | C |
| 6 | Be explicit about guarantee scope — document what is NOT checked (deadlock, ordering, progress) | High | A, B |
| 7 | Add sequential flow syntax — even minimal ordering enables projection and coverage checking | High | A |
| 8 | Typed machine handles (`Handle<Protocol, Role>`) instead of raw `MachineId` | High | A, B |
| 9 | Bounded mailboxes by default with typed `SendError` returns | Medium | B |
| 10 | Reply obligation checking — `reply()` must be called on all paths, or handler marked `#[no_reply]` | Medium | A |

---

## Creative Alternatives Worth Exploring

Two panels independently converged on the same core idea from different angles.

### Typed Correlation Tokens (pragmatic, Panel A)

`emit Send()` returns a `CorrelationToken<ResponseTypes>` that is required by
matching handlers. The token carries the expected response types, enabling the
compiler to verify that handlers match.

```
let pending = emit Send(to: self.auth, AuthorizeReq { user: req.user });
// pending: CorrelationToken<AuthApproved | AuthDenied>
AwaitAuth { pending: pending }
```

### Protocol Channels (powerful, Panel C)

Typed channel endpoints where `send()` consumes the channel and returns a
next-state channel type. The channel *is* the correlation — routing is by
channel ID, not payload field matching. No manual correlation check is needed.

These are the same idea at different abstraction levels. The token approach is
the pragmatic middle ground for V1; the channel approach is more powerful but
harder to implement.

---

## Open Questions Resolved by This Review

The proposal listed four open questions. The panels reached consensus on three:

1. **Should correlation be enforced by syntax instead of convention?**
   Yes. All three panels agree. See Recommendation 1.

2. **Allow direct method calls on managed machines?**
   No. All three panels agree — managed machines must be mailbox-only.
   See Recommendation 3.

3. **Default error policy in debug vs release?**
   Not yet answerable — failure semantics must be defined first.
   See Recommendation 2.

4. **Typed machine handles in V1?**
   Yes. Two panels recommend this strongly. See Recommendation 8.
