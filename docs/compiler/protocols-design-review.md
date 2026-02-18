# Machina Protocol Design: Expert Panel Review

**Document under review:** `docs/compiler/protocols-design.md`  
**Review date:** 2026-02-18  
**Panel models:** GPT-5.2 · Claude Opus 4.6 · Google Gemini 3 Pro

---

## Panel Overview

Three independent experts reviewed the protocol design from distinct angles:

| Reviewer | Model | Focus |
|---|---|---|
| **Dr. Alex Chen** | GPT-5.2 | Type theory, formal methods, session types, correctness |
| **Dr. Sarah Kim** | Claude Opus 4.6 | Language design, syntax ergonomics, developer experience |
| **Dr. Marcus Rivera** | Gemini 3 Pro | Distributed systems, practical protocol engineering |

---

## Review: Dr. Alex Chen (Type Theory & Formal Methods)

### Summary Assessment

The proposed "role/state-centric" protocol syntax is a plausible surface language for
*local* typestate machines, but it is not yet a sound session-typing story: key global
well-formedness/projection obligations are missing, so many globally-invalid protocols
will still look locally coherent. Tier A/B as described can improve error messages and
catch obvious mismatches, but it will not deliver the safety/progress guarantees people
associate with session types unless Tier C (or an equivalent global coherence check)
becomes a first-class, formally specified requirement rather than a "later" add-on.

### Strengths

- **Local readability:** `role { state { on ... -> ... { effects: [...] }}}` is easy to
  read as a role-local automaton and aligns with typestate ergonomics.
- **Directionality without symbolic noise:** Avoiding `?`/`!` and `recv`/`send` can be
  fine if semantics are unambiguous (trigger = receive, effect = send).
- **Explicit peer attribution on triggers** (`SynAck@Server`) helps disambiguate
  multiparty roles better than many ad-hoc DSLs.
- **Separation of concerns** is reasonable: compile-time enforcement dominates, runtime
  stays lean — good for a systems language *if* the compile-time model is actually strong.

### Critical Concerns

**1. No formal global type / coherence criterion (projection is hand-waved).**

What you currently describe is essentially *a set of communicating finite state machines*
(CFSMs) written directly as local machines. In multiparty session types (MPST), you
typically write a *global type* and project to locals, with a "well-formedness" theorem
ensuring the locals are compatible (duality/consistency/mergeability). Here, you are
writing locals directly, but you still need the same global consistency checks; otherwise
you accept classic CFSM pathologies:
- One role can "send" a message no other role is ready to receive.
- Two senders can race to the same receiver state with incompatible merges.
- Locals can be individually deterministic yet globally lead to orphan messages or
  deadlocks.

Tier B ("projection-lite") as stated (per-state allowed incoming/outgoing sets) is not
enough: it is basically an interface check, not a global safety/progress check.

**2. `effects: [ ... ]` is under-specified and likely unsound under concurrency.**

"Must-emit list" makes every transition an *atomic multi-send transaction* — which is not
standard session typing and changes semantics depending on buffering. Hard questions arise:
- Is the order semantically observable and guaranteed across asynchronous delivery? If yes,
  you are implicitly assuming FIFO per (sender, receiver) channel with no interleavings.
- If effects are "must happen," how do you express silent/internal transitions, conditional
  sends, or "may send"? Real protocols need internal computation steps and optional behavior.

**3. Choice, mergeability, and nondeterminism are not modeled (but are required).**

The syntax implicitly treats multiple `on ...` handlers as external choice on arrival
events, and `effects` as internal actions. This is insufficient to ensure:
- Determinism at receive states (two `on M@X` transitions from the same state — is that
  illegal? If not, semantics becomes nondeterministic).
- MPST "mergeability": when two control paths converge, projected local types must be
  mergeable; your syntax has no rule preventing unmergeable joins, a known source of
  unsoundness.

**4. `req` declarations are not integrated with the state machine semantics.**

The `req` declaration is global-ish while the role state machines are local-ish. The
syntax does not show:
- Where the pending reply obligation lives in the typestate.
- Whether the server is allowed to reply with different branches depending on internal
  state, and how the client's local state refines accordingly.
- Whether multiple outstanding requests (pipelines) are permitted.

As written, it is easy to write a protocol where `req` says one thing but the state
machines permit another.

**5. Role instance model is unclear — singletons vs. many participants.**

Session typing critically depends on whether roles are single endpoints per session or
potentially many processes playing a role concurrently. Your `c.request(s, ...)` example
suggests "one machine handle = one endpoint," but the protocol syntax names roles globally
and does not express multiplicity. Without this, "projection" and peer-compatibility are
underspecified: what does `Syn ~> Server` mean if there are multiple servers?

**6. `@final` is role-local termination; global termination/quiescence is not addressed.**

If one role is final but others still expect messages, you have either deadlock or orphan
messages. There are no explicit rules:
- Is it legal for a role to reach `@final` while peers are not final?
- Are messages in-flight permitted at final?
- What does the runtime do with late messages to a final role?

**7. The Tier A/B/C conformance model doesn't state its guarantees.**

Tiering is fine as an engineering plan, but without precision:
- Tiers A/B sound like *local interface* checks ("allowed messages exist") rather than
  *global protocol adherence*.
- "Sequencing/progression" is where the real session-typing properties live (progress,
  communication safety, deadlock freedom). If Tier C is optional/indefinite, users will
  assume guarantees you do not provide.

### Comparison to Formal Models

- **MPST (Honda/Yoshida/Carbone):** Typically starts from a global choreography, projects
  to local types, and requires well-formedness conditions. The Machina design is closer to
  *writing projected local types by hand* without the projection correctness constraints —
  exactly where unsound protocols slip in.
- **CFSMs / communicating automata:** The syntax is essentially CFSM notation with peer
  labels. CFSM theory has well-known undecidability results; practical systems impose
  restrictions (1-bounded, half-duplex, multiparty compatibility conditions). A known
  decidable compatibility criterion should be adopted rather than inventing "Tier C."
- **CSP:** Models synchronous events with algebraic composition. Machina appears
  asynchronous with buffering and correlation — not CSP by default. CSP-like reasoning
  requires explicit semantics and different primitives.
- **π-calculus:** Emphasizes name passing. Machina is closer to *linear endpoints*
  (`Machine<...>`, `ReplyCap`) than to dynamic channel creation. If the language eventually
  wants π-like dynamic topologies, the named-role model will be too rigid.

### Concrete Suggestions

1. **Define a formal core** — either "protocol = global interaction spec" or "protocol =
   set of locals + compatibility check." Write down the minimal semantics (messages
   buffered? FIFO per pair? atomic effects?). If keeping local syntax, add a mandatory
   multiparty compatibility check (well-studied in CFSM/MPST literature) rather than
   deferring to Tier C.

2. **Make choice explicit and check mergeability:**
   ```mc
   state S {
     choose { // internal choice by this role
       -> S1 { effects: [ M1 ~> R ] }
       -> S2 { effects: [ M2 ~> R ] }
     }
   }
   ```

3. **Clarify obligation vs. permission in effects:**
   ```mc
   effects must { Syn ~> Server }
   effects may  { Telemetry ~> System }
   ```

4. **Integrate `req` into state transitions as first-class:**
   ```mc
   state Idle {
     on Start -> Awaiting {
       request AuthReq ~> Server await (AuthOk@Server -> Ready | AuthErr@Server -> Idle)
     }
   }
   ```

5. **Define and enforce termination semantics:** Entering `@final` should forbid emitting
   and forbid having pending replies; disallow transitions in peers that would send to a
   role that can be final at that point.

6. **State Tier guarantees explicitly:**
   - Tier A: message-family and reply-linearity safety (no double-reply, no reply-without-cap).
   - Tier B: state-local permission safety (no handler/emit outside allowed sets).
   - Tier C: global communication safety + progress under stated runtime assumptions.

### Questions the Design Must Answer

- What is the precise message delivery semantics (buffered vs. synchronous, FIFO per peer
  pair, fairness, boundedness)?
- Are roles singleton per session instance, or can multiple machines play the same role
  concurrently?
- Are multiple `on M@R` transitions from the same state allowed (nondeterminism)? If not,
  is it a hard error?
- Can a role emit messages not listed in `effects`? Can it emit *more* than listed?
- How do you prevent orphan messages when two peers can both send to a receiver in
  different states?
- How is `Start@System` prevented from being spoofed or confused with user-defined `Start`
  messages across protocols?
- When a role reaches `@final`, what are the obligations on peers and on pending requests?

---

## Review: Dr. Sarah Kim (Language Design & Developer Experience)

### Summary Assessment

The protocol design shows genuine ambition — embedding session-type-adjacent protocol
contracts as first-class constructs is rare and valuable. The core conceptual model
(roles, states, transitions, effects) is sound. **However, the surface syntax has
significant ergonomic problems**: it introduces an entirely new mini-language inside
`protocol {}` blocks that partially duplicates the typestate syntax while diverging from
it in confusing ways, creating a dual-vocabulary problem. The `effects: [ Msg ~> Role ]`
notation, the `on Msg@Role` trigger form, and the `req` declaration each introduce novel
sigils and structural conventions that will raise the learning curve substantially and —
critically — create a readability gap between the *protocol specification* and the
*typestate implementation* that conformance is supposed to bridge.

### What Works Well

- **`protocol` as a top-level keyword** is the right call. It signals this is a
  first-class language concept, not a library pattern.
- **Named roles** (`role Client`, `role Server`) are intuitive. Every developer who has
  seen sequence diagrams or gRPC service definitions will understand this immediately.
- **`@final state`** attribute syntax is clean, consistent with the rest of the language's
  `@` attribute system, and reads naturally.
- **`msg` declarations at protocol scope** are a good idea in principle — they give the
  protocol a self-contained vocabulary and enable compiler reasoning about message families.
- **Separating protocol specification from typestate implementation** is architecturally
  correct. Protocols describe the contract; typestates implement it. This is the right
  layering.
- **The decision to reject `?`/`!` symbolic forms** is wise. Session type notation is
  beloved by PL researchers and despised by practitioners.

### What Needs Rethinking

**1. The protocol body is a parallel universe that doesn't compose with the rest of the
language.**

The protocol block introduces `state`, `on`, and `->` — all of which also exist in
typestate bodies — but with *different* semantics and *different* surrounding syntax. In a
typestate, `on Ping(p) -> Connected` is a handler with a body block. In a protocol,
`on SynAck@Server -> Established { effects: [...] }` is a declarative transition
specification. Same keywords, different grammars.

A developer reading a protocol spec and a typestate impl side-by-side will constantly trip
over the fact that `on` means "this handler runs code" in one place and "this transition
is declared legal" in the other.

**2. `effects: [ Msg ~> Role ]` introduces two novel concepts for one idea.**

A developer encountering this for the first time has to learn:
- That `effects:` is a special block label (not a field, not a keyword, not an attribute)
- That `~>` means "sends to" (as opposed to `->` for state transitions)
- That the square brackets denote an ordered list of must-emit messages

That is three new syntactic concepts to express "this transition sends these messages."

**3. The `@` operator in `on Msg@Role` is ambiguous with attributes.**

The language already uses `@` for attributes (`@final`, `@machines`). Now `@` also means
"from role" in protocol trigger context:

```mc
@final state Established;       // @final = attribute
on SynAck@Server -> Established // @Server = source role
```

These are visually similar, parse differently, and have completely different semantic
domains. This is especially bad because they can appear within the same `state` block.

**4. `req` declarations are disconnected from the role/state structure they constrain.**

```mc
req Client.AuthReq -> Server => AuthOk | AuthErr
```

This uses `.` for role-message association (not `::`, the established namespacing
operator), `->` for direction (overloaded from state transitions), and `=>` for reply
types (a new operator). A developer has to remember: `->` after `on` means "transitions to
state," `->` in `req` means "goes to role," and `=>` means "can reply with." Three
semantically distinct uses of arrow-family operators in the same block.

**5. The role-to-field binding gap is acknowledged but syntactically unresolved.**

The typestate declares `fields { auth: Machine<AuthService> }` and the protocol names role
`Server`, but there is no syntax to connect them. The comment "Binding intent: protocol
peer role `Server` is satisfied by `auth`" is exactly the kind of thing that should be in
the language, not in comments.

### Syntax Critique

Walking through the TcpHandshake example:

- `msg Start` through `msg Timeout` — these read like forward declarations but their
  semantics relative to the rest of the type system are unclear. Is `msg AuthReq` the same
  as `type AuthReq`? If not, can you use it as a type outside the protocol block?
- `on Start -> SynSent` — the sugar for `Start@System` hides the trigger source in a
  *protocol specification*, which is supposed to be the unambiguous contract.
- `effects: [ Syn ~> Server ]` — the `effects:` label with the colon reads like a struct
  field, but it is in a block context. The `~>` is visually close enough to `->` to cause
  constant mental pauses.
- `req Client.AuthReq -> Server => AuthOk | AuthErr` — packs too much into one line with
  too many novel operators: `.`, `->`, and `=>` all doing different things.

### Mental Model Analysis

Most developers think about protocols as **sequences of message exchanges between
parties** — the classic sequence diagram mental model. The proposed syntax models protocols
as **per-role state machines with side effects**, which is more precise but requires
holding two state machines in your head simultaneously and mentally interleaving them to
understand the conversation.

The deeper problem: the protocol spec and typestate impl use *different syntactic
vocabularies* for the *same conceptual entity* (a state machine). A protocol
`role Client { state Closed { on Start -> SynSent { ... } } }` looks similar to but is
fundamentally different from a typestate `state Closed { on Start(evt) -> SynSent { ... } }`.
Using the same keywords for both creates a false sense of familiarity that breaks when
rules from one context are applied to the other.

### Concrete Syntax Proposals

#### Proposal A: Declarative Flow Tables (minimize new syntax)

Keep protocols as flat contracts; let state machine structure live exclusively in typestates:

```mc
protocol TcpHandshake {
    msg Syn
    msg SynAck
    msg Ack
    msg Timeout

    role Client
    role Server

    flow Syn:    Client -> Server
    flow SynAck: Server -> Client
    flow Ack:    Client -> Server
    flow Timeout: System -> Client

    // Request/reply contracts:
    // request AuthReq: Client -> Server => AuthOk | AuthErr
}
```

**Advantages:** Dramatically simpler. No nested blocks. No novel operators. The protocol is
a flat table of "who can send what to whom." State-aware conformance is enforced by the
compiler against typestate implementations.

**Tradeoff:** Loses per-role state machine specification at the protocol level.

#### Proposal B: Arrow-Consistent Role Transitions (`from`/`sends` keywords)

Keep nested role/state structure but unify arrow syntax and eliminate `effects:` / `~>`:

```mc
protocol TcpHandshake {
    msg Syn, SynAck, Ack, Timeout

    role Client {
        state Closed {
            on Start from System => SynSent
                sends Syn to Server
        }
        state SynSent {
            on SynAck from Server => Established
                sends Ack to Server
            on Timeout from System => Closed
        }
        @final state Established
    }

    role Server {
        state Listen {
            on Syn from Client => AwaitAck
                sends SynAck to Client
        }
        state AwaitAck {
            on Ack from Client => Listen
        }
    }
}
```

**Key changes:** `from` instead of `@` (avoids attribute collision, reads as English);
`=>` for state transitions (distinguishes from `->` elsewhere); `sends ... to ...` for
effects (no `~>`, no `effects: [...]` block).

#### Proposal C: `transition` Keyword to Break the `on` Overload

```mc
protocol TcpHandshake {
    msg Syn, SynAck, Ack, Timeout

    role Client {
        state Closed {
            transition Start -> SynSent {
                send Syn -> Server
            }
        }
        state SynSent {
            transition SynAck <- Server -> Established {
                send Ack -> Server
            }
            transition Timeout <- System -> Closed
        }
        @final state Established
    }

    role Server {
        state Listen {
            transition Syn <- Client -> AwaitAck {
                send SynAck -> Client
            }
        }
        state AwaitAck {
            transition Ack <- Client -> Listen
        }
    }
}
```

**Key changes:** `transition` keyword reserves `on` exclusively for typestate impl
handlers; `<-` for message source (intuitive direction); `send` keyword (clear verb, no
novel operator).

#### Explicit Role-to-Field Binding (required regardless of syntax chosen)

```mc
typestate Gateway : Auth::Client {
    fields {
        auth: Machine<AuthService> as Server,  // explicit role binding
    }
    // ...
}
```

Or with a separate binding block:

```mc
typestate Gateway : Auth::Client {
    roles {
        Server = self.auth,
    }
    fields {
        auth: Machine<AuthService>,
    }
}
```

### Overall Recommendations

1. **Eliminate the `on` keyword overload.** Use `transition` or another distinct keyword
   for protocol-level declarations. The `on` keyword should be reserved exclusively for
   typestate handler implementations.

2. **Replace `@Role` with `from Role`.** The `@` sigil collision with attributes is a
   concrete readability hazard.

3. **Replace `effects: [ Msg ~> Role ]` with `sends Msg to Role`.** Self-documenting,
   requires zero learning, eliminates two novel sigils.

4. **Unify arrow operators.** Currently `->` serves state transitions, role-to-role flow
   direction, and `~>` serves effect sends, `=>` serves reply types. Pick one arrow per
   concept.

5. **Add explicit role-to-field binding syntax.** Don't ship Tier B without this. The
   implicit binding makes conformance error messages unactionable.

6. **Update `grammar.bnf` concurrently with the design.** Writing the grammar will force
   resolution of ambiguities that prose descriptions gloss over.

7. **Reconsider whether protocol-level state machines are worth the complexity.** Flat flow
   tables (Proposal A) may be sufficient for Tier B conformance if the compiler infers
   state-level constraints from typestate implementations. The ROI of embedding state
   machines inside protocols — when the typestate *already is* the state machine — deserves
   careful scrutiny.

---

## Review: Dr. Marcus Rivera (Distributed Systems & Protocol Engineering)

### Summary Assessment

Machina's proposal for first-class protocols is a breath of fresh air in a landscape
dominated by ad-hoc implementations and loose coupling. The intent to enforce state-machine
transitions at the type level is ambitious and correct. However, the current design feels
too "happy path" focused — it models the textbook definition of a protocol but glosses over
the messy reality of distributed systems: timeouts, partial failures, version negotiation,
and the asymmetry of real-world role bindings.

### What the Design Gets Right

- **Explicit roles:** Defining `Client` and `Server` explicitly within the protocol block
  creates a shared vocabulary that both sides must respect.
- **The `effects` block:** Forcing a state transition to declare its side effects
  (`effects: [ Syn ~> Server ]`) is a brilliant move for static analysis. It moves away
  from "state updates" to "state transitions with consequences," which is how reliable
  systems are actually built.
- **Message-driven transitions:** `on Msg@Role -> Next` is the correct primitive. In a
  distributed system, you don't call methods; you react to mail.

### Gaps from a Practitioner's View

- **The "Silent Failure" Problem:** Real protocols are defined by how they fail, not just
  how they succeed. The `Timeout` message in the TCP example is a patch, not a structural
  solution. Where are the supervisors? What happens if `Syn` is sent but `SynAck` never
  arrives? Error handling is an afterthought in this design.

- **State Payload Invisibility:** In `TcpHandshake`, `SynSent` likely needs to hold the
  sequence number and a timer reference. The current design treats states as opaque labels
  (`state SynSent`). If data cannot be bound to a state transition, the programmer will
  resort to mutable fields outside the FSM, breaking the safety guarantees.

- **Binding Ambiguity:** The document notes "Role-to-field binding is implicit." If `Auth::Client`
  is implemented, how does the compiler know that the `net_conn` field satisfies the
  `Server` role? Explicit wiring is painful but necessary.

### Comparison to Existing Approaches

- **vs. Erlang/OTP `gen_statem`:** Erlang wins on fault tolerance and dynamic supervision
  but loses on compile-time safety. Machina is trying to bring `gen_statem` semantics to a
  static type system. The `effects` list is stricter than Erlang's return tuples — good for
  correctness, but potentially rigid during prototyping.

- **vs. Rust typestates:** Rust patterns rely on moving `self` (consuming the previous
  state struct). Machina bakes this into language syntax. Cleaner than Rust's generic
  gymnastics (`Client<Connected>`), but needs to answer how data migrates between states.

- **vs. Session Types (Haskell/multiparty):** This looks like a pragmatic subset of
  session types. It avoids the academic density of full π-calculus notation but keeps the
  dual-compatibility check. However, it lacks "branching" support found in session types
  — explicitly offering a choice between two paths.

### Creative Alternatives

**Guarded Transitions:** Real protocols have conditions. Without guards, state explosion
is unmanageable:
```mc
on AuthReq@Client -> Ready if (req.version >= 2)
on AuthReq@Client -> Error if (req.version < 2)
```

**Protocol Composition:** Complex protocols are layers. `TLS` runs inside `TCP`.
`HTTP/2` runs inside `TLS`. The current design treats protocols as monolithic islands:
```mc
protocol SecureTransfer implements TcpHandshake + TlsHandshake { ... }
```

**Explicit Failure Channels:** Instead of just `on Msg`, allow `on Error` or
`on Timeout(Duration)` as first-class primitives that don't need to be defined as user
messages every time.

### Scaling Concerns

- **The "Mega-State" Problem:** In OAuth2 or Paxos, states share 90% of their behavior
  (e.g., all states handle a "Shutdown" signal). In the current syntax, you would have to
  copy the `on Shutdown -> Closed` handler into every state block. Hierarchical states
  (HSM) or state traits are needed.

- **Concurrency and cardinality:** How does this handle pipelining? HTTP/2 has a single
  connection state but thousands of multiplexed streams. Does one machine instance handle
  the connection, spawning sub-machines for streams? The "Role" concept needs to handle
  cardinality (1:1 vs 1:N).

### The Big Ideas Worth Stealing

1. **Hierarchical State Machines (from UML/Harel Statecharts):** Allow states to be nested.
   A `Connected` super-state handles common messages (ping/pong, errors), while sub-states
   (`Idle`, `Active`) handle specific logic. This directly addresses the mega-state problem.

2. **Correction-Oriented Design:** Allow a transition to "refuse" a state change but keep
   the current state, potentially emitting a correction message, without crashing the
   machine.

3. **Visual Isomorphism:** Ensure the syntax 1:1 maps to a state diagram (e.g., graphviz).
   If the code structure diverges from the mental model of the graph, the abstraction has
   failed.

### Final Verdict

**Keep:** The `protocol` block, explicit roles, and the `effects` requirement. These are
the bedrock of safety.

**Add:**
1. State data: `state SynSent { seq_num: u32 }`
2. Guards: `on Msg if expr -> Next`
3. Default/wildcard handlers: to avoid repetition for common messages (Log, Ping, Shutdown)

**Cut:** The implicit binding of roles to fields. Be explicit: something like
`implement Client relying on server_field`.

---

## Cross-Panel Synthesis

### Where All Three Agree

These themes appeared independently across all three reviewers — they are the most
important issues to address:

#### 1. Role-to-Field Binding Must Be Explicit

Every reviewer flagged this. Dr. Kim: *"The comment about binding intent is exactly the
kind of thing that should be in the language, not in comments."* Dr. Rivera: *"Explicit
wiring is painful but necessary."* Dr. Chen: *"Diagnostics can identify role mismatch, but
cannot point to the concrete field/handle that should satisfy that role."*

**Consensus:** Add explicit binding syntax — e.g., `as Server` on a field declaration, or
a dedicated `roles { }` block in typestate — before implementing Tier B. Without it,
conformance errors will be unactionable.

#### 2. The `effects: [ Msg ~> Role ]` Block Needs Redesign

All three reviewers questioned the `effects:` label and `~>` operator. Dr. Kim wants
English (`sends ... to ...`). Dr. Chen wants semantic precision (obligation vs. permission,
ordering guarantees). Dr. Rivera wants it expanded (guards, conditions).

**Consensus:** The current form is under-specified and introduces too much novel syntax.
Decide whether effects are *obligations* or *permissions*, state the ordering/atomicity
model explicitly, and reconsider the operator choice (`~>` is easy to confuse with `->`).

#### 3. Error, Failure, and Timeout Handling Is Missing as a First-Class Concept

Dr. Rivera raised the "Silent Failure Problem" most directly, but Dr. Chen's concern about
"internal transitions and optional behavior" and Dr. Kim's noting of missing "conditional
logic" all point to the same gap. Timeout is modeled as a user-defined `msg Timeout`
today, which is pragmatic but not structural.

**Consensus:** At minimum, document the intended pattern for failures and timeouts. At
best, introduce a first-class construct (e.g., `on timeout(Duration) -> State`) that
avoids the boilerplate of manually declaring timeout messages in every protocol.

#### 4. The Conformance Tier System Needs Explicit Guarantees Stated

Dr. Chen flagged this most rigorously (Tier C cannot be "later" if you want safety
claims). Dr. Kim flagged it from the user perspective (developers will assume more than is
guaranteed). Dr. Rivera implicitly raised it via scaling concerns.

**Consensus:** Publish clear, conservative statements of what each tier guarantees and does
*not* guarantee. Do not use "session types" language in documentation unless Tier C is
implemented and the global coherence check is proven sound.

### Where Reviewers Diverged

| Issue | Dr. Chen | Dr. Kim | Dr. Rivera |
|---|---|---|---|
| Nested role/state in protocol | Needs formal grounding | Consider flat tables instead | Keep, add hierarchy |
| `on` keyword overload | Not highlighted | Critical problem — use `transition` | Not highlighted |
| `@` operator for source role | Not highlighted | Collides with attributes — use `from` | Not highlighted |
| Protocol composition | Not discussed | Not discussed | High priority — protocols are layers |
| Guarded transitions | Implicit (as internal choice) | Not discussed | Explicit request |
| Formal semantics | Required before Tier C | Secondary concern | Not a concern |

### Prioritized Recommendations

Based on agreement strength and implementation risk:

#### Immediate (pre-Tier B)

1. **Add explicit role-to-field binding syntax.** All three reviewers agree this is broken.
   The simplest viable form: `field_name: Machine<T> as RoleName` or a `roles { }` block.

2. **Clarify `effects:` semantics.** State explicitly: are effects obligations or
   permissions? Is ordering guaranteed? If ordered and obligatory, document the required
   runtime delivery guarantee. If not, use a set-form syntax and drop the order implication.

3. **Resolve the `on` keyword overload.** Introduce `transition` (or another term) in
   protocol bodies to distinguish declarative transitions from imperative handlers. The
   current sharing of `on` between protocol spec and typestate impl is the single largest
   source of conceptual friction.

4. **Replace `@Role` source annotation with `from Role`.** Eliminates the attribute syntax
   collision. Low implementation cost, high readability gain.

5. **Update `grammar.bnf`** to reflect the new protocol surface. Prose design documents
   accumulate ambiguities that formal grammar specification reveals.

#### Near-term (during Tier B / alongside)

6. **Introduce structured timeout/error handling.** Even a simple `on timeout(dur) -> State`
   sugar removes significant boilerplate and makes failure paths a first-class concept.

7. **Publish explicit Tier guarantees.** Before releasing any tooling that surfaces protocol
   diagnostics to users, document exactly what Tier A and Tier B guarantee and what they do
   not. Prevents users from assuming deadlock-freedom from interface checks.

8. **Resolve `req` operator ambiguity.** The mix of `.`, `->`, and `=>` in a single
   declaration is the densest notation in the design. Candidates: align with the `flow`
   vocabulary from Proposal A, or integrate request/reply directly into state transitions
   (Dr. Chen's suggestion).

#### Future (Tier C design spike)

9. **Define a compatibility check.** If local role automata are written by hand (rather
   than projected from a global type), add a mandatory "multiparty compatibility" check
   to catch orphan messages, races, and unmergeable branches. This is the core of what
   makes Tier C non-trivial.

10. **Consider protocol composition.** `HTTP/2 over TLS` is a real pattern. Layered
    protocol syntax (whether via `implements`, `extends`, or subprotocol references) will
    be needed before the design can handle real-world protocol stacks.

11. **Consider hierarchical states.** The "mega-state" problem (shared transitions across
    many states) is a known pain point in FSM modeling at scale. Harel statecharts have
    decades of prior work. A `default` handler or nested state concept could address this
    without full HSM complexity.

---

## Closing Note

The core vision — first-class, statically-checked, role-aware protocol contracts for
typestate machines — is the right direction. The ideas that emerge from this combination
(explicit message vocabulary, per-role state projection, compiler-enforced effects) are
genuinely novel in systems language design. The design is at an early but critical juncture
where the key decisions about syntax and semantics will have long tails.

The three most important decisions to make right, before investing in deeper implementation,
are:

1. **The binding model** — how abstract protocol roles attach to concrete typestate fields.
2. **The `effects` model** — precisely what obligations the protocol imposes and what the
   compiler can enforce.
3. **The keyword vocabulary** — whether protocol blocks share syntax with typestate blocks
   (ergonomically convenient, semantically risky) or are given their own distinct vocabulary
   (more to learn, but less confusion in practice).

Get these three right and the rest of the implementation plan in the design document is
solid and well-sequenced.
