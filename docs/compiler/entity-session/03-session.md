# Sessions

Sessions are the primary user-facing interaction surface. A session is a typed,
linear value that targets one instance through one role.

## Session Types

The role is specified once at session creation and is implicit in the session
type thereafter. The session type reflects the linear type and its current
state:

```
PullRequest::Draft      -- Author: can call submit, revise, comment, add_reviewer
                        -- Reviewer: (no actions on Draft)
PullRequest::PendingCI  -- (no actions for any role — waiting for triggers)
PullRequest::Review     -- Author: can call comment
                        -- Reviewer: can call comment, approve, reject
PullRequest::Approved   -- Author: can call merge
PullRequest::Merged     -- @final, no actions
```

The compiler tracks the role internally. Method availability is the role's
permission set intersected with actions whose source state matches the current
session state.

While the role is not part of the surface type syntax, it is visible in
compiler diagnostics and tooling:

```
error[MC-SESSION-ACTION-NOT-ALLOWED]: `approve` is not available on
  PullRequest::Draft (session role: Author)
  note: `approve` requires role Reviewer
```

```
error[MC-SESSION-ACTION-INVALID-STATE]: `submit` requires PullRequest::Draft,
  got PullRequest::PendingCI
```

```
hover: PullRequest::Draft (as Author)
  available: submit, revise, comment, add_reviewer
```

The role is always trackable. The compiler and tooling make it explicit whenever
there is ambiguity.

## Role Syntax

The `as` keyword specifies the role when creating or resuming a session. This
distinguishes role syntax from state syntax (which uses `::`):

- `PullRequest::Draft` — a state (enum variant of the linear type).
- `PullRequest as Author` — a view/projection (access mode over the type).

## Creating a Session

For a new instance whose initial state is known at compile time:

```mc
let service = PRService::spawn()?;
let author = service.create(PullRequest as Author)?;
// author: PullRequest::Draft  -- statically known, role is Author (implicit)
```

`create` allocates a new instance with a fresh identity key (in the initial
state — the first state declared in the `states` block), opens a session for
the given role, and returns a typed session handle. The return type is
statically known because a new instance always starts in the initial state.

The identity key is assigned by the machine and is available on the session
handle (e.g., `author.id`).

## Resuming a Session

For an existing instance whose current state is unknown at compile time:

```mc
let session = service.resume(PullRequest as Author, pr_id)?;
// session: PullRequest::Draft | PullRequest::PendingCI
//        | PullRequest::Review | PullRequest::Approved
//        | PullRequest::Merged

match session {
    draft: Draft => {
        let pending = draft.submit()?;
        // ...
    }
    _ => {
        // instance is not in the expected state
    }
}
```

`resume` loads a persisted instance by its identity key, opens a session, and
returns a union of all possible states. The caller must match to get a typed
handle — the same way you match on any enum.

This is the bridge from the untyped world (an HTTP request with a PR id, a
database row) into the typed world (a `Draft` session handle where only valid
actions are available). Every `resume` requires a match — in exchange, the
compiler guarantees that subsequent actions are valid for the actual state. The
runtime cannot reach an invalid state.

The role must be statically known at each `resume` call site. If the role is
determined dynamically (e.g., based on user authentication), branch in
application code first:

```mc
if user == pr_author {
    let session = service.resume(PullRequest as Author, pr_id)?;
    match session {
        draft: Draft => { draft.submit()?; }
        _ => { ... }
    }
} else if reviewers.contains(user) {
    let session = service.resume(PullRequest as Reviewer, pr_id)?;
    match session {
        review: Review => { review.approve()?; }
        _ => { ... }
    }
} else {
    HttpResponse { status: 403 }
}
```

Each branch has a statically known role.

## Session Lifecycle

1. **Created / Resumed**
   A client session value is created for one instance and role. The host
   machine loads or creates the instance state.

2. **Active**
   Each action method advances the client session type. The host machine
   validates and applies the corresponding workflow step. The machine persists
   the updated state on commit.

3. **Waiting**
   The session is on a state with no available actions for this role but where
   triggers may cause transitions (e.g., `PendingCI`). The client can call
   `wait()` to block until the next state change, or come back later with
   `resume`.

4. **Completed**
   The instance reaches a `@final` state. The session value is consumed. The
   machine may auto-remove the instance.

5. **Aborted**
   The client drops the session, the host faults/stops, a timeout occurs, or
   an invalid state/action combination is detected at runtime. The machine
   cleans up the session record.

## Blocking Semantics

There are two modes of using linear types, with different blocking behavior:

**Direct mode** — action calls are regular function calls. Always blocking,
always synchronous. There is no machine, no mailbox, no round-trip:

```mc
let door = Door::Closed {};
let door = door.open();   // immediate function call
let door = door.close();  // immediate function call
```

**Hosted mode (sessions)** — action calls are session methods that round-trip
to a host machine. Blocking by default:

```mc
let author = service.create(PullRequest as Author)?;
let author = author.revise("cleanup")?;     // blocks, round-trips to machine
let pending = author.submit()?;              // blocks, round-trips to machine
```

Each call sends an action request to the host machine, waits for the machine to
execute the handler, and returns the next typed session state.

When the caller does not want to block, the step can be marked explicitly:

```mc
let pending_op = nowait author.submit();
let pending = pending_op.wait()?;
```

`nowait` is only available in hosted mode (it makes no sense in direct mode
where calls are immediate). The important points:
- direct mode is always blocking (regular function calls),
- hosted mode is blocking by default,
- non-blocking (`nowait`) is explicit and hosted-mode only,
- the result remains typed in terms of the next session state.

## Waiting for Trigger-Driven Transitions

When a session is in a state with no available actions (because the role has
no actions on that state, or because the state only has trigger-driven outgoing
transitions), the client can wait for the next state change:

```mc
let pending = draft.submit()?;           // -> PullRequest::PendingCI
let next = pending.wait()?;              // blocks until a trigger transitions the instance

match next {
    review: Review => {
        // CI passed, continue workflow
    }
    draft: Draft => {
        // CI failed, back to draft
    }
}
```

`wait()` blocks until the instance transitions to a new state (via a trigger)
and returns the new state as a union. The client matches to continue.

### Concurrency: `wait()` and the mailbox

`wait()` does not create a second thread of execution. It registers the client
as interested in the next state change for that instance, then blocks. The
machine continues processing its mailbox one message at a time.

When a trigger arrives and transitions the instance, the machine:
1. Runs the trigger handler to completion.
2. Updates the instance state.
3. Notifies waiting clients with the new state tag.
4. Proceeds to the next mailbox message.

If other messages targeting the same instance were queued (e.g., a session
action from another client), they are processed after the trigger completes.
The runtime's state check applies as usual — if the action's expected state no
longer matches, the caller gets `SessionError::InvalidState`.

There is no race between `wait()` and `self.deliver()`. The mailbox serializes
all inputs to the machine, and each is processed atomically.

## Chaining Actions

Once a session handle is obtained (via `create` or via `match` after `resume`),
actions chain naturally:

```mc
let author = service.create(PullRequest as Author)?;
let author = author.revise("initial draft")?;         // PullRequest::Draft
let author = author.comment("ready for review")?;     // PullRequest::Draft
let pending = author.submit()?;                        // PullRequest::PendingCI
```

The compiler checks each step. `author.approve()` would be rejected —
`approve` is not in the Author role.

---

## Persistence Model

### The Problem

A stateless request handler (e.g., an HTTP endpoint) does not hold a long-lived
typed session handle. When a request arrives, the handler has:
- an instance id (from the URL or request body),
- an action to perform (from the route or payload),
- no compile-time knowledge of the instance's current state.

The instance must be loaded from storage by its identity key, its state
determined at runtime, and the action validated against that state.

### Sessions as the Persistence Boundary

Persistence is modeled as session create/resume:

- **`create`** starts a new session at the initial state. The machine allocates
  a new instance with a fresh identity key and persists it. Returns a
  statically typed session handle.

- **`resume`** loads a persisted instance by its identity key and opens a
  session. Returns a union of possible states. The caller matches to get a
  typed handle.

- **Action commit** — after each action, the machine persists the updated
  instance state (keyed by the identity key). The session handle advances to
  the next typed state.

- **Session end** — when the session value is dropped or the instance reaches a
  `@final` state, the machine completes any final persistence and cleans up.

The caller never touches storage directly. The machine handles load-on-resume,
save-on-commit, using the identity key throughout.

### Stateless Request Handler Example

```mc
fn handle_submit(service: Machine<PRService>, pr_id: u64) -> HttpResponse {
    let session = service.resume(PullRequest as Author, pr_id)?;

    match session {
        draft: Draft => {
            let _pending = draft.submit()?;
            HttpResponse { status: 200 }
        }
        _ => {
            HttpResponse { status: 409, message: "PR is not in Draft state" }
        }
    }
}
```

### Multi-Action Request Handler

Multiple actions chain naturally after the match:

```mc
fn handle_comment_and_submit(
    service: Machine<PRService>,
    pr_id: u64,
    text: string,
) -> HttpResponse {
    let session = service.resume(PullRequest as Author, pr_id)?;

    match session {
        draft: Draft => {
            let draft = draft.comment(text)?;
            let _pending = draft.submit()?;
            HttpResponse { status: 200 }
        }
        _ => {
            HttpResponse { status: 409, message: "PR is not in Draft state" }
        }
    }
}
```

After the match, the handler is in typed-session land. Each action blocks, goes
through the machine, and returns the next typed state. No second match needed
because the intermediate types flow through ordinary let-bindings.

### Field Lookup

To inspect fields without opening a session (e.g., to determine the appropriate
role), use `lookup`:

```mc
let pr = service.lookup(PullRequest, pr_id)?;

if pr.author == current_user {
    let session = service.resume(PullRequest as Author, pr_id)?;
    // ...
} else if pr.reviewers.contains(current_user) {
    let session = service.resume(PullRequest as Reviewer, pr_id)?;
    // ...
} else {
    HttpResponse { status: 403 }
}
```

`lookup` returns the instance's fields and current state tag without opening a
session.

**Important:** `lookup` is advisory — a preflight convenience for routing, UI,
and role determination. Between `lookup` and `resume`, the instance's state may
change. The authoritative check is always `resume` + the runtime state
validation on each action. Do not rely on `lookup` for safety-critical
invariants.

### Machine-Side Persistence

The machine owns the persistence strategy. Conceptual options:

- **In-memory only** — instance table lives in the machine's state word.
  Suitable for ephemeral instances or testing.
- **Database-backed** — the machine loads instance state from a database on
  `resume` (keyed by identity key), writes it back on action commit.
- **Event-sourced** — the machine persists action events keyed by identity key
  and reconstructs state on `resume`.

The persistence strategy is an implementation detail of the machine, not visible
to session callers.

The exact persistence integration API is open. The important architectural
point is that persistence is the machine's responsibility and is invisible to
session callers.

---

## Concurrency and Staleness

All operations targeting instances within a machine — session actions, delivered
triggers, and any other inputs — go through the machine's mailbox and are
processed one at a time. The state machine is never confused.

The staleness concern applies to sessions. A session handle is a cached
projection of the instance's state. If the instance transitions via a different
input source (a trigger, or another session), the projection may become stale.

### When Staleness Is Not a Concern

- **States with only actions, no triggers.** The instance can't change unless a
  session holder acts on it. No surprises.
- **States with only triggers, no actions** (e.g., `PendingCI`). The client can
  only `wait()` or `resume` later. Both discover the actual state.

### When Staleness Can Occur

- **States with both actions and triggers as outgoing transitions.** A client
  holds a typed handle and is about to call an action, but a trigger transitions
  the instance out from under them.

### Staleness Resolution

When a session action arrives, the runtime checks the instance's actual state
against the action's source state. Mismatch returns
`SessionError::InvalidState`. The client re-resumes and matches on the actual
state.
