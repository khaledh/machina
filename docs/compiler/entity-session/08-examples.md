# Examples

## Direct Linear Type (Typestate)

A `@linear type` without roles or triggers is a plain typestate:

```mc
@linear
type Door = {
    states {
        Closed,
        Open,
    }

    actions {
        open: Closed -> Open,
        close: Open -> Closed,
    }
}

Door :: {
    fn open(self) -> Open { self }
    fn close(self) -> Closed { self }
}

let door = Door::Closed {};
let door = door.open();   // door: Door::Open  (direct call, always blocking)
let door = door.close();  // door: Door::Closed
```

The compiler enforces linear ownership and state correctness:

```mc
let door = Door::Closed {};
let door = door.open();
door.open();   // COMPILE ERROR: `open` requires Door::Closed, got Door::Open
```

```mc
let door = Door::Closed {};
// function returns without using `door`
// COMPILE ERROR: linear value `door` must be used — cannot silently drop
```

Matching on states works like matching on enum variants:

```mc
match door {
    closed: Closed => { closed.open(); }
    open: Open => { open.close(); }
}
```

---

## Hosted Linear Type + Machine + Session (V1)

This example uses the full V1 core: `@linear type` + machine + session.

### Event Types and Linear Type

```mc
// Event types — ordinary types used as triggers by PullRequest
type CIPassed = { commit_sha: string }
type CIFailed = { reason: string }

@linear
type PullRequest = {
    id: u64,
    author: UserId,
    reviewers: List<UserId>,

    states {
        Draft,
        PendingCI,
        Review,
        Approved,
        @final Merged,
    }

    actions {
        submit: Draft -> PendingCI,
        revise(note: string): Draft -> Draft,
        comment(text: string): Draft -> Draft,
        comment(text: string): Review -> Review,
        add_reviewer(reviewer: UserId): Draft -> Draft,
        approve: Review -> Approved,
        reject(reason: string): Review -> Draft,
        merge: Approved -> Merged,
    }

    triggers {
        CIPassed: PendingCI -> Review,
        CIFailed: PendingCI -> Draft,
    }

    roles {
        Author { submit, revise, comment, add_reviewer, merge }
        Reviewer { comment, approve, reject }
    }
}

PullRequest :: {
    fn submit(self) -> PendingCI { self }
    fn revise(self, note: string) -> Draft { self }
    fn comment(self: Draft, text: string) -> Draft { self }
    fn comment(self: Review, text: string) -> Review { self }
    fn add_reviewer(self, reviewer: UserId) -> Draft {
        self.reviewers.push(reviewer);
        self
    }
    fn approve(self) -> Approved { self }
    fn reject(self, reason: string) -> Draft { self }
    fn merge(self) -> Merged { self }
}
```

### Machine

```mc
machine PRService hosts PullRequest(key: id) {
    fields {
        ci_service: Machine<CIService>,
    }

    fn new(ci_service: Machine<CIService>) -> Self {
        Self { ci_service: ci_service }
    }

    // Action override — only for actions needing infrastructure
    action submit(draft) -> PendingCI {
        request(self.ci_service, RunCI { pr_id: draft.id });
        draft.submit()   // calls base implementation
    }

    // No overrides needed for revise, comment, add_reviewer, approve,
    // reject, merge — base implementations run directly.

    // Trigger handlers — names match externally declared event types
    trigger CIPassed(pending) { Review {} }
    trigger CIFailed(pending) { Draft {} }

    // Machine-level handler — bridges external message to trigger
    on CIResult(result) for RunCI(req) {
        if result.success {
            self.deliver(req.pr_id, CIPassed { commit_sha: result.sha });
        } else {
            self.deliver(req.pr_id, CIFailed { reason: result.reason });
        }
    }
}
```

### Client — Long-Lived Session

```mc
@machines
fn main() -> () | MachineError | SessionError {
    let service = PRService::spawn(ci_service)?;

    let author = service.create(PullRequest as Author)?;
    // author: PullRequest::Draft

    let author = author.add_reviewer(reviewer_id)?;
    let pending = author.submit()?;
    // pending: PullRequest::PendingCI — no Author actions available

    let next = pending.wait()?;
    match next {
        review: Review => {
            // CI passed, PR is now in review
        }
        draft: Draft => {
            // CI failed, back to draft
        }
    }
}
```

### Client — Stateless Request Handler

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

### Client — Multi-Action Request

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

### Compiler Errors

The compiler catches invalid session usage at compile time:

```mc
let author = service.create(PullRequest as Author)?;
author.approve();
// error[MC-SESSION-ACTION-NOT-ALLOWED]: `approve` is not available on
//   PullRequest::Draft (session role: Author)
//   note: `approve` requires role Reviewer
```

```mc
let pending = author.submit()?;
author.revise("oops");
// error[MC-SESSION-USE-AFTER-CONSUME]: `author` was consumed by `submit`
//   — linear value used after move
```

```mc
let pending = author.submit()?;
pending.submit();
// error[MC-SESSION-ACTION-INVALID-STATE]: `submit` requires PullRequest::Draft,
//   got PullRequest::PendingCI
```

---

## Extension Example: Channels (V2)

This example extends the core with channels, `emit`, events, and reactive
subscriptions. These are V2 features — see [04-channel.md](04-channel.md).

### Event Types

```mc
type CommentAdded = { author: UserId, text: string }
type ReviewerAssigned = { reviewer: UserId }
```

Event types are ordinary types. They can be emitted by handlers and routed
to channels. If another linear type declares them in its `triggers` block,
they can also drive state transitions.

### Channel

```mc
channel PREvents(PREvent);
```

### Machine with Channel and Emit

```mc
machine PRService hosts PullRequest(key: id), channel PREvents(PREvent) {
    // ... same fields and constructor as core example ...

    // Override comment to add emit (infrastructure concern)
    action comment(pr, text: string) {
        emit CommentAdded { author: session.user, text: text };
        pr.comment(text)   // calls base implementation
    }

    // Override add_reviewer to add emit
    action add_reviewer(draft, reviewer: UserId) {
        emit ReviewerAssigned { reviewer: reviewer };
        draft.add_reviewer(reviewer)   // calls base implementation
    }

    // ... remaining handlers unchanged ...
}
```

### Client — Channel Reader with Pipeline

```mc
fn stream_pr_events(service: Machine<PRService>) {
    let events = service.open(PREvents)?;

    events
    |> filter(fn(e) => e is CommentAdded or e is PullRequest::Merged)
    |> for_each(fn(e) => {
        match e {
            comment: CommentAdded => {
                println("{}: {}", comment.author, comment.text);
            }
            merged: PullRequest::Merged => {
                println("PR {} merged", merged.key);
            }
        }
    });
}
```

### Machine — Reactive Subscriber

```mc
machine DeployService subscribes PRService::PREvents {
    on PullRequest::Merged(evt) {
        self.trigger_deploy(evt.key);
    }
}
```
