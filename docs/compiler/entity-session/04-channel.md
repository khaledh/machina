# Channels and Event Flow (V2)

> **This is a V2 extension.** Channels are not part of the V1 core
> (`@linear type` + machine + session). They are designed to complement the core
> model once it is stable. Nothing in V1 depends on channels.

## Channel Declaration

A channel is a lightweight typed message pipe. It handles unbounded streaming
and fan-out — cases where there is no meaningful state progression, just typed
flow.

```mc
channel PREvents(PREvent);
```

A channel declaration names the channel and its item type. Send and recv are
built-in operations — no role declarations needed. The machine that hosts the
channel controls who writes and who reads.

Channels differ from linear types:

| | Linear Type | Channel |
|---|---|---|
| State machine | Rich, multi-state | None |
| Identity + persistence | Yes | No (ephemeral) |
| Compiler value | Checks workflow correctness | Checks message types |
| Sweet spot | Workflows with meaningful states | Unbounded streaming, fan-out |

### When to Use Channels vs Linear Types

Use a **linear type** when the domain has meaningful states that gate different
operations — a pull request progressing through Draft, Review, Approved, Merged.

Use a **channel** when the interaction is unbounded repetition without state
progression — streaming log entries, publishing metrics, fan-out notifications.
If the "state machine" would be a single self-loop, a channel is the right
tool.

Channels are intentionally tiny. They are a typed pipe, not a second workflow
system. They should not grow roles, lifecycle hooks, or session-like semantics.

---

## Emitting Values from Handlers

Handlers emit typed values via `emit`. In V2, the linear type's `events { }`
block declares which types are emittable:

```mc
type CommentAdded = { author: UserId, text: string }

action comment(review, text: string) {
    review.comments.push(text);
    emit CommentAdded { author: session.user, text: text };
    review  // state doesn't change — no transition notification
}

action submit(draft) {
    request(self.ci_service, RunCI { pr_id: draft.id });
    PendingCI {}
    // runtime auto-emits PullRequest::PendingCI transition notification
}
```

## Routing to Channels

A machine that hosts both a linear type and a channel routes emitted values
(both explicit and transition notifications) to the channel after each handler
completes.

```mc
machine PRService hosts PullRequest(key: id), channel PREvents(PREvent) {
    // ... handlers that emit values ...
    // All emitted values + transition notifications route to PREvents
}
```

## Reading a Channel (Pull)

A channel reader is a lazy iterator. It yields items on demand, blocking until
the next item arrives. This integrates with Machina's dataflow pipe operator:

```mc
let events = pr_service.open(PREvents)?;

events
|> filter(fn(e) => e is CommentAdded)
|> map(fn(e) => format("{}: {}", e.author, e.text))
|> for_each(fn(msg) => println(msg));
```

The fact that items come from a hosted machine over a channel is invisible to
the pipeline. A channel reader implements the iterator interface — the entire
pipeline/dataflow machinery works unchanged.

## Subscribing to a Channel (Push — Machine-to-Machine)

A machine that wants to react to values from another machine's channel can
bridge pull-to-push via `self.listen()`:

```mc
machine NotificationService {
    on start() {
        let events = pr_service.open(PREvents)?;
        self.listen(events);  // runtime pulls and delivers to mailbox
    }

    on CommentAdded(evt) {
        self.notify(evt.author, "New comment: {}", evt.text);
    }

    on PullRequest::Merged(evt) {
        self.trigger_deploy(evt.key);
    }
}
```

`self.listen()` tells the runtime: "read from this channel and deliver items
to my mailbox as messages." The `on` handler fires for each item, fully
linearized with the machine's other mailbox processing. Backpressure is
natural — the machine processes one message at a time.

## Declarative Subscription (Sugar)

When the listen-at-spawn pattern is common, it can be sugared:

```mc
machine NotificationService subscribes PRService::PREvents {
    on CommentAdded(evt) {
        self.notify(evt.author, "New comment: {}", evt.text);
    }

    on PullRequest::Merged(evt) {
        self.trigger_deploy(evt.key);
    }
}
```

`subscribes` in the machine header means: "at spawn time, open a channel
reader and wire it into my mailbox." Under the hood it desugars to the
`self.listen()` pattern.

The explicit form is more flexible — channels can be opened lazily, filtered
before listening, or stopped:

```mc
on start() {
    let events = pr_service.open(PREvents)?;

    // Only care about merges to main branch
    events
    |> filter(fn(e) => e is PullRequest::Merged and e.branch == "main")
    |> self.listen();
}
```

## Pull vs Push Summary

| Who | Mechanism | Style |
|---|---|---|
| Same machine | `on` handlers + `self.deliver()` | Push, internal |
| External, pull | `channel.open() \|> pipeline` | Pull, iterator |
| External, reactive | `channel.open()` + `self.listen()` | Pull-to-push bridge |
| External, sugar | `subscribes` clause | Declarative push |

Everything flows through channels. The only question is whether the consumer
pulls directly (iterator) or asks the runtime to pull-and-deliver (listen).
