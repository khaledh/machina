# Why Machina

Most languages can model part of a workflow.

Some can encode states. Some can handle async events. Some can express
permissions. Some can host long-lived entities.

Machina brings those ideas together into one coherent feature.

With a single declaration, we can define:

- what states an entity can be in
- which actions are valid from each state
- which roles are allowed to perform them
- which external events can move the entity
- how a hosted instance behaves at runtime when the world changes underneath us

Here is a payment workflow:

```machina
type FraudAlert = {
    payment_id: u64,
}

@linear
type Payment = {
    id: u64,

    states {
        Created,
        Authorized,
        Captured,
        Declined,
        Refunded,
    }

    actions {
        authorize: Created -> Authorized,
        capture: Authorized -> Captured,
        refund: Captured -> Refunded,
    }

    triggers {
        FraudAlert: Authorized -> Declined,
    }

    roles {
        Merchant { authorize, capture }
        Compliance { refund }
    }
}

Payment :: {
    fn authorize(self) -> Authorized { Authorized {} }
    fn capture(self) -> Captured { Captured {} }
    fn refund(self) -> Refunded { Refunded {} }
}

machine PaymentService hosts Payment(key: id) {
    fn new() -> Self { Self {} }

    trigger FraudAlert(payment) {
        payment;  // consume the incoming state
        Declined {}
    }

    on FraudAlert(event) {
        let _result = self.deliver(event.payment_id, event);
    }
}
```

This says:

- a `Payment` starts in `Created`
- only `authorize` can move it to `Authorized`
- only an `Authorized` payment can be `capture`d
- only a `Captured` payment can be `refund`ed
- merchants can authorize and capture
- compliance can refund
- a fraud alert can asynchronously move an authorized payment to `Declined`

That is already useful. The real value shows up when we try to do the wrong
thing.

## 1. Invalid transitions fail before the program runs

Trying to capture a payment before it has been authorized is a bug.

In most systems, that becomes a runtime check, a test case, or a production
incident. In Machina, it does not typecheck.

```machina
@linear
type Payment = {
    states {
        Created,
        Authorized,
        Captured,
    }

    actions {
        authorize: Created -> Authorized,
        capture: Authorized -> Captured,
    }
}

Payment :: {
    fn authorize(self) -> Authorized {
        Authorized {}
    }

    fn capture(self) -> Captured {
        Captured {}
    }
}

fn main() {
    let payment = Payment::Created {};
    let _captured = payment.capture();
}
```

Exact diagnostic:

```text
(27:21) [typecheck:MC-TYPECHECK-OverloadNoMatch] Function overload not found: capture
```

The important part is not the wording. The important part is that the illegal
transition never becomes runnable code.

## 2. Role violations fail at compile time too

State is only part of the story.

Real systems also have permissions:

- merchants can authorize and capture
- compliance can refund
- not everyone can do everything

Machina treats that as part of the same model.

```machina
@linear
type Payment = {
    id: u64,

    states {
        Created,
        Authorized,
        Captured,
        Refunded,
    }

    actions {
        authorize: Created -> Authorized,
        capture: Authorized -> Captured,
        refund: Captured -> Refunded,
    }

    roles {
        Merchant { authorize, capture }
        Compliance { refund }
    }
}

Payment :: {
    fn authorize(self) -> Authorized { Authorized {} }
    fn capture(self) -> Captured { Captured {} }
    fn refund(self) -> Refunded { Refunded {} }
}

machine PaymentService hosts Payment(key: id) {
    fn new() -> Self { Self {} }
}

fn main() -> () | MachineError | SessionError {
    let service = PaymentService::spawn()?;
    let payment = service.create(Payment as Compliance)?;
    let _authorized = payment.authorize()?;
}
```

Exact diagnostic:

```text
(39:23) [typecheck:MC-TYPECHECK-LinearSessionActionNotAllowed] `authorize` is not available on Payment::Created (session role: Compliance)
```

That is not an afterthought bolted onto the API. It is built into the entity
itself.

## 3. Hosted entities are still easy to inspect

Long-lived entities often need read-only inspection:

- dashboards
- admin tools
- CLIs
- support workflows

We should not need to open a full session just to ask, "what state is this
payment in right now?"

Machina gives us `lookup(...)` for that.

```machina
@linear
type Payment = {
    id: u64,

    states {
        Created,
        Authorized,
    }

    actions {
        authorize: Created -> Authorized,
    }

    roles {
        Merchant { authorize }
    }
}

Payment :: {
    fn authorize(self) -> Authorized { Authorized {} }
}

machine PaymentService hosts Payment(key: id) {
    fn new() -> Self { Self {} }
}

fn main() -> () | MachineError | SessionError {
    let service = PaymentService::spawn()?;
    let created = service.create(Payment as Merchant)?;
    let current = service.lookup(Payment, created.id)?;
    match current {
        Payment::Created(_) => println("created"),
        Payment::Authorized(_) => println("authorized"),
    };
    ()
}
```

This is a small feature, but it matters. It makes hosted entities practical to
operate.

## 4. A payment outlives any single caller

This is where the model really starts to separate itself.

A real payment is not processed in one function call. It is created during
checkout, authorized by a gateway, possibly flagged by a fraud service,
captured by a merchant, and settled by a bank -- each at a different time, often
by a different service. The only thing connecting those stages is a payment key
stored in a database or passed through a message queue.

That means three hard problems appear:

- how do we persist the entity's state between stages?
- how do we route an external event to the right hosted entity?
- what happens when an actor resumes a payment whose state has already changed?

Machina handles all three. The key is `resume(...)`: the point where an actor
coming back from a database row, queue message, or webhook payload re-enters the
typed workflow.

Here is the full program. Each stage is a separate helper function that takes a
`Machine<PaymentService>` handle plus a payment key -- the shape we wanted from
the start for checkout, gateway, fraud, and merchant code paths.

```machina
type FraudAlert = {
    payment_id: u64,
}

@linear
type Payment = {
    id: u64,

    states {
        Created,
        Authorized,
        Captured,
        Declined,
        Refunded,
    }

    actions {
        authorize: Created -> Authorized,
        capture: Authorized -> Captured,
        refund: Captured -> Refunded,
    }

    triggers {
        FraudAlert: Authorized -> Declined,
    }

    roles {
        Merchant { authorize, capture }
        Compliance { refund }
    }
}

Payment :: {
    fn authorize(self) -> Authorized { Authorized {} }
    fn capture(self) -> Captured { Captured {} }
    fn refund(self) -> Refunded { Refunded {} }
}

machine PaymentService hosts Payment(key: id) {
    fn new() -> Self { Self {} }

    trigger FraudAlert(payment) {
        payment;  // consume the incoming state
        Declined {}
    }

    on FraudAlert(event) {
        let _result = self.deliver(event.payment_id, event);
    }
}

fn checkout(service: Machine<PaymentService>) -> u64 | MachineError | SessionError {
    let created = service.create(Payment as Merchant)?;
    println("checkout");
    created.id
}

fn gateway_authorize(
    service: Machine<PaymentService>,
    payment_id: u64,
) -> () | MachineError | SessionError {
    let payment = service.resume(Payment as Merchant, payment_id)?;
    match payment {
        Payment::Created(_) => {
            let _authorized = payment.authorize()?;
            println("gateway");
        }
        _ => println("gateway-unexpected"),
    };
    ()
}

fn fraud_service(
    service: Machine<PaymentService>,
    payment_id: u64,
) -> () | MachineError {
    service.send(FraudAlert { payment_id })?;
    // Process the mailbox so the on handler and trigger run before the next stage.
    __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
    println("fraud");
    ()
}

fn merchant_capture(
    service: Machine<PaymentService>,
    payment_id: u64,
) -> () | MachineError | SessionError {
    let payment = service.resume(Payment as Merchant, payment_id)?;
    match payment {
        Payment::Declined(_) => println("declined"),
        Payment::Authorized(_) => println("authorized"),
        _ => println("unexpected"),
    };
    ()
}

fn main() -> () | MachineError | SessionError {
    let service = PaymentService::spawn()?;
    let payment_id = checkout(service)?;
    gateway_authorize(service, payment_id)?;
    fraud_service(service, payment_id)?;
    merchant_capture(service, payment_id)?;
    ()
}
```

Output:

```text
checkout
gateway
fraud
declined
```

Each stage uses only the payment key plus a typed `Machine<PaymentService>`
handle. It resumes the payment, enters the typed world through `match`, and
either acts or discovers that reality has changed. The merchant path does not
crash or silently succeed -- the type system and runtime together ensure it sees
`Declined` and handles it.

In a real system, these stages happen at different times -- checkout during an
order, authorization from a gateway callback, fraud detection from a background
service -- and the runtime loop naturally processes queued events between them. In
this single-file demo, we force one runtime step so the fraud event is processed
before the merchant resumes the payment.

That is very hard to assemble cleanly in other systems.

## How this compares

Other languages can approximate slices of this:

- Rust can encode local typestate, but only for values in one scope
- actor systems can model async message-driven state, but without compile-time
  transition checks
- workflow engines can host long-running processes, but with runtime-only
  validation
- policy layers can add permissions, but as a separate mechanism

Those are separate tools with separate failure modes, stitched together by the
developer.

Machina's claim is not that no one else can do any of this. It is that you do
not usually get all of it from one declaration, one compiler model, and one
runtime model:

- valid transitions enforced at compile time
- invalid transitions rejected before the program runs
- role-aware sessions with compile-time permission checks
- hosted long-lived instances with runtime-backed state
- external events routed to the right entity
- trigger-driven transitions that run real handler code
- stale-session safety when reality changes underneath you

That combination is the point.

A payment workflow makes this concrete because the stakes are immediately
obvious. Capturing before authorization, letting the wrong actor perform a
sensitive transition, ignoring external fraud events, operating on stale state
after the world has changed -- these are not hypothetical. They are the bugs that
cause real production incidents.

Machina turns them from "things we should be careful about" into "things the
model itself prevents."
