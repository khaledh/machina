// Multi-actor hosted workflow:
// - checkout creates the payment
// - gateway authorizes it later
// - fraud service delivers an async alert
// - merchant resumes and observes the changed state

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
