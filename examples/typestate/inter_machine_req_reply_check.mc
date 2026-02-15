requires {
    std::io::println
    std::machine::managed_runtime
    std::machine::send
    std::machine::step
    std::machine::Runtime
    std::machine::StepStatus
}

// Runnable managed typestate request/reply flow.
//
// Run:
//   cargo mcr --experimental typestate examples/typestate/inter_machine_req_reply_check.mc
//
// Expected output includes:
//   approve path
//   approved
//   deny path
//   denied

type KickApprove = {}
type KickDeny = {}
type AuthCheck = {}
type AuthReply = {}

typestate AuthClient {
    fn new() -> Idle {
        Idle {}
    }

    state Idle {
        on KickApprove(e) -> stay {
            e;
            // Two concurrent same-type inflight requests are disambiguated by
            // request-site labels.
            let p: Pending<AuthReply> =
                request:approve(1, AuthCheck {});
            p;
        }

        on KickDeny(e) -> stay {
            e;
            let p: Pending<AuthReply> =
                request:deny(1, AuthCheck {});
            p;
        }

        on AuthReply(resp) for AuthCheck:approve(req) -> stay {
            req;
            resp;
            println("approve path");
            println("approved");
        }

        on AuthReply(resp) for AuthCheck:deny(req) -> stay {
            req;
            resp;
            println("deny path");
            println("denied");
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthCheck(req: AuthCheck, cap: ReplyCap<AuthReply>) -> stay {
            req;
            reply(cap, AuthReply {});
        }
    }
}

@[machines]
fn main() {
    // Spawn server first so its machine id is 1 (used by client Request `to:`).
    let server: Machine = match AuthServer::spawn() {
        m: Machine => m,
        _ => { return; },
    };
    let client: Machine = match AuthClient::spawn() {
        m: Machine => m,
        _ => { return; },
    };
    let server_id = server._id;
    let client_id = client._id;

    server_id;
    client_id;

    let rt: Runtime = match managed_runtime() {
        r: Runtime => r,
        _ => { return; },
    };

    // Queue two concurrent same-type requests.
    match send(rt, client_id, 1, 0, 0) {
        ok: () => { ok; }
        _ => { return; },
    };
    match send(rt, client_id, 2, 0, 0) {
        ok: () => { ok; }
        _ => { return; },
    };

    // Drain a bounded number of dispatch steps.
    match step(rt) { StepStatus::Faulted => { return; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return; } _ => {} };
}
