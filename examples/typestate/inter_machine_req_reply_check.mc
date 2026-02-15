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
//   approved
//   denied

type KickApprove = {}
type KickDeny = {}
type ReqApprove = {}
type ReqDeny = {}
type Response = {}
type AuthApproved = {}
type AuthDenied = {}

typestate AuthClient {
    fn new() -> Idle {
        Idle {}
    }

    state Idle {
        on KickApprove(e) -> AwaitAuth {
            e;
            let pending: Pending<AuthApproved | AuthDenied> = request(1, ReqApprove {});
            AwaitAuth { pending: pending }
        }

        on KickDeny(e) -> AwaitAuth {
            e;
            let pending: Pending<AuthApproved | AuthDenied> = request(1, ReqDeny {});
            AwaitAuth { pending: pending }
        }
    }

    state AwaitAuth {
        fields {
            pending: Pending<AuthApproved | AuthDenied>,
        }

        on Response(pending, AuthApproved) -> Idle {
            println("approved");
            Idle {}
        }

        on Response(pending, AuthDenied) -> Idle {
            println("denied");
            Idle {}
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on ReqApprove(req: ReqApprove, cap: ReplyCap<AuthApproved | AuthDenied>) -> stay {
            req;
            reply(cap, AuthApproved {});
        }

        on ReqDeny(req: ReqDeny, cap: ReplyCap<AuthApproved | AuthDenied>) -> stay {
            req;
            reply(cap, AuthDenied {});
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

    // Kick approve flow: client -> server -> client(response).
    match send(rt, client_id, 1, 0, 0) {
        ok: () => { ok; }
        _ => { return; },
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => { return; },
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => { return; },
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => { return; },
    };

    // Kick deny flow: client -> server -> client(response).
    match send(rt, client_id, 2, 0, 0) {
        ok: () => { ok; }
        _ => { return; },
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => { return; },
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => { return; },
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => { return; },
    };
}
