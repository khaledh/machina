requires {
    std::io::println
    std::machine::new_runtime
    std::machine::close_runtime
    std::machine::spawn
    std::machine::start
    std::machine::send
    std::machine::bind_descriptor
    std::machine::step
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
        on KickApprove(e: KickApprove) -> AwaitAuth {
            e;
            let pending: Pending<AuthApproved | AuthDenied> =
                emit Request(to: 1, ReqApprove {});
            AwaitAuth { pending: pending }
        }

        on KickDeny(e: KickDeny) -> AwaitAuth {
            e;
            let pending: Pending<AuthApproved | AuthDenied> =
                emit Request(to: 1, ReqDeny {});
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
        on ReqApprove(req: ReqApprove, cap: ReplyCap<AuthApproved | AuthDenied>) -> Ready {
            req;
            reply(cap, AuthApproved {});
            Ready {}
        }

        on ReqDeny(req: ReqDeny, cap: ReplyCap<AuthApproved | AuthDenied>) -> Ready {
            req;
            reply(cap, AuthDenied {});
            Ready {}
        }
    }
}

fn main() -> u64 {
    var rt = new_runtime();

    // Spawn server first so its machine id is 1 (used by client Request `to:`).
    var server_id = 0;
    match spawn(rt, 8) {
        id: u64 => {
            server_id = id;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    var client_id = 0;
    match spawn(rt, 8) {
        id: u64 => {
            client_id = id;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    // Descriptor ids are deterministic by typestate name sort:
    //   AuthClient -> 1, AuthServer -> 2.
    // State tags are deterministic by state name sort:
    //   AuthClient: AwaitAuth -> 1, Idle -> 2.
    //   AuthServer: Ready -> 1.
    match bind_descriptor(rt, server_id, 2, 1) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match bind_descriptor(rt, client_id, 1, 2) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    match start(rt, server_id) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match start(rt, client_id) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    // Kick approve flow: client -> server -> client(response).
    match send(rt, client_id, 1, 0, 0) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    // Kick deny flow: client -> server -> client(response).
    match send(rt, client_id, 2, 0, 0) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    close_runtime(inout rt);
    0
}
