requires {
    std::io::println
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
            cap.reply(AuthReply {});
        }
    }
}

@machines
fn main() -> ()
    | MachineSpawnFailed
    | MachineBindFailed
    | MachineStartFailed
    | ManagedRuntimeUnavailable
    | MachineUnknown
    | MachineNotRunning
    | MailboxFull {
    // Spawn server first so its machine id is 1 (used by client Request `to:`).
    let _server = AuthServer::spawn()?;
    let client = AuthClient::spawn()?;
    // Queue two concurrent same-type requests.
    client.send(KickApprove {})?;
    client.send(KickDeny {})?;

    // `@machines` auto-drives the managed runtime until it reaches idle.
}
