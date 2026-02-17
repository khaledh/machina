requires {
    std::io::println
}

// Demonstrates machine-driven request/reply flow:
// - app sends a start message to Client
// - Client issues a request to AuthServer from inside a handler
// - request reaches server handler
// - server replies via `reply(cap, payload)`
// - response is delivered back to the requester machine and handled by
//   `on ResponseType(...) for RequestType(...)`.
//
// Run:
//   cargo mcr --experimental typestate examples/typestate/machine_handle_request_check.mc
//
// Expected output:
//   got 42

type StartAuth = { token: u64 }
type AuthCheck = { token: u64 }
type AuthReply = { accepted: u64 }

typestate Client {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on StartAuth(cmd) -> stay {
            let p: Pending<AuthReply> = request(1, AuthCheck { token: cmd.token });
            p;
        }

        on AuthReply(resp) for AuthCheck(origin) -> stay {
            origin;
            println(f"got {resp.accepted}");
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthCheck(req: AuthCheck, cap: ReplyCap<AuthReply>) -> stay {
            reply(cap, AuthReply { accepted: req.token + 1 });
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
    // Spawn server first so its machine id is 1 (used by client request `to:`).
    let _server = AuthServer::spawn()?;
    let client = Client::spawn()?;
    client.send(StartAuth { token: 41 })?;

    // `@machines` auto-drives the managed runtime until it reaches idle.
}
