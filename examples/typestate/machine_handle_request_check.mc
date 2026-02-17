requires {
    std::io::println
}

// Demonstrates machine-driven request/reply flow:
// - app sends a start message to Client
// - Client issues a request to AuthServer from inside a handler
// - request reaches server handler
// - server replies via capability-style `cap.reply(payload)`
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
    fields {
        auth: Machine<AuthServer>,
    }

    fn new(auth: Machine<AuthServer>) -> Ready {
        Ready { auth: auth }
    }

    state Ready {
        on StartAuth(cmd) -> stay {
            let p: Pending<AuthReply> = request(self.auth, AuthCheck { token: cmd.token });
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
            cap.reply(AuthReply { accepted: req.token + 1 });
        }
    }
}

@machines
fn main() -> () | MachineError {
    let auth = AuthServer::spawn()?;
    let client = Client::spawn(auth)?;
    client.send(StartAuth { token: 41 })?;

    // `@machines` auto-drives the managed runtime until it reaches idle.
}
