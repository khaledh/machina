requires {
    std::io::println
}

// Canonical request/reply example:
// app -> Client(StartAuth) -> request(AuthServer) -> reply(AuthReply) -> Client handler.
type StartAuth = { token: u64 }
type AuthCheck = { token: u64 }
type AuthReply = { accepted: u64 }

typestate Client {
    fields {
        auth: Machine<AuthServer>,
    }

    fn new(auth: Machine<AuthServer>) -> Ready {
        println("[Client] new");
        Ready { auth }
    }

    state Ready {
        on StartAuth(cmd) {
            println("[Client] recv: StartAuth");
            println("[Client] send: AuthCheck ~> AuthServer");
            let _pending: Pending<AuthReply> = request(self.auth, AuthCheck { token: cmd.token });
        }

        on AuthReply(resp) for AuthCheck(_origin) {
            println(f"[Client] recv: AuthReply (accepted={resp.accepted})");
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        println("[AuthServer] new");
        Ready {}
    }

    state Ready {
        on AuthCheck(req: AuthCheck, cap: ReplyCap<AuthReply>) {
            println(f"[Server] recv: AuthCheck (token={req.token})");
            println(f"[Server] reply: AuthReply (accepted={req.token + 1})");
            cap.reply(AuthReply { accepted: req.token + 1 });
        }
    }
}

@machines
fn main() -> () | MachineError {
    let auth = AuthServer::spawn()?;
    let client = Client::spawn(auth)?;
    client.send(StartAuth { token: 41 })?;
}
