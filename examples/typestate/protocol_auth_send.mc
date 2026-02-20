requires {
    std::io::println
}

// Protocol-driven send example with explicit role binding and state transition.

type Start = {}
type AuthReq = {}

protocol Auth {
    msg Start;
    msg AuthReq;

    role Client {
        state Idle {
            on Start -> Waiting {
                effects: [ AuthReq ~> Server ]
            }
        }

        state Waiting {}
    }

    role Server {
        state Ready {
            on AuthReq@Client -> Ready;
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        println("[AuthServer] new");
        Ready {}
    }

    state Ready {
        on AuthReq(_req: AuthReq) {
            println("[AuthServer] recv AuthReq");
        }
    }
}

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Idle {
        Idle { server }
    }

    state Idle {
        on Start() -> Waiting {
            println("[Gateway] send AuthReq");
            self.server.send(AuthReq {});
            Waiting { server: self.server }
        }
    }

    state Waiting {}
}

@machines
fn main() -> () | MachineError {
    let server = AuthServer::spawn()?;
    let gateway = Gateway::spawn(server)?;
    gateway.send(Start {})?;
}
