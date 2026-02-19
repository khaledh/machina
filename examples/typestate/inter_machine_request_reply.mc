requires {
    std::io::println
}

// Canonical concurrent request example:
// same request/response types, disambiguated by request-site labels.
type KickApprove = {}
type KickDeny = {}
type AuthCheck = {}
type AuthReply = {}

typestate AuthClient {
    fn new() -> Idle {
        println("[AuthClient] new");
        Idle {}
    }

    state Idle {
        on KickApprove(_e) {
            println("[AuthClient] recv: KickApprove");
            println("[AuthClient] send: AuthCheck:approve ~> AuthServer");
            let _p: Pending<AuthReply> = request:approve(1, AuthCheck {});
        }

        on KickDeny(_e) {
            println("[AuthClient] recv: KickDeny");
            println("[AuthClient] send: AuthCheck:deny ~> AuthServer");
            let _p: Pending<AuthReply> = request:deny(1, AuthCheck {});
        }

        on AuthReply(_resp) for AuthCheck:approve(_req) {
            println("[AuthClient] recv: AuthReply for AuthCheck:approve");
            println("[AuthClient] approve path (approved)");
        }

        on AuthReply(_resp) for AuthCheck:deny(_req) {
            println("[AuthClient] recv: AuthReply for AuthCheck:deny");
            println("[AuthClient] deny path (denied)");
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        println("[AuthServer] new");
        Ready {}
    }

    state Ready {
        on AuthCheck(_req: AuthCheck, cap: ReplyCap<AuthReply>) {
            println("[AuthServer] recv: AuthCheck");
            println("[AuthServer] reply: AuthReply");
            cap.reply(AuthReply {});
        }
    }
}

@machines
fn main() -> () | MachineError {
    // Spawn server first so its machine id is 1 (used by request(..., to: 1)).
    let _server = AuthServer::spawn()?;
    let client = AuthClient::spawn()?;

    client.send(KickApprove {})?;
    client.send(KickDeny {})?;
}
