// Frontend-only async/event typestate fixture.
//
// This example is intentionally "check-only" today:
//   cargo run -- check --experimental typestate examples/typestate/inter_machine_req_reply_check.mc
//
// It demonstrates:
// - protocol roles + request/reply flow contracts
// - client-side Pending<...> correlation
// - pattern-form response handlers (`on Response(pending, Variant) -> ...`)
// - server-side ReplyCap<...> usage through `reply(...)`

type AuthorizeReq = {
    user: string,
}

type Response = {}
type AuthApproved = {}
type AuthDenied = {}

protocol Auth {
    role Client;
    role Server;
    flow Client -> Server: AuthorizeReq -> AuthApproved | AuthDenied;
}

typestate GatewayClient : Auth::Client {
    fields {
        auth_peer: u64,
    }

    fn new(auth_peer: u64) -> Idle {
        Idle { auth_peer: auth_peer }
    }

    state Idle {
        fn authorize(user: string) -> AwaitAuth {
            let pending: Pending<AuthApproved | AuthDenied> =
                emit Request(to: self.auth_peer, AuthorizeReq { user: user });
            AwaitAuth { pending: pending }
        }
    }

    state AwaitAuth {
        fields {
            pending: Pending<AuthApproved | AuthDenied>,
        }

        on Response(pending, AuthApproved) -> Connected {
            Connected {}
        }

        on Response(pending, AuthDenied) -> Idle {
            Idle {}
        }
    }

    state Connected {}
}

typestate AuthService : Auth::Server {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthorizeReq(req: AuthorizeReq, cap: ReplyCap<AuthApproved | AuthDenied>) -> Ready {
            if req.user.len > 0 {
                reply(cap, AuthApproved {});
            } else {
                reply(cap, AuthDenied {});
            };
            Ready {}
        }
    }
}
