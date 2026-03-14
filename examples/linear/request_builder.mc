// Direct-mode builder ordering enforced by state.
// You cannot send before transitioning to `Ready`.

@linear
type RequestBuilder = {
    header_count: u64,
    status: u64,
    url: string,

    states {
        Build,
        Ready,
        Sent,
    }

    actions {
        with_header: Build -> Build,
        finish: Build -> Ready,
        send: Ready -> Sent,
    }
}

RequestBuilder :: {
    fn with_header(self) -> Build {
        Build { header_count: self.header_count + 1 }
    }

    fn finish(self) -> Ready {
        Ready { header_count: self.header_count }
    }

    fn send(self) -> Sent {
        Sent { status: 200 }
    }
}

fn main() {
    let req0 = RequestBuilder::Build {
        url: "https://example.com",
        header_count: 0,
        status: 0,
    };
    // req0.send(); // compile error: `send` is not available in `Build`
    println(f"[Build] url = {req0.url}");

    let req1 = req0.with_header();
    println(f"[Build] header_count = {req1.header_count}");

    let req2 = req1.with_header();
    println(f"[Build] header_count = {req2.header_count}");

    let ready = req2.finish();
    // ready.with_header(); // compile error: `with_header` is not available in `Ready`
    println(f"[Ready] header_count = {ready.header_count}");

    let sent = ready.send();
    println(f"[Sent] status = {sent.status}");
}
