requires {
    std::io::println
}

// Canonical typestate example: builder ordering enforced by state.
// You cannot send before transitioning to `Ready`.
typestate RequestBuilder {
    fields {
        url: string,
    }

    fn new(url: string) -> Build {
        Build { url, header_count: 0 }
    }

    state Build {
        fields {
            header_count: u64,
        }

        fn with_header() -> Build {
            Build { header_count: self.header_count + 1 }
        }

        fn finish() -> Ready {
          Ready { header_count: self.header_count }
        }
    }

    state Ready {
        fields {
            header_count: u64,
        }

        fn send() -> Sent {
            Sent { status: 200 }
        }
    }

    state Sent {
        fields {
            status: u64,
        }
    }
}

fn main() {
    let req0 = RequestBuilder::new("https://example.com");
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
