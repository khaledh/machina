typestate RequestBuilder {
    fields {
        url: string,
    }

    fn new(url: string) -> Empty {
        Empty { url: url }
    }

    state Empty {
        fn with_header() -> Ready {
            Ready { header_count: 1 }
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

fn main() -> u64 {
    let r0 = RequestBuilder::new("https://machina.dev");
    let r1 = r0.with_header();
    let r2 = r1.send();
    r2.status
}
