typestate Connection {
    fields {
        retries: u64,
    }

    fn new() -> Disconnected {
        Disconnected { retries: 0 }
    }

    state Disconnected {}
}

fn invalid_external_literal() -> u64 {
    let bad = Disconnected { retries: 1 };
    bad.retries
}
