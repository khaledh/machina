typestate Connection {
    fields {
        retries: u64,
    }

    fn new() -> Disconnected {
        Disconnected { retries: 0 }
    }

    state Disconnected {
        fn connect() -> Connected {
            Connected { fd: 7 }
        }
    }

    state Connected {
        fields {
            fd: u64,
        }

        fn disconnect() -> Disconnected {
            Disconnected
        }
    }
}

fn main() -> u64 {
    let c0 = Connection::new();
    let c1 = c0.connect();
    let c2 = c1.disconnect();
    c2.retries
}
