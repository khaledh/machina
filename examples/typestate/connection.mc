requires {
    std::io::println
}

// Canonical typestate example: method-driven state transitions with
// shared fields (`retries`) and state-local fields (`fd`).
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

fn main() {
    let c0 = Connection::new();
    let c1 = c0.connect();
    println(f"fd = {c1.fd}");

    let c2 = c1.disconnect();
    println(f"retries = {c2.retries}");

    // Uncomment to see a compile-time state-field error.
    // println(f"fd = {c2.fd}");
}
