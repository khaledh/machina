// Direct-mode linear lifecycle with shared fields carried across transitions.

@linear
type Connection = {
    fd: u64,
    retries: u64,

    states {
        Disconnected,
        Connected,
    }

    actions {
        connect: Disconnected -> Connected,
        disconnect: Connected -> Disconnected,
    }
}

Connection :: {
    fn connect(self) -> Connected {
        Connected { fd: 7 }
    }

    fn disconnect(self) -> Disconnected {
        Disconnected { fd: 0 }
    }
}

fn main() {
    let c0 = Connection::Disconnected { fd: 0, retries: 0 };
    let c1 = c0.connect();
    println(f"fd = {c1.fd}");

    let c2 = c1.disconnect();
    println(f"retries = {c2.retries}");
}
