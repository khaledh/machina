// Frontend-only async/event typestate fixture.
//
// This example is intentionally "check-only" today:
//   cargo run -- check --experimental typestate examples/typestate/machine_events_check.mc
//
// It demonstrates event handlers and protocol-flow conformance for a single
// machine role.

type Tick = {}

protocol Loopback {
    role Worker;
    flow Worker -> Worker: Tick;
}

typestate Timer : Loopback::Worker {
    fields {
        self_id: u64,
    }

    fn new(self_id: u64) -> Running {
        Running { self_id: self_id }
    }

    state Running {
        fn schedule_tick() -> Running {
            emit Send(to: self.self_id, Tick {});
            Running {}
        }

        on Tick() -> Running {
            Running {}
        }
    }
}
