requires {
    std::io::println
    std::machine::managed_runtime
    std::machine::step
    std::machine::Runtime
    std::machine::StepStatus
}

// Runnable managed typestate send/event loop.
//
// Run:
//   cargo mcr --experimental typestate examples/typestate/machine_events_check.mc
//
// Expected output:
//   tick

type Kick = {}
type Tick = {}

typestate Timer {
    fn new() -> Running {
        Running {}
    }

    state Running {
        on Kick(e) -> stay {
            e;
            send(1, Tick {});
        }

        on Tick(t) {
            t;
            println("tick");
        }
    }
}

@[machines]
fn main() {
    match Timer::spawn() {
        timer: Machine<Timer> => {
            match timer.send(1, 0, 0) {
                ok: () => { ok; }
                _ => { return; },
            };
        }
        _ => { return; },
    };

    let rt: Runtime = match managed_runtime() {
        r: Runtime => r,
        _ => { return; },
    };

    // Kick handler then Tick handler.
    match step(rt) {
        StepStatus::DidWork => {}
        _ => { return; },
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => { return; },
    };
}
