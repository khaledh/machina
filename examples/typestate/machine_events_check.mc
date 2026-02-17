requires {
    std::io::println
}

// Runnable managed typestate send/event loop.
//
// Run:
//   cargo mcr --experimental typestate examples/typestate/machine_events_check.mc
//
// Expected output:
//   tick 1

type Kick = { n: u64 }

typestate Timer {
    fn new() -> Running {
        Running {}
    }

    state Running {
        on Kick(e) -> stay {
            println(f"tick {e.n}");
        }
    }
}

@machines
fn main() -> () | MachineError {
    let timer = Timer::spawn()?;
    timer.send(Kick { n: 1 })?;
}
