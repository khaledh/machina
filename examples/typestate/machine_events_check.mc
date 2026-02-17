requires {
    std::io::println
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
            send(1, Tick {});
        }

        on Tick(t) {
            println("tick");
        }
    }
}

@machines
fn main() -> ()
    | MachineSpawnFailed
    | MachineBindFailed
    | MachineStartFailed
    | ManagedRuntimeUnavailable
    | MachineUnknown
    | MachineNotRunning
    | MailboxFull {
    let timer = Timer::spawn()?;
    timer.send(Kick {})?;
}
