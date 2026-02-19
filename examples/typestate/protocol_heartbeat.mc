requires {
    std::io::println
}

// Protocol-driven one-way messaging.
// Monitor forwards Start to Worker.

type Start = {}

protocol Heartbeat {
    msg Start;

    role Monitor {
        state Idle {
            on Start -> Idle {
                effects: [ Start ~> Worker ]
            }
        }
    }

    role Worker {
        state Ready {
            on Start@Monitor -> Ready;
        }
    }
}

typestate WorkerMachine {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on Start(_s) {
            println("[Worker] recv Start");
        }
    }
}

typestate MonitorMachine : Heartbeat::Monitor {
    fields {
        worker: Machine<WorkerMachine> as Worker,
    }

    fn new(worker: Machine<WorkerMachine>) -> Idle {
        Idle { worker: worker }
    }

    state Idle {
        on Start(_s) {
            println("[Monitor] recv Start");
            println("[Monitor] send Start -> Worker");
            self.worker.send(Start {});
        }
    }
}

@machines
fn main() -> () | MachineError {
    let worker = WorkerMachine::spawn()?;
    let monitor = MonitorMachine::spawn(worker)?;
    monitor.send(Start {})?;
}
