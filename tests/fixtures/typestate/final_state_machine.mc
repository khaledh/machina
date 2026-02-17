requires {
    std::io::println
    std::machine::Runtime
    std::machine::StepStatus
    std::machine::managed_runtime
    std::machine::step
}

type Start = {}

typestate OneShot {
    fn new() -> Idle {
        Idle {}
    }

    state Idle {
        on Start(s) -> Done {
            s;
            println("processed start event");
            Done {}
        }
    }

    @final
    state Done {}
}

@machines
fn main() {
    let machine: Machine<OneShot> = match OneShot::spawn() {
        m: Machine<OneShot> => m,
        _ => {
            println("spawn failed");
            return;
        }
    };

    match machine.send(1, 0, 0) {
        _ok: () => {}
        _ => {
            println("first send failed");
            return;
        }
    };

    let rt: Runtime = match managed_runtime() {
        runtime: Runtime => runtime,
        _ => {
            println("managed runtime unavailable");
            return;
        }
    };

    match step(rt) {
        StepStatus::DidWork => {}
        StepStatus::Idle => {
            println("unexpected idle step");
            return;
        }
        StepStatus::Faulted => {
            println("runtime fault");
            return;
        }
    };

    match machine.send(1, 0, 0) {
        _ok: () => {
            println("expected MachineError::NotRunning after final state");
        }
        err: MachineError => {
            match err {
                MachineError::NotRunning => {
                    println("machine stopped after final state");
                }
                _ => {
                    println("expected MachineError::NotRunning after final state");
                }
            };
        }
    };
}
