// Final state example

type Start = {}

typestate OneShot {
    fn new() -> Idle {
        Idle {}
    }

    state Idle {
        on Start(_s) -> Done {
            println("processed start event");
            Done {}
        }
    }

    @final
    state Done {}
}

@machines
fn main() -> () | MachineError {
    let machine = OneShot::spawn()?;
    machine.send(Start {})?;
    println("queued Start; machine will stop after reaching final state");
}
