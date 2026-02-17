requires {
    std::io::println
}

// Runnable managed typestate example with explicit state transitions between
// handlers plus inter-machine request/reply interaction.
//
// Run:
//   cargo mcr --experimental typestate examples/typestate/managed_state_transitions.mc
//
// Expected output:
//   open event 1
//   door transitioned: Closed -> Waiting -> Open

type OpenPressed = { id: u64 }
type OpenCmd = {}
type Opened = {}

typestate DoorActuator {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on OpenCmd(cmd: OpenCmd, cap: ReplyCap<Opened>) -> stay {
            cmd;
            cap.reply(Opened {});
        }
    }
}

typestate DoorController {
    fields {
        door: Machine<DoorActuator>,
    }

    fn new(door: Machine<DoorActuator>) -> Closed {
        Closed { door: door }
    }

    state Closed {
        on OpenPressed(evt) -> Waiting {
            println(f"open event {evt.id}");
            let _pending: Pending<Opened> = request(self.door, OpenCmd {});
            Waiting
        }
    }

    state Waiting {
        on Opened(_evt) for OpenCmd(_origin) -> Open {
            println("door transitioned: Closed -> Waiting -> Open");
            Open
        }
    }

    @final
    state Open {}
}

@machines
fn main() -> () | MachineError {
    let actuator = DoorActuator::spawn()?;
    let controller = DoorController::spawn(actuator)?;

    controller.send(OpenPressed { id: 1 })?;
}
