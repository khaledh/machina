requires {
    std::io::println
}

// Canonical machine + transition example:
// - Controller receives a user event.
// - Controller requests actuator work.
// - Controller transitions Closed -> Waiting -> Open.
// - Open is final.
type OpenPressed = { id: u64 }
type OpenCmd = {}
type Opened = {}

typestate DoorActuator {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on OpenCmd(_cmd: OpenCmd, cap: ReplyCap<Opened>) {
            cap.reply(Opened {});
        }
    }
}

typestate DoorController {
    fields {
        door: Machine<DoorActuator>,
    }

    fn new(door: Machine<DoorActuator>) -> Closed {
        Closed { door }
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
