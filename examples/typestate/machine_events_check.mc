requires {
    std::io::println
    std::machine::new_runtime
    std::machine::close_runtime
    std::machine::spawn
    std::machine::start
    std::machine::send
    std::machine::bind_descriptor
    std::machine::step
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
        on Kick(e: Kick) -> Running {
            e;
            emit Send(to: 1, Tick {});
            Running {}
        }

        on Tick(t: Tick) -> Running {
            t;
            println("tick");
            Running {}
        }
    }
}

fn main() -> u64 {
    var rt = new_runtime();
    var timer = 0;
    match spawn(rt, 8) {
        id: u64 => {
            timer = id;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    // Single typestate in this module => descriptor id 1, state tag 1.
    match bind_descriptor(rt, timer, 1, 1) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match start(rt, timer) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match send(rt, timer, 1, 0, 0) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    // Kick handler then Tick handler.
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    close_runtime(inout rt);
    0
}
