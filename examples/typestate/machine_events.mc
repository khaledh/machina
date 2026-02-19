requires {
    std::io::println
}

// Canonical machine example: spawn + send + handler execution.
type Tick = { n: u64 }

typestate Timer {
    fn new() -> Running {
        Running {}
    }

    state Running {
        on Tick(t) {
            println(f"tick {t.n}");
        }
    }
}

@machines
fn main() -> () | MachineError {
    let timer = Timer::spawn()?;
    timer.send(Tick { n: 42 })?;
}
