trait Runnable {
    fn run(self) -> u64;
}

trait HasTickCount {
    prop tick_count: u64 { get; set; }
}

type Process = {
    name: string,
    ticks: u64,
}

type Task = {
    id: u64,
}

Process :: {
    fn next_tick(self) -> u64 {
        self.ticks + 1
    }
}

Process :: Runnable {
    fn run(self) -> u64 {
        self.next_tick()
    }
}

Process :: HasTickCount {
    prop tick_count: u64 {
        get { self.ticks }
        set(v) { self.ticks = v; }
    }
}

fn execute(p: Process) -> u64 {
    p.run()
}

fn accept_runnable<T: Runnable>(value: T) -> u64 {
    value.run()
}

fn add_ticks(inout p: Process, delta: u64) -> u64 {
    p.ticks = p.ticks + delta;
    p.ticks
}

fn main() {
    let p = Process {
        name: "worker-1",
        ticks: 41,
    };
    let result = execute(p);
    println(f"result = {result}");

    let ran = Process {
        name: "worker-2",
        ticks: 10,
    }
    .run();
    println(f"ran = {ran}");

    let ok = accept_runnable(Process {
        name: "worker-3",
        ticks: 0,
    });
    println(f"ok = {ok}");

    var tracked = Process {
        name: "worker-4",
        ticks: 5,
    };
    let after = add_ticks(inout tracked, 7);
    println(f"after = {after}");

    // Uncommenting this line should fail type checking because Task does not
    // implement Runnable:
    // let bad = accept_runnable(Task { id: 1 });
}
