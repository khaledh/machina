requires {
    std::io as io
}

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

fn add_ticks<T: HasTickCount>(inout p: T, delta: u64) {
    p.tick_count = p.tick_count + delta;
}

fn main() {
    let p = Process {
        name: "worker-1",
        ticks: 41,
    };
    let result = execute(p);
    io::println(f"result = {result}");

    let ran = Process {
        name: "worker-2",
        ticks: 10,
    }
    .run();
    io::println(f"ran = {ran}");

    let ok = accept_runnable(Process {
        name: "worker-3",
        ticks: 0,
    });
    io::println(f"ok = {ok}");

    var tracked = Process {
        name: "worker-4",
        ticks: 5,
    };
    io::println(f"before = {tracked.ticks}");
    add_ticks(inout tracked, 7);
    io::println(f"after = {tracked.ticks}");

    // Uncommenting this line should fail type checking because Task does not
    // implement Runnable:
    // let bad = accept_runnable(Task { id: 1 });
}
