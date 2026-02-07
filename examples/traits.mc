trait Runnable {
    fn run(self) -> u64;
}

type Process = {
    name: string,
    ticks: u64,
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

fn execute(p: Process) -> u64 {
    p.run()
}

fn main() {
    let p = Process {
        name: "worker-1",
        ticks: 41,
    };
    let result = execute(p);
    println(f"result = {result}");
}
