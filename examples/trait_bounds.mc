trait Runnable {
    fn run(self) -> u64;
}

type Process = {
    ticks: u64,
}

type Task = {
    id: u64,
}

Process :: Runnable {
    fn run(self) -> u64 {
        self.ticks + 1
    }
}

fn accept_runnable<T: Runnable>(value: T) -> u64 {
    let consumed = value;
    consumed;
    1
}

fn main() {
    let p = Process { ticks: 41 };
    let ran = p.run();
    println(f"ran = {ran}");

    let ok = accept_runnable(p);
    println(f"ok = {ok}");

    // Uncommenting this line should fail type checking because Task does not
    // implement Runnable:
    // let bad = accept_runnable(Task { id: 1 });
}
