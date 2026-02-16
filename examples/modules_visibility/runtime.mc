@public
trait Runnable {
    fn run(self) -> u64;
}

@public
fn execute<T: Runnable>(value: T) -> u64 {
    value.run()
}

@public
fn answer() -> u64 {
    40
}

fn internal_offset() -> u64 {
    2
}

@public
fn answer_with_offset() -> u64 {
    answer() + internal_offset()
}
