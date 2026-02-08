requires {
    std::io as io
}

type Counter = { total: u64, last: u64 }
type Event = Add(u64) | Reset

Counter :: {
    fn add(inout self, value: u64) {
        self.total = self.total + value;
        self.last = value;
    }

    fn reset(inout self) {
        self.total = 0;
        self.last = 0;
    }

    fn snapshot(self) -> (u64, u64) {
        (self.total, self.last)
    }
}

fn main() {
    let events = [
        Event::Add(2),
        Event::Add(10),
        Event::Reset,
        Event::Add(3),
        Event::Add(5),
    ];
    var counter = Counter { total: 0, last: 0 };

    for ev in events {
        match ev {
            Event::Add(n) => counter.add(n),
            Event::Reset => counter.reset(),
        }
    }

    let (total, last) = counter.snapshot();
    io::println(f"total={total}, last={last}");
}
