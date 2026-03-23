requires {
    std::io::println
    std::iter::map
}

type Counter = {
    cur: u64,
    end: u64,
}

type CounterIter = {
    cur: u64,
    end: u64,
}

Counter :: {
    fn iter(self) -> CounterIter {
        CounterIter { cur: self.cur, end: self.end }
    }
}

CounterIter :: {
    fn next(inout self) -> u64 | IterDone {
        if self.cur < self.end {
            let value = self.cur;
            self.cur = self.cur + 1;
            value
        } else {
            IterDone {}
        }
    }
}

fn double(n: u64) -> u64 { n * 2 }

fn main() {
    let counter = Counter { cur: 2, end: 5 };
    let mapped = map(counter.iter(), double);
    for n in mapped {
        println(n);
    }
}
