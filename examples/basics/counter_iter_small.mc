requires {
    std::io::println
}

type Counter = {
    start: u64,
    end: u64,
}

type CounterIter = {
    cur: u64,
    end: u64,
}

Counter :: {
    fn iter(self) -> CounterIter {
        CounterIter { cur: self.start, end: self.end }
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

fn main() {
    let counter = Counter { start: 2, end: 5 };
    var sum: u64 = 0;
    for n in counter {
        println(n);
        sum = sum + n;
    }
    println(sum);
}
