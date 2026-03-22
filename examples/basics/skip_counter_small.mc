type Counter = {
    start: u64,
    end: u64,
}

Counter :: {
    fn iter(self) -> CounterIter {
        CounterIter { cur: self.start, end: self.end }
    }
}

type CounterIter = {
    cur: u64,
    end: u64,
}

CounterIter :: {
    fn next(inout self) -> u64 | IterDone {
        if self.cur < self.end {
            let value = self.cur;
            self.cur += 1;
            value
        } else {
            IterDone {}
        }
    }
}

type SkipCounter = {
    inner: Counter,
    skip: u64,
}

type SkipCounterIter = {
    inner: CounterIter,
    remaining: u64,
}

SkipCounter :: {
    fn iter(self) -> SkipCounterIter {
        SkipCounterIter {
            inner: self.inner.iter(),
            remaining: self.skip,
        }
    }
}

SkipCounterIter :: {
    fn next(inout self) -> u64 | IterDone {
        while self.remaining > 0 {
            let step = self.inner.next();
            match step {
                value: u64 => {
                    self.remaining -= 1;
                }
                done: IterDone => {
                    return IterDone {};
                }
            }
        }

        self.inner.next()
    }
}

fn main() {
    let skipped = SkipCounter {
        inner: Counter { start: 2, end: 7 },
        skip: 2,
    };

    var sum: u64 = 0;
    for n in skipped {
        println(n);
        sum += n;
    }
    println(sum);
}
