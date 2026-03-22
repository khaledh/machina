requires {
    std::io::println
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

type MapIter<S, In, Out> = {
    source: S,
    f: fn(In) -> Out,
}

MapIter<S, In, Out> :: {
    fn iter(self) -> MapIter<S, In, Out> {
        self
    }

    fn next(inout self) -> Out | IterDone {
        match self.source.next() {
            item: In => {
                let f = self.f;
                f(item)
            },
            done: IterDone => IterDone {},
        }
    }
}

fn double(n: u64) -> u64 { n * 2 }

fn map_values<S, In, Out>(source: S, f: fn(In) -> Out) -> MapIter<S, In, Out> {
    MapIter { source, f }
}

fn main() {
    let counter = Counter { cur: 2, end: 5 };
    let mapped: MapIter<CounterIter, u64, u64> = map_values(counter.iter(), double);
    for n in mapped {
        println(n);
    }
}
