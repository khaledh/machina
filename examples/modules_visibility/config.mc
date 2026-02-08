requires {
    examples.modules_visibility.runtime as rt
}

@[opaque]
type Config = {
    _name: string,
    _ticks: u64,
}

@[public]
fn make(name: string, ticks: u64) -> Config {
    Config {
        _name: name,
        _ticks: ticks,
    }
}

Config :: {
    @[public]
    prop name: string {
        get { self._name }
    }

    @[public]
    prop tick_count: u64 {
        get { self._ticks }
        set(v) { self._ticks = v; }
    }
}

Config :: rt.Runnable {
    fn run(self) -> u64 {
        self.tick_count + rt.answer_with_offset()
    }
}

@[public]
fn answer() -> u64 {
    2
}

fn internal_make_default() -> Config {
    make("default", 0)
}
