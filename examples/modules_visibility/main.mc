requires {
    std.io as io
    examples.modules_visibility.config as cfg
    examples.modules_visibility.runtime as rt
}

fn main() {
    var c = cfg.make("worker-a", 3);

    // Public property access on an opaque type is allowed.
    c.tick_count = c.tick_count + 4;

    // Trait-bounded cross-module call.
    let ran = rt.execute(c);

    // Both modules export `answer`; alias-qualified calls disambiguate.
    let total = cfg.answer() + rt.answer();

    io.println(f"name = {c.name}");
    io.println(f"tick_count = {c.tick_count}");
    io.println(f"ran = {ran}");
    io.println(f"total = {total}");

    // These should fail if uncommented:
    // let bad = Config { _name: "x", _ticks: 0 };      // opaque construction
    // io.println(f"ticks raw = {c._ticks}");              // opaque field access
}
