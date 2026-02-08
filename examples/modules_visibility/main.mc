requires {
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

    println(f"name = {c.name}");
    println(f"tick_count = {c.tick_count}");
    println(f"ran = {ran}");
    println(f"total = {total}");

    // These should fail if uncommented:
    // let bad = Config { _name: "x", _ticks: 0 };      // opaque construction
    // println(f"ticks raw = {c._ticks}");              // opaque field access
}
