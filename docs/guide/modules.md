# Modules and Imports

Machina source files are modules. Imports are declared with a top-level
`requires` block.

## Importing Symbols

```mc
requires {
    std::io::println
    app::config::Config
    app::config::load
}
```

Imported symbols can be used directly:

```mc
fn main() {
    let cfg: Config = load("config.toml");
    println(cfg.name);
}
```

## Import Aliases

Use `as` to rename imports locally.

```mc
requires {
    app::config::load as load_config
}

fn main() {
    let cfg = load_config("config.toml");
}
```

## Importing Whole Modules

You can import a module path and reference its public symbols by name if that
module exports them.

```mc
requires {
    std::io
}

fn main() {
    io::println("hello");
}
```

## Visibility

Top-level declarations are module-private by default.

Use attributes to expose items:

```mc
@public
fn parse_u64(s: string) -> u64 {
    // ...
}

@opaque
type Buffer = {
    _data: u8[]^,
    _len: u64,
}
```

- `@public`: exported from the module.
- `@opaque`: exported type whose internal fields are hidden outside the
  defining module.

## Opaque Types and APIs

Opaque types are intended to be manipulated through public methods/properties:

```mc
Buffer :: {
    @public
    fn new() -> Buffer {
        // ...
    }

    @public
    prop len: u64 {
        get { self._len }
    }
}
```

## Capsule Root

For module-graph compilation, Machina resolves the capsule root from:

1. nearest `machina.toml`
2. otherwise nearest `.git`

You can also override root resolution with `MACHINA_CAPSULE_ROOT`.
