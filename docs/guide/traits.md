# Traits

Traits define shared behavior that types can implement. They are used for
abstraction and generic bounds.

## Defining a Trait

A trait declares method signatures that implementing types must provide:

```mc
trait Runnable {
    fn run(self) -> u64;
}
```

## Implementing a Trait

Use `Type :: TraitName { ... }` to implement a trait for a type:

```mc
type Process = { name: string, ticks: u64 }

Process :: Runnable {
    fn run(self) -> u64 {
        self.ticks + 1
    }
}
```

A type can have both regular methods and trait implementations:

```mc
Process :: {
    fn next_tick(self) -> u64 {
        self.ticks + 1
    }
}

Process :: Runnable {
    fn run(self) -> u64 {
        self.next_tick()
    }
}
```

## Trait Properties

Traits can declare properties with getters and setters:

```mc
trait HasTickCount {
    prop tick_count: u64 { get; set; }
}

Process :: HasTickCount {
    prop tick_count: u64 {
        get { self.ticks }
        set(v) { self.ticks = v; }
    }
}
```

Properties are accessed like fields:

```mc
var p = Process { name: "worker", ticks: 5 };
println(p.tick_count);    // 5
p.tick_count = 10;
```

## Trait Bounds

Use trait bounds to constrain generic type parameters:

```mc
fn execute<T: Runnable>(value: T) -> u64 {
    value.run()
}

fn add_ticks<T: HasTickCount>(inout p: T, delta: u64) {
    p.tick_count += delta;
}
```

Calling with a type that does not implement the trait is a compile error:

```mc
type Task = { id: u64 }

execute(Task { id: 1 });    // ERROR: Task does not implement Runnable
```

## Runnable Examples

- `examples/methods_traits_props/traits.mc`
- `examples/modules_visibility/runtime.mc`
