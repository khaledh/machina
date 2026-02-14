# Methods

Methods are functions associated with a type. They provide a way to define
behavior for your custom types.

> Note: snippets that call `println` assume:
>
> ```mc
> requires {
>     std::io::println
> }
> ```

## Defining Methods

Methods are defined in a block associated with a type using the `::` syntax:

```
type Counter = { value: u64 }

Counter :: {
    fn get(self) -> u64 {
        self.value
    }

    fn increment(inout self) {
        self.value = self.value + 1;
    }
}
```

The first parameter of a method is `self`, which refers to the instance the
method is called on.

## Calling Methods

Use dot notation to call methods:

```
var c = Counter { value: 0 };
c.increment();
let v = c.get();    // 1
```

## The self Parameter

The `self` parameter can use different modes:

### Read-Only self

The default `self` is read-only:

```
Counter :: {
    fn get(self) -> u64 {
        self.value    // can read
        // self.value = 0;  // error: cannot mutate
    }
}
```

### Mutable self

Use `inout self` to mutate the instance:

```
Counter :: {
    fn increment(inout self) {
        self.value = self.value + 1;
    }

    fn reset(inout self) {
        self.value = 0;
    }
}
```

### Consuming self

Use `sink self` to take ownership (for heap types):

```
type Box = { data: Data^ }

Box :: {
    fn unwrap(sink self) -> Data^ {
        move self.data
    }
}
```

## Multiple Methods

A type can have many methods:

```
type Point = { x: u64, y: u64 }

Point :: {
    fn distance_from_origin(self) -> u64 {
        self.x + self.y
    }

    fn translate(inout self, dx: u64, dy: u64) {
        self.x = self.x + dx;
        self.y = self.y + dy;
    }

    fn scale(self, factor: u64) -> Point {
        Point { x: self.x * factor, y: self.y * factor }
    }
}
```

## Constructors

Method blocks require a `self` parameter. Use a free function for constructors:

```
fn origin() -> Point {
    Point { x: 0, y: 0 }
}
```

## Method Overloading

Methods can be overloaded by parameter types:

```
type Printer = { prefix: string }

Printer :: {
    fn print(self, x: u64) {
        println(f"{self.prefix}: {x}");
    }

    fn print(self, x: bool) {
        let s = if x { "true" } else { "false" };
        println(f"{self.prefix}: {s}");
    }
}
```

## Example: Counter with Events

Combining methods with enums for a more complete example:

```
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
    ];

    var counter = Counter { total: 0, last: 0 };

    for ev in events {
        match ev {
            Event::Add(n) => counter.add(n),
            Event::Reset => counter.reset(),
        }
    }

    let (total, last) = counter.snapshot();
    println(f"total={total}, last={last}");
}
```

## Method Chaining

Methods that return the same type enable chaining:

```
type Builder = { value: u64 }

Builder :: {
    fn add(self, n: u64) -> Builder {
        Builder { value: self.value + n }
    }

    fn multiply(self, n: u64) -> Builder {
        Builder { value: self.value * n }
    }

    fn build(self) -> u64 {
        self.value
    }
}

fn main() {
    let result = Builder { value: 0 }
        .add(5)
        .multiply(2)
        .add(3)
        .build();
    // result is 13
}
```
