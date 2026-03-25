# Generics

Generics let you write functions and types that work with any type. The compiler
monomorphizes each use -- every generic is specialized to concrete types at
compile time with zero runtime cost.

## Generic Functions

Add type parameters in angle brackets after the function name:

```mc
fn id<T>(x: T) -> T {
    x
}

let n = id(42);          // T inferred as i32
let s = id("hello");     // T inferred as string
let b: u64 = id(42);     // T inferred as u64 from annotation
```

Multiple type parameters:

```mc
type Pair<L, R> = { left: L, right: R }

fn pair<L, R>(left: L, right: R) -> Pair<L, R> {
    Pair { left, right }
}

let p = pair("age", 7);    // Pair<string, i32>
```

## Generic Types

Types can be parameterized the same way.

### Generic Structs

```mc
type Pair<T> = { left: T, right: T }

let p = Pair { left: 1, right: 2 };    // Pair<i32>
println(p.left + p.right);              // 3
```

### Generic Enums

```mc
type Option<T> = Some(T) | None

let x = Option::Some(42);
match x {
    Some(n) => println(n),
    None => println("nothing"),
};
```

## Generic Methods

Methods can introduce their own type parameters:

```mc
type Boxed = { value: u64 }

Boxed :: {
    fn cast<T>(self, x: T) -> T {
        x
    }
}

let b = Boxed { value: 1 };
let n = b.cast(42);       // T = i32
let s = b.cast(false);    // T = bool
```

## Trait Bounds

Constrain type parameters to types that implement a trait:

```mc
trait Runnable {
    fn run(self) -> u64;
}

fn execute<T: Runnable>(value: T) -> u64 {
    value.run()
}
```

The compiler rejects calls where the argument type does not implement the
required trait.

See [Traits](traits.md) for more on defining and implementing traits.

## Type Inference

Type arguments are usually inferred from the call site:

```mc
fn make_some<T>(x: T) -> Option<T> {
    Some(x)
}

let a = make_some(42);       // Option<i32>
let b = make_some("hello");  // Option<string>
```

You do not need to write explicit type arguments at call sites -- the compiler
infers them from argument types and return context.

## Runnable Examples

- `examples/generics/generics.mc`
- `examples/generics/generic_types.mc`
- `examples/generics/generic_methods.mc`
- `examples/generics/type_infer.mc`
