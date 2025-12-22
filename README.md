# Machina Programming Language

Machina is a modern systems programming language that is still in early
development. The compiler currently targets only ARM64 assembly.

## Current features

- Expression oriented syntax
- Variables: `let` (immutable), `var` (mutable) with lexical scoping
- Arithmetic and comparison operators
- Basic types: `u64`, `bool`, `()`
- Compound types: tuples `(u64, bool)`, arrays `u64[N]`, multi-dimensional arrays `u64[M, N]`
- Type aliases: `type Size = u64`
- Array destructuring: `let [a, b, c] = [1, 2, 3]`
- Tuple destructuring: `let (x, y) = (1, true)`
- Struct destructuring: `let Point { x, y } = p`
- Blocks (last expression is the block value)
- Control flow: `if`/`else`, `while`
- Functions and function calls (recursion supported)
- Code generation to ARM64 assembly

## Example

```
fn main() -> u64 {
    let t = {
      let a = create_array(true);
      let s = a[0] + a[1] + a[2];
      (a, s)
    };
    let ([a, b, c], sum) = t;
    let double = a + b + c + sum;
    double
}

fn create_array(b: bool) -> u64[3] {
    if b {
        [1, 2, 3]
    } else {
        [4, 5, 6]
    }
}
```

## Compiling and running

Change the `SOURCE` constant in `src/main.rs` to your source code and run:
```
cargo run
```

This will compile the source code to arm64 assembly in `output.s` in the root
directory. Use clang to assemble and link:
```
clang output.s -o output
```

Then run the executable:
```
./output; echo $?  # or ./output; echo $status (for fish shell)
```

This should print the return value of the `main` function.
