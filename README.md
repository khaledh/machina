# Machina Programming Language

Machina is a modern systems programming language that is still in early
development. The compiler currently targets only ARM64 assembly.

## Current features

- Expression oriented syntax
- Basic types: `u32`, `bool`, `()`
- Variables: `let` (immutable), `var` (mutable) with lexical scoping
- Arithmetic and comparison operators
- Blocks (last expression is the block value)
- Control flow: `if`/`else`, `while`
- Functions and function calls (recursion supported)
- Code generation to ARM64 assembly

## Example

```
fn main() -> u32 {
    let x = {
      let value = foo();
      value + 2
    };
    x
}

fn foo() -> u32 {
    if true { 40 } else { 0 }
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
