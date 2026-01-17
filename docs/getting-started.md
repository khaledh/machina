# Getting Started

This guide walks you through building the Machina compiler and running your
first program.

## Requirements

- Rust toolchain (stable)
- ARM64 system (macOS on Apple Silicon, or Linux on ARM64)

## Clone and Build

```sh
git clone https://github.com/khaledh/machina.git
cd machina
cargo build --release
```

The compiler binary is `mcc`, located in `target/release/`.

## Write Your First Program

Create a file called `hello.mc`:

```
fn main() {
    println("Hello, Machina!");
}
```

## Compile and Run

_Note: the `mcc` command is an alias for `cargo run --bin mcc --`. It is defined
in the `.cargo/config.toml` file._

```sh
cargo mcc build -o hello hello.mc
./hello

# or

cargo mcc run hello.mc
```

You should see:

```
Hello, Machina!
```

## What Just Happened?

The compiler:

1. Parsed your source file
2. Type-checked and validated memory safety rules
3. Generated ARM64 assembly
4. Assembled and linked an executable

## Next Steps

- Read the [Language Tour](tour.md) for a quick overview of all features
- Explore the [Guide](guide/) for in-depth coverage of specific topics
- Check out the [examples/](../examples/) directory for more code samples
