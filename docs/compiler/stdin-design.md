# Standard Input Design

## Status

Proposed.

## Goals

- Add a standard-library surface for reading from process standard input.
- Reuse the existing `std::io` handle and `IoError` model where it fits.
- Prevent accidental closing of stdin through the type system.
- Support both whole-stream input (`read_all`) and interactive line-oriented input
  (`read_line`).
- Keep the design compatible with Machina's existing error-union model.

## Non-Goals (V1)

- General stream abstractions shared across files, pipes, sockets, and stdin.
- Buffered-reader policies beyond what individual operations already provide.
- Async stdin or scheduler-integrated input.
- Named stdin handles for stderr/stdout symmetry.
- New language syntax; this is a `std::io` library/runtime feature.

## Motivation

Today `std::io` supports:

- file handles via `open_read`, `open_write`, and `open_rw`
- text and binary adapters (`TextReader`, `BinaryReader`, etc.)
- runtime-backed read/write/close operations on file descriptors

The runtime already treats file descriptor `0` as standard input, so Machina does
not need new low-level read primitives to access stdin. The missing piece is a
safe public surface.

The main design constraint is that stdin is **not an owned file resource** in the
same sense as `ReadFile` or `TextReader`:

- user code should be able to read from it
- user code should generally not close it
- `using` should not apply to it

That points toward a dedicated stdin handle type instead of reusing
`TextReader` or `BinaryReader` directly.

## Surface API

Add the following to `std::io`:

```machina
@opaque
type Stdin = {
    _fd: u64,
}

@public
type EndOfInput = {}

@public
fn stdin() -> Stdin {
    Stdin { _fd: 0 }
}

Stdin :: {
    @public
    fn read(self, inout buf: u8[]) -> u64 | IoError

    @public
    fn read_all(self) -> string | IoError

    @public
    fn read_line(self) -> string | IoError | EndOfInput
}
```

### Why a dedicated `Stdin` type

This is preferred over returning `TextReader` or `BinaryReader` from `stdin()`.

If `stdin()` returned one of the existing file-backed reader types, users could
call:

```machina
using input = stdin();
input.close()?;
```

That would make it easy to close file descriptor `0`, which is the process-wide
standard input stream. A dedicated `Stdin` type avoids that entire class of
errors by construction:

- no `close`
- no `close_ignore_error`
- no accidental `using`

## Semantics

### `stdin()`

`stdin()` returns a lightweight, non-owning handle to file descriptor `0`.
Multiple calls may return equivalent handles:

```machina
let a = stdin();
let b = stdin();
```

This is acceptable because the handle is just a typed capability to the same
process-global input stream.

### `read(self, inout buf)`

`read` follows the existing `BinaryReader.read` convention:

- returns the number of bytes read
- returns `0` on EOF
- returns `IoError` on failure

This keeps stdin compatible with existing low-level read loops.

### `read_all(self)`

`read_all` reads until EOF and returns the full accumulated string:

```machina
let text = stdin().read_all()?;
```

EOF is normal completion for `read_all`, so there is no `EndOfInput` branch on
this method.

This is especially useful for piped input:

```machina
cat data.txt | my_program
```

### `read_line(self)`

`read_line` reads a single line of text and returns:

- `string` when a line was read
- `EndOfInput` when EOF occurs before any bytes are read
- `IoError` on failure

```machina
match stdin().read_line() {
    line: string => println(line),
    EndOfInput => println("done"),
    err: IoError => println(err.code),
}
```

Newline handling:

- strip trailing `\n`
- strip trailing `\r\n`
- if EOF occurs after some bytes but before a newline, return the partial line
- return `EndOfInput` only when EOF occurs before reading any bytes for that
  call

This avoids the ambiguity between:

- an empty line entered by the user, and
- EOF with no more input available

### `EndOfInput`

`EndOfInput` should live in `std::io`, not on `Stdin` specifically. EOF is a
useful shared vocabulary item for future stream-like APIs (pipes, sockets,
network readers), even though stdin is the first consumer.

In V1, `EndOfInput` only appears in the `read_line` API because that is the one
operation where “no more input” and “empty value” are meaningfully distinct.

## Runtime and Stdlib Implementation

### Reuse existing runtime helpers where possible

The current runtime surface is already sufficient for:

- `read(self, inout buf)` via `__rt_file_read`
- `read_all(self)` via `__rt_file_read_all_text`

That means `std::io` can reuse its existing helpers with `fd = 0`:

- `file_read(0, inout buf)`
- `read_all_text_from_fd(out dst, 0)`

### New runtime helper for line reading

`read_line` is the one operation that likely deserves a dedicated runtime helper.

Implementing line reading purely in Machina would require repeated tiny reads and
manual string accumulation. That is possible, but it would be a poor default for
interactive input and would duplicate buffer-management logic in the stdlib.

Add a runtime intrinsic along the lines of:

```machina
fn __rt_stdin_read_line(out dst: string) -> u64;
```

Suggested contract:

- writes the line contents into `dst`
- strips `\n` / `\r\n`
- returns:
  - `0` for I/O error (with `__rt_file_last_errno()` carrying the OS error)
  - `1` for EOF before any bytes
  - `2` for success with a line in `dst`

`std::io` can then map that into:

- `IoError`
- `EndOfInput`
- `string`

This is intentionally a dedicated three-way decode path rather than reuse of
`decode_file_result`. Existing file helpers follow the binary convention
`0 = error, encoded_success = value + 1`; `read_line` needs an additional EOF
state, so `std::io` should give it a small stdin-specific decode helper.

Any equivalent runtime ABI is acceptable as long as the observable Machina
semantics stay the same.

## Compiler Impact

This feature should be mostly library/runtime work.

### Frontend

No parser, resolver, or type-system changes are required.

The only compiler-facing work should be:

- adding the new `std::io` declarations to the stdlib source
- adding the new runtime intrinsic declaration to `std/prelude.mc` or
  `std/builtin.mc`, depending on where I/O runtime hooks are currently declared
- ensuring module interfaces for `std::io` expose `Stdin`, `EndOfInput`, and the
  new methods correctly

### Analysis / IDE support

Once the stdlib declarations exist, hover, signature help, completion, and go-to
-definition should pick them up naturally through the normal stdlib interface
pipeline.

## Examples

### Read all stdin

```machina
requires { std::io }

using std::io.*;

fn main() -> () | IoError {
    let text = stdin().read_all()?;
    println(text);
}
```

### Read lines until EOF

```machina
requires { std::io }

using std::io.*;

fn main() -> () | IoError {
    while true {
        match stdin().read_line() {
            line: string => println(line),
            EndOfInput => break,
            err: IoError => return err,
        }
    }
}
```

## Alternatives Considered

### Reuse `TextReader` / `BinaryReader`

Rejected.

This would make stdin look mechanically similar to ordinary file-backed readers,
but it would also expose `close` and `close_ignore_error`, which are the wrong
operations for a process-global standard stream.

### Return empty string on EOF from `read_line`

Rejected.

That would blur together:

- a genuinely empty line, and
- EOF before reading any bytes

Machina's error-union model is a better fit for representing EOF explicitly via
`EndOfInput`.

### Implement `read_line` entirely in Machina

Rejected for V1.

It is possible, but it would require byte-at-a-time reads plus manual string
accumulation and would likely become the worst-performing path in interactive
stdin usage. A small runtime helper is a better tradeoff.

## Recommended Implementation Plan

1. Add `EndOfInput` and `Stdin` to `std/io.mc`.
2. Add `stdin()` plus `read` / `read_all` / `read_line` methods.
3. Reuse existing file-read helpers for `read` and `read_all`.
4. Add a dedicated runtime helper for `read_line`.
5. Add examples and integration tests for:
   - piped `read_all`
   - repeated `read_line`
   - EOF handling
   - line normalization (`\n`, `\r\n`)
