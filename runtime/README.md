# Machina Runtime (dev)

## ABI
### Traps
- Symbol: `__rt_trap`
- Signature: `void __rt_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2)`
- Purpose: terminates execution with a formatted runtime error.
  
### Printing
- Symbol: `__rt_print`
- Signature: `void __rt_print(uint64_t ptr, uint64_t len, uint64_t newline)`
  - `newline`: non-zero to append a trailing "\n"
- Purpose: writes a string to stdout.

### Integer conversion
- Symbol: `__rt_u64_to_dec`
- Signature: `uint64_t __rt_u64_to_dec(uint64_t ptr, uint64_t len, uint64_t value)`
- Purpose: writes a decimal `u64` into a byte slice; returns digits written.

- Symbol: `__rt_i64_to_dec`
- Signature: `uint64_t __rt_i64_to_dec(uint64_t ptr, uint64_t len, int64_t value)`
- Purpose: writes a decimal `i64` into a byte slice; returns digits written.

- Symbol: `__rt_u64_to_bin`
- Signature: `uint64_t __rt_u64_to_bin(uint64_t ptr, uint64_t len, uint64_t value)`
- Purpose: writes a binary `u64` into a byte slice; returns digits written.

- Symbol: `__rt_u64_to_oct`
- Signature: `uint64_t __rt_u64_to_oct(uint64_t ptr, uint64_t len, uint64_t value)`
- Purpose: writes an octal `u64` into a byte slice; returns digits written.

- Symbol: `__rt_u64_to_hex`
- Signature: `uint64_t __rt_u64_to_hex(uint64_t ptr, uint64_t len, uint64_t value)`
- Purpose: writes a hex `u64` into a byte slice; returns digits written.

### String helpers
- Symbol: `__rt_string_from_bytes`
- Signature: `void __rt_string_from_bytes(uint64_t out_ptr, uint64_t ptr, uint64_t len)`
- Purpose: creates a string view over a byte slice (expects valid UTF-8).

- Symbol: `__rt_string_append_bytes`
- Signature: `void __rt_string_append_bytes(uint64_t s_ptr, uint64_t ptr, uint64_t len)`
- Purpose: appends bytes to an owned string buffer (promotes if needed).

- Symbol: `__rt_string_ensure`
- Signature: `void __rt_string_ensure(uint64_t s_ptr, uint32_t min_cap)`
- Purpose: ensures owned capacity for a string buffer.

- Symbol: `__rt_string_drop`
- Signature: `void __rt_string_drop(uint64_t s_ptr)`
- Purpose: drops an owned string buffer (no-op for views).

### Formatting
- Symbol: `__rt_fmt_init`
- Signature: `void __rt_fmt_init(uint64_t fmt_ptr, uint64_t buf_ptr, uint64_t buf_len)`
- Purpose: initializes a formatter over a byte buffer.

- Symbol: `__rt_fmt_append_bytes`
- Signature: `void __rt_fmt_append_bytes(mc_fmt_t *fmt, uint64_t ptr, uint64_t len)`
- Purpose: appends a byte range to the formatter buffer.

- Symbol: `__rt_fmt_append_u64`
- Signature: `void __rt_fmt_append_u64(mc_fmt_t *fmt, uint64_t value)`
- Purpose: appends a decimal `u64` to the formatter buffer.

- Symbol: `__rt_fmt_append_i64`
- Signature: `void __rt_fmt_append_i64(mc_fmt_t *fmt, int64_t value)`
- Purpose: appends a decimal `i64` to the formatter buffer.

- Symbol: `__rt_fmt_finish`
- Signature: `void __rt_fmt_finish(uint64_t out_ptr, uint64_t fmt_ptr)`
- Purpose: finalizes the formatter output as a `string` view.

### Memory
- Symbol: `__rt_memset`
- Signature: `void __rt_memset(uint64_t ptr, uint64_t len, uint8_t value)`
- Purpose: fills a byte slice with a single byte value.

- Symbol: `__rt_memcpy`
- Signature: `void __rt_memcpy(uint64_t dst_ptr, uint64_t src_ptr, uint64_t len)`
- Purpose: copies bytes between slices (expected to match in length).

### Allocation
- Symbol: `__rt_alloc`
- Signature: `void *__rt_alloc(uint64_t size, uint64_t align)`
- Purpose: allocates an owned heap block and returns a pointer.

- Symbol: `__rt_realloc`
- Signature: `void *__rt_realloc(void *ptr, uint64_t size, uint64_t align)`
- Purpose: reallocates a block to a new size and returns the new pointer.

- Symbol: `__rt_free`
- Signature: `void __rt_free(void *ptr)`
- Purpose: frees a block allocated by `__rt_alloc`/`__rt_realloc`.

### Allocation Trace Control

- Symbol: `__rt_set_alloc_trace`
- Signature: `void __rt_set_alloc_trace(uint8_t enabled)`
- Purpose: enables (`enabled != 0`) or disables allocation tracing to stderr.

### Managed machine runtime core (experimental)

- Header: `machine_runtime.h`
- Purpose: foundational scheduler substrate for managed typestate machines.
- Includes:
  - machine table with lifecycle (`Created | Running | Faulted | Stopped`)
  - bounded per-machine FIFO mailboxes
  - global ready queue
  - single-envelope deterministic dispatch entrypoint (transactional callback form)
  - request/reply correlation plumbing (`Pending`/`ReplyCap` ids)
  - dead-letter and fault hook callbacks

## Types
- `mc_string_t` matches Machina's `string` layout:
  ```
  { ptr: u64, len: u32, cap: u32 }
  ```

- `mc_slice_t` is a simple pointer and length:
  ```
  { ptr: u64, len: u64 }
  ```

- `mc_fmt_t` is a simple formatter state:
  ```
  { ptr: u64, len: u64, cap: u64 }
  ```

## CheckKind mapping
- 0: DivByZero (arg0/arg1/arg2 unused)
- 1: Bounds (arg0 = index, arg1 = len, arg2 unused)
- 2: Range (arg0 = value, arg1 = min, arg2 = max)
- 3: SignedRange (arg0 = value, arg1 = min, arg2 = max)
- 4: NonZero (arg0 = value, arg1/arg2 unused)
- 5: SignedNonZero (arg0 = value, arg1/arg2 unused)
- 6: MainUnhandledError (arg0 = message ptr, arg1 = message len, arg2 unused)

## Exit codes
- Exit code = 100 + kind

## Messages
- DivByZero: "Runtime error: Division by zero\n"
- Bounds: "Runtime error: Index out of bounds: index=<i>, len=<n>\n"
- Range: "Runtime error: Value out of range: value=<v>, min=<min>, max=<max>\n"
- MainUnhandledError: "Runtime error: Unhandled error in main: <ErrorType>\n"
- Default: "Runtime error: Unknown trap\n"
