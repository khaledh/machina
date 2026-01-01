# Machina Runtime (dev)

## ABI
### Traps
- Symbol: `__mc_trap`
- Signature: `void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2)`
- Purpose: terminates execution with a formatted runtime error.
  
### Printing
- Symbol: `__mc_print`
- Signature: `void __mc_print(const mc_string_t *str, uint64_t newline)`
  - `newline`: non-zero to append a trailing "\n"
- Purpose: writes a string to stdout.

### Integer conversion
- Symbol: `__mc_u64_to_dec`
- Signature: `uint64_t __mc_u64_to_dec(const mc_slice_t *s, uint64_t value)`
- Purpose: writes a decimal `u64` into a byte slice; returns digits written.

- Symbol: `__mc_i64_to_dec`
- Signature: `uint64_t __mc_i64_to_dec(const mc_slice_t *s, int64_t value)`
- Purpose: writes a decimal `i64` into a byte slice; returns digits written.

- Symbol: `__mc_u64_to_bin`
- Signature: `uint64_t __mc_u64_to_bin(const mc_slice_t *s, uint64_t value)`
- Purpose: writes a binary `u64` into a byte slice; returns digits written.

- Symbol: `__mc_u64_to_oct`
- Signature: `uint64_t __mc_u64_to_oct(const mc_slice_t *s, uint64_t value)`
- Purpose: writes an octal `u64` into a byte slice; returns digits written.

- Symbol: `__mc_u64_to_hex`
- Signature: `uint64_t __mc_u64_to_hex(const mc_slice_t *s, uint64_t value)`
- Purpose: writes a hex `u64` into a byte slice; returns digits written.

### String helpers
- Symbol: `__mc_string_from_bytes`
- Signature: `void __mc_string_from_bytes(mc_string_t *out, const mc_slice_t *s)`
- Purpose: creates a string view over a byte slice (expects valid UTF-8).

### Formatting
- Symbol: `__mc_fmt_init`
- Signature: `void __mc_fmt_init(mc_fmt_t *fmt, const mc_slice_t *buf)`
- Purpose: initializes a formatter over a byte buffer.

- Symbol: `__mc_fmt_append_bytes`
- Signature: `void __mc_fmt_append_bytes(mc_fmt_t *fmt, uint64_t ptr, uint64_t len)`
- Purpose: appends a byte range to the formatter buffer.

- Symbol: `__mc_fmt_append_u64`
- Signature: `void __mc_fmt_append_u64(mc_fmt_t *fmt, uint64_t value)`
- Purpose: appends a decimal `u64` to the formatter buffer.

- Symbol: `__mc_fmt_append_i64`
- Signature: `void __mc_fmt_append_i64(mc_fmt_t *fmt, int64_t value)`
- Purpose: appends a decimal `i64` to the formatter buffer.

- Symbol: `__mc_fmt_finish`
- Signature: `void __mc_fmt_finish(mc_string_t *out, const mc_fmt_t *fmt)`
- Purpose: finalizes the formatter output as a `string` view.

### Memory
- Symbol: `__mc_memset`
- Signature: `void __mc_memset(mc_slice_t *dst, uint8_t value)`
- Purpose: fills a byte slice with a single byte value.

- Symbol: `__mc_memcpy`
- Signature: `void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src)`
- Purpose: copies bytes between slices (expected to match in length).

## Types
- `mc_string_t` matches Machina's `string` layout:
  ```
  { ptr: u64, len: u32, tag: u8, padding: [u8; 3] }
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

## Exit codes
- Exit code = 100 + kind

## Messages
- DivByZero: "Runtime error: Division by zero\n"
- Bounds: "Runtime error: Index out of bounds: index=<i>, len=<n>\n"
- Range: "Runtime error: Value out of range: value=<v>, min=<min>, max=<max>\n"
- Default: "Runtime error: Unknown trap\n"
