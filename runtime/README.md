# Machina Runtime (dev)

## ABI
- Symbol: `__mc_trap`
- Signature: `void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2)`
  
- Symbol: `__mc_print`
- Signature: `void __mc_print(const mc_string_t *str, uint64_t newline)`
  - `newline`: non-zero to append a trailing "\n"

- Symbol: `__mc_u64_to_dec`
- Signature: `uint64_t __mc_u64_to_dec(const mc_slice_t *s, uint64_t value)`

## Types
- `mc_string_t` matches Machina's `string` layout:
  ```
  { ptr: u64, len: u32, tag: u8, padding: [u8; 3] }
  ```

- `mc_slice_t` is a simple pointer and length:
  ```
  { ptr: u64, len: u64 }
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
