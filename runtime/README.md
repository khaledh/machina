# Machina Runtime (dev)

## ABI
- Symbol: `__mc_trap`
- Signature: `void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1)`

## CheckKind mapping
- 0: Bounds (arg0 = index, arg1 = len)
- 1: DivByZero (arg0/arg1 unused)

## Exit codes
- Exit code = 100 + kind

## Messages
- Bounds: "Machina runtime error: bounds check failed (index=<i>, len=<n>)\n"
- DivByZero: "Machina runtime error: divide by zero\n"
- Default: "Machina runtime error: unknown trap\n"
