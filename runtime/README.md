# Machina Runtime (dev)

## ABI
- Symbol: `__mc_trap`
- Signature: `void __mc_trap(uint64_t kind)`

## CheckKind mapping
- 0: Bounds

## Exit codes
- Exit code = 100 + kind

## Messages
- Bounds: "Machina runtime error: bounds check failed\n"
- Default: "Machina runtime error: unknown trap\n"
