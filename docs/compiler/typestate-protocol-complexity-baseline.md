# Typestate / Protocol / Machine Complexity Baseline

Date: 2026-02-19

This document tracks complexity for the typestate/protocol/machine path and the
remaining high-ROI simplification targets.

## Scope

- `src/core/typestate/*`
- `src/core/protocol/*`
- `src/core/typecheck/validate/*`
- `src/core/semck/protocol_progression.rs`
- `src/core/semck/protocol_progression_check.rs`
- `runtime/machine/*`
- `runtime/tests/machine_runtime*.c` (test-only baseline)

## LOC Snapshot (Current)

### Core compiler path

| Area | LOC | Notes |
|---|---:|---|
| `src/core/typestate/*` | 3,793 | Desugar, managed API generation, and machine bridge lowering |
| `src/core/protocol/*` | 591 | `index.rs` + shared extraction helpers |
| `validate/* + protocol semck files` | 1,589 | Protocol checks + reply-cap checks + progression checks |

### Runtime machine path

| Area | LOC | Notes |
|---|---:|---|
| `runtime/machine/*` | 3,972 | Dominated by `runtime.c` + `runtime.h` |
| `runtime/tests/machine_runtime*.c` | 2,413 | Broad coverage; reduced Rust harness duplication |

## Largest Files (Hotspot by size)

### Compiler

| File | LOC |
|---|---:|
| `src/core/typestate/mod.rs` | 832 |
| `src/core/typecheck/validate/protocol.rs` | 745 |
| `src/core/typestate/managed_api.rs` | 661 |
| `src/core/typestate/handlers.rs` | 412 |
| `src/core/typestate/support_types.rs` | 392 |
| `src/core/typestate/analysis.rs` | 369 |
| `src/core/protocol/index.rs` | 404 |

### Runtime

| File | LOC |
|---|---:|
| `runtime/machine/runtime.c` | 1,458 |
| `runtime/machine/runtime.h` | 690 |
| `runtime/machine/descriptor.c` | 512 |
| `runtime/machine/emit.c` | 363 |
| `runtime/machine/bridge.c` | 351 |

## Largest Rust Functions (approximate by span)

| Function | File | Approx LOC |
|---|---|---:|
| `machine_handle_method_block` | `src/core/typestate/managed_api.rs` | 413 |
| `lower_spawn_func` | `src/core/typestate/managed_api.rs` | 242 |
| `desugar_typestate` | `src/core/typestate/mod.rs` | 215 |
| `rewrite_machine_request_method_destinations` | `src/core/typestate/rewrite_handles.rs` | 197 |
| `build_protocol_fact` | `src/core/protocol/index.rs` | 96 |
| `analyze_typestate` | `src/core/typestate/analysis.rs` | 149 |
| `wrap_main_with_managed_runtime` | `src/core/typestate/managed_entrypoint.rs` | 170 |
| `ensure_machine_runtime_intrinsics` | `src/core/typestate/support_types.rs` | 126 |

## Duplication / Coupling Hotspots

1. **Managed API builders still dominate hotspot size**
   - `machine_handle_method_block` and `lower_spawn_func` still carry the
     majority of generated API complexity.
   - Remaining wins come from extracting narrowly-scoped builder helpers.

2. **Typestate managed API generation has repeated ABI scaffolding**
   - `send`/`request` raw and typed variants repeat: runtime acquisition, payload packing, status checks, error-union branching.
   - Appears in one giant constructor function (`machine_handle_method_block`).

3. **Runtime lifecycle/dispatch path still large**
   - `runtime.c` remains a primary hotspot even after lifecycle API tightening.
   - Further cleanup should continue shrinking branch-heavy commit/rollback
     logic and shared status mapping.

4. **Protocol progression diagnostics can still be more compact**
   - Progression checks are in place, but error rendering and test fixtures can
     continue to be reduced/normalized.

## Complexity Reduction Targets (Milestone 12)

- Reduce core compiler path LOC (`typestate/* + protocol/* + validate/progression`) by **25-35%** through decomposition and dedup.
- Reduce top function spans to an upper bound of **~120 LOC** for non-runtime code (exceptions documented if justified).
- Ensure protocol emit extraction has **one canonical implementation** consumed by typecheck + semck.
- Keep behavior stable (existing typestate/protocol tests remain green).

## Refactor Status (Milestone 12)

Completed:
1. Unified protocol event extraction (issue #140).
2. Split typecheck validation domains (issue #141).
3. Split typestate desugar pipeline by concern (issue #142).
4. Deduplicated managed API generation scaffolding (issue #143).
5. Tightened runtime lifecycle API + dispatch binding path cleanup (issue #144).
6. Centralized managed status/error mapping (issue #145).
7. Reduced runtime test harness duplication (issue #146).
8. Added complexity ratchet script + thresholds (issue #147).

## Notes

- Milestone 12 now includes both compiler-side dedup and targeted runtime
  cleanup (lifecycle API + dispatch binding path).
- The baseline numbers are intended for trend tracking; exact values may shift with formatting and nearby feature work.

## Complexity Ratchet

Use the checked-in script:

```bash
python3 scripts/complexity/typestate_protocol_ratchet.py
```

Enforced mode (non-zero exit on threshold breach):

```bash
python3 scripts/complexity/typestate_protocol_ratchet.py --enforce
```

Current guardrails are encoded in:

- file limits: `FILE_LIMITS` in `scripts/complexity/typestate_protocol_ratchet.py`
- function limits: `FUNCTION_LIMITS` in `scripts/complexity/typestate_protocol_ratchet.py`

Policy:

- keep default mode warning-only for local exploration,
- use `--enforce` in quality gates to prevent complexity regressions in the
  typestate/protocol hot path.
