# Typestate / Protocol / Machine Complexity Baseline

Date: 2026-02-19

This document captures the initial complexity baseline for Milestone 12 issue #139 and identifies the highest-ROI simplification targets.

## Scope

- `src/core/typestate/*`
- `src/core/protocol/*`
- `src/core/typecheck/validate.rs`
- `src/core/semck/protocol_progression.rs`
- `src/core/semck/protocol_progression_check.rs`
- `runtime/machine/*`
- `runtime/tests/machine_runtime*.c` (test-only baseline)

## LOC Baseline

### Core compiler path

| Area | LOC | Notes |
|---|---:|---|
| `src/core/typestate/*` | 3,785 | Dominated by AST desugar + managed API generation |
| `src/core/protocol/*` | 417 | Mostly `index.rs` fact building |
| `validate.rs + protocol semck files` | 1,798 | Protocol checks + reply-cap checks + progression extraction/check |

### Runtime machine path

| Area | LOC | Notes |
|---|---:|---|
| `runtime/machine/*` | 3,971 | Dominated by `runtime.c` + `runtime.h` |
| `runtime/tests/machine_runtime*.c` | 2,373 | Good coverage but partly redundant scenarios |

## Largest Files (Hotspot by size)

### Compiler

| File | LOC |
|---|---:|
| `src/core/typestate/mod.rs` | 1,834 |
| `src/core/typecheck/validate.rs` | 1,296 |
| `src/core/typestate/managed_api.rs` | 737 |
| `src/core/typestate/support_types.rs` | 392 |
| `src/core/semck/protocol_progression.rs` | 340 |
| `src/core/protocol/index.rs` | 404 |

### Runtime

| File | LOC |
|---|---:|
| `runtime/machine/runtime.c` | 1,462 |
| `runtime/machine/runtime.h` | 683 |
| `runtime/machine/descriptor.c` | 512 |
| `runtime/machine/emit.c` | 363 |
| `runtime/machine/bridge.c` | 351 |

## Largest Rust Functions (approximate by span)

| Function | File | Approx LOC |
|---|---|---:|
| `machine_handle_method_block` | `src/core/typestate/managed_api.rs` | 489 |
| `lower_spawn_func` | `src/core/typestate/managed_api.rs` | 242 |
| `rewrite_machine_request_method_destinations` | `src/core/typestate/rewrite_handles.rs` | 236 |
| `desugar_typestate` | `src/core/typestate/mod.rs` | 220 |
| `check_protocol_shape_conformance` | `src/core/typecheck/validate.rs` | 196 |
| `wrap_main_with_managed_runtime` | `src/core/typestate/managed_entrypoint.rs` | 170 |
| `analyze_typestate` | `src/core/typestate/mod.rs` | 149 |
| `ensure_machine_runtime_intrinsics` | `src/core/typestate/support_types.rs` | 124 |

## Duplication / Coupling Hotspots

1. **Protocol emit extraction duplicated across passes**
   - Typecheck protocol conformance extraction in `validate.rs`
   - Semck progression extraction in `protocol_progression.rs`
   - Shared concerns duplicated: payload type recovery, destination extraction, request/response-set handling, method-sugar handling.

2. **Typestate managed API generation has repeated ABI scaffolding**
   - `send`/`request` raw and typed variants repeat: runtime acquisition, payload packing, status checks, error-union branching.
   - Appears in one giant constructor function (`machine_handle_method_block`).

3. **Typestate desugar orchestration carries too many concerns**
   - `mod.rs` blends analysis, lowering, naming, sugar rewriting, transition rewriting, and constructor/spawn wiring.
   - High argument threading and local state maps create maintenance friction.

4. **`validate.rs` mixes unrelated validator domains**
   - Control-flow validation, protocol conformance, request/reply checks, and reply-cap linearity in one file.
   - Hard to reason about invariants and regressions independently.

5. **Runtime status/error mapping duplicated in generated surfaces**
   - Numeric status handling appears in multiple generated method templates.
   - Changes to runtime statuses risk fan-out edits.

## Complexity Reduction Targets (Milestone 12)

- Reduce core compiler path LOC (`typestate/* + protocol/* + validate/progression`) by **25-35%** through decomposition and dedup.
- Reduce top function spans to an upper bound of **~120 LOC** for non-runtime code (exceptions documented if justified).
- Ensure protocol emit extraction has **one canonical implementation** consumed by typecheck + semck.
- Keep behavior stable (existing typestate/protocol tests remain green).

## Refactor Order (high ROI first)

1. Unify protocol event extraction (issue #140).
2. Split typecheck validation domains (issue #141).
3. Split typestate desugar pipeline by concern (issue #142).
4. Deduplicate managed API generation scaffolding (issue #143).

## Notes

- Runtime cleanup is deferred to Milestone 13; compiler-side dedup first keeps risk lower while preserving test confidence.
- The baseline numbers are intended for trend tracking; exact values may shift with formatting and nearby feature work.
