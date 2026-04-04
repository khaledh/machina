# Runtime Layers Design

Status: **Proposal**

## Problem

The current runtime is a monolithic C archive that mixes three unrelated
concerns under one `__rt_*` / `__mc_*` namespace:

- primitives the compiler needs to emit working code (memcpy, trap)
- services that assume a POSIX hosted process (print, file I/O, argv)
- managed infrastructure for the machine/linear-type scheduler

When `platform = "none"`, the entire archive is dropped and the compiler must
inline everything or leave symbols unresolved. This makes it impossible to
incrementally adopt runtime capabilities as a new platform (like Fusion OS)
matures.

## Architecture

There are **four layers** separated by **three interfaces**.

```
┌──────────┐
│ Compiler │   lowers language features against capability requirements
└────┬─────┘
     │  Compiler ABI — abstract capabilities the compiler assumes
┌────┴──────────────────────┐
│ Portable Runtime          │   platform-agnostic logic (formatting, strings, ...)
└────┬──────────────────────┘
     │  Platform ABI — thin abstract primitives (write_bytes, alloc, ...)
┌────┴──────────────────────┐
│ Platform Glue             │   adapts platform ABI to real environment
└────┬──────────────────────┘
     │  Platform Primitives — raw environment (syscalls, ports, MMIO, ...)
┌────┴──────────────────────┐
│ Platform                  │   POSIX process, bare metal, Fusion OS, ...
└───────────────────────────┘
```

### Interface 1: Compiler ABI

What the compiler depends on. Defined as **capabilities**, not symbol names.
A capability may be satisfied by a runtime call, by direct codegen, or may be
unavailable for a given target.

Examples:
- "copy N bytes" — hosted: `call __rt_memcpy`; bare x86-64: inline `rep movsb`
- "abort with diagnostic" — hosted: `call __rt_trap`; bare: inline `ud2`
- "format u64 to decimal into buffer" — runtime call on all targets

The compiler never hard-codes "which symbol to call" — it emits a capability
request and the backend resolves it per target.

### Interface 2: Platform ABI

What the portable runtime calls into. This is a small set of abstract
primitives that a platform glue layer must implement:

| Primitive        | Description                        |
|------------------|------------------------------------|
| `write_bytes`    | emit bytes to the diagnostic sink  |
| `abort`          | terminate execution                |
| `alloc`          | allocate heap memory               |
| `realloc`        | resize heap allocation             |
| `free`           | release heap memory                |

This list grows slowly and only when the portable runtime genuinely needs
a new environmental primitive.

### Interface 3: Platform Primitives

How the glue layer talks to the actual environment. This is platform-specific
and not standardized by the compiler:

| Platform   | `write_bytes`         | `alloc`         | `abort`       |
|------------|-----------------------|-----------------|---------------|
| POSIX      | `write(2)` to fd      | `malloc(3)`     | `_exit(2)`    |
| Bare       | debugcon `out 0xE9`   | *(unavailable)* | `ud2`         |
| Fusion OS  | syscall               | syscall         | syscall       |

## Capabilities

The portable runtime is **decomposable by capability**. Each capability is an
independent module with explicit dependencies. A platform adopts capabilities
incrementally — it does not have to support the full surface.

```
Capability        Requires             Provides (RuntimeFn variants)
─────────────────────────────────────────────────────────────────────
mem_ops           (none)               MemCopy, MemSet
trap              write_bytes, abort   Trap
formatting        mem_ops, trap        FmtInit, FmtAppendBytes,
                                       FmtAppendU64, FmtAppendI64,
                                       FmtAppendBool, FmtFinish,
                                       U64ToDec, I64ToDec,
                                       U64ToBin, U64ToOct, U64ToHex
heap              alloc, realloc,      Alloc, Realloc, Free,
                  free                 SetAllocTrace
strings           heap, mem_ops, trap  StringFromBytes, StringEnsure,
                                       StringRetain, StringDrop, StringEq,
                                       StringAppendBytes, StringAppendBool,
                                       StringLines, StringSplit
collections       heap, strings        DynArray*, Set*, Map*
console           write_bytes          Print
process           console, strings     ProcessArgsInit (args, stdin, file I/O)
machines          heap, strings,       MachineEmit*, MachineRegister*
                  collections
```

### Dependency graph

```
machines ──> collections ──> strings ──> heap ──> [alloc, realloc, free]
                                  │        │
console ──> [write_bytes]         │        └──> mem_ops
                                  │               │
process ──> console, strings      └──> trap ──> [write_bytes, abort]
                                         │
formatting ──> mem_ops, trap             └──> mem_ops
```

Square brackets denote platform ABI primitives (provided by the glue layer).
Everything else is portable runtime code.

## Target capability profiles

Each target declares which capabilities are available:

| Target         | Capabilities                                          |
|----------------|-------------------------------------------------------|
| arm64-macos    | all                                                   |
| x86-64-linux   | all                                                   |
| x86-64-bare    | mem_ops, trap (inline)                                |

As Fusion OS develops, its profile grows:

| Fusion OS stage | Added capabilities                                   |
|-----------------|------------------------------------------------------|
| Day 1           | mem_ops, trap                                        |
| + debugcon      | formatting, console                                  |
| + page alloc    | heap                                                 |
| + heap alloc    | strings, collections                                 |
| + syscalls      | process, machines                                    |

The compiler validates at compile time that user code does not use language
features requiring capabilities the target lacks. For example, on a target
with only `mem_ops + formatting`, using an owned string is a compile error,
but `f"count = {n}"` passed to a function taking `string` (view) works.

## Classification of current symbols

### Compiler ABI (capability: mem_ops)
| Symbol            | Purpose                  | Bare strategy      |
|-------------------|--------------------------|---------------------|
| `__rt_memcpy`     | struct/array copy        | inline `rep movsb`  |
| `__rt_memset`     | zero-init                | inline `rep stosb`  |

### Compiler ABI (capability: trap)
| Symbol            | Purpose                  | Bare strategy      |
|-------------------|--------------------------|---------------------|
| `__rt_trap`       | runtime check failure    | inline `ud2`        |

### Portable runtime (capability: formatting)
| Symbol                | Purpose                               |
|-----------------------|---------------------------------------|
| `__rt_fmt_init`       | init fixed-buffer formatter           |
| `__rt_fmt_append_bytes` | append literal bytes to formatter   |
| `__rt_fmt_append_u64` | format + append unsigned decimal      |
| `__rt_fmt_append_i64` | format + append signed decimal        |
| `__rt_fmt_append_bool`| format + append true/false            |
| `__rt_fmt_finish`     | finalize formatter into string view   |
| `__rt_u64_to_dec`     | u64 -> decimal into buffer            |
| `__rt_i64_to_dec`     | i64 -> decimal into buffer            |
| `__rt_u64_to_bin`     | u64 -> binary into buffer             |
| `__rt_u64_to_oct`     | u64 -> octal into buffer              |
| `__rt_u64_to_hex`     | u64 -> hex into buffer                |

Note: the formatting capability is **self-contained**. `FmtInit` through
`FmtFinish` operate on a caller-provided stack buffer. They do not allocate.
They need `mem_ops` (for the byte copies) and `trap` (for buffer overflow),
but **not** `heap` or `strings`. This means f-strings that only interpolate
integers and booleans can work on bare targets with no allocator — the result
is a string view into a stack buffer.

### Portable runtime (capability: heap)
| Symbol                | Purpose                               |
|-----------------------|---------------------------------------|
| `__rt_alloc`          | allocate                              |
| `__rt_realloc`        | resize                                |
| `__rt_free`           | deallocate                            |
| `__rt_set_alloc_trace`| toggle allocation tracing             |

### Portable runtime (capability: strings)
| Symbol                    | Purpose                           |
|---------------------------|-----------------------------------|
| `__rt_string_from_bytes`  | create view from byte slice       |
| `__rt_string_ensure`      | ensure owned capacity             |
| `__rt_string_retain`      | increment refcount                |
| `__rt_string_drop`        | decrement refcount / free         |
| `__rt_string_eq`          | byte equality                     |
| `__rt_string_append_bytes`| append bytes (promotes to owned)  |
| `__rt_string_append_bool` | append true/false                 |
| `__rt_string_lines`       | split into lines                  |
| `__rt_string_split`       | split by delimiter                |

### Portable runtime (capability: collections)
| Symbol                             | Purpose                     |
|------------------------------------|-----------------------------|
| `__rt_dyn_array_ensure`            | ensure array capacity       |
| `__rt_dyn_array_append_elem`       | append element              |
| `__rt_dyn_array_retain`            | increment refcount          |
| `__rt_dyn_array_release`           | decrement refcount          |
| `__rt_dyn_array_free_backing`      | free backing store          |
| `__rt_set_insert_string`           | set insert (string key)     |
| `__rt_set_contains_string`         | set contains (string key)   |
| `__rt_set_remove_string`           | set remove (string key)     |
| `__rt_set_clear_string`            | set clear (string keys)     |
| `__rt_set_drop_string`             | set drop (string keys)      |
| `__rt_set_insert_elem`             | set insert (value key)      |
| `__rt_set_contains_elem`           | set contains (value key)    |
| `__rt_set_remove_elem`             | set remove (value key)      |
| `__rt_set_clear`                   | set clear (value keys)      |
| `__rt_map_insert_or_assign_string_key` | map insert (string key) |
| `__rt_map_contains_string_key`     | map contains (string key)   |
| `__rt_map_get_value_string_key`    | map get (string key)        |
| `__rt_map_remove_string_key`       | map remove (string key)     |
| `__rt_map_clear_string_keys`       | map clear (string keys)     |
| `__rt_map_drop_string_keys`        | map drop (string keys)      |
| `__mc_map_table_*`                 | map ops (value keys)        |
| `__rt_map_iter_init`               | map iterator init           |
| `__rt_map_iter_is_done`            | map iterator done check     |
| `__rt_map_iter_load_string_key`    | map iterator load (string)  |
| `__rt_map_iter_advance`            | map iterator advance        |

### Platform services (capability: console)

Console is a general text output capability — not tied to hosted processes.
Debugcon, serial ports, firmware text output, and future Fusion OS kernel
console all fit here.

| Symbol            | Purpose                               |
|-------------------|---------------------------------------|
| `__rt_print`      | write string + optional newline       |

### Platform services (capability: process)

Process is specifically hosted/process-oriented functionality: argument
vectors, file descriptors, stdin, errno semantics.
| Symbol                    | Purpose                           |
|---------------------------|-----------------------------------|
| `__rt_process_args_init`  | capture argc/argv at startup      |
| `__rt_args_len`           | return argument count             |
| `__rt_arg_at`             | return argument by index          |
| `__rt_stdin_read_line`    | read line from stdin              |
| `__rt_file_open_read`     | open file read-only               |
| `__rt_file_open_write`    | open file write (truncate)        |
| `__rt_file_open_rw`       | open file read-write              |
| `__rt_file_read`          | read bytes from fd                |
| `__rt_file_write`         | write bytes to fd                 |
| `__rt_file_close`         | close fd                          |
| `__rt_file_read_all_text` | read entire file as string        |
| `__rt_file_last_errno`    | last errno from file ops          |

### Managed services (capability: machines)
| Symbol                                          | Purpose              |
|-------------------------------------------------|----------------------|
| `__mc_machine_emit_send`                        | send message         |
| `__mc_machine_emit_request`                     | request/reply        |
| `__mc_machine_emit_reply`                       | reply to request     |
| `__mc_machine_runtime_register_thunk_meta_u64`  | register thunk       |
| `__mc_machine_runtime_register_payload_drop_u64`| register drop hook   |
| `__mc_machine_runtime_register_descriptor_u64`  | register descriptor  |

## F-strings on bare targets: a concrete example

With the capability model, f-strings on bare targets work without special
compiler logic:

1. The compiler lowers `f"count = {n}\n"` into the same `FmtInit` /
   `FmtAppendBytes` / `FmtAppendU64` / `FmtFinish` sequence on all targets.

2. The formatting capability is available because:
   - `mem_ops`: provided (inline or linked)
   - `trap`: provided (inline or linked)
   - The formatting functions themselves are portable C that only call
     `__rt_memcpy` and `__rt_trap` — no allocation, no platform I/O.

3. `FmtFinish` produces a **string view** into the stack buffer. This is
   passed to `debugcon_write(text: string)` which is a user-provided
   function backed by assembly.

4. No heap, no owned strings, no platform glue beyond what the user already
   wrote.

The only prerequisite is that the bare target's profile includes the
`formatting` capability, which transitively requires `mem_ops` and `trap`.

## Migration path

### Phase 1: Document and classify (this document)

Classify every `__rt_*` / `__mc_*` symbol by capability. No code changes.

### Phase 2: Make formatting work on bare targets

- Link the formatting + conversion runtime C files into a minimal
  `librt_core.a` for bare targets (they have no POSIX dependencies)
- Alternatively, rewrite them in Machina or hand-written assembly
- Validate with f-strings in `kernel.mc` via debugcon

### Phase 3: Compiler capability checking

- Associate each `RuntimeFn` variant with a capability
- Derive target capabilities from platform + machina.toml
- Emit compile errors when user code requires unavailable capabilities

### Phase 4: Platform glue extraction

- Factor POSIX-specific code out of `runtime/*.c` into a `runtime/posix/`
  glue layer
- The remaining portable code becomes `runtime/core/`
- Bare and Fusion targets provide their own glue layers

### Phase 5: Fusion OS platform glue

- As kernel services come online, implement glue for each new capability
- Profile grows from `mem_ops + trap` toward full feature parity

## Open questions

1. **Capability declaration syntax** — should machina.toml list capabilities
   explicitly, or should they be inferred from which glue symbols are
   available at link time? Explicit is safer for compile-time diagnostics.

2. **Trap on bare** — currently bare codegen inlines `ud2`. Should it instead
   link a minimal trap handler that writes to debugcon before halting? That
   would give better diagnostics at the cost of pulling in `formatting`.

3. **Owned strings on bare** — once a bare target has a heap allocator, owned
   strings could work. Should the capability check be purely compile-time, or
   should we also support a "link-time discovery" mode?

4. **Naming** — should we keep the `__rt_` prefix for all compiler-ABI
   symbols and `__mc_` for internal implementation, or adopt a new naming
   scheme that reflects the layers?
