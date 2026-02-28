# I/O Conformance Spec (V1)

This document defines the cross-platform behavior contract for Machina file I/O.
It is an internal compiler/runtime spec used by stdlib and runtime implementers.

## 1) Scope

V1 scope is file I/O only:

- open (read, write, read-write, append/create variants)
- read
- write
- flush/sync
- seek/tell
- close

Out of scope for V1:

- sockets/pipes/tty-specific semantics
- async/non-blocking APIs
- file locking APIs
- mmap

## 2) Layering Contract

The I/O stack has three layers:

1. `std::io` (public, portable): typestate-first APIs and ergonomic helpers.
2. `std::sys::io` (internal/advanced): thin low-level wrappers.
3. runtime OS ports (`runtime/*`): platform-specific syscall bindings.

Rule: platform divergence must be handled at layers 2/3. Layer 1 behavior must
remain stable across targets.

## 3) Normative Semantics

Keywords: **MUST**, **SHOULD**, **MAY**.

### 3.1 Open

- Open calls MUST return either a valid handle or `IoError`.
- Path interpretation MUST be delegated to the host OS (no implicit
  normalization by Machina).
- Implementations MUST map OS failures to `IoErrorKind` + `os_code`.

### 3.2 Read

- `read(buf)` MUST return `0` at EOF.
- `read(buf)` MAY return fewer bytes than requested.
- If `buf.len == 0`, `read` MUST return `0` and MUST NOT fail.
- Transient interruption (`EINTR` / equivalent) MUST be retried internally.

### 3.3 Write

- `write(buf)` MAY return fewer bytes than requested (partial write).
- If `buf.len == 0`, `write` MUST return `0` and MUST NOT fail.
- Transient interruption (`EINTR` / equivalent) MUST be retried internally.

### 3.4 Flush / Sync

- `flush` MUST ensure buffered userspace/runtime data is submitted to the OS.
- `sync` MUST request durable persistence according to OS guarantees.
- `flush` and `sync` are not equivalent and MUST remain separate operations.

### 3.5 Seek / Tell

- Offsets are modeled as `u64` in Machina APIs.
- Implementations MUST fail with `IoErrorKind::InvalidInput` when offset cannot
  be represented by the platform API.

### 3.6 Close

- Successful `close` releases OS resources.
- Close failure MUST be surfaced as `IoError` (not silently ignored).
- Public typestate APIs SHOULD make double-close unrepresentable.
- Runtime ports MUST NOT blindly retry `close` on `EINTR`; descriptor reuse can
  make retries unsafe. Ports should perform a single close attempt and surface
  any resulting error code.

## 4) Text/Binary and Encoding

- V1 file I/O MUST be binary-preserving by default.
- No implicit newline translation (for example, no CRLF conversion layer).
- `string` convenience helpers in `std::io` MUST perform explicit UTF-8 decode
  or encode and return decode/encode errors as `IoError`.

## 5) Error Model

`IoError` MUST include:

- portable kind (`IoErrorKind`)
- raw OS code (`os_code`) when available
- operation name (for diagnostics)
- optional path context

Minimum portable kinds for V1:

- `NotFound`
- `PermissionDenied`
- `AlreadyExists`
- `WouldBlock`
- `Interrupted`
- `InvalidInput`
- `InvalidData`
- `BrokenPipe`
- `UnexpectedEof`
- `Unsupported`
- `Other`

Rule: mapping MUST be stable and deterministic per OS port. Unknown codes map
to `Other` while preserving `os_code`.

## 6) Platform Port Requirements

Each OS port MUST provide:

- open/read/write/flush/sync/seek/close primitives
- error code mapping table to `IoErrorKind`
- conformance test pass for shared I/O suite

Each port SHOULD provide:

- notes for intentional behavioral differences required by the host OS

## 7) Conformance Tests

Conformance test suite MUST include:

1. Open missing file -> `NotFound`
2. Read EOF returns `0`
3. Zero-length read/write returns `0`
4. Partial-write tolerance (`write_all` helper loops correctly)
5. Seek + read consistency
6. Close actually releases file (re-open check)
7. Error mapping sanity for common failures
8. Binary roundtrip (no newline translation)

Tests MUST run on all supported targets in CI.

## 8) Typestate API Implications

`std::io` typestate APIs SHOULD encode capability/lifecycle constraints:

- read-only handles expose read APIs only
- write-only handles expose write APIs only
- close transitions to closed state

But conformance is defined at operation semantics level, independent of exact
surface syntax.

## 9) Future Extension Points (Non-Breaking)

Reserved for later without changing V1 guarantees:

- non-blocking mode
- deadlines/timeouts
- vectored I/O
- socket and pipe conformance profiles
- async runtime integration

---

If a behavior is not explicitly specified here, the runtime port should keep
the behavior as close as possible to host OS semantics while preserving stable
`IoErrorKind` mapping and safety invariants.
