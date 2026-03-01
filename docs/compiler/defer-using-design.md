# `defer` and `using` Design

## Goal

Add a minimal, predictable scoped-cleanup mechanism that improves resource
ergonomics without introducing full destructor-based RAII yet.

This design provides:

- `defer` as the primitive scope-exit cleanup mechanism
- `using` as sugar for common resource-management patterns
- a clean path for std I/O ergonomics (`open_*` without mandatory manual close)
- room to generalize later to custom scoped resources without committing to
  Python-style dynamic context managers today

## Non-Goals

V1 explicitly does **not** attempt to provide:

- user-defined destructors / general RAII
- Python-style dynamic `__enter__` / `__exit__`
- fallible deferred cleanup with merged error propagation semantics
- async-aware scoped cleanup semantics

## Motivation

Today file handles and similar resources require explicit `close()` calls:

```machina
let file = io::open_read(path)?;
let text = file.text().read_all()?;
file.close()?;
```

This is workable but easy to forget and noisy in the common case.

We want a scoped resource story that:

- is easy to read
- composes with error unions and `?`
- is explicit about lifetime
- avoids hidden destructor behavior

## Overview

### Primitive

```machina
defer expr;
```

Registers `expr` to run when the current lexical scope exits.

### Sugar

```machina
using file = io::open_read(path)? {
    let text = file.text().read_all()?;
}
```

Desugars to:

```machina
{
    let file = io::open_read(path)?;
    defer file.close_ignore_error();
    let text = file.text().read_all()?;
}
```

`using` is pure surface sugar. `defer` is the only core mechanism.

## Syntax

### `defer`

Statement form:

```machina
defer expr;
```

Examples:

```machina
defer unlock(mutex);
defer tmp.cleanup();
defer file.close_ignore_error();
```

### `using`

V1 uses a block form only:

```machina
using name = expr {
    ...
}
```

Example:

```machina
using file = io::open_read(path)? {
    let text = file.text().read_all()?;
    println(text);
}
```

We deliberately do not support a statement-only `using name = expr;` form.
The block makes the cleanup lifetime explicit at the call site and keeps
`using` visually distinct from `let`.

## `defer` Semantics

### Scope exit

A deferred expression runs whenever its enclosing lexical scope exits:

- normal fallthrough
- `return`
- `break`
- `continue`
- early exit through `?`

This matches the intuition that `?` leaves the current scope just like `return`.

### Ordering

Multiple defers in the same scope run in reverse order of registration (LIFO).

Example:

```machina
defer println("first");
defer println("second");
```

Behavior on scope exit:

1. `println("second")`
2. `println("first")`

### Lexical ownership

`defer` is lexical and scope-based. It does not introduce dynamic cleanup
regions or stack-unwinding objects.

The deferred expression is evaluated at scope exit, using the bindings captured
from the surrounding scope in the same way closures capture surrounding values.

### Fallibility

V1 deferred expressions must be non-fallible.

That means a deferred expression may not use `?`, and must not have an
error-union type.

Allowed:

```machina
defer file.close_ignore_error();
defer unlock(mutex);
```

Rejected:

```machina
defer file.close()?;
defer may_fail();
```

This keeps `defer` semantics simple and avoids complicated cleanup/error-merging
rules.

## `using` Semantics

`using` is a convenience form for a resource binding with automatic cleanup.

The initializer may produce a temporary that is immediately consumed by the
bound value. For example, the following is valid and should not require an
intermediate `let`:

```machina
using writer = io::open_write(path)?.text() {
    writer.write_all("hello\n")?;
}
```

### Lowering

```machina
using name = expr {
    body...
}
```

lowers to:

```machina
{
    let name = expr;
    defer name.close_ignore_error();
    body...
}
```

This implies:

- `expr` is evaluated immediately
- cleanup registration happens only if binding succeeds
- resource lifetime is exactly the `using` block

### Resource contract

V1 `using` is intentionally narrow:

- it is intended for types that expose a non-fallible cleanup method
- std I/O should provide such a method for scoped cleanup

This keeps the language feature simple while still being broadly useful.

### Name binding

`using` binds a normal name in the current scope. After lowering, the bound
name behaves exactly like a `let` binding.

## I/O Integration

The immediate target is std I/O ergonomics.

Example:

```machina
fn main() -> () | IoError {
    using file = io::open_read("notes.txt")? {
        let text = file.text().read_all()?;
        println(text);
    }
}
```

Lowered behavior:

- `open_read(...)` remains fallible
- `read_all()` remains fallible
- scope-exit cleanup uses a non-fallible file cleanup path

### Cleanup method naming

Keep explicit checked close separate from scope-exit cleanup:

- `close() -> () | IoError`
- `close_ignore_error() -> ()`

`using` should target the non-fallible cleanup path.

We should avoid `close(quiet: bool)` because a boolean argument makes call-site
semantics unclear and couples two different behaviors into one API surface.

## Lowering Strategy

### Parser / AST

Add dedicated statement nodes first:

- `StmtKind::Defer { expr }`
- `StmtKind::Using { name, init }`

Do not desugar in the parser.

### Type checking

`defer`:

- typecheck `expr`
- reject error-union result types
- reject `?` within deferred expressions if not already implied by the above

`using`:

- typecheck initializer normally
- verify the bound type supports the cleanup method used by lowering

### Lowering / normalization

Lower `using` to:

1. ordinary binding statement
2. synthesized `defer` statement that invokes cleanup
3. `using` body inside the same synthesized block

Implement `defer` using scope-aware cleanup tracking in elaborate, not in the
parser.

Current implementation direction:

1. syntax desugar lowers `using` to a scoped `let + defer`
2. elaborate tracks active defers per lexical block
3. `return`, `break`, and `continue` get cleanup expressions inserted directly
   before the control transfer
4. bare `?` records the active cleanup set in a side table
5. backend try-propagation lowering emits that cleanup before returning the
   propagated error

This keeps cleanup semantics explicit without cloning arbitrary subtrees for
each control-flow shape.

## Control Flow Behavior

### `return`

```machina
fn f() {
    defer cleanup();
    return;
}
```

`cleanup()` runs before returning.

### `break` / `continue`

If a loop body contains a `defer`, it runs whenever control leaves that body
scope.

Example:

```machina
while cond {
    defer cleanup();
    break;
}
```

`cleanup()` runs before the `break` leaves the loop body scope.

### Nested scopes

Defers belong to the nearest enclosing lexical scope.

Example:

```machina
defer outer();
{
    defer inner();
}
```

Exit order:

1. `inner()`
2. `outer()`

## Diagnostics

### `defer`

Useful diagnostics:

- deferred expression is fallible
- deferred expression uses `?`
- deferred expression type is invalid for statement position

Suggested message style:

- `` `defer` requires non-fallible cleanup; expression has type `() | IoError` ``

### `using`

Useful diagnostics:

- bound type does not support scope-exit cleanup
- cleanup method is ambiguous
- cleanup method is fallible

Suggested message style:

- `` `using` requires a non-fallible cleanup method for type `ReadFile` ``

## Future Evolution

### 1. Custom scoped resource protocol

If we later want Python-like custom context behavior, do it through a static
protocol/trait rather than dynamic `__enter__` / `__exit__`.

Conceptually:

- enter/acquire step
- bound value
- non-fallible exit step

But this is explicitly deferred until we need it.

### 2. Additional `using` sugar

If we later want a shorter form, we can revisit statement-style `using`, but
V1 intentionally keeps the block form as the only spelling.

### 3. Fallible scope-exit cleanup

Not part of V1.

If we later decide to support it, we will need explicit semantics for:

- cleanup failure on normal scope exit
- cleanup failure while another error is already propagating
- interaction with error unions and `main`

This should be designed deliberately, not accidentally through `defer`.

### 4. General RAII / destructors

This design does not block future destructor support, but does not depend on
it either.

`defer` + `using` should stand on their own as explicit scoped-cleanup tools.

## Recommendation

Implement in this order:

1. `defer` statement
2. scope-exit lowering / cleanup execution model
3. `using` sugar
4. std I/O integration using `close_ignore_error()`

This yields a useful, small feature set with clear semantics and low surprise.
