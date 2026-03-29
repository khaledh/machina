# Default Argument Values Design

## Status

Proposed.

Builds on: `docs/compiler/named-args-design.md`.

## Goals

- Allow function and method parameters to declare default values.
- Let callers omit arguments for parameters that have defaults.
- Integrate cleanly with the named-argument matching infrastructure.
- Keep overload resolution predictable.

## Non-Goals (V1)

- Default values for `out`, `inout`, or `sink` parameters.
- Default values that reference other parameters (dependent defaults).
- Default values in closure parameter lists.
- Default values in trait method declarations (trait impls provide bodies).

## Surface Syntax

### Declaration

Default values use `= expr` after the parameter type:

```machina
fn connect(host: string, port: i32 = 443, timeout: i32 = 30) -> Connection
```

Rules:
- Default parameters must be trailing — once a parameter has a default, all
  subsequent parameters must also have defaults.
- Only `in`-mode parameters may have defaults. `inout`/`out`/`sink` modes
  require explicit caller participation and are incompatible with silent
  default injection.

### Call site

Callers may omit trailing defaulted arguments:

```machina
connect("example.com")              // port=443, timeout=30
connect("example.com", 8080)        // timeout=30
connect("example.com", 8080, 5)     // all explicit
```

With named arguments, callers can skip middle defaults:

```machina
connect("example.com", timeout: 5)  // port=443, timeout=5
```

### Methods

Same rules apply:

```machina
HttpClient :: {
    fn request(self, path: string, method: string = "GET") -> Response
}

client.request("/api/data")                   // method="GET"
client.request("/api/data", method: "POST")   // explicit
```

## Semantics

### Evaluation: per-call-site

Default expressions are evaluated fresh at each call site where they are
used. This avoids the Python mutable-default pitfall and matches Kotlin,
Swift, and C++ behavior.

Concretely: the compiler injects a copy of the default expression AST at
each call site that omits the argument. The injected expression goes through
the normal elaboration and lowering pipeline.

### Scope of default expressions

Default expressions are evaluated in the **declaration scope** of the
function, not the caller's scope. They may reference:

- Literals
- Top-level constants and functions visible at the declaration site
- Other in-scope names from the declaring module

They may **not** reference:
- Other parameters of the same function
- Local variables from any call site

This is enforced by resolving default expressions during the function's own
name resolution pass.

### Trailing-only rule

```machina
fn f(a: i32 = 0, b: i32)      // ERROR: non-default parameter after default
fn f(a: i32, b: i32 = 0)      // OK
fn f(a: i32 = 0, b: i32 = 1)  // OK
```

This is validated during parsing.

## Interaction with Overloads

### Arity overlap rejection

Two overloads of the same name must not have overlapping callable arity
ranges. The callable arity range of a function is
`[required_params, total_params]`.

```machina
fn f(a: i32, b: i32 = 0)  // callable with 1 or 2 args
fn f(a: i32)               // callable with 1 arg
// ERROR: arity overlap at 1 — ambiguous call f(42)
```

This is checked at the declaration level (during collection or resolve), not
at call sites. If two overloads can accept the same argument count, the
program is rejected regardless of whether an ambiguous call actually exists.

Rationale: rejecting at declaration time is simpler and more predictable
than call-site disambiguation scoring. It also prevents silent breakage when
a new overload is added.

### Candidate filtering

`named_call_candidates` and `method_call_candidates` currently filter by
`sig.params.len() == arity`. With defaults, this becomes:

```
min_arity(sig) <= arity && arity <= sig.params.len()
```

where `min_arity` is the count of parameters without defaults.

## Compiler Pipeline Changes

### 1. Parser

**`parse_param`** (`src/core/parse/func.rs`):

After parsing the type, check for `=`:

```
mode name: type [= expr]
```

If `=` is present, parse the expression. Store in `Param.default`.

**Trailing-only enforcement**: Track whether the previous parameter had a
default. If it did and the current one does not, emit a parse error.

**`parse_closure_param`**: No change — closure params do not support defaults
in V1.

### 2. AST

Extend `Param`:

```rust
pub struct Param {
    pub id: NodeId,
    pub ident: String,
    pub typ: TypeExpr,
    pub mode: ParamMode,
    pub default: Option<Expr>,  // NEW
    pub span: Span,
}
```

### 3. Name Resolution

Default expressions need name resolution in the function's declaring scope.
The resolver already walks function signatures — it would additionally walk
`param.default` expressions if present.

### 4. Type Checking

**`CollectedParamSig`**: Add `has_default: bool`.

**Candidate filtering**: Change arity filter from `== arity` to range check
using `min_arity..=max_arity`.

**`match_arg_labels_to_param_names`** (`call_args.rs`): The existing
completeness check (`MissingParam`) needs to be relaxed — a missing
parameter is only an error if it has no default. The matcher needs a
`has_default` slice alongside `param_names`:

```rust
fn match_arg_labels_to_param_names(
    arg_labels: &[Option<ArgLabel>],
    param_names: &[String],
    has_default: &[bool],        // NEW
) -> Result<MatchResult, CallArgMatchError>
```

The return type changes to carry both the arg order and the set of
parameters that need default injection.

**Arity overlap check**: During callable collection, for each pair of
overloads with the same name, check that their arity ranges do not overlap.
Emit a diagnostic if they do.

**Type checking default expressions**: Each default expression must be type-
checked against its parameter's declared type. This happens during the
function's own type checking pass, not at call sites.

### 5. Elaboration

When a call omits arguments for defaulted parameters, elaboration injects
the default expressions. The `CallSig.arg_order` already identifies which
parameters were matched. Elaboration checks which parameters are unmatched
and clones their default expression AST into the call.

This means `CallSig` needs to carry enough information to identify
defaulted parameters. Options:

**Option A — Store defaults in `CallSig`**: Add the default `Expr` nodes
directly. Heavy, but self-contained.

**Option B — Look up defaults from the def table**: Elaboration already has
access to the def table. It can look up the function's parameter defaults by
`DefId`. Lighter, avoids cloning expressions into every call sig.

Preferred: **Option B**. The def table is the canonical source for function
signatures. Elaboration uses the `CallSig.arg_order` to identify which
params are present and injects defaults for the rest.

### 6. IR Lowering and Codegen

No changes. After elaboration, all calls have full argument lists.

### 7. Module Interface (`.mci`)

Default values affect the callable arity range, which matters for external
callers. The `.mci` format should record which parameters have defaults.
The actual default expressions are **not** serialized — external callers
compile against source, not `.mci`, so the expressions are available from
the source file.

Add `has_default: bool` to each parameter entry in the interface.

## Diagnostics

| Situation | Error message |
|-----------|--------------|
| Non-default after default | `parameter 'b' has no default value but follows parameter 'a' which does` |
| Default on non-`in` param | `default values are only allowed for 'in' parameters` |
| Arity overlap | `overloads of 'f' have overlapping arity ranges` |
| Missing arg, no default | `missing argument for parameter 'x'` (existing) |

## Open Questions

1. **Should default expressions be const-only in V1?** Restricting to
   compile-time constants simplifies evaluation but limits expressiveness.
   Per-call-site evaluation of arbitrary expressions is more useful and
   not significantly harder — the expression just gets cloned into the
   call site AST before elaboration. Leaning toward allowing arbitrary
   expressions.

2. **Should defaults be visible in hover/signature help?** Yes — showing
   `port: i32 = 443` in signature help is valuable. This requires threading
   the default expression's source text into tooling metadata.

## Implementation Plan

### Phase 1 — AST + Parser
- Add `default: Option<Expr>` to `Param`
- Parse `= expr` after parameter type
- Enforce trailing-only rule
- Enforce `in`-mode-only rule
- Parser tests

### Phase 2 — Resolution + Type Checking
- Resolve default expressions in function scope
- Type-check defaults against parameter types
- Add `has_default: bool` to `CollectedParamSig`
- Relax arity filter to range check
- Update `match_arg_labels_to_param_names` to accept missing-with-default
- Add arity overlap check for overloads
- Type checking tests

### Phase 3 — Elaboration
- Inject default expressions for omitted arguments
- Look up defaults via def table using `CallSig` info
- End-to-end tests: calls with omitted defaults lower correctly

### Phase 4 — Tooling + Polish
- Show defaults in signature help
- Update `.mci` with `has_default` per parameter
- Diagnostic polish
