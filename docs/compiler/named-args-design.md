# Named Arguments Design

## Status

Proposed.

Foundation for default argument values (future doc).

## Goals

- Allow call-site arguments to be labeled by parameter name.
- Improve readability for functions with multiple parameters of the same type.
- Build the name-based argument-to-parameter matching infrastructure that
  default values will later rely on.
- Keep the feature orthogonal to overload resolution — named arguments do not
  change which overload is selected, only how arguments are mapped.

## Non-Goals (V1)

- Default argument values (separate feature, builds on this).
- Reordering-only named arguments (all named arguments must still map to
  declared parameters; this is not a record/struct-literal syntax).
- Named arguments in closure types or function-pointer types.
- Changing the ABI or calling convention — named arguments are a front-end
  feature resolved before IR lowering.

## Surface Syntax

### Call site

Named arguments use `name: expr` syntax at the call site:

```machina
fn connect(host: string, port: i32, timeout: i32) -> Connection

// all positional (still valid)
connect("example.com", 8080, 30)

// all named
connect(host: "example.com", port: 8080, timeout: 30)

// mixed: positional prefix, then named
connect("example.com", timeout: 30, port: 8080)
```

### Positional-then-named rule

At any call site, arguments must follow this order:

1. Zero or more **positional** arguments (matched left-to-right to parameters).
2. Zero or more **named** arguments (matched by name, in any order).

Once a named argument appears, all subsequent arguments must also be named.
This is the same rule used by Python, Kotlin, and Swift.

```machina
// OK: positional prefix, then named
connect("example.com", port: 8080, timeout: 30)

// ERROR: positional after named
connect(host: "example.com", 8080, timeout: 30)
//                            ~~~~ positional argument after named argument
```

### Method calls

Methods follow the same rules. The receiver is not part of the argument list:

```machina
server.listen(port: 8080, backlog: 128)
```

### Declaration side

No changes to declaration syntax. Parameter names in function/method
signatures already serve as the label vocabulary:

```machina
fn connect(host: string, port: i32, timeout: i32) -> Connection
//         ^^^^          ^^^^       ^^^^^^^
//         these names become available as labels at call sites
```

## Semantics

### Argument matching algorithm

Given a call `f(a0, a1, ..., name_x: ex, name_y: ey, ...)`:

1. **Positional phase**: Match `a0` to param 0, `a1` to param 1, etc.
2. **Named phase**: For each `name: expr`, find the parameter with that name
   among the *remaining unmatched* parameters.
3. **Completeness check**: Every parameter must be matched exactly once.

Errors:
- Unknown parameter name → `no parameter named 'foo' in function 'bar'`
- Duplicate parameter → `argument for parameter 'foo' provided more than once`
- Missing parameter → `missing argument for parameter 'foo'`
- Positional after named → `positional argument after named argument`

### No semantic effect on overload resolution

Named arguments do not influence which overload is chosen. Overload
candidates are still filtered by arity (total argument count) and ranked by
type compatibility. The named-argument matching step runs *after* candidate
selection, when exactly one overload has been chosen.

Rationale: keeping overload selection and argument matching as separate phases
avoids combinatorial complexity (e.g. "does naming this argument change which
overload wins?"). Kotlin and Swift both take this approach.

### Parameter modes

Named arguments work with all parameter modes:

```machina
fn swap(inout a: i32, inout b: i32)

// positional
swap(inout x, inout y)

// named
swap(a: inout x, b: inout y)
```

The mode keyword (`inout`, `out`, `move`) stays at the call site adjacent to
the expression, after the label. This mirrors the existing call-arg syntax
where the mode precedes the expression.

### Self parameter

The `self` parameter in methods is never named at the call site. It is always
the receiver expression before the dot.

### Closures and function values

Named arguments are not supported when calling through function values or
closures. The `Fn` type erases parameter names. This matches most languages
(Kotlin, C#, etc.) — named arguments are a feature of declared callables,
not of function types.

```machina
let f: fn(i32, i32) -> i32 = add
f(1, 2)          // OK
f(a: 1, b: 2)    // ERROR: named arguments not supported for function values
```

## Compiler Pipeline Changes

### 1. Parser

**Call argument parsing** (`src/core/parse/expr.rs`):

Currently `parse_call_arg` produces a `CallArg` with `mode` and `expr`.
Add optional label detection:

- If the current token is an identifier followed by `:` (and the identifier
  is not a keyword like `inout`/`out`/`move`), parse it as a named argument.
- Lookahead is needed to distinguish `name: expr` from an expression that
  starts with an identifier (e.g., `x + 1`). The `:` after a bare identifier
  is the disambiguator.

**AST change** — extend `CallArg`:

```rust
pub struct CallArg {
    pub label: Option<ArgLabel>,  // NEW
    pub mode: CallArgMode,
    pub expr: Expr,
    pub init: InitInfo,
    pub span: Span,
}

pub struct ArgLabel {
    pub name: String,
    pub span: Span,
}
```

**Positional-then-named enforcement**: The parser tracks whether it has seen
a named argument. If a positional argument appears after a named one, emit a
parse error.

### 2. Name Resolution

No changes needed. Named arguments reference parameter names, not resolved
definitions. The matching happens during type checking.

### 3. Type Checking

**Argument reordering and matching** (`src/core/typecheck/solver/calls.rs`):

After overload selection picks a single candidate, a new step matches
call-site arguments to parameters:

```
match_args_to_params(call_args, candidate_sig) -> Result<Vec<(usize, &CallArg)>, Error>
```

This produces an ordered list of `(param_index, arg)` pairs, which is then
used for the existing type-compatibility checks. The existing `zip` over
`obligation.arg_terms` and `instantiated.params` is replaced with iteration
over the matched pairs.

**Arity filtering** (`named_call_candidates`, `method_call_candidates`):

These functions currently filter by `sig.params.len() == arity`. No change
needed — the total argument count still must equal the total parameter count.
(Default values will relax this later.)

**Error quality**: Type mismatch errors should reference the parameter name
when available:

```
argument for parameter 'timeout' has type 'string', expected 'i32'
```

### 4. Type Map / Call Signatures

The `CallSig` stored in the type map after solving needs to record the
parameter ordering that was resolved, so elaboration knows how to reorder
arguments. Two options:

**Option A — Store a permutation vector**: Add `arg_order: Vec<usize>` to
`CallSig`, where `arg_order[i]` is the parameter index that source call-arg
`i` maps to.

**Option B — Reorder args eagerly in type checking**: After matching, rewrite
the call's AST arguments into parameter order immediately. Downstream stages
see only positional, correctly-ordered arguments.

Preferred direction: **Option A**.

Rationale:

- Machina's type checker primarily produces side tables (`TypeMap`,
  `CallSigMap`, `GenericInstMap`) rather than mutating the AST in place.
- Rewriting AST call arguments during type checking would blur the stage
  contract and make later source-oriented diagnostics harder to reason about.
- Carrying an explicit mapping in `CallSig` keeps named-argument resolution as
  a typed-call fact, which better fits the existing architecture.

### 5. Elaboration

If `CallSig` carries `arg_order`, elaboration applies that permutation before
building the final call plan. This keeps the AST source-faithful while still
letting lowering see parameter-ordered arguments.

Concretely:

- `build_call_plan` continues to operate in parameter order
- elaboration reorders source call arguments according to `call_sig.arg_order`
- `elab_call_arg` still pairs each argument with its `CallParam`

This is a small elaboration change, but it preserves the cleaner stage
boundary.

### 6. IR Lowering and Codegen

No changes. By elaboration time, arguments are in parameter order.

### 7. Module Interface (`.mci`)

Parameter names are already emitted in `.mci` tooling metadata (they appear
in `CallableSignature`). No format changes needed — the existing parameter
names become the label vocabulary for external callers.

### 8. LSP / Tooling

**Signature help**: Already shows parameter names. Named-argument labels
should highlight the *current* parameter by matching the typed label against
parameter names (in addition to position-based highlighting).

**Completion**: When the cursor is inside a call's argument list after a
positional prefix, offer parameter-name completions for unmatched parameters.
This is a nice-to-have, not a blocker.

## Interaction with Future Features

### Default argument values

Named arguments provide the matching infrastructure. With defaults:

1. The arity filter relaxes from `== arity` to `min_arity <= arity <= max_arity`.
2. The matching algorithm identifies *missing* parameters.
3. Missing parameters with defaults get their default expressions injected
   (likely during elaboration or as a type-checking rewrite).
4. Missing parameters without defaults produce the existing
   `missing argument` error.

### Overload resolution with defaults

When defaults make multiple overloads match the same call arity, a scoring
tiebreaker is needed: prefer the overload that requires fewer default
injections (i.e., more arguments were provided explicitly). This is standard
in Kotlin/C++/Swift.

## Diagnostics

| Situation | Error message |
|-----------|--------------|
| Unknown label | `no parameter named 'foo' in function 'connect'` |
| Duplicate | `argument for parameter 'port' provided more than once` |
| Missing param | `missing argument for parameter 'timeout'` |
| Positional after named | `positional argument after named argument` |
| Named arg on fn value | `named arguments not supported for function values` |
| Label matches but wrong type | `argument for parameter 'port' has type 'string', expected 'i32'` |

## Open Questions

1. **Should the parser allow `_: expr` as an explicitly positional argument
   after named arguments?** Some languages allow this. Likely not needed for
   V1 — the positional-then-named rule is simpler.

2. **Trailing closures**: If Machina adds trailing closure syntax later,
   should a trailing closure be treated as a named argument for the last
   parameter? Defer to that design.

3. **Struct literals vs named args ambiguity**: Both use `name: expr` syntax.
   Currently not a problem because struct literals use `TypeName { ... }` and
   call arguments use `f(...)`. Keep it that way.

## Implementation Plan

### Phase 1 — AST + Parser
- Add `label: Option<ArgLabel>` to `CallArg`
- Parse `name: expr` in call argument lists
- Enforce positional-then-named ordering in parser
- Existing code unaffected (all labels are `None`)

### Phase 2 — Type Checking
- Add `match_args_to_params` step after overload selection
- Record argument-to-parameter mapping in `CallSig`
- Thread matched arguments through existing type compatibility checks
- Update error messages to reference parameter names

### Phase 3 — Diagnostics + Polish
- Teach elaboration to reorder arguments using `CallSig.arg_order`
- Improve error messages for named-arg-specific failures
- LSP: highlight matched parameter in signature help
- LSP: offer parameter-name completions in call argument lists

### Phase 4 — Foundation for defaults (separate doc)
- Relax arity filtering
- Add `default: Option<Expr>` to `Param`
- Inject default expressions for missing parameters
