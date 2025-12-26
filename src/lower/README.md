# Lowering (AST -> MCIR)

Lowering converts AST expressions into MCIR values and statements. It preserves
the scalar/aggregate distinction and makes side effects explicit.

## Strategy

- Scalar expressions lower to `Operand` or `Rvalue`.
- Aggregate expressions lower into a destination `Place<Aggregate>` using
  projections to fill fields in-place.
- Expressions allowed only at block level (let/var/assign/while/for) are
  rejected in value position.

## Lowering Algebra

Lowering can be viewed as a composition over a few orthogonal axes:

- Origin: AST nodes (`Expr`, `StmtExpr`, `Block`, `Pattern`) vs. IR values.
- Value category: lvalue (addressable) vs. rvalue (computed).
- Size class: scalar vs. aggregate.
- Representation: `Operand` (scalar value), `Place` (addressable storage),
  `Rvalue` (scalar computation that must be emitted).
- Destination: return a value vs. write into a known destination.

From those axes, the core rules are:

- `Expr × LValue -> Place` (place-producing expressions).
- `Expr × RValue × Scalar -> Operand` (scalar value).
- `Expr × RValue × Aggregate -> Place` (materialized aggregate).
- `Expr × RValue × Aggregate × Into(dst) -> emit writes into dst`.
- `StmtExpr -> side effects, returns Unit`.

Common derived forms:

- `Value = Operand | Place<Aggregate>` (what the lowerer calls `ExprValue`).
- `Temp` = a fresh `Place` used to materialize an rvalue.
- `Literal` (AST) vs. `Const` (IR) are separate layers; literals become consts.

Naming follows the same algebra:

- `lower_*` consumes AST nodes.
- `emit_*` operates on IR-only inputs (no AST in the signature).
- `*_into` writes into a destination place.
- `*_to_temp` allocates a temp to materialize a value.

Examples:

- `x + y` (scalar rvalue) lowers to an `Operand` via `lower_expr_to_operand`.
- `arr[i]` (lvalue) lowers to `Place<Aggregate>` via `lower_expr_to_agg_place`.
- `[1, 2, 3]` (aggregate rvalue) lowers into a temp with `lower_expr_to_agg_temp`
  or directly into a destination via `lower_expr_into_agg_place`.
- `if cond { a } else { b }` lowers to a temp `Place` with join blocks via
  `lower_if_expr` and `lower_if_expr_into`.

Note: `Const::GlobalAddr` is treated as an address-sized `u64` in MCIR until
pointer types are introduced.
