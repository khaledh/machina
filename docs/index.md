# Machina Documentation

Machina is a compiled systems programming language with mutable value semantics
and explicit ownership management. It targets ARM64 and emphasizes predictable
memory behavior without hidden aliasing.

## Getting Started

- [Getting Started](getting-started.md) — Clone the repo, build, and run your
  first program

## Learning Machina

- [Language Tour](tour.md) — A quick walkthrough of core features
- [Guide](guide/) — Topic-focused guides:
  - [Types](guide/types.md) — Primitive, composite, heap, union, and collection types
  - [Variables](guide/variables.md) — Bindings, mutability, destructuring
  - [Control Flow](guide/control-flow.md) — Conditionals, loops, pattern matching
  - [Functions](guide/functions.md) — Signatures, parameter modes, overloading
  - [Structs and Enums](guide/structs-enums.md) — User-defined data types
  - [Methods](guide/methods.md) — Method blocks, `self`, and properties
  - [Typestate](guide/typestate.md) — State-aware APIs encoded in types
  - [Closures](guide/closures.md) — Anonymous functions and captures
  - [Arrays and Slices](guide/arrays-slices.md) — Fixed arrays, slices, and dynamic arrays
  - [Strings](guide/strings.md) — String literals, formatting, mutation
  - [Modules and Imports](guide/modules.md) — `requires`, symbols, aliases, visibility
  - [Error Handling](guide/error-handling.md) — Union returns and `?`
  - [Memory Safety](guide/mem-safety.md) — Ownership, moves, borrows, initialization

## Reference

- [Operators](reference/operators.md) — Operator forms and precedence
- [Keywords](reference/keywords.md) — Reserved words
- [Grammar](reference/grammar.md) — Formal syntax

## Examples

- [Annotated Examples](examples/annotated-examples.md) — Curated code samples
  with explanations
