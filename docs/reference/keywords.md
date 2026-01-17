# Keywords

Keywords are reserved words that have special meaning in Machina. They cannot
be used as identifiers.

## List of Keywords

| Keyword | Description |
|---------|-------------|
| `else` | Alternative branch in conditional |
| `false` | Boolean false literal |
| `fn` | Function definition |
| `for` | For loop |
| `if` | Conditional expression |
| `in` | Iterator binding in for loops |
| `inout` | Mutable borrow parameter mode |
| `let` | Immutable variable binding |
| `match` | Pattern matching expression |
| `move` | Ownership transfer |
| `out` | Output parameter mode |
| `range` | Bounded integer type |
| `self` | Method receiver |
| `sink` | Ownership transfer parameter mode |
| `true` | Boolean true literal |
| `type` | Type definition |
| `var` | Mutable variable binding |
| `while` | While loop |

## Keywords by Category

### Declarations

- `fn` — Define a function
- `type` — Define a type (alias, struct, or enum)
- `let` — Bind an immutable variable
- `var` — Bind or declare a mutable variable

### Control Flow

- `if` — Conditional expression
- `else` — Alternative branch
- `match` — Pattern matching
- `while` — While loop
- `for` — For loop
- `in` — Used with `for` to iterate

### Parameter Modes

- `inout` — Mutable borrow (read and write)
- `out` — Uninitialized output (write only)
- `sink` — Ownership transfer

### Ownership

- `move` — Transfer ownership of a value

### Types

- `range` — Bounded integer type constructor

### Methods

- `self` — The receiver in method definitions

### Literals

- `true` — Boolean true
- `false` — Boolean false

## Reserved for Future Use

The following words are not currently keywords but may be reserved:

- `return`
- `break`
- `continue`
- `pub`
- `mod`
- `use`
- `impl`
- `trait`
- `where`
- `async`
- `await`
- `const`
- `static`
- `mut`
- `ref`
