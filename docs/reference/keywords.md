# Keywords

Keywords are reserved words with special meaning in Machina. They cannot be
used as identifiers.

## Reserved Words

| Keyword | Category | Meaning |
|---------|----------|---------|
| `fn` | Declarations | Function definition |
| `type` | Declarations | Type definition |
| `trait` | Declarations | Trait definition |
| `typestate` | Declarations | Typestate definition |
| `requires` | Modules | Module/symbol imports |
| `let` | Bindings | Immutable binding |
| `var` | Bindings | Mutable binding/declaration |
| `if` | Control flow | Conditional expression |
| `else` | Control flow | Alternative branch |
| `match` | Control flow | Pattern matching expression |
| `while` | Control flow | While loop |
| `for` | Control flow | For loop |
| `in` | Control flow | Iterator binding in `for` |
| `break` | Control flow | Exit loop early |
| `continue` | Control flow | Skip to next loop iteration |
| `return` | Control flow | Return from a function |
| `inout` | Parameter mode | Mutable borrow parameter |
| `out` | Parameter mode | Output parameter |
| `sink` | Parameter mode | Ownership-transfer parameter |
| `move` | Ownership | Explicit ownership transfer |
| `self` | Methods | Method receiver |
| `prop` | Properties | Property declaration in method blocks |
| `get` | Properties | Property getter accessor |
| `set` | Properties/Types | Property setter accessor and builtin `set<T>` type name |
| `map` | Types | Builtin `map<K, V>` type name |
| `range` | Types | Bounded integer type constructor |
| `bounds` | Refinements | Range refinement constructor |
| `nonzero` | Refinements | Non-zero refinement constructor |

## Reserved Literals

`true` and `false` are boolean literals and are also reserved.
