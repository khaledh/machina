# Keywords

Keywords are reserved words with special meaning in Machina. They cannot be
used as identifiers.

## Reserved Words

| Keyword | Category | Meaning |
|---------|----------|---------|
| `fn` | Declarations | Function definition |
| `type` | Declarations | Type definition |
| `trait` | Declarations | Trait definition |
| `machine` | Declarations | Hosted machine definition |
| `hosts` | Machine | Machine hosting clause |
| `action` | Machine | Action override handler in a machine |
| `trigger` | Machine | Trigger handler in a machine |
| `on` | Machine | Machine-level message handler |
| `emit` | Machine | Emit an event value from a handler |
| `send` | Machine | Send a message to another machine |
| `requires` | Modules | Module/symbol imports |
| `using` | Resource mgmt | Scoped resource block (auto-close on exit) |
| `as` | Aliasing/Roles | Import alias or role specification |
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
| `map` | Types/Functions | Builtin `map<K, V>` type; also `map()` iterator adapter |
| `range` | Types | Bounded integer type constructor |
| `bounds` | Refinements | Range refinement constructor |
| `nonzero` | Refinements | Non-zero refinement constructor |

## Attributes

| Attribute | Meaning |
|-----------|---------|
| `@public` | Export a declaration from its module |
| `@opaque` | Export a type with hidden internal fields |
| `@linear` | Mark a type as linear (must be consumed, state-tracked) |
| `@final` | Mark a linear type state as terminal (no outgoing transitions) |

## Reserved Literals

`true` and `false` are boolean literals and are also reserved.

