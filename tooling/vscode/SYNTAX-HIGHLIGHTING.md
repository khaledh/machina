# Machina Syntax Highlighting

This VSCode extension provides comprehensive syntax highlighting for the Machina programming language.

## Features

The TextMate grammar (`syntaxes/machina.tmLanguage.json`) provides highlighting for:

### Keywords
- **Control flow**: `if`, `else`, `match`, `while`, `for`, `break`, `continue`, `return`
- **Declarations**: `fn`, `type`, `trait`, `protocol`, `typestate`, `let`, `var`
- **Modifiers**: `inout`, `out`, `sink`, `move`, `self`
- **Other**: `prop`, `get`, `set`, `map`, `requires`, `role`, `flow`, `on`, `emit`, `reply`
- **Refinements**: `range`, `bounds`, `nonzero`

### Literals
- **Numbers**:
  - Binary: `0b1010_1010`
  - Octal: `0o755`
  - Hexadecimal: `0xFF_00`
  - Decimal: `42`, `1_000_000`
- **Booleans**: `true`, `false`
- **Strings**: `"hello"`, with escape sequences `\n`, `\t`, etc.
- **Format strings**: `f"Value: {x}"`
- **Characters**: `'A'`, `'\n'`

### Types
- **Primitive types**: `u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `bool`, `char`, `string`, `unit`
- **User-defined types**: Any identifier starting with uppercase (e.g., `Point`, `Connection`)

### Attributes
- Python-style attributes: `@public`, `@machines`, `@opaque`, `@intrinsic`
- With arguments: `@link_name("foo")`

### Functions
- Function definitions: `fn add(x: u64, y: u64) -> u64`
- Function calls: `println("hello")`

### Operators
- **Logical**: `&&`, `||`, `!`
- **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Bitwise**: `&`, `|`, `^`, `~`, `<<`, `>>`
- **Assignment**: `=`
- **Arrows**: `->`, `=>`

### Comments
- Line comments: `// comment`

### Editor Features

The `language-configuration.json` provides:
- **Auto-closing pairs**: `{}`, `[]`, `()`, `""`, `''`
- **Bracket matching**: Highlights matching brackets
- **Comment toggling**: `Cmd+/` (Mac) or `Ctrl+/` (Windows/Linux)
- **Auto-indentation**: Smart indentation based on braces and parentheses
- **Code folding**: Fold/unfold code blocks

## Color Themes

The syntax highlighting works with any VSCode color theme. The grammar uses standard TextMate scopes that themes recognize:

- `keyword.control` → Control flow keywords
- `keyword.other` → Other keywords
- `entity.name.function` → Function names
- `entity.name.type` → Type names
- `entity.name.tag` → Attributes
- `string.quoted` → String literals
- `constant.numeric` → Numbers
- `constant.language` → Booleans
- `comment.line` → Comments
- `keyword.operator` → Operators

## Testing

Open the sample file `sample.mc` in VSCode to see the syntax highlighting in action.

## Building

To rebuild the extension after making changes:

```bash
cd tooling/vscode
npm run build
```

Then reload the VSCode window or press F5 to test the extension in a new Extension Development Host window.
