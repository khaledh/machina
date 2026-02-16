# Machina Syntax Highlighting - Theme Mapping

This document shows how Machina language constructs map to TextMate scopes, which in turn map to colors in your VSCode theme.

## Scope Mapping Table

| Language Element | Example | TextMate Scope | Typical Color (Dark+ Theme) |
|-----------------|---------|----------------|----------------------------|
| **Comments** | `// comment` | `comment.line.double-slash` | Green |
| **Keywords (Control)** | `if`, `else`, `match`, `while`, `for`, `break`, `continue`, `return` | `keyword.control` | Purple/Magenta |
| **Keywords (Other)** | `fn`, `type`, `trait`, `protocol`, `typestate`, `let`, `var` | `keyword.other` | Blue |
| **Storage Modifiers** | `inout`, `out`, `sink`, `move` | `storage.modifier` | Blue |
| **Booleans** | `true`, `false` | `constant.language.boolean` | Blue |
| **Attributes** | `@public`, `@machines` | `entity.name.tag` | Yellow/Gold |
| **Function Names (Definition)** | `fn add(...)` | `entity.name.function` | Yellow |
| **Function Calls** | `println(...)` | `entity.name.function` | Yellow |
| **Type Names (User-defined)** | `Point`, `Connection` | `entity.name.type` | Green (Bright) |
| **Type Names (Primitive)** | `u64`, `i32`, `bool`, `string` | `support.type.primitive` | Cyan/Blue |
| **String Literals** | `"hello"` | `string.quoted.double` | Orange/Red |
| **Character Literals** | `'A'` | `string.quoted.single` | Orange/Red |
| **Escape Sequences** | `\n`, `\t` | `constant.character.escape` | Yellow/Gold |
| **Format String Expressions** | `f"Value: {x}"` | `meta.embedded.expression` | White (with braces highlighted) |
| **Numbers (Decimal)** | `42`, `1_000` | `constant.numeric.decimal` | Light Green |
| **Numbers (Binary)** | `0b1010` | `constant.numeric.binary` | Light Green |
| **Numbers (Octal)** | `0o755` | `constant.numeric.octal` | Light Green |
| **Numbers (Hex)** | `0xFF` | `constant.numeric.hex` | Light Green |
| **Logical Operators** | `&&`, `\|\|`, `!` | `keyword.operator.logical` | White/Light Gray |
| **Comparison Operators** | `==`, `!=`, `<`, `>` | `keyword.operator.comparison` | White/Light Gray |
| **Arithmetic Operators** | `+`, `-`, `*`, `/`, `%` | `keyword.operator.arithmetic` | White/Light Gray |
| **Bitwise Operators** | `&`, `\|`, `^`, `~`, `<<`, `>>` | `keyword.operator.bitwise` | White/Light Gray |
| **Assignment** | `=` | `keyword.operator.assignment` | White/Light Gray |
| **Arrows** | `->`, `=>` | `keyword.operator.arrow` | White/Light Gray |
| **Punctuation (Separators)** | `,`, `;` | `punctuation.separator` | White |
| **Punctuation (Accessors)** | `.`, `..`, `::` | `punctuation.accessor` | White |
| **Braces** | `{`, `}` | `punctuation.section.braces` | Yellow/Gold |
| **Brackets** | `[`, `]` | `punctuation.section.brackets` | Yellow/Gold |
| **Parentheses** | `(`, `)` | `punctuation.section.parens` | Yellow/Gold |

## Example with Highlighted Scopes

```machina
// This is a comment
           ↑ comment.line.double-slash.machina

@public
 ↑ entity.name.tag.machina

fn add(x: u64, y: u64) -> u64 {
↑  ↑       ↑        ↑     ↑  ↑
│  │       │        │     │  └─ support.type.primitive.machina
│  │       │        │     └─── keyword.operator.arrow.machina
│  │       │        └───────── support.type.primitive.machina
│  │       └───────────────── support.type.primitive.machina
│  └─────────────────────── entity.name.function.machina
└───────────────────────── keyword.other.machina

    let result = x + y;
        ↑        ↑   ↑
        │        │   └─ keyword.operator.arithmetic.machina
        │        └──── (identifier)
        └─────────── keyword.other.machina

    if result > 100 {
    ↑         ↑
    │         └──── keyword.operator.comparison.machina
    └────────────── keyword.control.machina

        println("Large!");
                 ↑
                 └──── string.quoted.double.machina
    }
    result
}
```

## Testing Your Theme

1. Open `sample.mc` in VSCode
2. Try different themes from the Command Palette (`Cmd+K Cmd+T` or `Ctrl+K Ctrl+T`)
3. Popular themes that work well:
   - Dark+ (default dark)
   - Light+ (default light)
   - Monokai
   - Solarized Dark/Light
   - One Dark Pro
   - Dracula
   - Nord

## Creating a Custom Theme

To create a custom color scheme for Machina:

1. Create a new theme in `.vscode/extensions/`
2. Add custom color rules for Machina scopes:

```json
{
  "tokenColors": [
    {
      "scope": "entity.name.tag.machina",
      "settings": {
        "foreground": "#FFD700",
        "fontStyle": "bold"
      }
    },
    {
      "scope": "keyword.other.machina",
      "settings": {
        "foreground": "#569CD6"
      }
    }
  ]
}
```

## Semantic Highlighting

Note: This extension currently provides **syntactic** highlighting via TextMate grammars. For more advanced **semantic** highlighting (based on type information, scope analysis, etc.), the Language Server Protocol implementation would need to provide semantic tokens.
