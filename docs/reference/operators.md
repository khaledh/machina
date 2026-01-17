# Operators

This reference lists all operators in Machina, organized by category and
precedence.

## Operator Precedence

Operators are listed from lowest to highest precedence. Operators with higher
precedence bind more tightly.

| Precedence | Operators | Associativity | Description |
|------------|-----------|---------------|-------------|
| 1 (lowest) | `\|\|` | Left | Logical OR |
| 2 | `&&` | Left | Logical AND |
| 3 | `\|` | Left | Bitwise OR |
| 4 | `^` | Left | Bitwise XOR |
| 5 | `&` | Left | Bitwise AND |
| 6 | `==` `!=` `<` `<=` `>` `>=` | Left | Comparison |
| 7 | `<<` `>>` | Left | Bit shift |
| 8 | `+` `-` | Left | Addition, subtraction |
| 9 | `*` `/` `%` | Left | Multiplication, division, remainder |
| 10 | `-` `!` `~` `move` | Right (unary) | Negation, logical NOT, bitwise NOT, move |
| 11 (highest) | `[]` `.` `()` | Left | Index, field access, function call |

## Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Integer division | `a / b` |
| `%` | Remainder (modulo) | `a % b` |
| `-` (unary) | Negation | `-x` |

```
let sum = 10 + 5;      // 15
let diff = 10 - 5;     // 5
let prod = 10 * 5;     // 50
let quot = 10 / 3;     // 3 (integer division)
let rem = 10 % 3;      // 1
let neg = -42;         // -42
```

Division by zero causes a runtime error.

## Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal | `a == b` |
| `!=` | Not equal | `a != b` |
| `<` | Less than | `a < b` |
| `<=` | Less than or equal | `a <= b` |
| `>` | Greater than | `a > b` |
| `>=` | Greater than or equal | `a >= b` |

```
let eq = 5 == 5;       // true
let ne = 5 != 3;       // true
let lt = 3 < 5;        // true
let le = 5 <= 5;       // true
let gt = 5 > 3;        // true
let ge = 5 >= 5;       // true
```

## Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `&&` | Logical AND (short-circuit) | `a && b` |
| `\|\|` | Logical OR (short-circuit) | `a \|\| b` |
| `!` | Logical NOT | `!a` |

```
let both = true && false;   // false
let either = true || false; // true
let not = !true;            // false
```

Short-circuit evaluation: `&&` and `||` only evaluate the right operand if
needed.

```
// b() is only called if a() returns true
if a() && b() { ... } else { }

// b() is only called if a() returns false
if a() || b() { ... } else { }
```

## Bitwise Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Bitwise AND | `a & b` |
| `\|` | Bitwise OR | `a \| b` |
| `^` | Bitwise XOR | `a ^ b` |
| `~` | Bitwise NOT (complement) | `~a` |
| `<<` | Left shift | `a << n` |
| `>>` | Right shift | `a >> n` |

```
let and = 0b1100 & 0b1010;   // 0b1000 (8)
let or = 0b1100 | 0b1010;    // 0b1110 (14)
let xor = 0b1100 ^ 0b1010;   // 0b0110 (6)
let not = ~0b1100;           // inverts all bits
let lsh = 1 << 4;            // 16
let rsh = 16 >> 2;           // 4
```

## Assignment Operator

| Operator | Description | Example |
|----------|-------------|---------|
| `=` | Assignment | `x = value` |

```
var x = 10;
x = 20;
```

Machina does not have compound assignment operators (`+=`, `-=`, etc.).

## Access Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `.` | Field/method access | `point.x`, `obj.method()` |
| `[]` | Array index | `arr[i]` |
| `[..]` | Slice | `arr[1..4]` |

```
let p = Point { x: 10, y: 20 };
let x = p.x;              // field access

let arr = [1, 2, 3];
let first = arr[0];       // index
let slice = arr[1..3];    // slice

counter.increment();      // method call
```

## Other Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `^` (prefix) | Heap allocation | `^Point { x: 0, y: 0 }` |
| `move` | Ownership transfer | `move value` |
| `..` | Range (for `for` loops) | `0..10` |
| `::` | Scope resolution | `Color::Red` |

```
let heap = ^Point { x: 1, y: 2 };   // allocate on heap
let owned = move heap;              // transfer ownership
for i in 0..10 { }                  // range
let c = Color::Red;                 // enum variant
```

Range syntax is only valid in `for` loops and currently requires literal
bounds.

## Operator Overloading

Machina does not support operator overloading. Operators work only on built-in
types.
