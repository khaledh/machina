# Arrays and Slices

Arrays are fixed-size sequences. Slices are non-owning views into arrays.

## Arrays

### Creating Arrays

Array literals list elements in brackets:

```
let numbers = [1, 2, 3, 4, 5];
let bools = [true, false, true];
```

Repeat syntax creates arrays with the same value:

```
let zeros = [0; 10];           // [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
let flags = [false; 8];        // 8 false values
```

Typed literals specify the element type:

```
let bytes = u8[1, 2, 3];       // u8[3]
let small = u8[0; 32];         // 32 u8 zeros
```

### Array Types

The type is written as `T[N]` where `T` is the element type and `N` is the
length:

```
let arr: u64[5] = [1, 2, 3, 4, 5];
```

### Indexing

Access elements by index (0-based):

```
let arr = [10, 20, 30];
let first = arr[0];     // 10
let second = arr[1];    // 20
```

For mutable arrays, elements can be assigned:

```
var arr = [1, 2, 3];
arr[0] = 100;
```

### Bounds Checking

Array indices are checked at runtime. Out-of-bounds access causes an error:

```
let arr = [1, 2, 3];
// let x = arr[10];    // runtime error: index out of bounds
```

All bounds checks are runtime today, even for constant indices.

## Multi-Dimensional Arrays

### Creating Multi-Dimensional Arrays

Nest array literals for multiple dimensions:

```
let matrix = [[1, 2, 3], [4, 5, 6]];    // 2 rows, 3 columns
```

The type is `T[R, C]` for a 2D array with R rows and C columns:

```
let grid: u64[2, 3] = [[1, 2, 3], [4, 5, 6]];
```

### Multi-Dimensional Indexing

Access elements with multiple indices:

```
let matrix = [[1, 2, 3], [4, 5, 6]];
let elem = matrix[1, 2];    // 6 (row 1, column 2)
```

Assign to elements in mutable arrays:

```
var matrix = [[1, 2], [3, 4]];
matrix[0, 1] = 10;
```

## Slices

Slices are non-owning views into arrays. They have type `T[]`.

### Creating Slices

Use range syntax to create slices:

```
let arr = [10, 20, 30, 40, 50];
let mid = arr[1..4];      // [20, 30, 40]
```

Ranges are half-open (include start, exclude end).

### Slice Syntax Variations

```
let arr = [10, 20, 30, 40, 50];

let full = arr[..];       // entire array
let head = arr[..3];      // [10, 20, 30]
let tail = arr[2..];      // [30, 40, 50]
let mid = arr[1..4];      // [20, 30, 40]
```

### Slice Type

Slices have type `T[]` without a length:

```
fn sum(xs: u64[]) -> u64 {
    var total = 0;
    for x in xs {
        total = total + x;
    }
    total
}

let arr = [1, 2, 3, 4, 5];
let s = sum(arr[..]);     // pass entire array as slice
let t = sum(arr[1..4]);   // pass partial slice
```

### Slice Indexing

Access slice elements by index:

```
let arr = [10, 20, 30, 40, 50];
let s = arr[1..4];
let x = s[0];    // 20
let y = s[2];    // 40
```

### Subslicing

Slices can be sliced again:

```
let arr = [1, 2, 3, 4, 5];
let s1 = arr[0..4];       // [1, 2, 3, 4]
let s2 = s1[1..3];        // [2, 3]
```

## Slice Rules

Slices have restrictions to ensure safety:

### Rule 1: Targets Must Be Lvalues

You can only slice variables, not temporaries:

```
let arr = [1, 2, 3];
let s = arr[0..2];           // ok: arr is a variable

// let s = [1, 2, 3][0..2];  // error: slicing a temporary
```

### Rule 2: No Escape

Slices cannot be returned or stored in aggregates:

```
fn bad(arr: u64[3]) -> u64[] {
    arr[0..2]    // error: slice cannot be returned
}

fn also_bad() {
    let arr = [1, 2, 3];
    let s = arr[..];
    // let pair = (s, s);    // error: slice cannot be stored
}
```

### Rule 3: No Mutation While Live

The source cannot be mutated while a slice is live:

```
var arr = [1, 2, 3];
let s = arr[..];
// arr[0] = 10;    // error: arr is borrowed by s
use(s);
```

Mutation is allowed after the slice's last use:

```
var arr = [1, 2, 3];
let s = arr[..];
let x = s[0];      // last use of s
arr[0] = 10;       // ok: s is no longer live
```

## Iterating Over Arrays

### For Loop

```
let items = [10, 20, 30];
for x in items {
    println(x);
}
```

### With Index

Use a range to iterate with indices:

```
let items = [10, 20, 30];
for i in 0..3 {
    println(f"items[{i}] = {items[i]}");
}
```

## Two-Dimensional Slice Example

```
let arr2d = [[1, 2, 3], [4, 5, 6]];
let rows = arr2d[0..2];       // slice of rows: u64[3][]
let row = rows[0];            // first row: u64[3]
let elems = row[0..3];        // slice of elements: u64[]
let elem = elems[0];          // 1
```

## Passing Arrays to Functions

Arrays are borrowed by default when passed to functions. Use slices when you
want a flexible-length view:

```
// Read-only borrow of a fixed-size array
fn process_array(arr: u64[100]) { ... }

// Read-only borrow of a slice view
fn process_slice(arr: u64[]) { ... }
```
