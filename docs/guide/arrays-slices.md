# Arrays and Slices

Arrays are fixed-size values. Slices are borrowed views. Machina also supports
owned growable arrays (`T[*]`).

## Fixed Arrays (`T[N]`)

```mc
let nums = [1, 2, 3, 4];
let zeros = [0; 8];
let bytes = u8[1, 2, 3];

let first = nums[0];
```

Indexing is bounds-checked at runtime.

Multi-dimensional arrays use multiple dimensions:

```mc
let grid: i32[2, 3] = [[1, 2, 3], [4, 5, 6]];
let x = grid[1, 2];
```

## Slices (`T[]`)

Slices are non-owning views into arrays and dynamic arrays.

```mc
fn sum(xs: i32[]) -> i32 {
    var acc = 0;
    for x in xs {
        acc = acc + x;
    }
    acc
}

let arr = [10, 20, 30, 40, 50];
let mid = arr[1..4];
let head = arr[..2];
let tail = arr[3..];
let all = arr[..];
```

Ranges are half-open (`start` inclusive, `end` exclusive).

## Growable Arrays (`T[*]`)

`T[*]` values are owned and mutable containers.

```mc
var xs: i32[*] = [1, 2, 3];
xs.append(4);

let n = xs.len;
let cap = xs.capacity;
let empty = xs.is_empty;
```

You can pass a growable array where a slice is expected:

```mc
let total = sum(xs);
```

## Borrow Rules with Slices

A live slice borrow prevents mutation of the base array/dyn-array.

```mc
var xs: i32[*] = [1, 2, 3, 4];
let s = xs[1..3];

// xs.append(5);   // error: base mutated while slice borrow is live
let a = s[0];      // last use of s
xs.append(5);      // ok after last use
```

Slices cannot escape their safe region:

```mc
fn bad(arr: i32[4]) -> i32[] {
    arr[0..2]   // error
}
```

## Set/Map Contrast

Arrays and dynamic arrays are ordered/indexed.

- Use `set<T>` when you need uniqueness.
- Use `map<K, V>` for key/value lookup.
