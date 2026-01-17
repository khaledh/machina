type Point = { x: u64, y: u64 }
type AddFn = fn(u64) -> u64;

fn main() {
    move_primitive();
    move_aggregate();
    move_heap();
    borrow_mut();
    borrow_after_last_call();

    // reject_use_after_move();
    // reject_escape_store();
    // reject_borrow_mutation_conflict();
    // reject_borrow_move_conflict();
}

fn move_primitive() {
    // Move-capturing a primitive.
    var base = 10;
    let add = [move base] |x: u64| -> u64 {
        base + x
    };

    let a = add(5);
    let b = add(7);

    println(f"a={a}, b={b}");
}

fn move_aggregate() {
    // Move-capturing an aggregate.
    let p = Point { x: 1, y: 2 };
    let sum = [move p] || -> u64 {
        p.x + p.y
    };
    let s = sum();
    println(f"sum={s}");
}

fn move_heap() {
    // Move-capturing a heap value.
    let hp = ^Point { x: 3, y: 4 };
    let hsum = [move hp] || -> u64 {
        hp.x + hp.y
    };
    let hs = hsum();
    println(f"(heap) sum={hs}");
}

fn borrow_mut() {
    // Borrow-capturing a mutable local (no capture list).
    var counter = 0;
    let bump = || -> u64 {
        counter = counter + 1;
        counter
    };
    let c1 = bump();
    let c2 = bump();
    counter = counter + 1;

    println(f"c1={c1}, c2={c2}, counter={counter}");
}

fn borrow_after_last_call() {
    // Allowed: base used after the closure's last call.
    var n = 1;
    let get = || -> u64 n;
    let v = get();
    n = n + 1;
    println(f"v={v}, n={n}");
}

// Rejected: move-captured base used after closure creation.
fn reject_use_after_move() {
    // let moved = 5;
    // let bad = [move moved] || -> u64 moved;
    // let again = moved;
    // let bad_val = bad();
}

// Rejected: captured closures cannot be stored/returned (escape).
fn reject_escape_store() {
    // var base = 10;
    // let add = [move base] |x: u64| -> u64 {
    //     base + x
    // };
    // var store: AddFn[2];
    // store = [add, add];
    // store[0] = add;
}

// Rejected: mutating a captured base while the closure is still live.
fn reject_borrow_mutation_conflict() {
    // var y = 1;
    // let read_y = || -> u64 y;
    // y = 2;
    // read_y();
}

// Rejected: moving a captured base while the closure is still live.
fn reject_borrow_move_conflict() {
    // let h = ^Point { x: 9, y: 9 };
    // let read_h = || -> u64 h.x;
    // let moved_h = move h;
    // read_h();
}
