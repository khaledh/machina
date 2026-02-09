requires {
    std::io::println
}

fn main() {
    var m = map<u64, u64>{1: 10, 2: 20};
    let inserted = m.insert(3, 30);
    let inserted_existing = m.insert(2, 22);
    let has_one = m.contains_key(1);
    let removed_one = m.remove(1);
    let has_one_after = m.contains_key(1);
    let read_two = m[2];
    let read_missing = m[99];
    let read_three_via_get = m.get(3);
    let read_missing_via_get = m.get(100);

    println(f"len = {m.len}");
    println(f"cap = {m.capacity}");
    println(f"is_empty = {m.is_empty}");
    println(f"inserted = {inserted}");
    println(f"inserted_existing = {inserted_existing}");
    println(f"has_one = {has_one}");
    println(f"removed_one = {removed_one}");
    println(f"has_one_after = {has_one_after}");
    match read_two {
        value: u64 => println(f"read_two = {value}"),
        missing: KeyNotFound => println("read_two missing"),
    };
    match read_missing {
        value: u64 => println(f"read_missing = {value}"),
        missing: KeyNotFound => println("read_missing missing"),
    };
    match read_three_via_get {
        value: u64 => println(f"read_three_via_get = {value}"),
        missing: KeyNotFound => println("read_three_via_get missing"),
    };
    match read_missing_via_get {
        value: u64 => println(f"read_missing_via_get = {value}"),
        missing: KeyNotFound => println("read_missing_via_get missing"),
    };

    m.clear();
    println(f"after clear len = {m.len}");
    println(f"after clear empty = {m.is_empty}");
}
