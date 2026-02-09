requires {
    std::io::println
}

fn main() {
    var m = {1: 10, 2: 20};
    let inserted = m.insert(3, 30);
    let inserted_existing = m.insert(2, 22);
    let has_one = m.contains_key(1);
    let removed_one = m.remove(1);
    let has_one_after = m.contains_key(1);

    println(f"len = {m.len}");
    println(f"cap = {m.capacity}");
    println(f"is_empty = {m.is_empty}");
    println(f"inserted = {inserted}");
    println(f"inserted_existing = {inserted_existing}");
    println(f"has_one = {has_one}");
    println(f"removed_one = {removed_one}");
    println(f"has_one_after = {has_one_after}");

    m.clear();
    println(f"after clear len = {m.len}");
    println(f"after clear empty = {m.is_empty}");
}
