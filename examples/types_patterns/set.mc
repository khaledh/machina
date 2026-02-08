requires {
    std::io::println
}

fn main() {
    var s = {1, 2, 2, 3};
    let inserted = s.insert(4);
    let inserted_dup = s.insert(4);
    let has_two = s.contains(2);
    let removed_two = s.remove(2);
    let has_two_after = s.contains(2);

    println(f"len = {s.len}");
    println(f"cap = {s.capacity}");
    println(f"is_empty = {s.is_empty}");
    println(f"inserted = {inserted}");
    println(f"inserted_dup = {inserted_dup}");
    println(f"has_two = {has_two}");
    println(f"removed_two = {removed_two}");
    println(f"has_two_after = {has_two_after}");

    s.clear();
    println(f"after clear len = {s.len}");
    println(f"after clear empty = {s.is_empty}");
}
