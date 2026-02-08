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
    let inserted_i = if inserted { 1 } else { 0 };
    let inserted_dup_i = if inserted_dup { 1 } else { 0 };
    let has_two_i = if has_two { 1 } else { 0 };
    let removed_two_i = if removed_two { 1 } else { 0 };
    let has_two_after_i = if has_two_after { 1 } else { 0 };

    println(f"len = {s.len}");
    println(f"cap = {s.capacity}");
    println(f"is_empty = {if s.is_empty { 1 } else { 0 }}");
    println(f"inserted = {inserted_i}");
    println(f"inserted_dup = {inserted_dup_i}");
    println(f"has_two = {has_two_i}");
    println(f"removed_two = {removed_two_i}");
    println(f"has_two_after = {has_two_after_i}");

    s.clear();
    println(f"after clear len = {s.len}");
    println(f"after clear empty = {if s.is_empty { 1 } else { 0 }}");
}
