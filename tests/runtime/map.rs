use crate::common::run_program;

#[test]
fn test_map_hashed_operations_runtime() {
    let source = r#"
fn main() -> u64 {
    var m = map<u64, u64>{};

    var i = 0;
    while i < 256 {
        let k = i * 17;
        let inserted = m.insert(k, i);
        if inserted {
        } else {
            return 1;
        }
        i = i + 1;
    }

    if m.len != 256 {
        return 2;
    } else {
    }

    i = 0;
    while i < 256 {
        let k = i * 17;
        if m.contains_key(k) {
        } else {
            return 3;
        }
        i = i + 1;
    }

    // Re-inserting existing keys should report update (false) and keep len stable.
    i = 0;
    while i < 256 {
        let k = i * 17;
        let inserted = m.insert(k, 9999);
        if inserted {
            return 4;
        } else {
        }
        i = i + 1;
    }

    if m.len != 256 {
        return 5;
    } else {
    }

    i = 0;
    while i < 256 {
        if i % 2 == 1 {
            let removed = m.remove(i * 17);
            if removed {
            } else {
                return 6;
            }
        } else {
        }
        i = i + 1;
    }

    if m.len != 128 {
        return 7;
    } else {
    }

    i = 0;
    while i < 256 {
        let has = m.contains_key(i * 17);
        if i % 2 == 0 {
            if has {
            } else {
                return 8;
            }
        } else {
            if has {
                return 9;
            } else {
            }
        }
        i = i + 1;
    }

    m.clear();
    if m.is_empty {
    } else {
        return 10;
    }
    if m.len == 0 {
    } else {
        return 11;
    }

    return 0;
}
"#;

    let run = run_program("map_hashed_ops", source);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_map_index_get_runtime() {
    let source = r#"
fn main() -> u64 {
    let m = map<u64, u64>{1: 10, 2: 20};

    let hit = m[2];
    let hit_via_method = m.get(1);
    match hit {
        value: u64 => {
            if value == 20 {
            } else {
                return 1;
            }
        }
        _ => {
            return 1;
        }
    }
    match hit_via_method {
        value: u64 => {
            if value == 10 {
            } else {
                return 3;
            }
        }
        _ => {
            return 3;
        }
    }

    let miss = m[99];
    let miss_via_method = m.get(100);
    match miss {
        value: u64 => {
            return 2;
        }
        _ => {
        }
    }
    match miss_via_method {
        value: u64 => {
            return 4;
        }
        _ => {
        }
    }

    return 0;
}
"#;

    let run = run_program("map_index_get", source);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_map_string_keys_runtime() {
    let source = r#"
fn main() -> u64 {
    var m = map<string, u64>{};

    let cols = "alice,42\nbob,7\n".lines();
    let first = cols[0].split(",");
    let second = cols[1].split(",");

    let inserted_a = m.insert(first[0], 42);
    if inserted_a {
    } else {
        return 1;
    }

    let inserted_b = m.insert(second[0], 7);
    if inserted_b {
    } else {
        return 2;
    }

    let query = " bob ".trim();
    if m.contains_key(query) {
    } else {
        return 3;
    }

    match m.get(query) {
        value: u64 => {
            if value == 7 {
            } else {
                return 4;
            }
        }
        _ => {
            return 5;
        }
    }

    let updated = m.insert("alice", 99);
    if updated {
        return 6;
    } else {
    }

    match m.get("alice") {
        value: u64 => {
            if value == 99 {
            } else {
                return 7;
            }
        }
        _ => {
            return 8;
        }
    }

    let removed = m.remove("alice");
    if removed {
    } else {
        return 9;
    }

    if m.contains_key("alice") {
        return 10;
    } else {
    }

    m.clear();
    if m.is_empty {
    } else {
        return 11;
    }

    return 0;
}
"#;

    let run = run_program("map_string_keys", source);
    assert_eq!(run.status.code(), Some(0));
}
