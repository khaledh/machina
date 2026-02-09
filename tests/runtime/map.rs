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
        };
        i = i + 1;
    }

    if m.len != 256 {
        return 2;
    } else {
    };

    i = 0;
    while i < 256 {
        let k = i * 17;
        if m.contains_key(k) {
        } else {
            return 3;
        };
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
        };
        i = i + 1;
    }

    if m.len != 256 {
        return 5;
    } else {
    };

    i = 0;
    while i < 256 {
        if i % 2 == 1 {
            let removed = m.remove(i * 17);
            if removed {
            } else {
                return 6;
            };
        } else {
        };
        i = i + 1;
    }

    if m.len != 128 {
        return 7;
    } else {
    };

    i = 0;
    while i < 256 {
        let has = m.contains_key(i * 17);
        if i % 2 == 0 {
            if has {
            } else {
                return 8;
            };
        } else {
            if has {
                return 9;
            } else {
            };
        };
        i = i + 1;
    }

    m.clear();
    if m.is_empty {
    } else {
        return 10;
    };
    if m.len == 0 {
    } else {
        return 11;
    };

    return 0;
}
"#;

    let run = run_program("map_hashed_ops", source);
    assert_eq!(run.status.code(), Some(0));
}
