use crate::common::run_program;

#[test]
fn test_set_hashed_operations_runtime() {
    let source = r#"
fn main() -> u64 {
    var s = set<u64>{};

    var i = 0;
    while i < 256 {
        let v = i * 17;
        let inserted = s.insert(v);
        if inserted {
        } else {
            return 1;
        };
        i = i + 1;
    }

    i = 0;
    while i < 256 {
        let v = i * 17;
        if s.contains(v) {
        } else {
            return 2;
        };
        i = i + 1;
    }

    i = 0;
    while i < 256 {
        if i % 2 == 1 {
            let removed = s.remove(i * 17);
            if removed {
            } else {
                return 3;
            };
        } else {
        };
        i = i + 1;
    }

    i = 0;
    while i < 256 {
        let has = s.contains(i * 17);
        if i % 2 == 0 {
            if has {
            } else {
                return 4;
            };
        } else {
            if has {
                return 5;
            } else {
            };
        };
        i = i + 1;
    }

    // Force insertions into deleted slots (tombstone reuse).
    i = 1;
    while i < 256 {
        let inserted = s.insert(i * 17);
        if inserted {
        } else {
            return 6;
        };
        i = i + 2;
    }

    if s.len != 256 {
        return 7;
    } else {
    };

    return 0;
}
"#;

    let run = run_program("set_hashed_ops", source);
    assert_eq!(run.status.code(), Some(0));
}
