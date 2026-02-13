use crate::common::run_program;

#[test]
fn test_call_torture_high_arity_and_nested_calls() {
    let source = r#"
type Pair = {
    a: u64,
    b: u64,
}

fn add12(
    a0: u64, a1: u64, a2: u64, a3: u64, a4: u64, a5: u64,
    a6: u64, a7: u64, a8: u64, a9: u64, a10: u64, a11: u64
) -> u64 {
    // Keep arithmetic simple and linear so this test isolates call-path behavior.
    var acc = 0;
    acc = acc + a0;
    acc = acc + a1;
    acc = acc + a2;
    acc = acc + a3;
    acc = acc + a4;
    acc = acc + a5;
    acc = acc + a6;
    acc = acc + a7;
    acc = acc + a8;
    acc = acc + a9;
    acc = acc + a10;
    acc = acc + a11;
    acc
}

fn mix(p: Pair, x: u64, y: u64, z: u64, w: u64) -> u64 {
    add12(p.a, p.b, x, y, z, w, 1, 2, 3, 4, 5, 6)
}

fn main() -> u64 {
    let direct = add12(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
    if direct == 78 {
    } else {
        return 1;
    };

    let p = Pair { a: 10, b: 20 };
    let nested = mix(p, 30, 40, 50, 60);
    if nested == 231 {
    } else {
        return 2;
    };

    0
}
"#;

    let run = run_program("call_torture_high_arity_nested", source);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_call_torture_register_pressure_across_call() {
    let source = r#"
fn sum10(a0: u64, a1: u64, a2: u64, a3: u64, a4: u64, a5: u64, a6: u64, a7: u64, a8: u64, a9: u64) -> u64 {
    var acc = 0;
    acc = acc + a0;
    acc = acc + a1;
    acc = acc + a2;
    acc = acc + a3;
    acc = acc + a4;
    acc = acc + a5;
    acc = acc + a6;
    acc = acc + a7;
    acc = acc + a8;
    acc = acc + a9;
    acc
}

fn main() -> u64 {
    var i = 0;
    var acc = 0;

    while i < 64 {
        let v0 = i + 1;
        let v1 = i + 2;
        let v2 = i + 3;
        let v3 = i + 4;
        let v4 = i + 5;
        let v5 = i + 6;
        let v6 = i + 7;
        let v7 = i + 8;
        let v8 = i + 9;
        let v9 = i + 10;
        let v10 = i + 11;
        let v11 = i + 12;

        let call_sum = sum10(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9);
        var local_sum = 0;
        local_sum = local_sum + v0;
        local_sum = local_sum + v1;
        local_sum = local_sum + v2;
        local_sum = local_sum + v3;
        local_sum = local_sum + v4;
        local_sum = local_sum + v5;
        local_sum = local_sum + v6;
        local_sum = local_sum + v7;
        local_sum = local_sum + v8;
        local_sum = local_sum + v9;
        if call_sum == local_sum {
        } else {
            return 1;
        };

        // Keep values live after the call to stress caller-save preservation/spills.
        acc = acc + call_sum + v10 + v11 + v0 + v1 + v2 + v3;
        i = i + 1;
    }

    if acc > 0 {
        0
    } else {
        2
    }
}
"#;

    let run = run_program("call_torture_reg_pressure", source);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_call_torture_aggregate_args_and_returns() {
    let source = r#"
type Big = {
    a: u64,
    b: u64,
    c: u64,
    d: u64,
    e: u64,
}

fn make_big(a: u64, b: u64, c: u64, d: u64, e: u64) -> Big {
    Big { a: a, b: b, c: c, d: d, e: e }
}

fn score(x: Big, y: Big, z: Big) -> u64 {
    x.a + x.b + x.c + x.d + x.e +
    y.a + y.b + y.c + y.d + y.e +
    z.a + z.b + z.c + z.d + z.e
}

fn main() -> u64 {
    let x = make_big(1, 2, 3, 4, 5);
    let y = make_big(10, 11, 12, 13, 14);
    let z = make_big(20, 21, 22, 23, 24);

    let s1 = score(x, y, z);
    if s1 == 185 {
    } else {
        return 1;
    };

    let s2 = score(make_big(2, 4, 6, 8, 10), y, z);
    if s2 == 200 {
    } else {
        return 2;
    };

    0
}
"#;

    let run = run_program("call_torture_aggregate_args_returns", source);
    assert_eq!(run.status.code(), Some(0));
}
