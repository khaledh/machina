requires {
    std::io::println
    std::io as io
}

// Break, continue, and return

fn last_even_below(limit: u64) -> u64 {
    var i = 0;
    var last = 0;
    while i < limit {
        if i % 2 == 1 {
            i += 1;
            continue;
        } else {
            last = i;
            i += 1;
        }
    }
    return last;
}

fn sum_until(limit: u64) -> u64 {
    var acc = 0;
    var i = 0;
    while i < limit {
        if i == 5 {
            break;
        } else {
            acc += i;
            i += 1;
        }
    }
    acc
}

fn main() {
    let a = last_even_below(9);
    let b = sum_until(10);
    println(a);
    println(b);
}
