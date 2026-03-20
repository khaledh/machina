fn args_len() -> u64 {
    __rt_args_len()
}

fn arg(index: u64) -> string {
    var value: string;
    __rt_arg_at(out value, index);
    value
}

@public
fn args() -> string[*] {
    let len = args_len();
    var values: string[*] = [];
    var i: u64 = 0;
    while i < len {
        let value = arg(i);
        values.append(move value);
        i += 1;
    }
    values
}
