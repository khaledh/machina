// I/O utilities backed by runtime intrinsics declared in prelude_decl.mc.

@public
fn print(s: string) {
    __rt_print(s, 0);
}

@public
fn println(s: string) {
    __rt_print(s, 1);
}

@public
fn println() {
    __rt_print("", 1);
}

@public
fn print(value: u64) {
    var buf = u8[0; 32];
    let len = __rt_u64_to_dec(inout buf[..], value);
    var s: string;
    __rt_string_from_bytes(out s, buf[..len]);
    __rt_print(s, 0);
}

@public
fn println(value: u64) {
    var buf = u8[0; 32];
    let len = __rt_u64_to_dec(inout buf[..], value);
    var s: string;
    __rt_string_from_bytes(out s, buf[..len]);
    __rt_print(s, 1);
}
