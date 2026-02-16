@public
type Foo = {
    value: u64,
}

@public
fn make_foo(value: u64) -> Foo {
    Foo { value: value }
}

@public
fn do_something() -> u64 {
    7
}
