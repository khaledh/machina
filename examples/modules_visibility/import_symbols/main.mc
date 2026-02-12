requires {
    examples::modules_visibility::import_symbols::foo::Foo
    examples::modules_visibility::import_symbols::foo::make_foo
    examples::modules_visibility::import_symbols::foo::do_something
    std::io::println
}

fn main() {
    let f: Foo = make_foo(35);
    println(f.value);
    println(do_something());
}
