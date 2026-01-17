fn main() {
    var base = 10;
    let add = |x: u64| -> u64 {
        base + x
    };

    let a = add(5);
    let b = add(7);

    println(f"a={a}, b={b}");

    var counter = 0;
    let bump = || -> u64 {
        counter = counter + 1;
        counter
    };
    let c1 = bump();
    let c2 = bump();
    counter = counter + 1;

    println(f"c1={c1}, c2={c2}, counter={counter}");
}
