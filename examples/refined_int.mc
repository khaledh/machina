type NonZeroInt = u64: bounds(1, 10);
type SignedDelta = i8: bounds(-5, 6) & nonzero;

type MidRange = u64: bounds(50, 100);

fn takes_nonzero(x: NonZeroInt) -> u64 { x }
fn takes_delta(x: SignedDelta) -> i8 { x }

fn main() {
    let ok: NonZeroInt = 5;
    takes_nonzero(ok);

    let d: SignedDelta = -3;
    takes_delta(d);

    // Compile-time errors (uncomment to test):
    // let bad_zero: NonZeroInt = 0;
    // let bad_bounds: SignedDelta = 0;
    // let bad_delta: SignedDelta = -6;
}
