// Runtime extern: implemented in runtime/print.c
fn __mc_print(s: string, newline: u64);

// Stdlib wrappers (string only for now)
fn print(s: string) {
  __mc_print(s, 0);
}

fn println(s: string) {
  __mc_print(s, 1);
}

fn println() {
  __mc_print("", 1);
}
