requires {
    std.io as io
}

// String literals

fn main() {
  // Literal string
  let literal = "I'm a string literal";

   // Return string from function
   let message = greeting();

  // Pass string to function
   consume(message);
}

fn greeting() -> string {
    "Hello, Machina!"
}

fn consume(s: string) {
  io.println(s);
}
