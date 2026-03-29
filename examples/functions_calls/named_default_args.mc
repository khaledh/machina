// named arguments + default parameter values

type Config = {}

fn connect(host: string = "localhost") -> u64 {
    80
}

fn connect(host: string, port: u64, timeout: u64 = 30) -> u64 {
    port + timeout
}

Config :: {
    fn connect(self, host: string, port: u64 = 443, timeout: u64 = 30) -> u64 {
        port + timeout
    }
}

fn main() {
    let cfg = Config {};

    // First overload
    println(connect());                          // host uses default
    println(connect("example.com"));

    // Second overload
    println(connect("example.com", port: 8080));  // port uses default
    println(connect("example.com", timeout: 5, port: 8080)); // named args, out of order

    // Method call
    println(cfg.connect("example.com", timeout: 5));  // port uses default
    println(cfg.connect("example.com", 8080, 5));     // positional only
}
