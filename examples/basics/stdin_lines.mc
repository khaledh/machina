requires {
    std::io::EndOfInput
    std::io::IoError
    std::io::println
    std::io::stdin
}

fn main() -> () | IoError {
    while true {
        match stdin().read_line() {
            line: string => println(line),
            done: EndOfInput => {
                return ();
            }
            err: IoError => {
                return err;
            }
        }
    }
}
