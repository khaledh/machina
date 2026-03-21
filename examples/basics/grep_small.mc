requires {
    std::env::args
    std::io::IoError
    std::io::open_read
}

fn main() -> () | IoError {
    let argv = args();
    if argv.len < 3 {
        println("usage: grep_small <path> <needle>");
        return ();
    }

    let path = argv[1];
    let needle = argv[2];

    using reader = open_read(path)?.text() {
        let text = reader.read_all()?;

        for line in text.lines() {
            if line.contains(needle) {
                println(line);
            }
        }
    }
}
