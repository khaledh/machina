requires {
    std::env::args
    std::io::IoError
    std::io::open_read
}

fn main() -> () | IoError {
    let argv = args();
    if argv.len < 2 {
        println("usage: uniq_small <path>");
        return ();
    }

    let [_, path, ...] = argv;

    using reader = open_read(path)?.text() {
        let text = reader.read_all()?;
        let lines = text.lines();

        var first = true;
        var prev = "";

        for line in lines {
            if first || line != prev {
                println(line);
                first = false;
                prev = line;
            }
        }
    }
}
