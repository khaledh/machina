requires {
    std::env::args
    std::io::IoError
    std::io::open_read
    std::io::println
}

fn main() -> () | IoError {
    let argv = args();
    if argv.len < 2 {
        println("usage: freq_small <path>");
        return ();
    }

    let [_, path, ...] = argv;

    using reader = open_read(path)?.text() {
        let text = reader.read_all()?;
        var counts: map<string, u64> = map<string, u64>{};

        for line in text.lines() {
            for raw_word in line.split(" ") {
                let word = raw_word.trim();
                if word != "" {
                    match counts.get(word) {
                        value: u64 => {
                            let next = value + 1;
                            counts.insert(word, next);
                        }
                        _ => {
                            counts.insert(word, 1);
                        }
                    }
                }
            }
        }

        for (word, count) in counts {
            println(f"{word}: {count}");
        }
    }
}
