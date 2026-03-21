requires {
    std::io::IoError
    std::io::open_read
    std::io::open_write
}

type Counts = {
    bytes: u64,
    lines: u64,
    words: u64,
}

fn is_whitespace(b: u8) -> bool {
    match b {
        32 | 10 | 9 | 13 => true,
        _ => false,
    }
}

fn count_file(path: string) -> Counts | IoError {
    var counts = Counts {
        bytes: 0,
        lines: 0,
        words: 0,
    };
    var in_word = false;

    using reader = open_read(path)?.binary() {
        var buf = u8[0; 16];
        var done = false;

        while !done {
            let n = reader.read(inout buf[..])?;
            if n == 0 {
                done = true;
            } else {
                counts.bytes += n;
                var i: u64 = 0;
                while i < n {
                    let b = buf[i];
                    if b == 10 {
                        counts.lines += 1;
                    };
                    if is_whitespace(b) {
                        in_word = false;
                    } else if !in_word {
                        counts.words += 1;
                        in_word = true;
                    };
                    i += 1;
                }
            }
        }
    }

    counts
}

fn main() -> () | IoError {
    let path = "/tmp/machina_wc_small.txt";

    using writer = open_write(path)?.text() {
        writer.write_all("hello world\nsmall file\n")?;
    }

    let counts = count_file(path)?;
    println(f"bytes={counts.bytes} lines={counts.lines} words={counts.words}");
    ()
}
