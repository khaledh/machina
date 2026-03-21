requires {
    std::io::IoError
    std::io::open_read
    std::io::open_write
    std::parse as parse
    std::parse::ParseError
}

type Row = {
    name: string,
    score: u64,
}

fn main() -> () | IoError | ParseError {
    let path = "/tmp/machina_csv_small.txt";

    using writer = open_write(path)?.text() {
        writer.write_all("name,score\n")?;
        writer.write_all("alice,10\n")?;
        writer.write_all("bob,42\n")?;
        writer.write_all("cara,7\n")?;
    }

    using reader = open_read(path)?.text() {
        let text = reader.read_all()?;

        var first = true;
        for line in text.lines() {
            if first {
                first = false;
            } else {
                let [name_text, score_text, ...] = line.split(",");
                let score = parse::parse_u64(score_text.trim())?;
                let row = Row {
                    name: name_text.trim(),
                    score,
                };
                if row.score > 9 {
                    println(row.name);
                }
            }
        }
    }
}
