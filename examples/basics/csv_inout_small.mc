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

fn parse_row(line: string) -> Row | ParseError {
    let [name_text, score_text, ...] = line.split(",");
    let score = parse::parse_u64(score_text.trim())?;
    Row {
        name: name_text.trim(),
        score,
    }
}

fn collect_rows(text: string) -> Row[*] | ParseError {
    var rows: Row[*] = [];
    var first = true;

    for line in text.lines() {
        if first {
            first = false;
        } else {
            let row = parse_row(line)?;
            rows.append(move row);
        }
    }

    rows
}

fn add_row(inout rows: Row[*], sink row: Row) {
    rows.append(move row);
}

fn print_high_scores(rows: Row[*], threshold: u64) {
    for row in rows {
        if row.score > threshold {
            println(row.name);
        }
    }
}

fn main() -> () | IoError | ParseError {
    let path = "/tmp/machina_csv_inout_small.txt";

    using writer = open_write(path)?.text() {
        writer.write_all("name,score\n")?;
        writer.write_all("alice,10\n")?;
        writer.write_all("bob,42\n")?;
        writer.write_all("cara,7\n")?;
    }

    using reader = open_read(path)?.text() {
        let text = reader.read_all()?;

        let original = collect_rows(text)?;
        var working = original;
        let extra = Row {
            name: "zoe",
            score: 99,
        };
        add_row(inout working, move extra);

        println("original:");
        print_high_scores(original, 0);

        println("working:");
        print_high_scores(working, 0);
    }
}
