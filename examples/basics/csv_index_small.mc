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
    let cols = line.split(",");
    let score = parse::parse_u64(cols[1].trim())?;
    Row {
        name: cols[0].trim(),
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

fn build_index(rows: Row[*]) -> map<string, u64> {
    var index = map<string, u64>{};
    var i: u64 = 0;
    for row in rows {
        let inserted = index.insert(row.name, i);
        if inserted {
        } else {
        }
        i += 1;
    }
    index
}

fn print_lookup(rows: Row[*], index: map<string, u64>, name: string) {
    let found = index.get(name);
    match found {
        value: u64 => {
            let row = rows[value];
            println(f"{row.name}: {row.score}");
        }
        missing: KeyNotFound => {
            println("missing");
        }
    }
}

fn main() -> () | IoError | ParseError {
    let path = "/tmp/machina_csv_index_small.txt";

    using writer = open_write(path)?.text() {
        writer.write_all("name,score\n")?;
        writer.write_all("alice,10\n")?;
        writer.write_all("bob,42\n")?;
        writer.write_all("cara,7\n")?;
    }

    using reader = open_read(path)?.text() {
        var text: string;
        reader.read_all(out text)?;

        let rows = collect_rows(text)?;
        let index = build_index(rows);
        print_lookup(rows, index, "bob");
    }
}
