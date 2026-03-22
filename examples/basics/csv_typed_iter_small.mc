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

// Wrap CSV field arrays so the line->fields layer stays nominal while we
// still have a gap around raw `string[*]` typed match bindings.
type CsvFields = {
    values: string[*],
}

fn iter_fields(fields: CsvFields) -> CsvFields | IterDone {
    fields
}

fn iter_done_fields() -> CsvFields | IterDone {
    IterDone {}
}

type CsvFieldIter = {
    lines: string[*],
    index: u64,
}

CsvFieldIter :: {
    fn iter(self) -> CsvFieldIter {
        self
    }

    fn next(inout self) -> CsvFields | IterDone {
        if self.index < self.lines.len {
            let line = self.lines[self.index];
            self.index = self.index + 1;
            iter_fields(CsvFields { values: line.split(",") })
        } else {
            iter_done_fields()
        }
    }
}

type RowIter = {
    source: CsvFieldIter,
}

RowIter :: {
    fn iter(self) -> RowIter {
        self
    }

    fn next(inout self) -> Row | ParseError | IterDone {
        match self.source.next() {
            fields: CsvFields => {
                let [name_text, score_text, ...] = fields.values;
                let score = parse::parse_u64(score_text.trim())?;
                Row {
                    name: name_text.trim(),
                    score,
                }
            }
            done: IterDone => {
                IterDone {}
            }
        }
    }
}

fn main() -> () | IoError | ParseError {
    let path = "/tmp/machina_csv_typed_iter_small.txt";

    using writer = open_write(path)?.text() {
        writer.write_all("name,score\n")?;
        writer.write_all("alice,10\n")?;
        writer.write_all("bob,42\n")?;
        writer.write_all("cara,7\n")?;
    }

    using reader = open_read(path)?.text() {
        let text = reader.read_all()?;
        let rows = RowIter {
            source: CsvFieldIter {
                lines: text.lines(),
                index: 1,
            },
        };
        for row in rows {
            if row.score > 9 {
                println(row.name);
            }
        }
    }
}
