requires {
    std::io::IoError
    std::io::open_read
    std::io::open_write
    std::parse as parse
    std::parse::ParseError
}

type InputRow = {
    name: string,
    score: u64,
}

type OutputRow = {
    name: string,
    grade: string,
}

type CsvFields = {
    values: string[*],
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
            CsvFields { values: line.split(",") }
        } else {
            IterDone {}
        }
    }
}

fn csv_fields(lines: string[*], start_index: u64) -> CsvFieldIter {
    CsvFieldIter {
        lines,
        index: start_index,
    }
}

fn parse_input_row(fields: CsvFields) -> InputRow | ParseError {
    let [name_text, score_text, ...] = fields.values;
    let score = parse::parse_u64(score_text.trim())?;
    InputRow {
        name: name_text.trim(),
        score,
    }
}

fn grade_of(score: u64) -> string {
    if score > 9 {
        "pass"
    } else {
        "fail"
    }
}

type TryMapIter<S, In, Out, E> = {
    source: S,
    f: fn(In) -> Out | E,
}

TryMapIter<S, In, Out, E> :: {
    fn iter(self) -> TryMapIter<S, In, Out, E> {
        self
    }

    fn next(inout self) -> Out | E | IterDone {
        match self.source.next() {
            item: In => {
                let f = self.f;
                return f(item);
            }
            done: IterDone => IterDone {},
        }
    }
}

fn try_map_values<S, In, Out, E>(
    source: S,
    f: fn(In) -> Out | E,
) -> TryMapIter<S, In, Out, E> {
    TryMapIter { source, f }
}

type GradeTransform = {
    source: TryMapIter<CsvFieldIter, CsvFields, InputRow, ParseError>,
}

GradeTransform :: {
    fn iter(self) -> GradeTransform {
        self
    }

    fn next(inout self) -> OutputRow | ParseError | IterDone {
        match self.source.next() {
            row: InputRow => OutputRow {
                name: row.name,
                grade: grade_of(row.score),
            },
            err: ParseError => err,
            done: IterDone => IterDone {},
        }
    }
}

fn grade_rows(source: TryMapIter<CsvFieldIter, CsvFields, InputRow, ParseError>) -> GradeTransform {
    GradeTransform { source }
}

type CsvEncoder = {
    source: GradeTransform,
    wrote_header: bool,
}

CsvEncoder :: {
    fn iter(self) -> CsvEncoder {
        self
    }

    fn next(inout self) -> string | ParseError | IterDone {
        if !self.wrote_header {
            self.wrote_header = true;
            "name,grade"
        } else {
            match self.source.next() {
                row: OutputRow => f"{row.name},{row.grade}",
                err: ParseError => err,
                done: IterDone => IterDone {},
            }
        }
    }
}

fn csv_encode(source: GradeTransform) -> CsvEncoder {
    CsvEncoder {
        source,
        wrote_header: false,
    }
}

fn write_lines(writer: TextWriter, lines: Iterable<string>) -> () | IoError | ParseError {
    for line in lines {
        writer.write_all(line)?;
        writer.write_all("\n")?;
    }
}

fn main() -> () | IoError | ParseError {
    let input_path = "/tmp/machina_csv_grade_rewrite_input.txt";
    let output_path = "/tmp/machina_csv_grade_rewrite_output.txt";

    using writer = open_write(input_path)?.text() {
        writer.write_all("name,score\n")?;
        writer.write_all("alice,10\n")?;
        writer.write_all("bob,42\n")?;
        writer.write_all("cara,7\n")?;
    }

    using reader = open_read(input_path)?.text() {
        let text = reader.read_all()?;
        let pipeline = csv_encode(grade_rows(try_map_values(
            csv_fields(text.lines(), 1),
            parse_input_row,
        )));

        using output_writer = open_write(output_path)?.text() {
            write_lines(output_writer, pipeline)?;
        }
    }

    println(f"wrote {output_path}");
}
