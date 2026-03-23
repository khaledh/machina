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

type CsvParseOptions = {
    skip: u64,
    delimiter: string,
}

type CsvFormatOptions = {
    header: string,
    delimiter: string,
}

fn parse_row(fields: string[*]) -> InputRow | ParseError {
    let [name_text, score_text, ...] = fields;
    let score = parse::parse_u64(score_text.trim())?;
    InputRow {
        name: name_text.trim(),
        score,
    }
}

fn format_row(row: OutputRow) -> string {
    f"{row.name},{row.grade}"
}

fn grade_of(score: u64) -> string {
    if score > 9 {
        "pass"
    } else {
        "fail"
    }
}

fn grade_row(row: InputRow) -> OutputRow {
    OutputRow {
        name: row.name,
        grade: grade_of(row.score),
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
            other => other,
        }
    }
}

fn try_map_values<S, In, Out, E>(
    source: S,
    f: fn(In) -> Out | E,
) -> TryMapIter<S, In, Out, E> {
    TryMapIter { source, f }
}

type MapIter<S, In, Out> = {
    source: S,
    f: fn(In) -> Out,
}

MapIter<S, In, Out> :: {
    fn iter(self) -> MapIter<S, In, Out> {
        self
    }

    fn next(inout self) -> Out | IterDone {
        match self.source.next() {
            item: In => {
                let f = self.f;
                f(item)
            }
            done: IterDone => IterDone {},
            other => other,
        }
    }
}

fn map_values<S, In, Out>(source: S, f: fn(In) -> Out) -> MapIter<S, In, Out> {
    MapIter { source, f }
}

type CsvParseIter<T, E> = {
    source: string[*],
    parse: fn(string[*]) -> T | E,
    opts: CsvParseOptions,
    index: u64,
}

CsvParseIter<T, E> :: {
    fn iter(self) -> CsvParseIter<T, E> {
        self
    }

    fn next(inout self) -> T | E | IterDone {
        while self.index < self.opts.skip {
            self.index = self.index + 1;
        }

        while self.index < self.source.len {
            let line = self.source[self.index];
            self.index = self.index + 1;
            if line.trim() == "" {
                continue;
            }
            let parse = self.parse;
            return parse(line.split(","));
        }
        IterDone {}
    }
}

fn from_csv<T, E>(
    source: string[*],
    parse: fn(string[*]) -> T | E,
    opts: CsvParseOptions,
) -> CsvParseIter<T, E> {
    CsvParseIter {
        source,
        parse,
        opts,
        index: 0,
    }
}

type CsvFormatIter<S, T> = {
    source: S,
    format: fn(T) -> string,
    opts: CsvFormatOptions,
    wrote_header: bool,
}

CsvFormatIter<S, T> :: {
    fn iter(self) -> CsvFormatIter<S, T> {
        self
    }

    fn next(inout self) -> string | IterDone {
        if !self.wrote_header {
            self.wrote_header = true;
            self.opts.header
        } else {
            match self.source.next() {
                item: T => {
                    let format = self.format;
                    format(item)
                }
                done: IterDone => IterDone {},
                other => other,
            }
        }
    }
}

fn to_csv<S, T>(
    source: S,
    format: fn(T) -> string,
    opts: CsvFormatOptions,
) -> CsvFormatIter<S, T> {
    CsvFormatIter {
        source,
        format,
        opts,
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
        let pipeline = to_csv(
            map_values(
                from_csv(
                    text.lines(),
                    parse_row,
                    CsvParseOptions {
                        skip: 1,
                        delimiter: ",",
                    },
                ),
                grade_row,
            ),
            format_row,
            CsvFormatOptions {
                header: "name,grade",
                delimiter: ",",
            },
        );

        using output_writer = open_write(output_path)?.text() {
            write_lines(output_writer, pipeline)?;
        }
    }

    println(f"wrote {output_path}");
}
