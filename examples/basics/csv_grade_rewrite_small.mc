requires {
    std::format::csv::CsvFormatOptions
    std::format::csv::CsvParseOptions
    std::format::csv::from_csv
    std::format::csv::to_csv
    std::io::IoError
    std::io::open_read
    std::io::open_write
    std::iter::map
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

    let csv_parse_opts = CsvParseOptions {
        skip: 1,
        delimiter: ",",
    };
    let csv_format_opts = CsvFormatOptions {
        header: "name,grade",
        delimiter: ",",
    };

    using reader = open_read(input_path)?.text() {
        let text = reader.read_all()?;
        let pipeline: Iterable<string> =
            text.lines()
            |> from_csv(parse_row, csv_parse_opts)
            |> map(grade_row)
            |> to_csv(format_row, csv_format_opts);

        using output_writer = open_write(output_path)?.text() {
            write_lines(output_writer, pipeline)?;
        }
    }

    println(f"wrote {output_path}");
}
