requires {
    std::io::IoError
    std::io::open_read
    std::io::open_write
    std::io::print
}

fn main() -> () | IoError {
    let path = "/tmp/machina_file_read_stdout_example.txt";

    // Create a small file payload so this example is fully self-contained.
    let raw_writer = open_write(path)?;
    let writer = raw_writer.text();
    writer.write_all("hello from machina io\n")?;
    writer.close()?;

    // Read the file contents back and print the text to stdout.
    let raw_reader = open_read(path)?;
    let reader = raw_reader.text();
    var text: string;
    reader.read_all(out text)?;
    reader.close()?;

    print(text);
}
