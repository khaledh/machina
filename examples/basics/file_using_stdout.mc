requires {
    std::io::IoError
    std::io::ReadFile
    std::io::WriteFile
    std::io::open_read
    std::io::open_write
}

fn main() -> () | IoError {
    let path = "/tmp/machina_io_using_example.txt";

    // `using` scopes the writer and closes it automatically on block exit.
    let raw_writer: WriteFile = open_write(path)?;
    using writer = raw_writer.text() {
        writer.write_all("hello from using\n")?;
    }

    // The reader follows the same pattern: open, use, then auto-close.
    let raw_reader: ReadFile = open_read(path)?;
    using reader = raw_reader.text() {
        let text = reader.read_all()?;
        println(text);
    }
}
