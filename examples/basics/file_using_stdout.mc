requires {
    std::io::IoError
    std::io::open_read
    std::io::open_write
    std::io::println
}

fn main() -> () | IoError {
    let path = "/tmp/machina_io_using_example.txt";

    // `using` scopes the writer and closes it automatically on block exit.
    using writer = open_write(path)?.text() {
        writer.write_all("hello from using\n")?;
    }

    // The reader follows the same pattern: open, use, then auto-close.
    using reader = open_read(path)?.text() {
        var text: string;
        reader.read_all(out text)?;
        println(text);
    }
}
