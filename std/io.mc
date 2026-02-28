// I/O utilities backed by runtime intrinsics declared in prelude_decl.mc.

@public
fn print(s: string) {
    __rt_print(s, 0);
}

@public
fn println(s: string) {
    __rt_print(s, 1);
}

@public
fn println() {
    __rt_print("", 1);
}

@public
fn print(value: u64) {
    var buf = u8[0; 32];
    let len = __rt_u64_to_dec(inout buf[..], value);
    var s: string;
    __rt_string_from_bytes(out s, buf[..len]);
    __rt_print(s, 0);
}

@public
fn println(value: u64) {
    var buf = u8[0; 32];
    let len = __rt_u64_to_dec(inout buf[..], value);
    var s: string;
    __rt_string_from_bytes(out s, buf[..len]);
    __rt_print(s, 1);
}

// Converts a byte slice into a string view. Callers should only pass valid
// UTF-8 bytes when they need human-readable text output.
@public
fn bytes_to_string(bytes: u8[]) -> string {
    var s: string;
    __rt_string_from_bytes(out s, bytes);
    s
}

@public
type IoError = {
    code: u64,
}

@opaque
type ReadFile = {
    _fd: u64,
}

@opaque
type WriteFile = {
    _fd: u64,
}

@opaque
type ReadWriteFile = {
    _fd: u64,
}

@opaque
type TextReader = {
    _fd: u64,
}

@opaque
type BinaryReader = {
    _fd: u64,
}

@opaque
type TextWriter = {
    _fd: u64,
}

@opaque
type BinaryWriter = {
    _fd: u64,
}

@opaque
type TextFile = {
    _fd: u64,
}

@opaque
type BinaryFile = {
    _fd: u64,
}

// Runtime open helpers encode success as (fd + 1) so zero can remain the
// generic failure sentinel and errno can carry the OS detail.
fn decode_open_result(encoded: u64) -> u64 | IoError {
    if encoded == 0 {
        return IoError {
            code: __rt_file_last_errno(),
        };
    } else {
        encoded - 1
    }
}

fn file_read(fd: u64, inout buf: u8[]) -> u64 | IoError {
    let encoded = __rt_file_read(fd, inout buf);
    if encoded == 0 {
        return IoError {
            code: __rt_file_last_errno(),
        };
    } else {
        encoded - 1
    }
}

fn file_write(fd: u64, data: u8[]) -> u64 | IoError {
    let encoded = __rt_file_write(fd, data);
    if encoded == 0 {
        return IoError {
            code: __rt_file_last_errno(),
        };
    } else {
        encoded - 1
    }
}

fn file_close(fd: u64) -> () | IoError {
    if __rt_file_close(fd) == 0 {
        return IoError {
            code: __rt_file_last_errno(),
        };
    } else {
        ()
    }
}

// Scoped cleanup uses a best-effort close path so `using` can guarantee
// resource release without introducing scope-exit error propagation in v1.
fn file_close_ignore_error(fd: u64) {
    __rt_file_close(fd);
}

// Text read-all currently goes through a dedicated runtime helper so std::io
// can expose a simple API without re-implementing string accumulation policy at
// every callsite.
fn read_all_text_from_fd(out dst: string, fd: u64) -> () | IoError {
    dst = "";
    __rt_file_read_all_text(out dst, fd);
    let err: u64 = __rt_file_last_errno();
    if err != 0 {
        return IoError {
            code: err,
        };
    } else {
        ()
    }
}

fn write_all_binary_to_fd(fd: u64, data: u8[]) -> () | IoError {
    var off: u64 = 0;
    while off < data.len {
        let n: u64 = file_write(fd, data[off..])?;
        off += n;
    }
    ()
}

fn write_all_text_to_fd(fd: u64, text: string) -> () | IoError {
    var off: u64 = 0;
    while off < text.len {
        let n: u64 = file_write(fd, text[off..])?;
        off += n;
    }
    ()
}

@public
fn open_read(path: string) -> ReadFile | IoError {
    let fd = decode_open_result(__rt_file_open_read(path))?;
    ReadFile { _fd: fd }
}

@public
fn open_write(path: string) -> WriteFile | IoError {
    let fd = decode_open_result(__rt_file_open_write(path))?;
    WriteFile { _fd: fd }
}

@public
fn open_rw(path: string) -> ReadWriteFile | IoError {
    let fd = decode_open_result(__rt_file_open_rw(path))?;
    ReadWriteFile { _fd: fd }
}

ReadFile :: {
    @public
    fn text(sink self) -> TextReader {
        TextReader { _fd: self._fd }
    }

    @public
    fn binary(sink self) -> BinaryReader {
        BinaryReader { _fd: self._fd }
    }

    @public
    fn close(sink self) -> () | IoError {
        file_close(self._fd)
    }

    fn close_ignore_error(sink self) {
        file_close_ignore_error(self._fd);
    }
}

WriteFile :: {
    @public
    fn text(sink self) -> TextWriter {
        TextWriter { _fd: self._fd }
    }

    @public
    fn binary(sink self) -> BinaryWriter {
        BinaryWriter { _fd: self._fd }
    }

    @public
    fn close(sink self) -> () | IoError {
        file_close(self._fd)
    }

    fn close_ignore_error(sink self) {
        file_close_ignore_error(self._fd);
    }
}

ReadWriteFile :: {
    @public
    fn text(sink self) -> TextFile {
        TextFile { _fd: self._fd }
    }

    @public
    fn binary(sink self) -> BinaryFile {
        BinaryFile { _fd: self._fd }
    }

    @public
    fn close(sink self) -> () | IoError {
        file_close(self._fd)
    }

    fn close_ignore_error(sink self) {
        file_close_ignore_error(self._fd);
    }
}

TextReader :: {
    @public
    fn read_all(self, out dst: string) -> () | IoError {
        read_all_text_from_fd(out dst, self._fd)
    }

    @public
    fn close(sink self) -> () | IoError {
        file_close(self._fd)
    }

    fn close_ignore_error(sink self) {
        file_close_ignore_error(self._fd);
    }
}

BinaryReader :: {
    @public
    fn read(self, inout buf: u8[]) -> u64 | IoError {
        file_read(self._fd, inout buf)
    }

    @public
    fn close(sink self) -> () | IoError {
        file_close(self._fd)
    }

    fn close_ignore_error(sink self) {
        file_close_ignore_error(self._fd);
    }
}

TextWriter :: {
    @public
    fn write_all(self, text: string) -> () | IoError {
        write_all_text_to_fd(self._fd, text)
    }

    @public
    fn close(sink self) -> () | IoError {
        file_close(self._fd)
    }

    fn close_ignore_error(sink self) {
        file_close_ignore_error(self._fd);
    }
}

BinaryWriter :: {
    @public
    fn write_all(self, data: u8[]) -> () | IoError {
        write_all_binary_to_fd(self._fd, data)
    }

    @public
    fn close(sink self) -> () | IoError {
        file_close(self._fd)
    }

    fn close_ignore_error(sink self) {
        file_close_ignore_error(self._fd);
    }
}

TextFile :: {
    @public
    fn read_all(self, out dst: string) -> () | IoError {
        read_all_text_from_fd(out dst, self._fd)
    }

    @public
    fn write_all(self, text: string) -> () | IoError {
        write_all_text_to_fd(self._fd, text)
    }

    @public
    fn close(sink self) -> () | IoError {
        file_close(self._fd)
    }

    fn close_ignore_error(sink self) {
        file_close_ignore_error(self._fd);
    }
}

BinaryFile :: {
    @public
    fn read(self, inout buf: u8[]) -> u64 | IoError {
        file_read(self._fd, inout buf)
    }

    @public
    fn write_all(self, data: u8[]) -> () | IoError {
        write_all_binary_to_fd(self._fd, data)
    }

    @public
    fn close(sink self) -> () | IoError {
        file_close(self._fd)
    }

    fn close_ignore_error(sink self) {
        file_close_ignore_error(self._fd);
    }
}
