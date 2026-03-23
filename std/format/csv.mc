@public
type CsvParseOptions = {
    skip: u64,
    delimiter: string,
}

@public
type CsvFormatOptions = {
    header: string,
    delimiter: string,
}

@public
type CsvParseIter<T, E> = {
    source: string[*],
    parse: fn(string[*]) -> T | E,
    opts: CsvParseOptions,
    index: u64,
}

CsvParseIter<T, E> :: {
    @public
    fn iter(self) -> CsvParseIter<T, E> {
        self
    }

    @public
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

@public
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

@public
type CsvFormatIter<S, T> = {
    source: S,
    format: fn(T) -> string,
    opts: CsvFormatOptions,
    wrote_header: bool,
}

CsvFormatIter<S, T> :: {
    @public
    fn iter(self) -> CsvFormatIter<S, T> {
        self
    }

    @public
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

@public
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
