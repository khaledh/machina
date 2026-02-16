requires {
    std::io::println
}

typestate FileHandle {
    fields {
        path: string,
        retries: u64,
    }

    fn new(path: string) -> Closed {
        Closed { path: path, retries: 0 }
    }

    state Closed {
        fn open() -> Open {
            Open { fd: 3 }
        }
    }

    state Open {
        fields {
            fd: u64,
        }

        fn close() -> Closed {
            Closed
        }
    }
}

fn main() {
    let h0 = FileHandle::new("a.txt");
    let h1 = h0.open();
    let h2 = h1.close();
    println(f"path.len + retries = {h2.path.len + h2.retries}");
}
