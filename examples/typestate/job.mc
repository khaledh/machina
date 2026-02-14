typestate Job {
    fields {
        attempts: u64,
    }

    fn new() -> Created {
        Created { attempts: 0 }
    }

    state Created {
        fn start() -> Running {
            Running { pid: 1 }
        }
    }

    state Running {
        fields {
            pid: u64,
        }

        fn succeed() -> Succeeded {
            Succeeded
        }

        fn fail() -> Failed {
            Failed { code: 1 }
        }
    }

    state Succeeded {}

    state Failed {
        fields {
            code: u64,
        }
    }
}

fn main() -> u64 {
    let j0 = Job::new();
    let j1 = j0.start();
    let j2 = j1.succeed();
    j2.attempts
}
