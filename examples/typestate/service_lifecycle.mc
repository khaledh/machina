typestate Service {
    fields {
        retries: u64,
    }

    fn new() -> Stopped {
        Stopped { retries: 0 }
    }

    state Stopped {
        fn start() -> Starting {
            Starting
        }
    }

    state Starting {
        fn ready() -> Running {
            Running { uptime: 1 }
        }
    }

    state Running {
        fields {
            uptime: u64,
        }

        fn stop() -> Stopped {
            Stopped
        }
    }
}

fn main() -> u64 {
    let s0 = Service::new();
    let s1 = s0.start();
    let s2 = s1.ready();
    let s3 = s2.stop();
    s3.retries
}
